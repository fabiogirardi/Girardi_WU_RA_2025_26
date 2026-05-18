# =============================================================================
# Appendix I — Constructing Risk-Neutral Moments from Swaption Prices
# Rogers (2026), LSE Working Paper
# =============================================================================
# CHANGES FROM v2:
#   [I-1] g2_moment2 / g2_moment3 rewritten transparently as
#         g''(K) = r''(K) f(K) + 2 r'(K) f'(K) + r(K) f''(K).
#         (The v2 expressions were arithmetically equivalent but confusingly
#          formatted, e.g. "2 * rd$r1 * 2 * (K-mu)" instead of "4 * rd$r1 * (K-mu)".)
#   [I-2] Tail extrapolation is now controlled by the caller, with a much
#         wider default (1000 bp instead of "max observed strike + 25 bp").
#         The tails matter for the third moment.
#   [I-3] moments_for_date() no longer caps extrap_bps at the strike grid;
#         it uses a fixed 1000 bp extension by default, with the lower side
#         clipped only to keep K > 0.
#   [I-4] Skewness uses (sigma^*)^3 in the denominator with sign protection
#         to handle near-zero variance gracefully (no behaviour change at
#         normal variance levels, just defensive).
# =============================================================================

# =============================================================================
# SECTION 1: HELPER FUNCTIONS
# =============================================================================

#' Bachelier (Normal) Option Pricing Formula
#'
#' Prices a European option under the normal (Bachelier) model where
#' dF = sigma * dW (arithmetic BM). Standard model for swaptions in normal vol.
#'
#' All arguments must be in the same units (rate units after /10000).
#'
#' Returns the annuity-normalised forward option price C(K)/A_fwd in rate
#' units, equal to E^A[max(y - K, 0)] under the forward annuity measure.
bachelier_price <- function(F, K, sigma, T_exp, type = "call") {

  sigma_T <- sigma * sqrt(T_exp)

  if (sigma_T < 1e-10) {
    if (type == "call") return(max(F - K, 0))
    if (type == "put")  return(max(K - F, 0))
  }

  d     <- (F - K) / sigma_T
  phi_d <- dnorm(d)

  if (type == "call") {
    (F - K) * pnorm(d)  + sigma_T * phi_d
  } else if (type == "put") {
    (K - F) * pnorm(-d) + sigma_T * phi_d
  } else {
    stop("type must be 'call' or 'put'")
  }
}

#' Forward Annuity Price — Quarterly Compounding
#'
#' A(y) = (1/freq) * sum_{j=1}^{T*freq} (1 + y/freq)^(-j)
annuity_price <- function(y, T_tenor, freq = 4) {
  periods          <- seq_len(T_tenor * freq)
  discount_factors <- (1 + y / freq)^(-periods)
  sum(discount_factors) / freq
}

#' Annuity Adjustment Ratio r(K) = A_fwd / A(K)
annuity_ratio <- function(K, A_fwd, T_tenor, freq = 4) {
  A_fwd / annuity_price(K, T_tenor, freq)
}

# =============================================================================
# SECTION 2: SECOND DERIVATIVES OF g(y) FOR EACH MOMENT
# =============================================================================
# Breeden-Litzenberger:
#   E^A[g(y)] = g(F) + int_{-inf}^{F} g''(K) [P(K)/A_fwd] dK
#                    + int_{F}^{inf}  g''(K) [C(K)/A_fwd] dK
# where g(y) = r(y) f(y), r(y) = A_fwd / A(y).
#
# Product rule:  g''(K) = r''(K) f(K) + 2 r'(K) f'(K) + r(K) f''(K)
#
# Targets:
#   Moment 1: f(y) = y                  -> rn_mean (not used in estimation)
#   Moment 2: f(y) = (y - mu)^2         -> rn_variance
#   Moment 3: f(y) = (y - mu)^3         -> rn_third_moment

annuity_ratio_derivs <- function(K, A_fwd, T_tenor, freq = 4, h = 1e-5) {
  r    <- annuity_ratio(K,     A_fwd, T_tenor, freq)
  r_up <- annuity_ratio(K + h, A_fwd, T_tenor, freq)
  r_dn <- annuity_ratio(K - h, A_fwd, T_tenor, freq)
  list(
    r  = r,
    r1 = (r_up - r_dn) / (2 * h),
    r2 = (r_up - 2 * r + r_dn) / h^2
  )
}

# Moment 1: f(y) = y           -> f'(y) = 1, f''(y) = 0
g2_moment1 <- function(K, A_fwd, T_tenor, freq = 4) {
  rd <- annuity_ratio_derivs(K, A_fwd, T_tenor, freq)
  rd$r2 * K + 2 * rd$r1 * 1 + rd$r * 0
}

# [I-1] Moment 2: f(y) = (y-mu)^2  -> f'(y) = 2(y-mu), f''(y) = 2
g2_moment2 <- function(K, mu, A_fwd, T_tenor, freq = 4) {
  rd <- annuity_ratio_derivs(K, A_fwd, T_tenor, freq)
  rd$r2 * (K - mu)^2 + 2 * rd$r1 * 2 * (K - mu) + rd$r * 2
}

# [I-1] Moment 3: f(y) = (y-mu)^3  -> f'(y) = 3(y-mu)^2, f''(y) = 6(y-mu)
g2_moment3 <- function(K, mu, A_fwd, T_tenor, freq = 4) {
  rd <- annuity_ratio_derivs(K, A_fwd, T_tenor, freq)
  rd$r2 * (K - mu)^3 + 2 * rd$r1 * 3 * (K - mu)^2 + rd$r * 6 * (K - mu)
}

# =============================================================================
# SECTION 3: SWAPTION PRICE SURFACE FROM IMPLIED VOLS
# =============================================================================

#' Build Swaption Price Grid from Normal (Bachelier) Implied Vols
#'
#' For one date/tenor/maturity: takes the vector of strikes (in bp offsets
#' from ATM) and normal implied vols (in RATE units after /10000), interpolates
#' to a fine strike grid, and computes annuity-normalised call/put prices
#' and the g'' values for each moment at each strike.
#'
#' [I-2] Default extrap_bps widened to 1000 bp (was 500). The third moment
#' is sensitive to the upper tail; the paper assumes constant implied vol
#' beyond the outermost observed strike (200 bp), and integrates "to infinity"
#' (in practice, until the integrand vanishes).
build_price_surface <- function(F, strikes_bp, vols, T_exp, T_tenor,
                                n_grid = 2000, extrap_bps = 1000,
                                freq = 4) {

  strikes <- F + strikes_bp / 10000

  # Grid: from near-zero (rates must be positive) to F + extrap_bps
  K_min  <- max(F - extrap_bps / 10000, 1e-4)
  K_max  <- F + extrap_bps / 10000
  K_grid <- seq(K_min, K_max, length.out = n_grid)

  # Linearly interpolate vols across observed strikes; flat extrapolation
  valid     <- strikes > 0
  vol_fn    <- approxfun(strikes[valid], vols[valid], rule = 2)
  vols_grid <- vol_fn(K_grid)

  # Annuity-normalised option prices = E^A[max(y - K, 0)]
  calls <- numeric(n_grid)
  puts  <- numeric(n_grid)
  for (i in seq_along(K_grid)) {
    calls[i] <- bachelier_price(F, K_grid[i], vols_grid[i], T_exp, "call")
    puts[i]  <- bachelier_price(F, K_grid[i], vols_grid[i], T_exp, "put")
  }

  # Forward annuity price (approx: A evaluated at the forward) and g'' values
  A_fwd <- annuity_price(F, T_tenor, freq)
  mu    <- F  # risk-neutral mean proxy = forward (paper Section I.2)

  g2_m1 <- numeric(n_grid)
  g2_m2 <- numeric(n_grid)
  g2_m3 <- numeric(n_grid)
  for (i in seq_along(K_grid)) {
    g2_m1[i] <- g2_moment1(K_grid[i],     A_fwd, T_tenor, freq)
    g2_m2[i] <- g2_moment2(K_grid[i], mu, A_fwd, T_tenor, freq)
    g2_m3[i] <- g2_moment3(K_grid[i], mu, A_fwd, T_tenor, freq)
  }

  prices <- tibble(
    K     = K_grid,
    vol   = vols_grid,
    call  = calls,
    put   = puts,
    g2_m1 = g2_m1,
    g2_m2 = g2_m2,
    g2_m3 = g2_m3
  )

  list(prices = prices, F = F, A_fwd = A_fwd, mu = mu,
       T_exp = T_exp, T_tenor = T_tenor)
}

# =============================================================================
# SECTION 4: BREEDEN-LITZENBERGER INTEGRATION
# =============================================================================

#' Compute Risk-Neutral Moments via Breeden-Litzenberger Integration
#'
#' Outputs:
#'   rn_variance     in rate^2  (convert to ppt^2 with *100^2)
#'   rn_third_moment in rate^3  (convert to ppt^3 with *100^3)
#'   rn_skewness     dimensionless
compute_rn_moments <- function(surface) {

  df    <- surface$prices
  F     <- surface$F
  A_fwd <- surface$A_fwd
  mu    <- surface$mu

  below <- df %>% filter(K <= F)
  above <- df %>% filter(K >= F)

  # ---- Moment 2: E*[(y - mu)^2] = RN Variance ----
  # g(F) = r(F) (F-mu)^2 = 0 since mu = F
  int_below_m2 <- pracma::trapz(below$K, below$g2_m2 * below$put)
  int_above_m2 <- pracma::trapz(above$K, above$g2_m2 * above$call)
  rn_variance  <- int_below_m2 + int_above_m2

  # ---- Moment 3: E*[(y - mu)^3] = RN Third Moment ----
  # g(F) = r(F) (F-mu)^3 = 0 since mu = F
  int_below_m3 <- pracma::trapz(below$K, below$g2_m3 * below$put)
  int_above_m3 <- pracma::trapz(above$K, above$g2_m3 * above$call)
  rn_third     <- int_below_m3 + int_above_m3

  # ---- Moment 1: E*[y] = RN Mean (returned for completeness, not used) ----
  g_F_m1       <- F  # since A(F) = A_fwd => r(F) = 1, so g(F) = F
  int_below_m1 <- pracma::trapz(below$K, below$g2_m1 * below$put)
  int_above_m1 <- pracma::trapz(above$K, above$g2_m1 * above$call)
  rn_mean      <- g_F_m1 + int_below_m1 + int_above_m1

  # [I-4] Skewness with sign protection
  rn_skewness <- if (is.finite(rn_variance) && rn_variance > 1e-12) {
    rn_third / rn_variance^(3/2)
  } else NA_real_

  list(
    rn_mean         = rn_mean,
    rn_variance     = rn_variance,
    rn_third_moment = rn_third,
    rn_skewness     = rn_skewness
  )
}

# =============================================================================
# SECTION 5: MAIN PIPELINE — APPLY TO FULL TIME SERIES
# =============================================================================

#' Compute RN Moments for One Date
#'
#' [I-3] extrap_bps default is now a fixed 1000 bp, with the lower side
#' constrained only to keep K > 0. The previous "max observed strike + 25 bp"
#' rule was too tight for the third-moment integral.
moments_for_date <- function(date_data, F, T_exp, T_tenor, extrap_bps = 1000) {

  date_data  <- date_data %>% arrange(strike_bp)
  # Cap lower extension at F - 1 bp from zero so K stays positive
  extrap_use <- min(extrap_bps, F * 10000 - 1)

  surface <- build_price_surface(
    F          = F,
    strikes_bp = date_data$strike_bp,
    vols       = date_data$vol_normal,
    T_exp      = T_exp,
    T_tenor    = T_tenor,
    extrap_bps = extrap_use
  )

  compute_rn_moments(surface)
}

#' Apply Moment Computation Across All Dates
#'
#' @param vol_data      date, tenor, maturity, strike_bp, vol_normal (rate units)
#' @param forward_rates date, maturity (T_exp), tenor, forward_rate (rate units)
#' @param T_exp         Swaption maturity in years (e.g. 0.25 = 1q, 1 = 1y)
#' @param T_tenor       Swap tenor in years
compute_moments_panel <- function(vol_data, forward_rates, T_exp, T_tenor,
                                  extrap_bps = 1000) {

  vol_filtered <- vol_data      %>% filter(maturity == T_exp, tenor == T_tenor)
  fwd_filtered <- forward_rates %>% filter(maturity == T_exp, tenor == T_tenor)

  dates <- unique(vol_filtered$date)
  cat("Computing moments for", length(dates), "dates (T_exp =", T_exp,
      ", T_tenor =", T_tenor, ")...\n")

  results <- map_dfr(dates, function(d) {

    dv <- vol_filtered %>% filter(date == d)
    F  <- fwd_filtered %>% filter(date == d) %>% pull(forward_rate)

    if (length(F) == 0 || nrow(dv) < 3) return(NULL)

    tryCatch({
      m <- moments_for_date(dv, F, T_exp, T_tenor, extrap_bps = extrap_bps)
      tibble(
        date            = d,
        tenor           = T_tenor,
        maturity        = T_exp,
        rn_mean         = m$rn_mean,
        rn_variance     = m$rn_variance,
        rn_third_moment = m$rn_third_moment,
        rn_skewness     = m$rn_skewness
      )
    }, error = function(e) {
      warning(paste("Failed for date", d, ":", e$message))
      NULL
    })
  })

  results
}
