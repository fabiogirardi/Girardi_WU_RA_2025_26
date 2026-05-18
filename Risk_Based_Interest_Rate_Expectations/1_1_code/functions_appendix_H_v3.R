# =============================================================================
# Appendix H — Constructing Swap Forward Rates
# Rogers (2026), LSE Working Paper
# =============================================================================
# v3: UNCHANGED from v2. No bugs were identified in this file during the
# methodology review. All Appendix-H-related fixes are in the main script
# (use of par yields SVENPY, not zero-coupon SVENY).
#
# Functions provided:
#   bootstrap_treasury_df()       Step 1: Treasury discount factors from par yields
#   df_to_forward_rates()         Step 1: Discount factors -> forward rates
#   df_from_forwards()            Step 2: Forward rates -> discount factors
#   implied_swap_rate()           Step 2: Discount factors -> implied swap rate
#   solve_segment_spread()        Step 2: Solve one LIBOR-Treasury spread
#   construct_libor_curve()       Step 3: Full LIBOR curve for one date
#   compute_swap_forward()        Step 3: One forward swap rate
#   construct_forward_rate_panel()Step 4: Full daily panel of forward rates
#   extend_variance_pre2002()     Step 5: Pre-2002 RV extension (not used here)
#   validate_curve()              Step 6: Curve fitting validation
#   run_appendix_H()              Convenience wrapper
# =============================================================================

# =============================================================================
# STEP 1: TREASURY DISCOUNT FACTORS FROM GSW PAR YIELDS
# =============================================================================
# GSW data has par yields at integer maturities 1-30 years.
# We bootstrap quarterly discount factors from these.
#
# Par yield relationship:
#   1 = (y_T / freq) * sum_{j=1}^{T*freq} d(j/freq) + d(T)
# Rearranging for d(T):
#   d(T) = (1 - (y_T/freq) * sum_{j=1}^{T*freq - 1} d(j/freq))
#          / (1 + y_T/freq)

#' Bootstrap Quarterly Treasury Discount Factors from GSW Par Yields
#'
#' @param par_yields  Named numeric vector, names = maturity in years (integers),
#'                    values = par yields in RATE units (e.g. 0.05 not 5%)
#'                    Must cover integer maturities e.g. 1,2,...,30
#'                    Use SVENPY columns from GSW (par yields), NOT SVENY.
#' @param freq        Payment frequency per year (default 4 = quarterly)
#' @param max_mat     Maximum maturity to bootstrap to
#'
#' @return Named numeric vector of discount factors, names = time in years
bootstrap_treasury_df <- function(par_yields, freq = 4, max_mat = 30) {

  # All quarterly time steps
  times   <- seq(1/freq, max_mat, by = 1/freq)
  n_steps <- length(times)
  df      <- numeric(n_steps)
  names(df) <- round(times, 6)

  # Integer maturities where we observe par yields
  obs_mats <- sort(as.numeric(names(par_yields)))

  # For each quarterly time step, interpolate par yield then bootstrap
  for (i in seq_along(times)) {
    t <- times[i]

    # Linearly interpolate par yield at this maturity
    # rule=2 gives flat extrapolation beyond endpoints
    y_t <- approx(obs_mats, par_yields[as.character(obs_mats)],
                  xout = t, rule = 2)$y

    if (i == 1) {
      # First period: simple discounting
      df[i] <- 1 / (1 + y_t / freq)
    } else {
      # Bootstrap: solve for df[i] given all previous df
      # 1 = (y_t/freq) * sum_{j=1}^{i} df[j] + df[i] * (not yet included)
      # => df[i] * (1 + y_t/freq) = 1 - (y_t/freq) * sum_{j=1}^{i-1} df[j]
      sum_prev <- sum(df[1:(i-1)])
      df[i]    <- (1 - (y_t / freq) * sum_prev) / (1 + y_t / freq)
    }

    # Sanity check: discount factors must be positive and declining
    if (df[i] <= 0 || (i > 1 && df[i] >= df[i-1])) {
      warning(paste("Discount factor issue at t =", t, ": df =", df[i]))
      df[i] <- max(df[i], df[i-1] * 0.99)  # small safeguard
    }
  }

  df
}

#' Convert Discount Factors to Quarterly Forward Rates
#'
#' f(t-1/freq, t) = freq * (d(t-1/freq)/d(t) - 1)
#' These are the risk-free (Treasury) forward rates at each quarterly step.
#'
#' @param df    Named numeric vector of discount factors from bootstrap_treasury_df()
#' @param freq  Payment frequency
#'
#' @return Named numeric vector of quarterly forward rates
df_to_forward_rates <- function(df, freq = 4) {

  n      <- length(df)
  fwd    <- numeric(n)
  names(fwd) <- names(df)

  # First period forward rate
  fwd[1] <- (1/df[1] - 1) * freq

  # Subsequent periods
  for (i in 2:n) {
    fwd[i] <- (df[i-1]/df[i] - 1) * freq
  }

  fwd
}

# =============================================================================
# STEP 2: SOLVE FOR PIECEWISE-CONSTANT LIBOR-TREASURY SPREADS
# =============================================================================
# Per Appendix H: "I solve for the series of quarterly spreads between LIBOR
# forward rates and Treasury forward rates that successfully match the observed
# swap yields. The LIBOR forward spread is piecewise constant between
# available swap tenors."
#
# For each segment (prev_tenor, curr_tenor]:
#   LIBOR forward(t) = Treasury forward(t) + spread_k   (constant within segment)
#
# The spread_k is chosen so that the implied LIBOR swap rate at curr_tenor
# matches the observed LIBOR swap rate.

#' Build Discount Factors from a Vector of Forward Rates
#'
#' d(t_i) = prod_{j=1}^{i} 1/(1 + f_j/freq)
df_from_forwards <- function(fwd_rates, freq = 4) {
  df <- cumprod(1 / (1 + fwd_rates / freq))
  names(df) <- names(fwd_rates)
  df
}

#' Implied Par Swap Rate from Discount Factors
#'
#' y(T) = (1 - d(T)) / (1/freq * sum_{j=1}^{T*freq} d(j/freq))
implied_swap_rate <- function(df, T_tenor, freq = 4) {

  times     <- as.numeric(names(df))
  pmt_times <- seq(1/freq, T_tenor, by = 1/freq)

  # Interpolate discount factors at payment times
  dfs <- approx(times, df, xout = pmt_times, rule = 2)$y

  if (any(is.na(dfs)) || any(dfs <= 0)) return(NA)

  d_T         <- dfs[length(dfs)]  # terminal discount factor
  annuity     <- sum(dfs) / freq   # annuity value

  (1 - d_T) / annuity
}

#' Solve for LIBOR-Treasury Spread in One Tenor Segment
#'
#' Uses uniroot() to find the constant spread s such that the implied
#' LIBOR swap rate at T_curr matches the observed rate y_obs.
#'
#' @param tsy_fwd      Full Treasury forward curve (named vector)
#' @param libor_fwd    LIBOR forward curve so far (modified in place for prev segments)
#' @param seg_idx      Indices of the current segment in the forward curve
#' @param T_curr       Current tenor (years) — the target maturity
#' @param y_obs        Observed LIBOR swap rate at T_curr
#' @param freq         Payment frequency
solve_segment_spread <- function(tsy_fwd, libor_fwd, seg_idx,
                                 T_curr, y_obs, freq = 4) {

  objective <- function(s) {
    # Trial LIBOR forward curve: current segment gets spread s
    libor_fwd_trial          <- libor_fwd
    libor_fwd_trial[seg_idx] <- tsy_fwd[seg_idx] + s

    # Rebuild LIBOR discount factors
    libor_df_trial <- df_from_forwards(libor_fwd_trial, freq)

    # Implied swap rate vs observed
    y_impl <- implied_swap_rate(libor_df_trial, T_curr, freq)

    if (is.na(y_impl)) return(1)  # return large error if something breaks
    y_impl - y_obs
  }

  # LIBOR-Treasury spread is typically small: search [-200bp, +200bp]
  result <- tryCatch(
    uniroot(objective, interval = c(-0.02, 0.02), tol = 1e-10),
    error = function(e) {
      warning(paste("uniroot failed at T =", T_curr, ":", e$message))
      list(root = 0)
    }
  )

  result$root
}

#' Construct Full LIBOR Forward Curve for One Date
#'
#' Implements the full Appendix H procedure for a single date.
#'
#' @param libor_spot   Named numeric vector: names = tenors (years),
#'                     values = LIBOR swap rates in rate units
#'                     e.g. c("1"=0.05, "2"=0.055, ..., "10"=0.04)
#' @param tsy_par      Named numeric vector: names = maturities (years),
#'                     values = GSW PAR yields in rate units (SVENPY, /100)
#' @param freq         Payment frequency (default 4)
#' @param max_mat      Maximum maturity for curve construction
#'
#' @return List with:
#'   $libor_fwd  : full quarterly LIBOR forward curve
#'   $libor_df   : full LIBOR discount factors
#'   $tsy_fwd    : Treasury forward curve (for reference)
#'   $spreads    : solved spread per tenor segment
construct_libor_curve <- function(libor_spot, tsy_par,
                                  freq = 4, max_mat = 31) {

  # ---- Step 1: Treasury discount factors and forwards ----
  tsy_df  <- bootstrap_treasury_df(tsy_par, freq, max_mat)
  tsy_fwd <- df_to_forward_rates(tsy_df, freq)
  times   <- as.numeric(names(tsy_fwd))

  # ---- Step 2: Sort available LIBOR tenors ----
  tenors <- sort(as.numeric(names(libor_spot)))

  # Initialise LIBOR forwards = Treasury forwards (zero spread)
  libor_fwd <- tsy_fwd

  spreads     <- numeric(length(tenors))
  names(spreads) <- tenors
  prev_tenor  <- 0

  # ---- Step 3: Solve spread segment by segment ----
  # Per Appendix H: "a constant LIBOR spread between 3-months and 1-year,
  # and a (different) constant spread between 1-year and 2-years" etc.

  for (k in seq_along(tenors)) {
    T_curr <- tenors[k]
    y_obs  <- libor_spot[as.character(T_curr)]

    if (is.na(y_obs)) {
      prev_tenor <- T_curr
      next
    }

    # Indices of this segment: (prev_tenor, T_curr]
    seg_idx <- which(times > prev_tenor & times <= T_curr)

    if (length(seg_idx) == 0) {
      prev_tenor <- T_curr
      next
    }

    # Solve for spread in this segment
    s_k <- solve_segment_spread(
      tsy_fwd   = tsy_fwd,
      libor_fwd = libor_fwd,
      seg_idx   = seg_idx,
      T_curr    = T_curr,
      y_obs     = y_obs,
      freq      = freq
    )

    spreads[k]         <- s_k
    libor_fwd[seg_idx] <- tsy_fwd[seg_idx] + s_k
    prev_tenor         <- T_curr
  }

  # ---- Step 4: LIBOR discount factors ----
  libor_df <- df_from_forwards(libor_fwd, freq)

  list(
    libor_fwd = libor_fwd,
    libor_df  = libor_df,
    tsy_fwd   = tsy_fwd,
    tsy_df    = tsy_df,
    spreads   = spreads
  )
}

# =============================================================================
# STEP 3: COMPUTE SWAP FORWARD RATES
# =============================================================================
# Standard formula: the forward swap rate starting at T_exp on a T_tenor swap
#
#   F(T_exp, T_tenor) = [d_LIBOR(T_exp) - d_LIBOR(T_exp + T_tenor)]
#                       / [sum_{j=1}^{T_tenor*freq} d_LIBOR(T_exp + j/freq) / freq]

#' Compute One Swap Forward Rate from LIBOR Discount Factors
#'
#' @param libor_df  Named numeric vector of LIBOR discount factors
#' @param T_exp     Forward start in years (0.25 = 1 quarter, 1 = 1 year)
#' @param T_tenor   Swap tenor in years (e.g. 10)
#' @param freq      Payment frequency
#'
#' @return Scalar forward swap rate in rate units
compute_swap_forward <- function(libor_df, T_exp, T_tenor, freq = 4) {

  times <- as.numeric(names(libor_df))
  
  ## !!! NEW !!!
  # Prepend t=0 with df=1 so we can interpolate sub-quarterly forwards
  times    <- c(0, times)
  libor_df <- c(1, libor_df)

  # Payment dates of the forward swap
  pmt_times <- seq(T_exp + 1/freq, T_exp + T_tenor, by = 1/freq)

  if (max(pmt_times) > max(times)) {
    warning(paste("Forward maturity", max(pmt_times),
                  "exceeds curve max", max(times)))
    return(NA)
  }

  # Discount factors at payment dates
  pmt_dfs <- approx(times, libor_df, xout = pmt_times, rule = 2)$y

  # Discount factor at forward start date
  d_start <- approx(times, libor_df, xout = T_exp,           rule = 2)$y
  d_end   <- approx(times, libor_df, xout = T_exp + T_tenor, rule = 2)$y

  # Forward annuity (discounted from today)
  fwd_annuity <- sum(pmt_dfs) / freq

  # Forward swap rate
  (d_start - d_end) / fwd_annuity
}

# =============================================================================
# STEP 4: FULL DAILY PANEL
# =============================================================================

#' Construct Swap Forward Rates for All Dates
#'
#' @param libor_panel  Data frame: date, tenor (numeric years), rate (rate units)
#' @param gsw_panel    Data frame: date, maturity (numeric years),
#'                     par_yield (rate units — must be SVENPY, not SVENY)
#' @param T_exps       Numeric vector of forward start dates (default: 0.25 and 1)
#' @param T_tenor      Swap tenor (default: 10)
#'
#' @return Data frame: date, maturity (= T_exp), tenor, forward_rate (rate units)
construct_forward_rate_panel <- function(libor_panel, gsw_panel,
                                         T_exps  = c(0.25, 1),
                                         T_tenor = 10) {

  dates <- sort(unique(libor_panel$date))
  n     <- length(dates)

  cat("Constructing forward rates for", n, "dates...\n")

  results <- map_dfr(seq_along(dates), function(i) {

    if (i %% 250 == 0) cat("  Processing date", i, "of", n, "\n")

    d <- dates[i]

    # Extract named vectors for this date
    libor_spot <- libor_panel %>%
      filter(date == d) %>%
      select(tenor, rate) %>%
      deframe()                    # -> named vector c("1"=0.05, "2"=0.055, ...)

    tsy_par <- gsw_panel %>%
      filter(date == d) %>%
      select(maturity, par_yield) %>%
      deframe()                    # -> named vector c("1"=0.04, "2"=0.042, ...)

    # Need at least a few tenors to fit the curve
    if (length(libor_spot) < 3 || length(tsy_par) < 3) return(NULL)

    tryCatch({

      # Build LIBOR curve for this date
      curve <- construct_libor_curve(libor_spot, tsy_par)

      # Compute forward rate at each requested T_exp
      map_dfr(T_exps, function(T_exp) {
        fwd <- compute_swap_forward(curve$libor_df, T_exp, T_tenor)
        tibble(
          date         = d,
          maturity     = T_exp,    # forward start date
          tenor        = T_tenor,
          forward_rate = fwd       # rate units
        )
      })

    }, error = function(e) {
      warning(paste("Failed for date", d, ":", e$message))
      NULL
    })
  })

  results
}

# =============================================================================
# STEP 5: PRE-2002 EXTENSION (Appendix H)
# =============================================================================
# Before 2002, LSEG swap data is unavailable. Appendix H says:
#   - For VARIANCE: use GSW realized variance + constant adjustment
#   - For LEVELS:   use GSW par yield + constant LIBOR spread

#' Compute Pre-2002 Variance Adjustment
#'
#' Calculates the average difference between LIBOR swap rate realized variance
#' and GSW Treasury par yield realized variance over 2002-2023, then applies
#' this as a constant adjustment to extend the variance series back pre-2002.
#'
#' @param libor_rates  Data frame: date, rate  (LIBOR 10y spot rates, post-2002)
#' @param gsw_10y      Data frame: date, par_yield (GSW 10y par yields, full history)
#' @param rv_window    Days for realized variance calculation (default 63 = 1 quarter)
#'
#' @return List with $adjustment (scalar) and $rv_extended (full RV series)
extend_variance_pre2002 <- function(libor_rates, gsw_10y, rv_window = 63) {

  compute_rv <- function(rates, window) {
    changes <- diff(rates)
    rv      <- zoo::rollapply(changes^2, width = window,
                              FUN = sum, align = "right", fill = NA)
    rv
  }

  # Post-2002: both LIBOR and GSW available
  overlap <- inner_join(
    libor_rates %>% rename(libor = rate),
    gsw_10y     %>% rename(gsw   = par_yield),
    by = "date"
  ) %>% arrange(date)

  rv_libor <- compute_rv(overlap$libor, rv_window)
  rv_gsw   <- compute_rv(overlap$gsw,   rv_window)

  # Constant adjustment per Appendix H
  adjustment <- mean(rv_libor - rv_gsw, na.rm = TRUE)
  cat("RV adjustment (LIBOR - GSW):", round(adjustment * 10000, 4), "ppt^2\n")

  # Pre-2002: GSW RV + adjustment
  pre2002_gsw <- gsw_10y %>%
    filter(date < min(libor_rates$date)) %>%
    arrange(date)

  rv_pre2002 <- compute_rv(pre2002_gsw$par_yield, rv_window) + adjustment

  # Full extended series
  rv_post2002 <- tibble(date = overlap$date, rv = rv_libor)
  rv_pre2002_df <- tibble(date = pre2002_gsw$date, rv = rv_pre2002)

  rv_extended <- bind_rows(rv_pre2002_df, rv_post2002) %>% arrange(date)

  list(adjustment = adjustment, rv_extended = rv_extended)
}

# =============================================================================
# STEP 6: VALIDATION
# =============================================================================

#' Validate Curve Construction
#'
#' Checks that implied swap rates from the constructed LIBOR curve
#' match the observed LIBOR swap rates. Errors should be < 0.01 bp.
#'
#' @param curve       Output from construct_libor_curve()
#' @param libor_spot  Input named vector of observed swap rates (rate units)
validate_curve <- function(curve, libor_spot) {

  tenors <- sort(as.numeric(names(libor_spot)))

  results <- map_dfr(tenors, function(T) {
    y_obs  <- libor_spot[as.character(T)]
    y_impl <- implied_swap_rate(curve$libor_df, T)
    tibble(
      tenor       = T,
      y_observed  = y_obs,
      y_implied   = y_impl,
      error_bp    = (y_impl - y_obs) * 10000
    )
  })

  cat("Curve fit (errors should be < 0.01 bp):\n")
  print(results)

  max_err <- max(abs(results$error_bp), na.rm = TRUE)
  if (max_err > 0.1) warning(paste("Max fitting error:", round(max_err, 4), "bp"))

  results
}

# =============================================================================
# CONVENIENCE WRAPPER
# =============================================================================

run_appendix_H <- function(libor_panel, gsw_panel) {

  # libor_panel: data frame with columns date, tenor, rate
  #   - tenor in years as numeric (1, 2, 5, 10, 20, 30)
  #   - rate in rate units (0.05 not 5%) — divide by 100 in main script
  #
  # gsw_panel: data frame with columns date, maturity, par_yield
  #   - maturity in years as numeric (1, 2, ..., 30)
  #   - par_yield in rate units (SVENPY columns / 100) — NOT SVENY

  # Construct 10y-in-1q and 10y-in-1y forward rates
  forward_rates <- construct_forward_rate_panel(
    libor_panel = libor_panel,
    gsw_panel   = gsw_panel,
    T_exps      = c(0.25, 1),    # 3-month and 1-year forward starts
    T_tenor     = 10             # 10-year underlying swap
  )

  # Validate on a sample date
  sample_date <- median(unique(libor_panel$date))

  libor_spot <- libor_panel %>%
    filter(date == sample_date) %>%
    select(tenor, rate) %>%
    deframe()

  tsy_par <- gsw_panel %>%
    filter(date == sample_date) %>%
    select(maturity, par_yield) %>%
    deframe()

  curve <- construct_libor_curve(libor_spot, tsy_par)
  validate_curve(curve, libor_spot)

  forward_rates
}
