# =============================================================================
# Trolle-Schwartz + Bloomberg backfill — paper-faithful implementation
# Rogers (2026) Appendix I.1 methodology
# =============================================================================
#
# The paper's Appendix I.1 describes THREE components:
#
# COMPONENT 1 — Risk-neutral MEAN (forward rates)
#   Simple: regress 2011-2023 RN mean on forward yield → R² > 99.9%
#   Then backfill pre-2011 using forward rates directly.
#
# COMPONENT 2 — Risk-neutral VARIANCE (multi-step):
#   Step A: 2011-2023 period (Bloomberg OTM available):
#     Use Appendix H Carr-Madan integration → "true" RN variance
#   Step B: 2002-2011 period (Trolle-Schwartz data + ATM only):
#     ATM variance = Bloomberg ATM implied vol squared
#     Regression: RN_var ~ ATM_var + spread(1y ATM, 1q ATM), on 2011-2023
#     Use 6-month rolling averages of spread (slow-moving)
#   Step C: gap between TS end and Bloomberg OTM start:
#     Regression: ATM_var ~ RN_var, on 2011+ → R² 99.6%
#     Use to convert ATM to RN during gap
#   TS variance is under annuity measure → ~1.5% lower than RN var on average
#   Apply the step B regression to correct this
#
# COMPONENT 3 — Risk-neutral SKEWNESS (multi-step):
#   Step A: 2011-2023: Appendix H Carr-Madan → "true" RN skewness
#   Step B: 2002-2011 (10y-in-1y skewness):
#     TS skewness is under annuity measure → ~10% lower than RN measure
#     Regression: RN_skew_1y ~ TS_skew_1y, on 2011-2023 → R² 99.8%
#   Step C: gap (TS end to Bloomberg OTM start):
#     Use Bauer-Chernov (2024) treasury RN skewness in place of TS
#   Step D: 10y-in-1q skewness from 10y-in-1y skewness:
#     Subtract the constant average spread (TS Table 2 reports this)
#     R² (uncentered) of 90% for skewness, 94% for third moment
#
# INPUT FILES:
#   - ts_digitized.csv:       date, rn_variance_ts, rn_skewness_ts (1y horizon)
#   - ts_1q_spread.csv:       single number: avg(skew_1y - skew_1q) from TS Table 2
#   - BB_svol_data.RData:     Bloomberg swaption vol panel (bb_svol_10y_clean)
#                             NOTE: Only Bloomberg swaption VOLATILITY is needed here.
#                             Swap rates come from LSEG (same source as main replication).
#   - BB_atm_vol.RData:       Bloomberg ATM vol (bb_atm_10y_1y, bb_atm_10y_1q)
#                             ATM tickers: USSRAC10 (1q×10y ATM), USSRAL10 (1y×10y ATM)
#   - bauer_chernov.csv:      Bauer-Chernov (2024) treasury RN skewness
#                             Only needed for ~1-year gap between TS end and BB OTM start
#                             If unavailable: carry-forward of last TS skew is acceptable
#   - LSEG_all.RData:         Swap rates (same as main replication — NOT Bloomberg)
#   - LSEG_additional.RData:  Additional LSEG swap rates
#   - feds200628.csv:         GSW zero-coupon yields
#   - rep_results.RData:      LSEG-based RN moments (2013-2023)
#   - functions_appendix_H_v3.R, functions_appendix_I_v3.R
# =============================================================================

library(tidyverse)
library(lubridate)
library(zoo)

# ---- Load appendix functions ----
source("functions_appendix_H_v3.R")
source("functions_appendix_I_v3.R")

# ---- Load LSEG-based moments (2013-2023) ----
load("rep_results.RData")
results_1q_IS <- results_1q_IS %>% mutate(date = as.Date(date))
results_1y_IS <- results_1y_IS %>% mutate(date = as.Date(date))

lseg_q <- results_1q_IS %>%
  select(date, rn_variance_q = rn_variance,
         rn_third_moment_q = rn_third_moment,
         rn_skewness_q = rn_skewness,
         sigma2_hat_q = sigma2_hat_IS,
         forward_q = forward_rate,
         atm_vol_q = atm_vol) %>%
  filter(!is.na(rn_variance_q)) %>%
  mutate(atm_variance_q = atm_vol_q^2)

lseg_y <- results_1y_IS %>%
  select(date, rn_variance_y = rn_variance,
         rn_third_moment_y = rn_third_moment,
         rn_skewness_y = rn_skewness,
         forward_y = forward_rate,
         atm_vol_y = atm_vol) %>%
  filter(!is.na(rn_variance_y)) %>%
  mutate(atm_variance_y = atm_vol_y^2)

# ---- Load Bloomberg swaption vol ----
## !!rename in case the file name differs!! ##
load("BB_svol_data.RData")

# ---- Load LSEG swap rates (used for forward rate construction pre-2013) ----
# Swap rates come from LSEG throughout — NOT Bloomberg.
# The same LSEG_all.RData used in the main replication covers the full period.
if (!exists("LSEG_all2")) {
  load("LSEG_all.RData");        lseg_all        <- df_joined
  load("LSEG_additional.RData"); lseg_additional <- df2
  LSEG_all2 <- left_join(lseg_all, lseg_additional, by = "Date")
}

# ---- Load Bloomberg ATM vols (separate ticker series) ----
# ATM vol for 10y-in-1y and 10y-in-1q specifically
# These should already be in BB_svol_data or can be extracted from it
if (file.exists("BB_atm_vol.RData")) {
  load("BB_atm_vol.RData")
  # expects: bb_atm_1y (date, atm_vol), bb_atm_1q (date, atm_vol)
} else {
  # Extract ATM vol from the swaption vol panel (strike = 0 offset = ATM)
  if (exists("bb_svol_10y_1y_clean")) {
    bb_atm_1y <- bb_svol_10y_1y_clean %>%
      filter(abs(offset) < 1e-6) %>%   # ATM = offset 0
      select(date, atm_vol = vol_normal) %>%
      mutate(date = as.Date(date),
             atm_vol = atm_vol / 10000,  # convert from bp to rate
             atm_variance = atm_vol^2)
  }
  if (exists("bb_svol_10y_1q_clean")) {
    bb_atm_1q <- bb_svol_10y_1q_clean %>%
      filter(abs(offset) < 1e-6) %>%
      select(date, atm_vol = vol_normal) %>%
      mutate(date = as.Date(date),
             atm_vol = atm_vol / 10000,
             atm_variance = atm_vol^2)
  }
}

# ---- Load TS digitized data ----
ts_variance <- read.csv("Trolle_Schwarz_variance.csv")
ts_skewness <- read.csv2("Trolle_Schwarz_skewness.csv")
ts_data <- merge(ts_variance, ts_skewness) %>%
  rename(rn_variance_ts_annuity = volatility_bp,
         rn_skewness_ts_annuity = skewness) %>% 
  mutate(date = as.Date(date))

cat("TS data:", nrow(ts_data), "obs from",
    format(min(ts_data$date)), "to", format(max(ts_data$date)), "\n")

# ---- Load TS 1q/1y spread (from TS Table 2) ----
# value extracted from Trolle-Schwarz (2014) Table 2 [USD]
# 0.16 - 0.15 = 0.01
avg_skew_spread_1y_1q <- 0.01

# ---- Load Bauer-Chernov skewness (gap bridge for skewness only) ----
# https://www.frbsf.org/research-and-insights/data-and-indicators/treasury-yield-skewness/
bc_data <- NULL
if (file.exists("Bauer_Chernov_skewness.csv")) {
  bc_data <- read.csv("Bauer_Chernov_skewness.csv", stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(!is.na(date)) %>%
    arrange(date) %>%
    select(date, rn_skewness_bc = Skewness)
  cat("Bauer-Chernov data:", nrow(bc_data), "obs from",
      format(min(bc_data$date)), "to", format(max(bc_data$date)), "\n")
} else {
  cat("bauer_chernov.csv not found. Gap period skewness will use carry-forward.\n")
}

# =============================================================================
# STEP A — Compute Bloomberg OTM moments (2011-2023) via Appendix H
# =============================================================================
# Bloomberg OTM data starts ~2011. This gives us the "true" RN moments.
# These are the benchmark against which all calibrations are done.

cat("\n--- Step A: Computing Bloomberg OTM moments (2011-2023) ---\n")

# Build forward rates panel from LSEG swap rates
# (same construction as main replication — LSEG covers full 2002-2023 period)
bb_libor_panel <- LSEG_all2 %>%
  pivot_longer(cols = -Date, names_to = "ticker", values_to = "rate") %>%
  mutate(tenor = c("USDSB3L1Y"=1,"USDSB3L2Y"=2,"USDSB3L3Y"=3,"USDSB3L4Y"=4,
                   "USDSB3L5Y"=5,"USDSB3L6Y"=6,"USDSB3L7Y"=7,"USDSB3L8Y"=8,
                   "USDSB3L9Y"=9,"USDSB3L10Y"=10,"USDSB3L15Y"=15,
                   "USDSB3L20Y"=20,"USDSB3L25Y"=25,"USDSB3L30Y"=30)[ticker],
         rate = rate / 100) %>%
  filter(!is.na(tenor), !is.na(rate)) %>%
  rename(date = Date) %>%
  mutate(date = as.Date(date)) %>%
  select(date, tenor, rate) %>%
  arrange(date, tenor)

gsw_raw    <- read.csv("feds200628.csv", skip = 9)
gsw_panel  <- gsw_raw %>%
  rename(date = Date) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  select(date, matches("SVENPY")) %>%
  pivot_longer(cols = -date, names_to = "maturity_str", values_to = "par_yield") %>%
  mutate(maturity  = as.numeric(str_extract(maturity_str, "\\d+")),
         par_yield = par_yield / 100) %>%
  filter(!is.na(par_yield)) %>%
  select(date, maturity, par_yield) %>%
  arrange(date, maturity)

# 10y forwards at both horizons
fwd_10y <- construct_forward_rate_panel(
  libor_panel = bb_libor_panel,
  gsw_panel   = gsw_panel,
  T_exps      = c(0.25, 1),
  T_tenor     = 10
) %>% mutate(date = as.Date(date))

# Moments at T_exp = 0.25 (quarterly) from Bloomberg OTM vol
bb_svol_10y_q <- bb_svol_10y_1q_clean %>%
  mutate(vol_normal = vol_normal / 10000, date = as.Date(date))

bb_moments_q <- compute_moments_panel(
  vol_data      = bb_svol_10y_q,
  forward_rates = fwd_10y,
  T_exp         = 0.25,
  T_tenor       = 10
) %>%
  mutate(rn_variance     = rn_variance     * 100^2,
         rn_third_moment = rn_third_moment * 100^3,
         rn_skewness     = rn_third_moment / rn_variance^1.5,
         date            = as.Date(date)) %>%
  select(date, rn_variance_bb_q = rn_variance,
         rn_third_moment_bb_q = rn_third_moment,
         rn_skewness_bb_q = rn_skewness)

# Moments at T_exp = 1 (annual) from Bloomberg OTM vol
bb_svol_10y_y <- bb_svol_10y_1y_clean %>%
  mutate(vol_normal = vol_normal / 10000, date = as.Date(date))

bb_moments_y <- compute_moments_panel(
  vol_data      = bb_svol_10y_y,
  forward_rates = fwd_10y,
  T_exp         = 1,
  T_tenor       = 10
) %>%
  mutate(rn_variance     = rn_variance     * 100^2,
         rn_third_moment = rn_third_moment * 100^3,
         rn_skewness     = rn_third_moment / rn_variance^1.5,
         date            = as.Date(date)) %>%
  select(date, rn_variance_bb_y = rn_variance,
         rn_third_moment_bb_y = rn_third_moment,
         rn_skewness_bb_y = rn_skewness)

cat("BB quarterly moments:", nrow(bb_moments_q), "obs from",
    format(min(bb_moments_q$date)), "to", format(max(bb_moments_q$date)), "\n")
cat("BB annual moments:   ", nrow(bb_moments_y), "obs from",
    format(min(bb_moments_y$date)), "to", format(max(bb_moments_y$date)), "\n")

# =============================================================================
# STEP B — Calibrate variance: regression on 2011-2023 overlap
# =============================================================================
# Paper: RN_var ~ ATM_var + spread(1y_ATM_var - 1q_ATM_var)
# Use 6-month rolling averages of ATM spread (slow-moving)
# Annuity measure ~ 1.5% lower than RN → regression corrects this

cat("\n--- Step B: Variance calibration regression ---\n")

# Build the calibration panel: BB OTM (2011+) joined with ATM vols
bb_otm_start <- min(bb_moments_q$date)

calib_var_panel <- bb_moments_q %>%
  inner_join(bb_moments_y %>% select(date, rn_variance_bb_y), by = "date") %>%
  inner_join(bb_atm_1q %>% select(date, atm_variance_q = atm_variance), by = "date") %>%
  inner_join(bb_atm_1y %>% select(date, atm_variance_y = atm_variance), by = "date") %>%
  arrange(date) %>%
  # 6-month rolling average of ATM variance spread (paper's slow-moving adjustment)
  mutate(
    atm_spread_1y_1q = atm_variance_y - atm_variance_q,
    atm_spread_rolling = zoo::rollmean(atm_spread_1y_1q, k = 125,
                                       fill = NA, align = "right")
  ) %>%
  drop_na()

cat("Variance calibration panel:", nrow(calib_var_panel), "obs\n")

# Calibration regression: RN_var_1q ~ ATM_var_1q + rolling_spread
var_reg <- lm(rn_variance_bb_q ~ atm_variance_q + atm_spread_rolling,
              data = calib_var_panel)
cat(sprintf("Variance calibration R²: %.4f\n", summary(var_reg)$r.squared))
cat(sprintf("  Intercept:   %.6f\n", coef(var_reg)[1]))
cat(sprintf("  ATM var:     %.6f\n", coef(var_reg)[2]))
cat(sprintf("  Spread:      %.6f\n", coef(var_reg)[3]))

# Check the ~1.5% annuity measure discount mentioned in paper
if (nrow(ts_data) > 0 && any(ts_data$date >= bb_otm_start)) {
  ts_in_bb <- ts_data %>%
    filter(date >= bb_otm_start) %>%
    inner_join(bb_moments_q, by = "date")
  if (nrow(ts_in_bb) > 5) {
    annuity_disc <- mean(ts_in_bb$rn_variance_ts_annuity /
                           ts_in_bb$rn_variance_bb_q, na.rm = TRUE)
    cat(sprintf("  TS/BB variance ratio in overlap: %.4f (paper says ~0.985)\n",
                annuity_disc))
  }
}

# =============================================================================
# STEP B2 — Variance regression for gap period (TS end → Bloomberg OTM start)
# =============================================================================
# Paper: "for window between TS end and Bloomberg OTM start, use regression
# of ATM variance on RN variance, R² 99.6%"
# This is ATM_var ~ RN_var (inverse of main regression: predict RN from ATM)

ts_end     <- max(ts_data$date)
gap_start  <- ts_end + 1
gap_end    <- bb_otm_start - 1

cat(sprintf("\nGap period: %s to %s (%d days)\n",
            format(gap_start), format(gap_end),
            as.numeric(gap_end - gap_start)))

# Regression for gap: predict RN_var from ATM_var only
gap_var_reg <- lm(rn_variance_bb_q ~ atm_variance_q, data = calib_var_panel)
cat(sprintf("Gap variance calibration R²: %.4f\n", summary(gap_var_reg)$r.squared))

# =============================================================================
# STEP C — Calibrate skewness: regression on 2011-2023 overlap
# =============================================================================
# Paper: RN_skew_1y ~ TS_skew_annuity, R² 99.8%
# TS annuity measure ~10% lower than RN measure

cat("\n--- Step C: Skewness calibration regression ---\n")

# TS 1y skewness in overlap with BB OTM
ts_for_skew_calib <- ts_data %>%
  filter(date >= bb_otm_start) %>%
  inner_join(bb_moments_y %>% select(date, rn_skewness_bb_y), by = "date")

if (nrow(ts_for_skew_calib) >= 10) {
  skew_reg <- lm(rn_skewness_bb_y ~ rn_skewness_ts_annuity, data = ts_for_skew_calib)
  cat(sprintf("Skewness calibration R²: %.4f\n", summary(skew_reg)$r.squared))
  cat(sprintf("  Intercept:   %.6f\n", coef(skew_reg)[1]))
  cat(sprintf("  TS slope:    %.6f\n", coef(skew_reg)[2]))
} else {
  cat("Insufficient overlap for skewness calibration. Using identity.\n")
  skew_reg <- lm(rn_skewness_bb_y ~ 0 + rn_skewness_ts_annuity,
                 data = tibble(rn_skewness_bb_y = c(1), rn_skewness_ts_annuity = c(1)))
  coef(skew_reg) <- c(1)
}

# =============================================================================
# STEP D — Build forward rate panel for RN mean backfill
# =============================================================================
# Paper: RN mean ~ forward yield, R² > 99.9%
# Just use forward rates directly (essentially perfect predictor)

cat("\n--- Step D: Forward rate as RN mean ---\n")

bb_forwards <- fwd_10y %>%
  filter(abs(maturity - 0.25) < 1e-6) %>%
  mutate(date = as.Date(date)) %>%
  select(date, forward_rate_bb = forward_rate) %>%
  mutate(forward_rate_bb = forward_rate_bb * 100)

cat("BB forward rates:", nrow(bb_forwards), "obs\n")

# =============================================================================
# STEP E — Assemble complete backfilled moments
# =============================================================================
# Three periods:
#   Period 1: 2002-01 to TS end (use TS + calibration regressions)
#   Period 2: TS end to BB OTM start (use ATM vol + gap regression)
#   Period 3: BB OTM start to 2023 (use BB Appendix H moments)

cat("\n--- Step E: Assembling backfilled panel ---\n")

# --- Period 1: TS era ---
period1 <- ts_data %>%
  mutate(
    # Variance: apply calibration using ATM vol during TS era
    # Need ATM vol for pre-OTM period
    rn_variance_cal = if (exists("bb_atm_1q")) {
      # Join ATM vol and apply calibration
      date_num <- as.numeric(date)
    } else NA_real_
  )

# Rebuild period 1 with ATM vol if available
if (exists("bb_atm_1q")) {
  period1 <- ts_data %>%
    filter(date < gap_start) %>%
    join_carry_forward(bb_atm_1q %>% select(date, atm_variance_q = atm_variance),
                       "atm_variance_q", max_stale_days = 35) %>%
    join_carry_forward(bb_atm_1y %>% select(date, atm_variance_y = atm_variance),
                       "atm_variance_y", max_stale_days = 35) %>%
    mutate(
      # Rolling spread approximation for pre-OTM period
      # Use constant mean spread from calibration period
      rolling_spread = mean(calib_var_panel$atm_spread_rolling, na.rm = TRUE),
      
      # Apply variance calibration regression
      rn_variance_cal = predict(var_reg,
                                newdata = data.frame(
                                  atm_variance_q = atm_variance_q,
                                  atm_spread_rolling = rolling_spread
                                )),
      
      # Apply skewness calibration regression
      rn_skewness_1y_cal = predict(skew_reg,
                                   newdata = data.frame(
                                     rn_skewness_ts_annuity = rn_skewness_ts_annuity
                                   )),
      
      # 1q skewness from 1y skewness: subtract constant spread (TS Table 2)
      rn_skewness_1q_cal = rn_skewness_1y_cal - avg_skew_spread_1y_1q,
      
      # Third moment from skewness and variance
      rn_third_moment_cal = rn_skewness_1q_cal * rn_variance_cal^1.5
    ) %>%
    select(date,
           rn_variance     = rn_variance_cal,
           rn_skewness     = rn_skewness_1q_cal,
           rn_third_moment = rn_third_moment_cal)
} else {
  cat("WARNING: bb_atm_1q not available; Period 1 variance calibration skipped.\n")
  period1 <- ts_data %>%
    filter(date < gap_start) %>%
    transmute(date,
              rn_variance     = rn_variance_ts_annuity * 1.0/(1 - 0.015),
              rn_skewness     = rn_skewness_ts_annuity * 1.10,
              rn_third_moment = rn_skewness * rn_variance^1.5)
}

# --- Period 2: Gap era (TS end to BB OTM start) ---
gap_dates <- seq.Date(gap_start, gap_end, by = "day")

if (length(gap_dates) > 0 && exists("bb_atm_1q")) {
  period2_atm <- bb_atm_1q %>%
    filter(date >= gap_start, date <= gap_end) %>%
    select(date, atm_variance_q = atm_variance)
  
  if (nrow(period2_atm) > 0) {
    period2 <- period2_atm %>%
      mutate(
        # Variance from ATM via gap regression
        rn_variance = predict(gap_var_reg,
                              newdata = data.frame(atm_variance_q = atm_variance_q)),
        
        # Skewness: use Bauer-Chernov if available, else carry forward last TS
        rn_skewness = NA_real_,
        rn_third_moment = NA_real_
      )
    
    # Add Bauer-Chernov skewness for gap period
    if (!is.null(bc_data)) {
      period2 <- period2 %>%
        left_join(bc_data %>% filter(date >= gap_start, date <= gap_end),
                  by = "date") %>%
        mutate(
          rn_skewness     = if_else(!is.na(rn_skewness_bc),
                                    rn_skewness_bc, rn_skewness),
          rn_third_moment = rn_skewness * rn_variance^1.5
        ) %>%
        select(date, rn_variance, rn_skewness, rn_third_moment)
    } else {
      # Carry forward last TS skewness
      last_ts_skew <- tail(period1$rn_skewness, 1)
      period2 <- period2 %>%
        mutate(rn_skewness     = last_ts_skew,
               rn_third_moment = rn_skewness * rn_variance^1.5) %>%
        select(date, rn_variance, rn_skewness, rn_third_moment)
    }
    
    cat("Period 2 (gap):", nrow(period2), "obs\n")
  } else {
    period2 <- NULL
    cat("No ATM data in gap period.\n")
  }
} else {
  period2 <- NULL
  cat("No gap period or ATM data unavailable.\n")
}

# --- Period 3: Bloomberg OTM era ---
period3 <- bb_moments_q %>%
  filter(date >= bb_otm_start) %>%
  transmute(date,
            rn_variance     = rn_variance_bb_q,
            rn_skewness     = rn_skewness_bb_q,
            rn_third_moment = rn_third_moment_bb_q)

cat("Period 3 (BB OTM):", nrow(period3), "obs\n")

# --- Splice all three periods ---
extended_backfill <- bind_rows(
  period1 %>% mutate(source = "TS + calibration"),
  period2 %>% mutate(source = "Gap: ATM + BC"),
  period3 %>% mutate(source = "Bloomberg OTM")
) %>% arrange(date)

cat("\n=== Full backfill: ===\n")
cat(sprintf("  Period 1 (TS):      %d obs, %s to %s\n",
            nrow(period1), format(min(period1$date)), format(max(period1$date))))
if (!is.null(period2))
  cat(sprintf("  Period 2 (gap):     %d obs, %s to %s\n",
              nrow(period2), format(min(period2$date)), format(max(period2$date))))
cat(sprintf("  Period 3 (BB OTM):  %d obs, %s to %s\n",
            nrow(period3), format(min(period3$date)), format(max(period3$date))))
cat(sprintf("  TOTAL:              %d obs, %s to %s\n",
            nrow(extended_backfill),
            format(min(extended_backfill$date)),
            format(max(extended_backfill$date))))

# =============================================================================
# STEP F — Splice with LSEG-based moments (2013-2023)
# =============================================================================
# Prefer LSEG for post-2013 dates (exact same methodology as main replication)

lseg_moments <- results_1q_IS %>%
  select(date, rn_variance, rn_third_moment, rn_skewness) %>%
  filter(!is.na(rn_variance))

lseg_start <- min(lseg_moments$date)

pre_lseg <- extended_backfill %>% filter(date < lseg_start) %>%
  select(date, rn_variance, rn_skewness, rn_third_moment, source)

extended_moments <- bind_rows(
  pre_lseg,
  lseg_moments %>% mutate(source = "LSEG")
) %>% arrange(date)

cat(sprintf("\nFinal extended panel: %d obs from %s to %s\n",
            nrow(extended_moments),
            format(min(extended_moments$date)),
            format(max(extended_moments$date))))

# =============================================================================
# STEP G — Save
# =============================================================================
save(extended_moments, extended_backfill,
     var_reg, gap_var_reg, skew_reg,
     avg_skew_spread_1y_1q,
     bb_moments_q, bb_moments_y,
     file = "ts_bb_extended_panel.RData")

cat("\nSaved: ts_bb_extended_panel.RData\n")

# =============================================================================
# STEP H — Diagnostic plots
# =============================================================================
p_var <- ggplot(extended_moments, aes(date, rn_variance, color = source)) +
  geom_line(linewidth = 0.4) +
  geom_vline(xintercept = c(bb_otm_start, lseg_start),
             linetype = "dashed", alpha = 0.3) +
  labs(title = "RN variance: extended panel",
       subtitle = "Dashed lines = Bloomberg OTM start, LSEG start",
       y = "RN Variance (ppt²)", x = NULL) +
  theme_classic() + theme(legend.position = "top")

p_skew <- ggplot(extended_moments, aes(date, rn_skewness, color = source)) +
  geom_line(linewidth = 0.4) +
  geom_vline(xintercept = c(bb_otm_start, lseg_start),
             linetype = "dashed", alpha = 0.3) +
  labs(title = "RN skewness: extended panel",
       y = "RN Skewness", x = NULL) +
  theme_classic() + theme(legend.position = "top")

ggsave("extended_variance.pdf",  p_var,  width = 10, height = 4)
ggsave("extended_skewness.pdf",  p_skew, width = 10, height = 4)
cat("Saved diagnostics: extended_variance.pdf, extended_skewness.pdf\n")