# =============================================================================
# Trolle-Schwartz + Bloomberg backfill — paper-faithful implementation
# Rogers (2026) Appendix I.1 methodology
# =============================================================================
#
# THREE FIXES vs previous version:
#   1. Removed atm_vol from lseg_q/lseg_y (column doesn't exist in results_1q_IS)
#      ATM vol is extracted directly from Bloomberg svol panel instead
#   2. TS Figure 1 Panel A is VOLATILITY in bp, not variance. Conversion:
#      rn_variance_ts = (vol_bp / 10000)^2 × T_exp × 100^2   (in ppt²)
#      where T_exp = 1 year for the 1y horizon figure
#   3. Bloomberg swaption vol used for ENTIRE sample (2002-2023).
#      LSEG swaption vol is NOT used — this matches paper's data source.
#      LSEG swap RATES are still used (Rogers uses LSEG rates throughout).
#
# THREE PERIODS (even with full Bloomberg svol access):
#   Period 1: 2002 → ~2011: BB ATM vol only (OTM not available pre-2011)
#             → Use TS + ATM calibration regression for full RN moments
#   Period 2: ~2010 → ~2011: Gap between TS end and BB OTM start
#             → Use BB ATM + gap regression for variance
#             → Use Bauer-Chernov skewness (or carry-forward)
#   Period 3: ~2011 → 2023: BB OTM available
#             → Full Appendix H Carr-Madan integration
#
# INPUT FILES:
#   - ts_digitized.csv:   date, vol_bp_ts (volatility in bp!), rn_skewness_ts
#                         (digitized from TS Figure 1 Panel A and Panel B)
#   - ts_1q_spread.csv:   single number: avg(skew_1y - skew_1q) from TS Table 2
#   - BB_svol_data.RData: Bloomberg svol (bb_svol_10y_1q_clean, bb_svol_10y_1y_clean)
#                         Full OTM surface 2011+, ATM only pre-2011
#   - bauer_chernov.csv:  Bauer-Chernov (2024) treasury RN skewness (optional)
#   - LSEG_all.RData:     Swap rates (LSEG, used for forward construction)
#   - LSEG_additional.RData
#   - feds200628.csv:     GSW zero-coupon yields
#   - rep_results.RData:  (not used for ATM vol — only for LSEG moments reference)
#   - functions_appendix_H_v3.R, functions_appendix_I_v3.R
# =============================================================================

library(tidyverse)
library(lubridate)
library(zoo)


source("functions_appendix_H_v3.R")
source("functions_appendix_I_v3.R")

getwd()
## set working directory to be able to load files
setwd("~/GitHub/Girardi_WU_RA_2025_26/Risk_Based_Interest_Rate_Expectations/1_2_data")

join_carry_forward <- function(target_df, source_df, source_col,
                               max_stale_days = 35) {
  target_df <- target_df %>% arrange(date)
  source_df <- source_df %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
  src_dates <- as.numeric(source_df$date)
  src_vals  <- source_df[[source_col]]
  tgt_dates <- as.numeric(target_df$date)
  matched <- sapply(tgt_dates, function(td) {
    eligible <- which(src_dates <= td)
    if (length(eligible) == 0) return(NA_real_)
    last_idx <- max(eligible)
    if ((td - src_dates[last_idx]) > max_stale_days) return(NA_real_)
    src_vals[last_idx]
  })
  target_df %>% mutate(!!source_col := matched)
}


# =============================================================================
# STEP 0 — Load data
# =============================================================================

# ---- Bloomberg swaption vol (full sample 2002-2023) ----
## !!rename in case the file name differs!! ##
load("BB_svol_data.RData")
# Expects: bb_svol_10y_1q_clean and bb_svol_10y_1y_clean
# Each has columns: date, offset (strike offset in bp), vol_normal (in bp)
# ATM = offset 0; OTM surface available from ~2011

# ---- LSEG swap rates (for forward rate construction) ----
load("LSEG_all.RData");        lseg_all        <- df_joined
load("LSEG_additional.RData"); lseg_additional <- df2
LSEG_all2 <- left_join(lseg_all, lseg_additional, by = "Date")


# ---- TS digitized data ----
# IMPORTANT: TS Figure 1 Panel A is VOLATILITY in basis points, not variance.
# Formula from caption: sqrt(Var^A / (T_m - t)) * 10000
# So: vol_bp = sqrt(rn_variance_annuity / T_exp) * 10000
# Inverse: rn_variance_annuity = (vol_bp / 10000)^2 * T_exp
# Then convert to ppt²: multiply by 100^2
# For T_exp = 1 year: rn_variance_ppt2 = (vol_bp / 10000)^2 * 1 * 10000
#                                       = (vol_bp / 100)^2
ts_raw <- read.csv("ts_digitized.csv", stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(date)) %>%
  arrange(date)

ts_variance <- read.csv("Trolle_Schwarz_variance.csv")
ts_skewness <- read.csv2("Trolle_Schwarz_skewness.csv")
ts_raw <- merge(ts_variance, ts_skewness) %>% 
  mutate(date = as.Date(date))

# Convert TS volatility (bp) to variance (ppt²) for T_exp = 1 year
# vol_bp column from digitized figure, skewness column as-is (already dimensionless)
T_EXP_TS <- 1.0   # TS Figure 1 is 10y-in-1y horizon

ts_data <- ts_raw %>%
  mutate(
    # Convert bp volatility to annuity-measure variance in ppt²
    rn_variance_ts_annuity = (as.numeric(volatility_bp) / 100)^2,
    # Skewness is dimensionless — annuity measure, ~10% lower than RN measure
    rn_skewness_ts_annuity = as.numeric(skewness)
  )

cat("TS data:", nrow(ts_data), "obs from",
    format(min(ts_data$date)), "to", format(max(ts_data$date)), "\n")
cat(sprintf("  TS variance range (ppt²): [%.4f, %.4f]\n",
            min(ts_data$rn_variance_ts_annuity, na.rm = TRUE),
            max(ts_data$rn_variance_ts_annuity, na.rm = TRUE)))
cat(sprintf("  TS skewness range:        [%.4f, %.4f]\n",
            min(ts_data$rn_skewness_ts_annuity, na.rm = TRUE),
            max(ts_data$rn_skewness_ts_annuity, na.rm = TRUE)))

# ---- TS 1q/1y skewness spread (TS Table 2) ----
# value extracted from Trolle-Schwarz (2014) Table 2 [USD]
# 0.16 - 0.15 = 0.01
avg_skew_spread_1y_1q <- 0.01

# ---- Bauer-Chernov skewness (optional, for gap period only) ----
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
# STEP A — Extract ATM vol from Bloomberg (available for full 2002-2023)
# =============================================================================
# ATM = offset 0 in the vol panel. Available pre-2011 (only ATM) and post-2011
# (full OTM surface, from which we also get ATM as offset=0).

bb_atm_1q <- bb_svol_10y_1q_clean %>%
  filter(abs(offset) < 1e-6) %>%
  transmute(date = as.Date(date),
            atm_vol_bp = vol_normal,              # in bp
            atm_vol    = vol_normal / 10000,      # in rate units
            atm_variance = atm_vol^2) %>%         # in rate² units
  arrange(date)

bb_atm_1y <- bb_svol_10y_1y_clean %>%
  filter(abs(offset) < 1e-6) %>%
  transmute(date = as.Date(date),
            atm_vol_bp = vol_normal,
            atm_vol    = vol_normal / 10000,
            atm_variance = atm_vol^2) %>%
  arrange(date)

cat(sprintf("ATM 1q: %d obs from %s to %s\n", nrow(bb_atm_1q),
            format(min(bb_atm_1q$date)), format(max(bb_atm_1q$date))))
cat(sprintf("ATM 1y: %d obs from %s to %s\n", nrow(bb_atm_1y),
            format(min(bb_atm_1y$date)), format(max(bb_atm_1y$date))))

# Determine OTM availability start (when Bloomberg provides more than ATM only)
bb_otm_start <- bb_svol_10y_1q_clean %>%
  filter(offset != 0) %>%
  summarise(first_date = min(as.Date(date))) %>%
  pull(first_date)

ts_end    <- max(ts_data$date)
gap_start <- ts_end + 1
gap_end   <- bb_otm_start - 1

cat(sprintf("\nTimeline:\n"))
cat(sprintf("  TS period:  %s to %s\n", format(min(ts_data$date)), format(ts_end)))
cat(sprintf("  Gap period: %s to %s (%d days)\n",
            format(gap_start), format(gap_end), as.numeric(gap_end - gap_start)))
cat(sprintf("  BB OTM:     %s onwards\n", format(bb_otm_start)))

# =============================================================================
# STEP B — Compute Bloomberg Appendix H moments for OTM period (2011+)
# =============================================================================

# Forward rates from LSEG swap rates (paper uses LSEG rates throughout)
libor_panel <- LSEG_all2 %>%
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

gsw_raw   <- read.csv("feds200628.csv", skip = 9)
gsw_panel <- gsw_raw %>%
  rename(date = Date) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  select(date, matches("SVENPY")) %>%
  pivot_longer(cols = -date, names_to = "maturity_str", values_to = "par_yield") %>%
  mutate(maturity  = as.numeric(str_extract(maturity_str, "\\d+")),
         par_yield = par_yield / 100) %>%
  filter(!is.na(par_yield)) %>%
  select(date, maturity, par_yield) %>%
  arrange(date, maturity)

fwd_10y <- construct_forward_rate_panel(
  libor_panel = libor_panel,
  gsw_panel   = gsw_panel,
  T_exps      = c(0.25, 1),
  T_tenor     = 10
) %>% mutate(date = as.Date(date))

# Quarterly moments from Bloomberg OTM (2011+)
bb_svol_10y_q <- bb_svol_10y_1q_clean %>%
  filter(as.Date(date) >= bb_otm_start) %>%
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

# Annual moments from Bloomberg OTM (2011+)
bb_svol_10y_y <- bb_svol_10y_1y_clean %>%
  filter(as.Date(date) >= bb_otm_start) %>%
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

cat(sprintf("BB quarterly moments: %d obs from %s to %s\n",
            nrow(bb_moments_q), format(min(bb_moments_q$date)),
            format(max(bb_moments_q$date))))
cat(sprintf("BB annual moments:    %d obs from %s to %s\n",
            nrow(bb_moments_y), format(min(bb_moments_y$date)),
            format(max(bb_moments_y$date))))

# =============================================================================
# STEP C — Calibration regressions on the OTM period (2011+)
# =============================================================================
# C1: Variance: RN_var_1q ~ ATM_var_1q + rolling_spread(1y-1q ATM var)
# C2: Skewness: RN_skew_1y ~ TS_skew_annuity

# Build calibration panel: OTM moments + ATM vols
calib_panel <- bb_moments_q %>%
  inner_join(bb_moments_y %>% select(date, rn_variance_bb_y,
                                     rn_skewness_bb_y), by = "date") %>%
  inner_join(bb_atm_1q %>% select(date, atm_variance_q = atm_variance),
             by = "date") %>%
  inner_join(bb_atm_1y %>% select(date, atm_variance_y = atm_variance),
             by = "date") %>%
  arrange(date) %>%
  mutate(
    atm_spread_1y_1q  = atm_variance_y - atm_variance_q,
    # 6-month rolling average (paper specifies this for the spread)
    atm_spread_rolling = zoo::rollmean(atm_spread_1y_1q, k = 125,
                                       fill = NA, align = "right")
  ) %>%
  drop_na()

cat("Calibration panel:", nrow(calib_panel), "obs\n")

# C1: Variance calibration
var_reg <- lm(rn_variance_bb_q ~ atm_variance_q + atm_spread_rolling,
              data = calib_panel)
cat(sprintf("Variance R²: %.4f (paper: ~0.997)\n", summary(var_reg)$r.squared))

# C2: Gap variance (ATM only → RN)
gap_var_reg <- lm(rn_variance_bb_q ~ atm_variance_q, data = calib_panel)
cat(sprintf("Gap variance R²: %.4f (paper: ~0.996)\n",
            summary(gap_var_reg)$r.squared))

# C3: Skewness calibration using TS overlap with BB OTM period
ts_for_skew <- ts_data %>%
  filter(date >= bb_otm_start) %>%
  mutate(date_match = as.Date(sapply(date, function(td) {
    diffs <- abs(as.numeric(calib_panel$date - td))
    if (min(diffs) > 7) return(NA)
    as.character(calib_panel$date[which.min(diffs)])
  }))) %>%
  filter(!is.na(date_match)) %>%
  inner_join(calib_panel %>% select(date, rn_skewness_bb_y),
             by = c("date_match" = "date")) %>%
  drop_na(rn_skewness_ts_annuity, rn_skewness_bb_y)

if (nrow(ts_for_skew) >= 10) {
  skew_reg <- lm(rn_skewness_bb_y ~ rn_skewness_ts_annuity, data = ts_for_skew)
  cat(sprintf("Skewness R²: %.4f (paper: ~0.998)\n", summary(skew_reg)$r.squared))
} else {
  cat("Insufficient TS-BB overlap for skewness regression.\n")
  cat("Using 10% upward scale (paper's average annuity-to-RN correction)\n")
  # Fallback: simple scale by 1/0.90 (10% lower on average per paper)
  skew_reg <- NULL
}

# Mean rolling spread (used as constant for pre-OTM period)
mean_atm_spread <- mean(calib_panel$atm_spread_rolling, na.rm = TRUE)

# =============================================================================
# STEP D — Build Period 1: TS era (2002 → ~2010)
# =============================================================================
# Variance: apply C1 regression using Bloomberg ATM vol + constant spread
# Skewness: apply C3 regression (or scale by 1.10) to get 1y RN skewness
#           then subtract TS spread to get 1q skewness

period1 <- ts_data %>%
  filter(date < gap_start) %>%
  join_carry_forward(bb_atm_1q %>% select(date, atm_variance_q = atm_variance),
                     "atm_variance_q", max_stale_days = 35) %>%
  join_carry_forward(bb_atm_1y %>% select(date, atm_variance_y = atm_variance),
                     "atm_variance_y", max_stale_days = 35) %>%
  drop_na(atm_variance_q) %>%
  mutate(
    # Variance: use calibration regression with constant mean spread
    rn_variance_q = predict(var_reg,
                            newdata = data.frame(
                              atm_variance_q     = atm_variance_q,
                              atm_spread_rolling = mean_atm_spread
                            )),
    
    # Skewness 1y: apply calibration regression or constant scale
    rn_skewness_1y = if (!is.null(skew_reg)) {
      predict(skew_reg,
              newdata = data.frame(
                rn_skewness_ts_annuity = rn_skewness_ts_annuity
              ))
    } else {
      rn_skewness_ts_annuity / 0.90  # paper: annuity ~10% lower than RN
    },
    
    # 1q skewness: subtract TS Table 2 constant spread
    rn_skewness_q   = rn_skewness_1y - avg_skew_spread_1y_1q,
    rn_third_moment = rn_skewness_q * rn_variance_q^1.5
  ) %>%
  select(date,
         rn_variance     = rn_variance_q,
         rn_skewness     = rn_skewness_q,
         rn_third_moment = rn_third_moment)

cat(sprintf("Period 1: %d obs from %s to %s\n",
            nrow(period1), format(min(period1$date)), format(max(period1$date))))

# =============================================================================
# STEP E — Build Period 2: Gap era (TS end → BB OTM start)
# =============================================================================
# Variance: BB ATM only → apply gap_var_reg
# Skewness: Bauer-Chernov if available, else carry-forward last TS value

if (as.numeric(gap_end - gap_start) > 0) {
  period2_base <- bb_atm_1q %>%
    filter(date >= gap_start, date <= gap_end) %>%
    mutate(
      rn_variance = predict(gap_var_reg,
                            newdata = data.frame(atm_variance_q = atm_variance))
    ) %>%
    select(date, rn_variance)
  
  # Skewness in gap period
  last_ts_skew <- tail(period1$rn_skewness, 1)
  
  if (!is.null(bc_data) && any(bc_data$date >= gap_start & bc_data$date <= gap_end)) {
    period2 <- period2_base %>%
      left_join(bc_data %>% filter(date >= gap_start, date <= gap_end),
                by = "date") %>%
      mutate(
        rn_skewness     = coalesce(rn_skewness_bc, last_ts_skew),
        rn_third_moment = rn_skewness * rn_variance^1.5
      ) %>%
      select(date, rn_variance, rn_skewness, rn_third_moment)
    cat("  Using Bauer-Chernov for gap skewness\n")
  } else {
    period2 <- period2_base %>%
      mutate(
        rn_skewness     = last_ts_skew,
        rn_third_moment = rn_skewness * rn_variance^1.5
      ) %>%
      select(date, rn_variance, rn_skewness, rn_third_moment)
    cat("  Carrying forward last TS skewness for gap period\n")
  }
  
  cat(sprintf("Period 2: %d obs from %s to %s\n",
              nrow(period2), format(min(period2$date)), format(max(period2$date))))
} else {
  period2 <- NULL
  cat("No gap period (TS and BB OTM overlap).\n")
}

# =============================================================================
# STEP F — Period 3: Bloomberg OTM (2011 → 2023)
# =============================================================================

period3 <- bb_moments_q %>%
  transmute(date,
            rn_variance     = rn_variance_bb_q,
            rn_skewness     = rn_skewness_bb_q,
            rn_third_moment = rn_third_moment_bb_q)

cat(sprintf("Period 3: %d obs from %s to %s\n",
            nrow(period3), format(min(period3$date)), format(max(period3$date))))

# =============================================================================
# STEP G — Splice and finalise
# =============================================================================

extended_moments <- bind_rows(
  period1 %>% mutate(source = "TS + BB ATM calibration"),
  period2 %>% mutate(source = "Gap: BB ATM + BC skew"),
  period3 %>% mutate(source = "Bloomberg OTM (Appendix H)")
) %>% arrange(date)

cat(sprintf("Total: %d obs from %s to %s\n",
            nrow(extended_moments),
            format(min(extended_moments$date)),
            format(max(extended_moments$date))))

# Boundary check at period joins
check_boundary <- function(df, split_date, label) {
  pre  <- df %>% filter(date <= split_date) %>% tail(3)
  post <- df %>% filter(date >  split_date) %>% head(3)
  cat(sprintf("\nBoundary at %s (%s):\n", format(split_date), label))
  cat("  Pre:\n"); print(pre %>% select(date, rn_variance, rn_skewness))
  cat("  Post:\n"); print(post %>% select(date, rn_variance, rn_skewness))
}
if (!is.null(period2))
  check_boundary(extended_moments, ts_end, "TS → Gap")
check_boundary(extended_moments, bb_otm_start, "Gap → BB OTM")

# =============================================================================
# STEP H — Save
# =============================================================================
save(extended_moments, period1, period2, period3,
     var_reg, gap_var_reg, skew_reg,
     avg_skew_spread_1y_1q, mean_atm_spread,
     bb_moments_q, bb_moments_y,
     file = "ts_bb_extended_panel.RData")

cat("\nSaved: ts_bb_extended_panel.RData\n")
cat("Set EXTENDED_SAMPLE = 'extended' in master script to use this panel.\n")

# =============================================================================
# STEP I — Diagnostic plots
# =============================================================================
p_var <- ggplot(extended_moments, aes(date, rn_variance, color = source)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = c(bb_otm_start),
             linetype = "dashed", alpha = 0.4) +
  labs(title = "Extended RN variance (ppt²)",
       subtitle = "Dashed = Bloomberg OTM start",
       y = "RN Variance (ppt²)", x = NULL) +
  theme_classic() + theme(legend.position = "top")

p_skew <- ggplot(extended_moments, aes(date, rn_skewness, color = source)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = c(bb_otm_start),
             linetype = "dashed", alpha = 0.4) +
  labs(title = "Extended RN skewness",
       y = "RN Skewness", x = NULL) +
  theme_classic() + theme(legend.position = "top")

ggsave("extended_variance.pdf",  p_var,  width = 10, height = 4)
ggsave("extended_skewness.pdf",  p_skew, width = 10, height = 4)
cat("Saved: extended_variance.pdf, extended_skewness.pdf\n")