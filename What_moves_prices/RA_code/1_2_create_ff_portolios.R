##########################################
# Data Generation for Girardi & Schlag   #
# Analysis on Fama-French B/M Portfolios #
##########################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(zoo)
  library(data.table)
  library(ggplot2)
})

# ============================================
# PART 1: LOAD DATA
# ============================================

# Read csv file containing valuation ratios
val_rat <- data.table::fread("WRDS24102025.csv")


# Load Firm level data with weights, portfolio and returns
load("ccm4_new.RData")

# Eps and Dps data
# Importing Eps and Dps data used in Python code create_portfolios_ff5.py
dps_data <- read.csv2(unz("C:\\Users\\mholzman\\OneDrive - WU Wien\\Dokumente\\GitHub\\Girardi_WU_RA_2025_26\\What_moves_prices\\1_2_data\\eps_dps_data.zip", "eps_dps_data.csv"), sep = ",")

# Define characteristics
CHARS <- c("capital_ratio", "equity_invcap", "debt_invcap", "totdebt_invcap",
           "at_turn", "inv_turn", "pay_turn", "rect_turn", "sale_equity", "sale_invcap", "sale_nwc",
           "invt_act", "rect_act", "fcf_ocf", "ocf_lct", "cash_debt", "cash_lt", "cfm", "short_debt", "profit_lct", "curr_debt",
           "debt_ebitda", "dltt_be", "int_debt", "int_totdebt", "lt_debt", "lt_ppent",
           "cash_conversion", "cash_ratio", "curr_ratio", "quick_ratio",
           "efftax", "gprof", "aftret_eq", "aftret_equity", "aftret_invcapx", "gpm", "npm", "opmad", "opmbd", "pretret_earnat",
           "pretret_noa", "ptpm", "roa", "roce", "roe",
           "de_ratio", "debt_assets", "debt_at", "debt_capital", "intcov", "intcov_ratio",
           "dpr", "peg_trailing", "bm", "evm", "pcf", "pe_exi", "pe_inc", "pe_op_basic", "pe_op_dil", "ps", "ptb",
           "accrual", "rd_sale", "adv_sale", "staff_sale")

# ============================================
# PART 2: PREPARE DATA
# ============================================

# Change date format
val_rat <- val_rat %>% mutate(public_date = as.Date(public_date))

# Convert problematic columns to numeric
val_rat$cash_conversion <- as.numeric(val_rat$cash_conversion)
val_rat$debt_ebitda <- as.numeric(val_rat$debt_ebitda)
val_rat$evm <- as.numeric(val_rat$evm)

# Merge the data sets
# We are joining the data by both permno and gvkey to avoid many-to-many relationships
val_rat1 <- left_join(val_rat, ccm4, by = c("permno" = "permno", "public_date" = "jdate", "gvkey" = "gvkey"))

# Filter to valid portfolio assignments
val_rat1 <- val_rat1 %>%
  filter(!is.na(bmport) & bmport != "")






############################
#Add eps from imported data
############################

# We choose eps_diluted in order to be consistent with the pe_exi ratio (excluding extraordinary items)

# Prepare earnings data
eps_quarterly <- dps_data %>%
  select(permno, date, eps_diluted) %>%
  mutate(
    permno = as.integer(permno),
    date = as.Date(date),
    eps_diluted = as.numeric(eps_diluted)
  ) %>%
  distinct(permno, date, .keep_all = TRUE)


# Merge to firm-month data
val_rat1 <- val_rat1 %>%
  left_join(eps_quarterly, by = c("permno", "public_date" = "date"))




# ============================================
# PART 3: CALCULATE FIRM-LEVEL FUNDAMENTALS
# ============================================

val_rat1 <- val_rat1 %>%
  arrange(permno, public_date) %>%
  group_by(permno) %>%
  mutate(
    # Use Cumulative Factor (cfacpr) to Adjust Price for stock splits 
    prc_adj = prc / cfacpr,
    prc_adj_lag = lag(prc_adj),
    
    # Calculate dividends from return differential 
    ret_delta = retadj - retxadj,
    div = ret_delta * prc_adj_lag,
    
    # 12-month rolling sum of dividends (smoothed)
    div_smooth = zoo::rollsum(div, k = 12, fill = NA, align = "right"),
    
    # P/D (using adjusted price for consistency)
    pd_ratio = prc_adj / div_smooth,
    
    # 12-month rolling sum of EPS (smoothed)
    eps_diluted = ifelse(is.na(eps_diluted), 0, eps_diluted),
    eps_smooth = zoo::rollsum(eps_diluted, k = 12, fill = NA, align = "right"),
    
    # Earnings per share from P/E ratio
    eps = ifelse(!is.na(pe_exi) & pe_exi != 0, prc_adj / pe_exi, NA_real_),
  ) %>%
  ungroup()

# We have two different EPS: one from the csv file and one we calculate from the PE ratio.
# We use the actual EPS ratio (eps_diluted).



# ============================================
# PART 4: WINSORIZE
# ============================================

# Winsorize CHARS and calculated variables at 1% and 99% within each month
vars_to_winsorize <- c(CHARS, "eps_diluted", "eps_smooth", "div", "div_smooth", "eps",
                       "pd_ratio", "retadj")
vars_to_winsorize <- vars_to_winsorize[vars_to_winsorize %in% names(val_rat1)]

val_rat1 <- val_rat1 %>%
  group_by(public_date) %>%
  mutate(
    across(
      all_of(vars_to_winsorize),
      ~ {
        p_low  <- quantile(.x, 0.01, na.rm = TRUE)
        p_high <- quantile(.x, 0.99, na.rm = TRUE)
        pmin(pmax(.x, p_low), p_high)
      }
    )
  ) %>%
  ungroup()


# ============================================
# PART 5: 12-MONTH ROLLING AVERAGE FOR CHARS
# ============================================

# Apply 12-month rolling average to the set of WRDS financial ratios only
setDT(val_rat1)
setorder(val_rat1, permno, public_date)

chars_in_data <- CHARS[CHARS %in% names(val_rat1)]

for (col in chars_in_data) {
  val_rat1[, (col) := frollmean(get(col), n = 12, align = "right", na.rm = TRUE), by = permno]
}


# ============================================
# PART 6: AGGREGATE TO PORTFOLIO LEVEL
# ============================================

# Value-weighted mean function
vw_mean <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0 & is.finite(x) & is.finite(w)
  if (sum(valid) == 0) return(NA_real_)
  sum(x[valid] * w[valid], na.rm = TRUE) / sum(w[valid], na.rm = TRUE)
}

## ==================================================
## Aggregate to B/M portfolio level (monthly)
portfolio_monthly_bm <- val_rat1 %>%
  filter(!is.na(wt) & wt > 0) %>%
  group_by(public_date, bmport) %>%
  summarise(
    # Returns
    ret_vw = vw_mean(retadj, wt),
    retx_vw = vw_mean(retxadj, wt),
    
    # Dividends
    div_vw = vw_mean(div, wt),
    div_smooth_vw = vw_mean(div_smooth, wt),
    
    # Earnings
    eps_vw = vw_mean(eps, wt),
    
    # Ratios
    pd_ratio_vw = vw_mean(pd_ratio, wt),
    
    # Imported earnings
    eps_diluted_vw = vw_mean(eps_diluted, wt),
    eps_smooth_vw = vw_mean(eps_smooth, wt),
    
    # Market cap and firm count
    total_me = sum(me, na.rm = TRUE),
    n_firms = n(),
    
    # Value-weighted characteristics
    across(
      all_of(chars_in_data),
      ~ vw_mean(.x, wt),
      .names = "{.col}_vw"
    ),
    
    .groups = "drop"
  )


# Calculating logs of value-weighted ratios
portfolio_monthly_bm <- portfolio_monthly_bm %>%
  arrange(bmport, public_date) %>%
  group_by(bmport) %>%
  mutate(
    # log PD and PE
    log_pd_vw = ifelse(pd_ratio_vw > 0, log(pd_ratio_vw), NA_real_),
    log_pe_vw = ifelse(pe_exi_vw > 0, log(pe_exi_vw), NA_real_),
    
    # log E/D and D/E
    log_ed_vw = log_pd_vw - log_pe_vw,
    log_de_vw = log_pe_vw - log_pd_vw
  ) %>%
  ungroup()


## ===========================================
## Aggregate to SIZE portfolio level (monthly)
portfolio_monthly_sz <- val_rat1 %>%
  filter(!is.na(wt) & wt > 0) %>%
  group_by(public_date, szport) %>%
  summarise(
    # Returns
    ret_vw = vw_mean(retadj, wt),
    retx_vw = vw_mean(retxadj, wt),
    
    # Dividends
    div_vw = vw_mean(div, wt),
    div_smooth_vw = vw_mean(div_smooth, wt),
    
    # Earnings
    eps_vw = vw_mean(eps, wt),
    
    # Ratios
    pd_ratio_vw = vw_mean(pd_ratio, wt),
    
    # Imported earnings
    eps_diluted_vw = vw_mean(eps_diluted, wt),
    eps_smooth_vw = vw_mean(eps_smooth, wt),
    
    # Market cap and firm count
    total_me = sum(me, na.rm = TRUE),
    n_firms = n(),
    
    # Value-weighted characteristics
    across(
      all_of(chars_in_data),
      ~ vw_mean(.x, wt),
      .names = "{.col}_vw"
    ),
    
    .groups = "drop"
  )


# Calculating logs of value-weighted ratios
portfolio_monthly_sz <- portfolio_monthly_sz %>%
  arrange(szport, public_date) %>%
  group_by(szport) %>%
  mutate(
    # log PD and PE
    log_pd_vw = ifelse(pd_ratio_vw > 0, log(pd_ratio_vw), NA_real_),
    log_pe_vw = ifelse(pe_exi_vw > 0, log(pe_exi_vw), NA_real_),
    
    # log E/D and D/E
    log_ed_vw = log_pd_vw - log_pe_vw,
    log_de_vw = log_pe_vw - log_pd_vw
  ) %>%
  ungroup()



## ====================================
## merge the two portfolio factors into one dataframe
# rename bmport / szport column name to "port"
colnames(portfolio_monthly_bm)[2] <- "port"
colnames(portfolio_monthly_sz)[2] <- "port"

portfolio_monthly <- rbind(portfolio_monthly_bm, portfolio_monthly_sz)
portfolio_monthly <- portfolio_monthly %>% arrange(public_date)




###########################
#Check correlation of div
###########################


dps_data1 <- val_rat1 %>% 
  select(permno, div, eps, public_date)

#Create quarter variable for dps_data1
dps_data1_qtr <- dps_data1 %>%
  # Step 1: Create quarter_id from public_date
  mutate(
    quarter_id = paste0(
      year(public_date),
      sprintf("%02d", quarter(public_date)))
  ) %>%
  # Step 2: Sum all dividends per firm per quarter
  group_by(permno, quarter_id) %>%
  summarise(
    dps_qtr = sum(div, na.rm = TRUE),
    .groups = "drop")


#Merge the data
dps_data <- dps_data %>%
  mutate(permno = as.integer(permno),
         quarter_id = as.character(quarter_id),
         dps_split_adj = as.numeric(dps_split_adj))

dps_data2 <- dps_data1_qtr %>% 
  left_join(dps_data, by = c("quarter_id", "permno"))

dps_data2 <- dps_data2 %>% 
  select(permno, quarter_id, dps_qtr, dps_qtr, eps_basic, eps_diluted,dps, 
         eps_basic_split_adj, dps_split_adj)


#Winsorize dps_split_adj
dps_data2 <- dps_data2 %>%
  group_by(quarter_id) %>%
  mutate(
    dps_split_adj = {
      p_low  <- quantile(dps_split_adj, 0.01, na.rm = TRUE)
      p_high <- quantile(dps_split_adj, 0.99, na.rm = TRUE)
      pmin(pmax(dps_split_adj, p_low), p_high)
    }
  ) %>%
  ungroup()


#Correlation between the two dividends
cor(as.numeric(dps_data2$dps_split_adj), as.numeric(dps_data2$dps_qtr), use = "complete.obs")

summary(dps_data2$dps_qtr)
summary(dps_data2$dps_split_adj)


##################################
# Bring data into the right format
##################################

# Bring the data to the same format as used in the main R code for the analysis

portfolio_monthly <- portfolio_monthly %>%
  mutate(month_id = year(public_date) * 100 + month(public_date))

portfolio_monthly <- portfolio_monthly %>%
  rename(quarter_id = public_date)

portfolio_monthly <- portfolio_monthly %>%
  mutate(month = month(quarter_id))

portfolio_monthly <- portfolio_monthly %>%
  mutate(public_date_month_id = month_id)

portfolio_monthly <- portfolio_monthly %>%
  arrange(port, quarter_id) %>%
  group_by(port) %>%
  mutate(
    # One-year return: compound 12 months of returns
    ret_1y = (rollapply(1 + ret_vw, width = 12, FUN = prod, fill = NA, align = "right") - 1)
  ) %>%
  ungroup()

portfolio_monthly <- portfolio_monthly %>%
  arrange(port, quarter_id) %>%
  group_by(port) %>%
  mutate(
    # Calculate cumulative price starting at 1
    price = cumprod(1 + ret_vw)
  ) %>%
  ungroup()

portfolio_monthly <- portfolio_monthly %>%
  rename_with(~ gsub("_vw$", "", .), ends_with("_vw"))


# arrange by month
portfolio_monthly <- portfolio_monthly %>% arrange(month_id)

# save the dataframe
save(portfolio_monthly, file = "portfolio_monthly.RData")

