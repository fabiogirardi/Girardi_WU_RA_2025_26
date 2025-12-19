##########################################
# Fama French 3 Factors + 10 Portfolios  #
# Fabio Girardi (converted to R)         #
# Date: June 2023                        #
# Updated: June 2023                     #
##########################################

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(purrr)
  library(ggplot2)
  library(rlang)
  library(readr)
  library(fredr)
  library(Quandl)
  library(zoo)
  library(dbplyr)
  library(dplyr)
  library(lubridate)
})

# Set API keys
Quandl.api_key("") # add your key
fredr_set_key("") # add your key

#### Period
start_date <- as.Date("1959-01-01")
end_date <- as.Date("2022-12-31")

#### Configuration
is_market_factor <- TRUE
factors3 <- TRUE
portfolios10 <- TRUE

###################
# Connect to WRDS #
###################
Sys.setenv(WRDS_PASSWORD = "") # insert your password

library(DBI)
library(RPostgres)

wrds <- dbConnect(
  RPostgres::Postgres(),
  host     = "localhost",
  port     = 9737,
  dbname   = "wrds",
  user     = "", #insert your username
  password = Sys.getenv("WRDS_PASSWORD"),
  sslmode  = "require"
)
###################
# Market Factor   #
###################

if (is_market_factor) {
  
  # Query market returns
  market_returns <- dbGetQuery(wrds, sprintf("
    SELECT date, vwretd as market_return_with, vwretx as market_return_without
    FROM crsp.msi
    WHERE date >= '%s' AND date <= '%s'
  ", start_date, end_date))
  
  market_returns <- market_returns %>%
    mutate(
      date = as.Date(date),
      date = ceiling_date(date, "month") - days(1),
      market_return_with = as.numeric(market_return_with),
      market_return_without = as.numeric(market_return_without)
    )
  
  # Download risk-free rate
  risk_free_rate_data <- fredr(
    series_id = "TB3MS",
    observation_start = start_date,
    observation_end = end_date
  ) %>%
    rename(risk_free_rate = value) %>%
    mutate(
      risk_free_rate = risk_free_rate / 1200,
      date = ceiling_date(date, "month") - days(1)
    ) %>%
    select(date, risk_free_rate)
  
  market_factor <- market_returns %>%
    inner_join(risk_free_rate_data, by = "date") %>%
    mutate(
      MKT_with = market_return_with - risk_free_rate,
      MKT_without = market_return_without - risk_free_rate
    )
  
  # Calculate annualized means
  print(colMeans(market_factor[, 2:6], na.rm = TRUE) * 12)
}

###################
# Compustat Block #
###################

comp <- dbGetQuery(wrds, "
  SELECT gvkey, datadate, at, pstkl, txditc, pstkrv, seq, pstk
  FROM comp.funda
  WHERE indfmt='INDL' 
    AND datafmt='STD'
    AND popsrc='D'
    AND consol='C'
    AND datadate >= '1959-01-01'
")

comp <- comp %>%
  mutate(
    datadate = as.Date(datadate),
    year = year(datadate),
    # Create preferred stock
    ps = coalesce(pstkrv, pstkl, pstk, 0),
    txditc = coalesce(txditc, 0),
    # Create book equity
    be = seq + txditc - ps,
    be = ifelse(be > 0, be, NA)
  ) %>%
  arrange(gvkey, datadate) %>%
  group_by(gvkey) %>%
  mutate(count = row_number() - 1) %>%
  ungroup() %>%
  select(gvkey, datadate, year, be, count) %>%
  mutate(jdate = ceiling_date(datadate, "month") - days(1))

###################
# CRSP Block      #
###################

# MODIFIED: Added cfacpr to the SELECT statement
crsp_m <- dbGetQuery(wrds, "
  SELECT a.permno, a.permco, a.date, b.shrcd, b.exchcd,
         a.ret, a.retx, a.shrout, a.prc, a.cfacpr
  FROM crsp.msf as a
  LEFT JOIN crsp.msenames as b
    ON a.permno=b.permno
    AND b.namedt<=a.date
    AND a.date<=b.nameendt
  WHERE a.date BETWEEN '1959-01-01' AND '2023-12-31'
    AND b.exchcd BETWEEN 1 AND 3
")

crsp_m <- crsp_m %>%
  mutate(
    date = as.Date(date),
    jdate = ceiling_date(date, "month") - days(1),
    permco = as.integer(permco),
    permno = as.integer(permno),
    shrcd = as.integer(shrcd),
    exchcd = as.integer(exchcd),
    cfacpr = as.numeric(cfacpr)  # ADDED: Convert cfacpr to numeric
  )

# Add delisting returns
dlret <- dbGetQuery(wrds, "
  SELECT permno, dlret, dlstdt 
  FROM crsp.msedelist
") %>%
  mutate(
    permno = as.integer(permno),
    dlstdt = as.Date(dlstdt),
    jdate = ceiling_date(dlstdt, "month") - days(1)
  )

crsp <- crsp_m %>%
  left_join(dlret, by = c("permno", "jdate")) %>%
  mutate(
    dlret = coalesce(dlret, 0),
    ret = coalesce(ret, 0),
    retadj = (1 + ret) * (1 + dlret) - 1,
    retxadj = (1 + retx) * (1 + dlret) - 1,
    prc = abs(prc),
    me = abs(prc) * shrout
  ) %>%
  select(-dlret, -dlstdt, -shrout) %>% 
  arrange(jdate, permco, me)

### Aggregate Market Cap ###

crsp_summe <- crsp %>%
  group_by(jdate, permco) %>%
  summarise(me_sum = sum(me, na.rm = TRUE), .groups = "drop")

crsp_maxme <- crsp %>%
  group_by(jdate, permco) %>%
  summarise(me_max = max(me, na.rm = TRUE), .groups = "drop")

crsp1 <- crsp %>%
  inner_join(crsp_maxme, by = c("jdate", "permco")) %>%
  filter(me == me_max) %>%
  select(-me, -me_max)

crsp2 <- crsp1 %>%
  inner_join(crsp_summe, by = c("jdate", "permco")) %>%
  rename(me = me_sum) %>%
  arrange(permno, jdate) %>%
  distinct() %>%
  mutate(
    year = year(jdate),
    month = month(jdate)
  )

# Calculate dividend 
crsp2 <- crsp2 %>%
  mutate(ret_delta = ret - retx) %>%
  group_by(permno) %>%
  mutate(
    prc_lag = lag(prc),
    div = ret_delta * prc_lag,
    div_smooth = zoo::rollsum(div, k = 12, fill = NA, align = "right")
  ) %>%
  ungroup()

# December market cap
decme <- crsp2 %>%
  filter(month == 12) %>%
  select(permno, date, jdate, me, year) %>%
  rename(dec_me = me)

# July to June dates
crsp2 <- crsp2 %>%
  mutate(
    ffdate = jdate %m-% months(6),
    ffyear = year(ffdate),
    ffmonth = month(ffdate),
    `1+retx` = 1 + retx
  ) %>%
  arrange(permno, date) %>%
  group_by(permno, ffyear) %>%
  mutate(cumretx = cumprod(`1+retx`)) %>%
  ungroup() %>%
  group_by(permno) %>%
  mutate(
    lcumretx = lag(cumretx),
    lme = lag(me),
    count_perm = row_number() - 1,
    lme = ifelse(count_perm == 0, me / `1+retx`, lme)
  ) %>%
  ungroup()

# Baseline ME
mebase <- crsp2 %>%
  filter(ffmonth == 1) %>%
  select(permno, ffyear, lme) %>%
  rename(mebase = lme)

crsp3 <- crsp2 %>%
  left_join(mebase, by = c("permno", "ffyear")) %>%
  mutate(wt = ifelse(ffmonth == 1, lme, mebase * lcumretx))

decme <- decme %>%
  mutate(year = year + 1) %>%
  select(permno, year, dec_me)

# June info
crsp3_jun <- crsp3 %>%
  filter(month == 6)

crsp_jun <- crsp3_jun %>%
  inner_join(decme, by = c("permno", "year")) %>%
  select(permno, date, jdate, shrcd, exchcd, retadj, retxadj, 
         me, wt, cumretx, mebase, lme, dec_me) %>%
  arrange(permno, jdate) %>%
  distinct()

#######################
# CCM Block           #
#######################

ccm <- dbGetQuery(wrds, "
  SELECT gvkey, lpermno as permno, linktype, linkprim, linkdt, linkenddt
  FROM crsp.ccmxpf_linktable
  WHERE substr(linktype,1,1)='L'
    AND (linkprim ='C' OR linkprim='P')
") %>%
  mutate(
    gvkey = as.integer(gvkey),
    permno = as.integer(permno),
    linkdt = as.Date(linkdt),
    linkenddt = coalesce(as.Date(linkenddt), Sys.Date())
  )

comp <- comp %>%
  mutate(gvkey = as.integer(gvkey))

ccm1 <- comp %>%
  select(gvkey, datadate, be, count) %>%
  left_join(ccm, by = "gvkey") %>%
  mutate(
    yearend = ceiling_date(datadate, "year") - days(1),
    jdate = yearend %m+% months(6)
  )


ccm2 <- ccm1 %>%
  filter(jdate >= linkdt & jdate <= linkenddt) %>%
  # Prioritize 'P' (Primary) over 'C' if duplicates exist
  arrange(permno, jdate, desc(linkprim)) %>% 
  # Keep only one unique record per stock(permno) per year(jdate)
  distinct(permno, jdate, .keep_all = TRUE) %>%
  select(gvkey, permno, datadate, yearend, jdate, be, count)

# Link CRSP and Compustat
ccm_jun <- crsp_jun %>%
  inner_join(ccm2, by = c("permno", "jdate")) %>%
  mutate(beme = be * 1000 / dec_me)

# NYSE stocks for breakpoints
nyse <- ccm_jun %>%
  filter(
    exchcd == 1,
    beme > 0,
    me > 0,
    count >= 1,
    shrcd %in% c(10, 11)
  )

if (factors3) {
  
  # Size breakpoint
  nyse_sz <- nyse %>%
    group_by(jdate) %>%
    summarise(sizemedn = median(me, na.rm = TRUE), .groups = "drop")
  
  # BE/ME breakpoints
  nyse_bm <- nyse %>%
    group_by(jdate) %>%
    summarise(
      bm30 = quantile(beme, 0.3, na.rm = TRUE),
      bm70 = quantile(beme, 0.7, na.rm = TRUE),
      .groups = "drop"
    )
  
  nyse_breaks <- nyse_sz %>%
    inner_join(nyse_bm, by = "jdate")
  
  ccm1_jun <- ccm_jun %>%
    left_join(nyse_breaks, by = "jdate")
  
  # Assign portfolios
  ccm1_jun <- ccm1_jun %>%
    mutate(
      szport = case_when(
        is.na(me) ~ "",
        beme > 0 & me > 0 & count >= 1 & me <= sizemedn ~ "S",
        beme > 0 & me > 0 & count >= 1 & me > sizemedn ~ "B",
        TRUE ~ ""
      ),
      bmport = case_when(
        beme > 0 & me > 0 & count >= 1 & beme > 0 & beme <= bm30 ~ "L",
        beme > 0 & me > 0 & count >= 1 & beme <= bm70 ~ "M",
        beme > 0 & me > 0 & count >= 1 & beme > bm70 ~ "H",
        TRUE ~ ""
      ),
      posbm = ifelse(beme > 0 & me > 0 & count >= 1, 1, 0),
      nonmissport = ifelse(bmport != "", 1, 0)
    )
  
  # Store June portfolios
  # MODIFIED: Added gvkey to the select statement
  june <- ccm1_jun %>%
    select(permno, gvkey, date, jdate, bmport, szport, posbm, nonmissport) %>%
    mutate(ffyear = year(jdate))
  
  # MODIFIED: Added cfacpr to the select statement
  crsp3_subset <- crsp3 %>%
    select(date, permno, shrcd, exchcd, retadj, retxadj, me, wt, cumretx, ffyear, jdate, prc, cfacpr)
  
  # MODIFIED: Added gvkey to the select statement in the join
  ccm3 <- crsp3_subset %>%
    left_join(
      june %>% select(permno, ffyear, gvkey, szport, bmport, posbm, nonmissport),
      by = c("permno", "ffyear")
    )
  
  ccm4 <- ccm3 %>%
    filter(
      wt > 0,
      posbm == 1,
      nonmissport == 1,
      shrcd %in% c(10, 11)
    )
  
  ############################
  # Form Fama French Factors #
  ############################
  
  # Value-weighted returns
  vwret <- ccm4 %>%
    group_by(jdate, szport, bmport) %>%
    summarise(
      vwret = sum(retadj * wt, na.rm = TRUE) / sum(wt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(sbport = paste0(szport, bmport))
  
  # Firm count
  vwret_n <- ccm4 %>%
    group_by(jdate, szport, bmport) %>%
    summarise(n_firms = n(), .groups = "drop") %>%
    mutate(sbport = paste0(szport, bmport))
  
  # Pivot
  ff_factors <- vwret %>%
    select(jdate, sbport, vwret) %>%
    pivot_wider(names_from = sbport, values_from = vwret) %>%
    mutate(
      H = (BH + SH) / 2,
      L = (BL + SL) / 2,
      HML = H - L,
      B = (BL + BM + BH) / 3,
      S = (SL + SM + SH) / 3,
      SMB = S - B
    ) %>%
    rename(date = jdate)
  
  if (is_market_factor) {
    ff_factors <- ff_factors %>%
      inner_join(market_factor, by = "date")
  }
  
  # Firm counts
  ff_nfirms <- vwret_n %>%
    select(jdate, sbport, n_firms) %>%
    pivot_wider(names_from = sbport, values_from = n_firms) %>%
    mutate(
      H = SH + BH,
      L = SL + BL,
      HML = H + L,
      B = BL + BM + BH,
      S = SL + SM + SH,
      SMB = B + S,
      TOTAL = SMB
    ) %>%
    rename(date = jdate)
  
  ###################
  # Compare With FF #
  ###################
  
  ff_official <- dbGetQuery(wrds, "
    SELECT date, smb, hml
    FROM ff.factors_monthly
  ") %>%
    mutate(
      date = as.Date(date),
      date = ceiling_date(date, "month") - days(1)
    )
  
  ffcomp <- ff_official %>%
    inner_join(
      ff_factors %>% select(date, SMB, HML),
      by = "date"
    )
  
  ffcomp70 <- ffcomp %>%
    filter(date >= as.Date("1970-01-01"))
  
  # Correlations
  smb_cor <- cor.test(ffcomp70$smb, ffcomp70$SMB, use = "complete.obs")
  hml_cor <- cor.test(ffcomp70$hml, ffcomp70$HML, use = "complete.obs")
  
  print(paste("SMB Correlation:", round(smb_cor$estimate, 4), 
              "p-value:", format.pval(smb_cor$p.value)))
  print(paste("HML Correlation:", round(hml_cor$estimate, 4), 
              "p-value:", format.pval(hml_cor$p.value)))
  
  # Plotting
  library(ggplot2)
  library(gridExtra)
  
  p1 <- ggplot(ffcomp, aes(x = date)) +
    geom_line(aes(y = smb, color = "FF Official"), linetype = "dashed") +
    geom_line(aes(y = SMB, color = "My Calculation")) +
    xlim(as.Date("1962-06-01"), as.Date("2017-12-31")) +
    labs(title = "SMB Comparison", y = "Return", x = "Date") +
    scale_color_manual(values = c("FF Official" = "red", "My Calculation" = "blue")) +
    theme_minimal() +
    theme(legend.position = "top")
  
  p2 <- ggplot(ffcomp, aes(x = date)) +
    geom_line(aes(y = hml, color = "FF Official"), linetype = "dashed") +
    geom_line(aes(y = HML, color = "My Calculation")) +
    xlim(as.Date("1962-06-01"), as.Date("2017-12-31")) +
    labs(title = "HML Comparison", y = "Return", x = "Date") +
    scale_color_manual(values = c("FF Official" = "red", "My Calculation" = "blue")) +
    theme_minimal() +
    theme(legend.position = "top")
  
  grid.arrange(p1, p2, ncol = 1)
}

## =============================================================================
## save the ccm4 data which are then used to get the portfolios in the next code
save(ccm4, file = "ccm4_new.RData")





