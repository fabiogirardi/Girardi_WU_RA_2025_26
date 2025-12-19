data_preparation <- function(df, annualized = FALSE) {
  
  df <- df %>%
    arrange(port, quarter_id) %>%
    group_by(port) %>%
    mutate(
      # Rename for clarity
      D = div_smooth,
      E = eps_smooth,
      
      # Log ratios (we already have these, but keeping for consistency)
      p_d = log_pd,
      p_e = log_pe,
      e_d = log_ed,
      
      # Dividend growth rates (log differences) - monthly data
      d_gr_q  = c(rep(NA, 3),  diff(log(D), 3)),
      d_gr_1y = c(rep(NA, 12), diff(log(D), 12)),
      d_gr_2y = c(rep(NA, 24), diff(log(D), 24)),
      d_gr_3y = c(rep(NA, 36), diff(log(D), 36)),
      d_gr_5y = c(rep(NA, 60), diff(log(D), 60)),
      
      # Earnings growth rates (quarterly EPS, but monthly observations)
      e_gr_q  = c(rep(NA, 3),  diff(log(E), 3)),
      e_gr_1y = c(rep(NA, 12), diff(log(E), 12)),
      e_gr_2y = c(rep(NA, 24), diff(log(E), 24)),
      e_gr_3y = c(rep(NA, 36), diff(log(E), 36)),
      e_gr_5y = c(rep(NA, 60), diff(log(E), 60)),
      
      # Compounded returns at various horizons
      ret_q  = rollapply(log(1 + ret), width = 3, FUN = sum, fill = NA, align = "right"),
      ret_1y = rollapply(log(1 + ret), width = 12, FUN = sum, fill = NA, align = "right"),
      ret_2y = rollapply(log(1 + ret), width = 24, FUN = sum, fill = NA, align = "right"),
      ret_3y = rollapply(log(1 + ret), width = 36, FUN = sum, fill = NA, align = "right"),
      ret_5y = rollapply(log(1 + ret), width = 60, FUN = sum, fill = NA, align = "right")
      #ret_q  = rollapply(1 + ret, width = 3,  FUN = prod, fill = NA, align = "right") - 1,
      #ret_1y = rollapply(1 + ret, width = 12, FUN = prod, fill = NA, align = "right") - 1,
      #ret_2y = rollapply(1 + ret, width = 24, FUN = prod, fill = NA, align = "right") - 1,
      #ret_3y = rollapply(1 + ret, width = 36, FUN = prod, fill = NA, align = "right") - 1,
      #ret_5y = rollapply(1 + ret, width = 60, FUN = prod, fill = NA, align = "right") - 1
    ) %>%
    ungroup()
  
  # Annualize if requested
  if (annualized) {
    df <- df %>%
      mutate(
        d_gr_q  = d_gr_q * 4,
        d_gr_2y = d_gr_2y / 2,
        d_gr_3y = d_gr_3y / 3,
        d_gr_5y = d_gr_5y / 5,
        
        e_gr_q  = e_gr_q * 4,
        e_gr_2y = e_gr_2y / 2,
        e_gr_3y = e_gr_3y / 3,
        e_gr_5y = e_gr_5y / 5,
        
        ret_q  = ret_q * 4,
        ret_2y = ret_2y / 2,
        ret_3y = ret_3y / 3,
        ret_5y = ret_5y / 5
      )
  }
  
  # Lead values for predictive regressions
  df <- df %>%
    group_by(port) %>%
    mutate(
      # Quarter leads
      d_gr_q_lead = lead(d_gr_q, 3),
      e_gr_q_lead = lead(e_gr_q, 3),
      ret_q_lead  = lead(ret_q, 3),
      p_d_q_lead  = lead(p_d, 3),
      p_e_q_lead  = lead(p_e, 3),
      e_d_q_lead  = lead(e_d, 3),
      
      # 1-year leads
      d_gr_1y_lead = lead(d_gr_1y, 12),
      e_gr_1y_lead = lead(e_gr_1y, 12),
      ret_1y_lead  = lead(ret_1y, 12),
      p_d_1y_lead  = lead(p_d, 12),
      p_e_1y_lead  = lead(p_e, 12),
      e_d_1y_lead  = lead(e_d, 12),
      
      # 2-year leads
      d_gr_2y_lead = lead(d_gr_2y, 24),
      e_gr_2y_lead = lead(e_gr_2y, 24),
      ret_2y_lead  = lead(ret_2y, 24),
      p_d_2y_lead  = lead(p_d, 24),
      p_e_2y_lead  = lead(p_e, 24),
      e_d_2y_lead  = lead(e_d, 24),
      
      # 3-year leads
      d_gr_3y_lead = lead(d_gr_3y, 36),
      e_gr_3y_lead = lead(e_gr_3y, 36),
      ret_3y_lead  = lead(ret_3y, 36),
      p_d_3y_lead  = lead(p_d, 36),
      p_e_3y_lead  = lead(p_e, 36),
      e_d_3y_lead  = lead(e_d, 36),
      
      # 5-year leads
      d_gr_5y_lead = lead(d_gr_5y, 60),
      e_gr_5y_lead = lead(e_gr_5y, 60),
      ret_5y_lead  = lead(ret_5y, 60),
      p_d_5y_lead  = lead(p_d, 60),
      p_e_5y_lead  = lead(p_e, 60),
      e_d_5y_lead  = lead(e_d, 60)
    ) %>%
    ungroup()
  
  return(df)
}

# Usage:
#portfolio_monthly <- data_preparation_portfolio(portfolio_monthly, annualized = FALSE)