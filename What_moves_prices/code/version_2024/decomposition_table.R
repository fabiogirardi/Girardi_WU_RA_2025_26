decomposition_table <- function(data, horizon, kappa){
  
  if (horizon=="y"){
    quarters <- 4
  } else if (horizon=="q"){
    quarters <- 1
  }
  # prediction p-d ratio - historical mean p-d ratio squared
  #data[,"wedge"] <- (data[,"p_d_pred"] - data[,"p_d_hist_mean"])**2
  # define structure of output table
  tab.out = as.data.frame(matrix(NA, nrow = 3, ncol = 4))
  row.names <- c("div_gr", "p_d", "ret")#, "wedge")
  rownames(tab.out) <- row.names
  colnames(tab.out) <- c("horizon", "stdv", "corr", "cov_var_ratio")
  tab.out[row.names, "horizon"] <- quarters

  # standard deviation of predictions/statistical expectations
  if(horizon=="y"){
    vars.pred <- c("d_gr_1y_pred_lead", "p_d_pred_lead", "ret_1y_pred_lead")#, "wedge")
  } else{
    vars.pred <- c("d_gr_q_pred_lead", "p_d_pred_lead", "ret_q_pred_lead")#, "wedge")
  }
  
  tab.out[row.names, "stdv"] <- colSds(as.matrix(data[,vars.pred]), na.rm = T)
  
  # correlation prediction and p-d ratio
  #TODO too much hard coding
  tab.out[row.names,"corr"] <- cor(as.matrix(data[,c(vars.pred, "p_d_real")]),
                                                  use = "pairwise.complete.obs")[vars.pred, "p_d_real"]
  
  # ratio of covariance and variance
  tab.out[row.names,"cov_var_ratio"] = (cov(as.matrix(data[,c(vars.pred, "p_d_real")]),
                                                      use = "pairwise.complete.obs")[vars.pred, "p_d_real"]
                                                      / var(data[,'p_d_real'], na.rm = T))

  # identity value
  tab.out["div_gr", "cs_identity_value"] = tab.out["div_gr", "cov_var_ratio"]
  tab.out["ret", "cs_identity_value"]    = -1*tab.out[ "ret", "cov_var_ratio"]
  tab.out["p_d", "cs_identity_value"]    = tab.out["p_d", "cov_var_ratio"] * kappa
  #tab.out["wedge", "cs_identity_value"]  = tab.out["wedge", "cov_var_ratio"] * 0.5 * (1-kappa) * kappa

  tab.out
}