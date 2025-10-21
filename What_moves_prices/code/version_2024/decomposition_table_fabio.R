decomposition_table <- function(data, horizon, kappa, sub_1){

 
  if (horizon=="1y"){
    
    quarters <- "1 year"
    
  }else if (horizon=="5y"){
   
    quarters <- "5 years"
  
  }
  
  row.names <- c("div_gr","ear_gr","p_d", "p_e", "ret") #, "wedge")
  
  # define structure of output table
  
  for (type in c("real","pred")) {
    
  
  tab.out = as.data.frame(matrix(NA, nrow = 10, ncol = 8))
  
  tab.out[,1] <- row.names
  colnames(tab.out) <- c("variable", "horizon", "sample", "sdv", "cor_pd", "cov_var_ratio_pd", "cor_pe", "cov_var_ratio_pe" )
  tab.out[tab.out[,'variable'] == row.names, "horizon"] <- quarters

  # standard deviation of predictions/statistical expectations
  if(horizon=="1y"){
  
    if(type=="pred"){
      
    vars.pred = c("d_gr_1y_pred_1y_lead","e_gr_1y_pred_1y_lead", "p_d_pred_1y_lead", "p_e_pred_1y_lead", "ret_1y_pred_1y_lead")
    
    }
    
    if(type=="real"){
      
      vars.pred = c("d_gr_1y_real_1y_lead","e_gr_1y_real_1y_lead", "p_d_real_1y_lead", "p_e_real_1y_lead", "ret_1y_real_1y_lead")
      
    }
  }
  
  if(horizon=="5y"){
    
    if(type=="pred"){
      
      vars.pred = c("d_gr_5y_pred_5y_lead", "p_d_pred_5y_lead", "p_e_pred_5y_lead", "ret_5y_pred_5y_lead")
    
      }
    
    if(type=="real"){
      
      vars.pred = c("d_gr_5y_real_5y_lead", "p_d_real_5y_lead", "p_e_real_5y_lead", "ret_5y_real_5y_lead")
    
      }
  }
  
  ## full sample
 
  sample = paste0(format(data[,'quarter_id'], format="%Y")[1],"-",format(data[,'quarter_id'], format="%Y")[nrow(data)])
  
  tab.out[1:5, "sample"] <- as.character(sample)
  
  tab.out[tab.out[,'sample'] %in% sample, "sdv"] <- colSds(as.matrix(data[,vars.pred]), na.rm = T)
  
  # correlation prediction with p-d and p-e ratios

  tab.out[tab.out[,'variable'] == row.names  & !is.na(tab.out[,'sample']),"cor_pd"] <- cor(as.matrix(data[,c(vars.pred, "p_d_real")]), use = "pairwise.complete.obs")[vars.pred, "p_d_real"]
  
  tab.out[tab.out[,'variable'] == row.names  & !is.na(tab.out[,'sample']),"cor_pe"] <- cor(as.matrix(data[,c(vars.pred, "p_e_real")]), use = "pairwise.complete.obs")[vars.pred, "p_e_real"]
  
  
  # ratio between covariance and variance
  
  tab.out[tab.out[,'variable'] == row.names  & !is.na(tab.out[,'sample']),"cov_var_ratio_pd"] = (cov(as.matrix(data[,c(vars.pred, "p_d_real")]), use = "pairwise.complete.obs")[vars.pred, "p_d_real"] / var(data[,'p_d_real'], na.rm = T))
  
  tab.out[tab.out[,'variable'] == row.names  & !is.na(tab.out[,'sample']),"cov_var_ratio_pe"] = (cov(as.matrix(data[,c(vars.pred, "p_e_real")]), use = "pairwise.complete.obs")[vars.pred, "p_e_real"] / var(data[,'p_e_real'], na.rm = T))
  

  ## sub_1 sample

  sample = paste0(year(sub_1),"-",year(data[,'quarter_id'])[nrow(data)])
  
  data_sub = data[data['quarter_id'] >= sub_1,]
  
  tab.out[6:10, "sample"] <- sample
  
  tab.out[tab.out[,'sample'] %in% sample, "sdv"] <- colSds(as.matrix(data_sub[,vars.pred]), na.rm = T)
  
  # correlation prediction with p-d and p-e ratios
  
  tab.out[tab.out[,'sample'] %in% sample,"cor_pd"] <- cor(as.matrix(data_sub[,c(vars.pred, "p_d_real")]), use = "pairwise.complete.obs")[vars.pred, "p_d_real"]
  
  tab.out[tab.out[,'sample'] %in% sample,"cor_pe"] <- cor(as.matrix(data_sub[,c(vars.pred, "p_e_real")]), use = "pairwise.complete.obs")[vars.pred, "p_e_real"]
  
  
  # ratio between covariance and variance
  
  tab.out[tab.out[,'sample'] %in% sample,"cov_var_ratio_pd"] = (cov(as.matrix(data_sub[,c(vars.pred, "p_d_real")]), use = "pairwise.complete.obs")[vars.pred, "p_d_real"] / var(data_sub[,'p_d_real'], na.rm = T))
  
  tab.out[tab.out[,'sample'] %in% sample,"cov_var_ratio_pe"] = (cov(as.matrix(data_sub[,c(vars.pred, "p_e_real")]), use = "pairwise.complete.obs")[vars.pred, "p_e_real"] / var(data_sub[,'p_e_real'], na.rm = T))
  
  tab.out = tab.out %>% mutate(across(where(is.numeric), ~ round(. , 3)))
  
  assign(paste0("tab.out.",type),tab.out)
  
}
  # identity value
  #tab.out["div_gr", "cs_identity_value"] = tab.out["div_gr", "cov_var_ratio"]
  #tab.out["ret", "cs_identity_value"]    = -1*tab.out[ "ret", "cov_var_ratio"]
  #tab.out["p_d", "cs_identity_value"]    = tab.out["p_d", "cov_var_ratio"] * kappa
  #tab.out["wedge", "cs_identity_value"]  = tab.out["wedge", "cov_var_ratio"] * 0.5 * (1-kappa) * kappa

  return(list(tab.out.real = tab.out.real, tab.out.pred = tab.out.pred))
}


#data = tempData
#horizon = "1y"
#type = "pred"
#kappa=kappa
#sub_1 = "2001-01-01"