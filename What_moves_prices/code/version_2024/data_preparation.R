#TODO use loops to make code more compact !!!

data_preparation = function(dataframe, market="shiller", freq.raw.data, annualized = F){
  
  # use fun_insert function to add a hyphen

  source(file=paste0(path_functions,"fun_insert.R"), chdir = T)
  
  dataframe[,'quarter_id'] <- fun_insert(x = dataframe[,'quarter_id'], pos = 4, insert = "-")  # Apply own function
  
  ## convert quarter_id to datetime
  
  dataframe[,'quarter_id']   = as.Date(as.yearqtr(dataframe[,'quarter_id']), format = "%Y%m", frac=1)
  dataframe[,'only_quarter'] = quarters(dataframe[,'quarter_id'])

  ## Create dividend/earning/price growth, return, log P/D and log P/E ratio,
 
  if (market == "shiller") {
    
    dataframe$e_d =  log(dataframe[,'E'])-log(dataframe[,'D'])
    dataframe$p_d =  log(dataframe[,'P'])-log(dataframe[,'D'])
    dataframe$p_e =  log(dataframe[,'P'])-log(dataframe[,'E'])
    
    if (freq.raw.data == "m") {
      

      dataframe$d_gr_q = c(rep(NA,3),diff(log(dataframe[,'D']),3))
      dataframe$e_gr_q = c(rep(NA,3),diff(log(dataframe[,'E']),3))
      dataframe$p_gr_q = c(rep(NA,3),diff(log(dataframe[,'P']),3))
      dataframe$ret_q  = log(dataframe[,'P'] + dataframe[,'D']/4) - log(lag(dataframe[,'P'],3))
      
      
      dataframe$d_gr_1y = c(rep(NA,12),diff(log(dataframe[,'D']),12))
      dataframe$e_gr_1y = c(rep(NA,12),diff(log(dataframe[,'E']),12))
      dataframe$p_gr_1y = c(rep(NA,12),diff(log(dataframe[,'P']),12))
      dataframe$ret_1y  = log(dataframe[,'P'] + dataframe[,'D']) - log(lag(dataframe[,'P'],12))
      
      
      dataframe$d_gr_2y = c(rep(NA,24),diff(log(dataframe[,'D']),24))
      dataframe$e_gr_2y = c(rep(NA,24),diff(log(dataframe[,'E']),24))
      dataframe$p_gr_2y = c(rep(NA,24),diff(log(dataframe[,'P']),24))
      dataframe$ret_2y  = log(dataframe[,'P'] + dataframe[,'D']+ lag(dataframe[,'D'],24)) - log(lag(dataframe[,'P'],24)) 
      
      
      dataframe$d_gr_3y = c(rep(NA,36),diff(log(dataframe[,'D']),36))
      dataframe$e_gr_3y = c(rep(NA,36),diff(log(dataframe[,'E']),36))
      dataframe$p_gr_3y = c(rep(NA,36),diff(log(dataframe[,'P']),36))
      dataframe$ret_3y  = log(dataframe[,'P'] + dataframe[,'D']+ lag(dataframe[,'D'],12)+ lag(dataframe[,'D'],24)) - log(lag(dataframe[,'P'],36)) 
      
      
      dataframe$d_gr_5y = c(rep(NA,60),diff(log(dataframe[,'D']),60))
      dataframe$e_gr_5y = c(rep(NA,60),diff(log(dataframe[,'E']),60))
      dataframe$p_gr_5y = c(rep(NA,60),diff(log(dataframe[,'P']),60))
      dataframe$ret_5y  = log(dataframe[,'P'] + dataframe[,'D']+ lag(dataframe[,'D'],12)+ lag(dataframe[,'D'],24)+ lag(dataframe[,'D'],36)+ lag(dataframe[,'D'],48)) - log(lag(dataframe[,'P'],60)) 
      
      
    }
    
    if (freq.raw.data == "q") {
    
    dataframe$d_gr_q = c(rep(NA,1),diff(log(dataframe[,'D']),1))
    dataframe$e_gr_q = c(rep(NA,1),diff(log(dataframe[,'E']),1))
    dataframe$p_gr_q = c(rep(NA,1),diff(log(dataframe[,'P']),1))
    dataframe$ret_q  = log(dataframe[,'P'] + dataframe[,'D']/4) - log(lag(dataframe[,'P'],1))
  
    
    dataframe$d_gr_1y = c(rep(NA,4),diff(log(dataframe[,'D']),4))
    dataframe$e_gr_1y = c(rep(NA,4),diff(log(dataframe[,'E']),4))
    dataframe$p_gr_1y = c(rep(NA,4),diff(log(dataframe[,'P']),4))
    dataframe$ret_1y  = log(dataframe[,'P'] + dataframe[,'D']) - log(lag(dataframe[,'P'],4))
    
    
    dataframe$d_gr_2y = c(rep(NA,8),diff(log(dataframe[,'D']),8))
    dataframe$e_gr_2y = c(rep(NA,8),diff(log(dataframe[,'E']),8))
    dataframe$p_gr_2y = c(rep(NA,8),diff(log(dataframe[,'P']),8))
    dataframe$ret_2y  = log(dataframe[,'P'] + dataframe[,'D']+ lag(dataframe[,'D'],4)) - log(lag(dataframe[,'P'],8)) 
    
    
    dataframe$d_gr_3y = c(rep(NA,12),diff(log(dataframe[,'D']),12))
    dataframe$e_gr_3y = c(rep(NA,12),diff(log(dataframe[,'E']),12))
    dataframe$p_gr_3y = c(rep(NA,12),diff(log(dataframe[,'P']),12))
    dataframe$ret_3y  = log(dataframe[,'P'] + dataframe[,'D']+ lag(dataframe[,'D'],4)+ lag(dataframe[,'D'],8)) - log(lag(dataframe[,'P'],12)) 
    
    
    dataframe$d_gr_5y = c(rep(NA,20),diff(log(dataframe[,'D']),20))
    dataframe$e_gr_5y = c(rep(NA,20),diff(log(dataframe[,'E']),20))
    dataframe$p_gr_5y = c(rep(NA,20),diff(log(dataframe[,'P']),20))
    dataframe$ret_5y  = log(dataframe[,'P'] + dataframe[,'D']+ lag(dataframe[,'D'],4)+ lag(dataframe[,'D'],8)+ lag(dataframe[,'D'],12)+ lag(dataframe[,'D'],16)) - log(lag(dataframe[,'P'],20)) 
    
    }
    
    
    
  } else{
    
    if (freq.raw.data == "m") {
      
      dataframe$e_d =  log(dataframe[,'earnings_index'])-log(dataframe[,'dividends_index'])
      dataframe$p_d =  log(dataframe[,'sp500'])-log(dataframe[,'dividends_index'])
      dataframe$p_e =  log(dataframe[,'sp500'])-log(dataframe[,'earnings_index'])
      
      dataframe$d_gr_q = c(rep(NA,3),diff(log(dataframe[,'dividends_index']),3))
      dataframe$e_gr_q = c(rep(NA,3),diff(log(dataframe[,'earnings_index']),3))
      dataframe$p_gr_q = c(rep(NA,3),diff(log(dataframe[,'sp500']),3))
      dataframe$ret_q  = ((dataframe[,'sp500'] + dataframe[,'dividends_index']/4)/lag(dataframe[,'sp500'],3) -1)
      
      dataframe$d_gr_1y = c(rep(NA,12),diff(log(dataframe[,'dividends_index']),12))
      dataframe$e_gr_1y = c(rep(NA,12),diff(log(dataframe[,'earnings_index']),12))
      dataframe$p_gr_1y = c(rep(NA,12),diff(log(dataframe[,'sp500']),12))
      dataframe$ret_1y  = ((dataframe[,'sp500'] + dataframe[,'dividends_index'])/lag(dataframe[,'sp500'],12) -1)
      
      dataframe$d_gr_5y = c(rep(NA,60),diff(log(dataframe[,'dividends_index']),60))
      dataframe$e_gr_5y = c(rep(NA,60),diff(log(dataframe[,'earnings_index']),60))
      dataframe$p_gr_5y = c(rep(NA,60),diff(log(dataframe[,'sp500']),60))
      dataframe$ret_5y  = ((dataframe[,'sp500'] + dataframe[,'dividends_index'] + lag(dataframe[,'dividends_index'],12) + lag(dataframe[,'dividends_index'],24) + lag(dataframe[,'dividends_index'],36) + lag(dataframe[,'dividends_index'],48))/lag(dataframe[,'sp500'],60) -1)
      
    }
    
    if (freq.raw.data == "q") {
      
      dataframe$e_d =  log(dataframe[,'earnings_index'])-log(dataframe[,'dividends_index'])
      dataframe$p_d =  log(dataframe[,'sp500'])-log(dataframe[,'dividends_index'])
      dataframe$p_e =  log(dataframe[,'sp500'])-log(dataframe[,'earnings_index'])
      
      dataframe$d_gr_q = c(rep(NA,1),diff(log(dataframe[,'dividends_index']),1))
      dataframe$e_gr_q = c(rep(NA,1),diff(log(dataframe[,'earnings_index']),1))
      dataframe$p_gr_q = c(rep(NA,1),diff(log(dataframe[,'sp500']),1))
      dataframe$ret_q  = ((dataframe[,'sp500'] + dataframe[,'dividends_index'])/lag(dataframe[,'sp500'],1) -1)
      
      dataframe$d_gr_1y = c(rep(NA,4),diff(log(dataframe[,'dividends_index']),4))
      dataframe$e_gr_1y = c(rep(NA,4),diff(log(dataframe[,'earnings_index']),4))
      dataframe$p_gr_1y = c(rep(NA,4),diff(log(dataframe[,'sp500']),4))
      dataframe$ret_1y  = ((dataframe[,'sp500'] + dataframe[,'dividends_index'])/lag(dataframe[,'sp500'],4) -1)
      
      dataframe$d_gr_5y = c(rep(NA,20),diff(log(dataframe[,'dividends_index']),20))
      dataframe$e_gr_5y = c(rep(NA,20),diff(log(dataframe[,'earnings_index']),20))
      dataframe$p_gr_5y = c(rep(NA,20),diff(log(dataframe[,'sp500']),20))
      dataframe$ret_5y  = ((dataframe[,'sp500'] + dataframe[,'dividends_index'] + lag(dataframe[,'dividends_index'],4) + lag(dataframe[,'dividends_index'],8) + lag(dataframe[,'dividends_index'],12) + lag(dataframe[,'dividends_index'],16))/lag(dataframe[,'sp500'],20) -1)
      
    }
  }


  if(annualized == T){
    
    dataframe$d_gr_q = dataframe$d_gr_q * 4
    dataframe$e_gr_q = dataframe$e_gr_q * 4
    dataframe$p_gr_q = dataframe$p_gr_q * 4
    dataframe$ret_q  = dataframe$ret_q  * 4
    
    dataframe$d_gr_2y = dataframe$d_gr_2y / 2
    dataframe$e_gr_2y = dataframe$e_gr_2y / 2
    dataframe$p_gr_2y = dataframe$p_gr_2y / 2
    dataframe$ret_2y  = dataframe$ret_2y  / 2
    
    dataframe$d_gr_3y = dataframe$d_gr_3y / 3
    dataframe$e_gr_3y = dataframe$e_gr_3y / 3
    dataframe$p_gr_3y = dataframe$p_gr_3y / 3
    dataframe$ret_3y  = dataframe$ret_3y  / 3
    
    dataframe$d_gr_5y = dataframe$d_gr_5y / 5
    dataframe$e_gr_5y = dataframe$e_gr_5y / 5
    dataframe$p_gr_5y = dataframe$p_gr_5y / 5
    dataframe$ret_5y  = dataframe$ret_5y  / 5
    
  }



  ## Lead values
  
  if (freq.raw.data == "m") {
    
    dataframe$d_gr_q_lead = lead(dataframe$d_gr_q,3)
    dataframe$e_gr_q_lead = lead(dataframe$e_gr_q,3)
    dataframe$p_gr_q_lead = lead(dataframe$p_gr_q,3)
    dataframe$ret_q_lead  = lead(dataframe$ret_q,3)
    dataframe$p_d_q_lead  = lead(dataframe$p_d,3)
    dataframe$p_e_q_lead  = lead(dataframe$p_e,3)
    dataframe$e_d_q_lead  = lead(dataframe$e_d,3)
    
    dataframe$d_gr_1y_lead = lead(dataframe$d_gr_1y,12)
    dataframe$e_gr_1y_lead = lead(dataframe$e_gr_1y,12)
    dataframe$p_gr_1y_lead = lead(dataframe$p_gr_1y,12)
    dataframe$ret_1y_lead  = lead(dataframe$ret_1y,12)
    dataframe$p_d_1y_lead = lead(dataframe$p_d,12)
    dataframe$p_e_1y_lead = lead(dataframe$p_e,12)
    dataframe$e_d_1y_lead = lead(dataframe$e_d,12)
    
    dataframe$d_gr_2y_lead = lead(dataframe$d_gr_2y,24)
    dataframe$e_gr_2y_lead = lead(dataframe$e_gr_2y,24)
    dataframe$p_gr_2y_lead = lead(dataframe$p_gr_2y,24)
    dataframe$ret_2y_lead  = lead(dataframe$ret_2y,24)
    dataframe$p_d_2y_lead = lead(dataframe$p_d,24)
    dataframe$p_e_2y_lead = lead(dataframe$p_e,24)
    dataframe$e_d_2y_lead = lead(dataframe$e_d,24)
    
    dataframe$d_gr_3y_lead = lead(dataframe$d_gr_3y,36)
    dataframe$e_gr_3y_lead = lead(dataframe$e_gr_3y,36)
    dataframe$p_gr_3y_lead = lead(dataframe$p_gr_3y,36)
    dataframe$ret_3y_lead  = lead(dataframe$ret_3y,36)
    dataframe$p_d_3y_lead = lead(dataframe$p_d,36)
    dataframe$p_e_3y_lead = lead(dataframe$p_e,36)
    dataframe$e_d_3y_lead = lead(dataframe$e_d,36)
    
    dataframe$d_gr_5y_lead = lead(dataframe$d_gr_5y,60)
    dataframe$e_gr_5y_lead = lead(dataframe$e_gr_5y,60)
    dataframe$p_gr_5y_lead = lead(dataframe$p_gr_5y,60)
    dataframe$ret_5y_lead  = lead(dataframe$ret_5y,60)
    dataframe$p_d_5y_lead = lead(dataframe$p_d,60)
    dataframe$p_e_5y_lead = lead(dataframe$p_e,60)
    dataframe$e_d_5y_lead = lead(dataframe$e_d,60)
    
  }

  if (freq.raw.data == "q") {
    
    dataframe$d_gr_q_lead = lead(dataframe$d_gr_q,1)
    dataframe$e_gr_q_lead = lead(dataframe$e_gr_q,1)
    dataframe$p_gr_q_lead = lead(dataframe$p_gr_q,1)
    dataframe$ret_q_lead  = lead(dataframe$ret_q,1)
    dataframe$p_d_q_lead  = lead(dataframe$p_d,1)
    dataframe$p_e_q_lead  = lead(dataframe$p_e,1)
    dataframe$e_d_q_lead  = lead(dataframe$e_d,1)
    
    dataframe$d_gr_1y_lead = lead(dataframe$d_gr_1y,4)
    dataframe$e_gr_1y_lead = lead(dataframe$e_gr_1y,4)
    dataframe$p_gr_1y_lead = lead(dataframe$p_gr_1y,4)
    dataframe$ret_1y_lead  = lead(dataframe$ret_1y,4)
    dataframe$p_d_1y_lead = lead(dataframe$p_d,4)
    dataframe$p_e_1y_lead = lead(dataframe$p_e,4)
    dataframe$e_d_1y_lead = lead(dataframe$e_d,4)
    
    dataframe$d_gr_2y_lead = lead(dataframe$d_gr_2y,8)
    dataframe$e_gr_2y_lead = lead(dataframe$e_gr_2y,8)
    dataframe$p_gr_2y_lead = lead(dataframe$p_gr_2y,8)
    dataframe$ret_2y_lead  = lead(dataframe$ret_2y,8)
    dataframe$p_d_2y_lead = lead(dataframe$p_d,8)
    dataframe$p_e_2y_lead = lead(dataframe$p_e,8)
    dataframe$e_d_2y_lead = lead(dataframe$e_d,8)
    
    dataframe$d_gr_3y_lead = lead(dataframe$d_gr_3y,12)
    dataframe$e_gr_3y_lead = lead(dataframe$e_gr_3y,12)
    dataframe$p_gr_3y_lead = lead(dataframe$p_gr_3y,12)
    dataframe$ret_3y_lead  = lead(dataframe$ret_3y,12)
    dataframe$p_d_3y_lead = lead(dataframe$p_d,12)
    dataframe$p_e_3y_lead = lead(dataframe$p_e,12)
    dataframe$e_d_3y_lead = lead(dataframe$e_d,12)
    
    dataframe$d_gr_5y_lead = lead(dataframe$d_gr_5y,20)
    dataframe$e_gr_5y_lead = lead(dataframe$e_gr_5y,20)
    dataframe$p_gr_5y_lead = lead(dataframe$p_gr_5y,20)
    dataframe$ret_5y_lead  = lead(dataframe$ret_5y,20)
    dataframe$p_d_5y_lead = lead(dataframe$p_d,20)
    dataframe$p_e_5y_lead = lead(dataframe$p_e,20)
    dataframe$e_d_5y_lead = lead(dataframe$e_d,20)
    
  }
  

  return(dataframe)        
}