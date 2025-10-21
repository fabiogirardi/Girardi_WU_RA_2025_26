## Filter the data for the chosen frequency
choose_data_frequency <- function(dataframe, freq_column="month", freq="y"){
  if (freq == "q") {
    data <- dataframe    
    DEPVAR = c('d_gr_q','p_d','p_e',"ret_q")
  }else if(freq == "y") {
    data <- dataframe[dataframe[,freq_column]==12,]
    DEPVAR = c('d_gr_1y','p_d','p_e',"ret_1y")
  }
  return(list(dataframe=data, dep_var=DEPVAR))
}