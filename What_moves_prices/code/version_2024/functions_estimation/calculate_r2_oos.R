calculate_r2_oos <- function(real.pred.matrix, full.data, variables){
      # number of training samples
      n.train <- nrow(full.data) - nrow(real.pred.matrix)  
      output <- list()
      for (var in variables){
        real.var <- paste0(var)
        pred.var <- paste0(var,"_pred")
        # calculate historical mean
        hist.mean <- (cumsum(full.data[,var]) / seq_along(full.data[,var]))[(n.train+1):nrow(full.data)]
        # squared prediction error
        numerator   <- sum((real.pred.matrix[,real.var] - real.pred.matrix[,pred.var])**2, na.rm = T)
        # error of benchmark (hist mean)
        denominator <- sum((real.pred.matrix[,real.var] - hist.mean)**2, na.rm = T)
        # R2 against benchmark
        r2.oos <- 1 - numerator/ denominator
        output[var] <- r2.oos 
      }
      return(output)
}


#real.pred.matrix = var_hat

#full.data = data

#variables = system.vars
