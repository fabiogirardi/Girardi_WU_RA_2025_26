library("lubridate") 
library("sparsepca")
library("dplyr")

estimate_model_in_sample <- function(data, data.pc, dep.vars, add.vars, pca.cat.dict, time.col, data.freq, lags, pca, where.funcs, model, 
                                     optional.args=FALSE, sparse.pca = T, alpha.pca = 1e-3, beta.pca = 0, t1 = t1, t2 = t2, n_pc = n_pc, extra = NA){
  
  # TODO hyperparameters currently not in output
  # first load all the functions that are needed
  
  source(file =paste0(where.funcs,"generate_output_table_rolling.R"), chdir = T)
  source(file =paste0(where.funcs,"create_train_test_data.R"), chdir = T)
  source(file =paste0(where.funcs,"estimate_lasso_var_fabio.R"), chdir = T)
  source(file =paste0(where.funcs,"estimate_univariate_var.R"), chdir = T)
  source(file =paste0(where.funcs,"calculate_r2_oos.R"), chdir = T)
  
  # what are the optional arguments (mostly related to LassoVAR)

  if (is.list(optional.args)){
    
    sep.lambdas   <- optional.args$sep.lambdas
    fit.intercept <- optional.args$fit.intercept
    pen.search <-  optional.args$pen.search
  
  }
  
    data_temp = data
    
    #data <- df.freq %>% select_at(c(time.col, dep.vars, "e_gr_1y")) %>% drop_na() %>% as.data.frame()
    data <- data %>% select_at(c(time.col, dep.vars, add.vars)) %>% drop_na() %>% as.data.frame()

    data.pc = data.pc %>% select_at(c(time.col, "month", dep.vars, add.vars)) %>% drop_na() %>% as.data.frame()
    
    if (data.freq == "y"){
      
      data.pc = data.pc[data.pc[,'month'] == 12,]
      
    }
  
  # set the number of fit/predicted values

  n.pred  <- nrow(data %>% dplyr::select(all_of(c(time.col, dep.vars, add.vars))) %>% drop_na()) - 1
  
  # create output tables
  
  output.tables  <- generate_output_table(dep.vars, add.vars, pca, pca.cat.dict, n.pred, sep.lambdas, n_pc)
  r2.is_uncon    <- output.tables$output_table
  r2.is_lasso    <- output.tables$output_table
  r2.is_ridge    <- output.tables$output_table
  r2.is_elnet    <- output.tables$output_table
  r2.is_uncon_adj    <- output.tables$output_table
  r2.is_lasso_adj    <- output.tables$output_table
  r2.is_ridge_adj    <- output.tables$output_table
  r2.is_elnet_adj    <- output.tables$output_table
  hyperparam.out <- output.tables$hyper_table
  
  # reset running variables
  
  j <- 12
  i <- 1
  
  
  if (pca){
    
    chars <- add.vars
    add.vars <- names(pca.cat.dict)

    }
  
  
  vars_unc_list = list()
  coefs_unc_list = list()
  
  vars_lasso_list = list()
  coefs_lasso_list = list()
  
  vars_ridge_list = list()
  coefs_ridge_list = list()
  
  vars_elnet_list = list()
  coefs_elnet_list = list()
  
  
  pred_lasso_t1_list = list()
  pred_lasso_t2_list = list()
  pred_lasso_t3_list = list()
  pred_lasso_t4_list = list()
  pred_lasso_t5_list = list()
  
  pred_ridge_t1_list = list()
  pred_ridge_t2_list = list()
  pred_ridge_t3_list = list()
  pred_ridge_t4_list = list()
  pred_ridge_t5_list = list()
  
  pred_elnet_t1_list = list()
  pred_elnet_t2_list = list()
  pred_elnet_t3_list = list()
  pred_elnet_t4_list = list()
  pred_elnet_t5_list = list()
  
  
  
  if (sparse.pca==T) {
    
    data.train.temp <- data.pc 
    pca.fit = spca(data.train.temp[pca.cat.dict[["full"]]], center=TRUE, scale = TRUE, alpha = alpha.pca, beta = beta.pca, max_iter = 1000, verbose = F)
    pca.X =  scale(data.pc[pca.cat.dict[["full"]]], center=T, scale = F)
    projected = (as.matrix(pca.X) %*% pca.fit$loadings)[,1:n_pc]   %>% as.data.frame 
    colnames(projected) = paste0("SPC",seq(1:n_pc),"_full")
    loadings_spca_full = pca.fit$loadings
    pca_temp <- prcomp(data.train.temp[pca.cat.dict[["full"]]], center=TRUE, scale = TRUE)
    loadings_pca_full = pca_temp$rotation
    
  }else{
    
    data.train.temp <- data.pc 
    pca_temp <- prcomp(data.train.temp[pca.cat.dict[["full"]]], center=TRUE, scale = TRUE)
    pca.X =  scale(data.pc[pca.cat.dict[["full"]]], center=T, scale = F)
    projected <- (as.matrix(pca.X) %*% pca_temp$rotation)[,1:n_pc]   %>% as.data.frame 
    colnames(projected) = paste0("PC",seq(1:n_pc),"_full")
    loadings_pca_full = pca_temp$rotation
    
  }
  
  
  if(model == "all_models"){
    
    jj = 1
    jjj = (length(add.vars)) + 1
    
  }else{
    

    jj <- which(add.vars == model) + 1
    jjj = jj
    
  }
  
  j=6
  
  for (j in jj:jjj) {
    
    # start w/o any characteristic
    
    if (j == 1) {
      
      system.vars = dep.vars
    
    }
    
    if (j > 1 && j<11 ) {
      
        # check if pca characteristics are used
      
        if (pca){
          
          cat <- c(paste0(add.vars[j-1],"_pca_",seq(1:n_pc)), paste0("full_spc_",seq(1:n_pc)))
          
          system.vars = c(dep.vars, cat)
          
        }else{
        
            system.vars = c(dep.vars, add.vars[j-1])
            
            }
    }
    
    if (j == 11 ) {
      
      # check if pca characteristics are used
      
      if (pca){
        
        cat <- c(paste0("full_spc_",seq(1:n_pc)))
        
        system.vars = c(dep.vars, cat)
        
      }else{
        
        system.vars = c(dep.vars, add.vars[j-1])
        
      }
    }
    
    # if j=1 no principal components
    
    if (j == 1 && pca){
      
      data <- data.pc[c(time.col,"month", dep.vars)]
      
    }
    
    # if j>1 construct principal components
      
    if (j > 1 && pca){
      
      df.pc <- data.pc[c(time.col,"month", dep.vars)]
      
      data.train.temp <- data.pc 
      
      if (j > 1 & j<11) {
        
        pca.fit <- prcomp(data.train.temp[pca.cat.dict[[add.vars[j-1]]]], center=TRUE, scale = TRUE)
        pca.X =  scale(data.pc[pca.cat.dict[[add.vars[j-1]]]], center=T, scale = F)
        projected1 <-   (as.matrix(pca.X) %*% pca.fit$rotation)[,1:n_pc]   %>% as.data.frame    
        colnames(projected1) <- paste0(add.vars[j-1],"_pca_", seq(1:n_pc))
        
        data <- cbind(df.pc, projected,projected1) 
        
      }
      
      if (j  == 11) {
        
        data <- cbind(df.pc, projected) 
        
      }
    
      
      if (data.freq == "y") {
      
          data = data %>% filter(month == 12)
      
          }
      
    }

      # set to time series data
      time.series.var = as.numeric(c(format(data[1,time.col], format ="%Y"), get_quarter(data[1,time.col]))) # starting quarter
          
      if (data.freq == "q") {
        data.ts = ts(data = data[,-c(1,2)], start = time.series.var, frequency = 4)
      } else{ 
        data.ts = ts(data = data[,-c(1,2)], start = time.series.var[1], frequency = 1)
      }
      
      if (!fit.intercept){
        uncond.mean <- colMeans(data.ts)
        data.ts <- data.ts - matrix(rep(uncond.mean, times = nrow(data.ts)), nrow = nrow(data.ts), byrow = T)
      }
    
     
    
      #data.ts = scale(data.ts, center=F, scale = T)
      
      ######################################################################################################
      # Constrained Var In sample
      ######################################################################################################
      
      
      ####
      ## Lasso
      ####
      
      
      if (fit.intercept){
        
        lasso.var.out <- estimate_lasso_var( data.ts, lags=lags, intercept=fit.intercept, separate.lambdas=sep.lambdas, alpha=1, pen.search= pen_search, t1 = t1, t2 = t2 )
        
        fitted_var <- t(lasso.var.out$fitted)
        
        res_var = lasso.var.out$res_var
        
        best.hyperparam <- lasso.var.out$best.hyper
        temp.coef.mat   <- lasso.var.out$coefMat
        

        for (n in 1:ncol(data.ts)) {
          
          r2.is_lasso[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2))
          r2.is_lasso_adj[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2)) * (nrow(res_var)-1)/(nrow(res_var)-1-ncol(data.ts))
          
        }

        #c(summary(VAR_est$varresult$d_gr_1y)$r.squared,summary(VAR_est$varresult$p_d)$r.squared,summary(VAR_est$varresult$p_e)$r.squared,summary(VAR_est$varresult$ret_1y)$r.squared, NA, NA)
      
      }else{
        
        lasso.var.out <- estimate_lasso_var( data.ts, lags=lags, intercept=fit.intercept, separate.lambdas=sep.lambdas, alpha=1, pen.search= pen_search, t1 = t1, t2 = t2 )
        
        fitted_var <- t(lasso.var.out$fitted) 
        fitted_var = fitted_var + matrix(rep(uncond.mean, times = nrow(fitted_var)), nrow = nrow(fitted_var), byrow = T)
        
        fitted_t2_var = t(lasso.var.out$fitted_t2) 
        fitted_t2_var = fitted_t2_var + matrix(rep(uncond.mean, times = nrow(fitted_t2_var)), nrow = nrow(fitted_t2_var), byrow = T)
        
        fitted_t3_var = t(lasso.var.out$fitted_t3) 
        fitted_t3_var = fitted_t3_var + matrix(rep(uncond.mean, times = nrow(fitted_t3_var)), nrow = nrow(fitted_t3_var), byrow = T)
        
        fitted_t4_var = t(lasso.var.out$fitted_t4) 
        fitted_t4_var = fitted_t4_var + matrix(rep(uncond.mean, times = nrow(fitted_t4_var)), nrow = nrow(fitted_t4_var), byrow = T)

        fitted_t5_var = t(lasso.var.out$fitted_t5)
        fitted_t5_var = fitted_t5_var + matrix(rep(uncond.mean, times = nrow(fitted_t5_var)), nrow = nrow(fitted_t5_var), byrow = T)
        
        
        best.hyperparam <- lasso.var.out$best.hyper
        temp.coef.mat   <- lasso.var.out$coefMat
      
        
        res_var = lasso.var.out$res_var
          
        for (n in 1:ncol(data.ts)) {
            
            r2.is_lasso[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n])^2))
            r2.is_lasso_adj[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2)) * (nrow(res_var)-1)/(nrow(res_var)-1-ncol(data.ts))
            
        }
      }

      output = cbind(as.Date(as.character(data[-1,'quarter_id'])), as.data.frame(fitted_var))
      colnames(output) = c('quarter_id', paste0(colnames(data.ts),"_pred"))
      output = merge(data, output, by = "quarter_id")
      output[,'e_gr_1y'] = data_temp[-1,'e_gr_1y'] 
      output[,'e_gr_1y_pred'] = output[,'d_gr_1y_pred'] + output[,'p_d_pred'] - output[,'p_e_pred'] + data_temp[1:(nrow(data_temp)-1),'p_e']  - data_temp[1:(nrow(data_temp)-1),'p_d'] 
      
 
      
      vars_lasso_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output
      coef_lasso_list = temp.coef.mat
      
      
      output_t1 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_var))
      colnames(output_t1) = c('quarter_id', paste0(colnames(data.ts),"_pred_t1"))
      pred_lasso_t1_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t1
      
      output_t2 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t2_var))
      colnames(output_t2) = c('quarter_id', paste0(colnames(data.ts),"_pred_t2"))
      pred_lasso_t2_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t2
      
      output_t3 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t3_var))
      colnames(output_t3) = c('quarter_id', paste0(colnames(data.ts),"_pred_t3"))
      pred_lasso_t3_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t3
      
      output_t4 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t4_var))
      colnames(output_t4) = c('quarter_id', paste0(colnames(data.ts),"_pred_t4"))
      pred_lasso_t4_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t4
      
      output_t5 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t5_var))
      colnames(output_t5) = c('quarter_id', paste0(colnames(data.ts),"_pred_t5"))
      pred_lasso_t5_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t5
      
      
      ####
      ## Ridge
      ####
      
      
      if (fit.intercept){
        
        ridge.var.out <- estimate_lasso_var( data.ts, lags=lags, intercept=fit.intercept, separate.lambdas=sep.lambdas, alpha=0.000025, pen.search= pen_search, t1 = t1, t2 = t2 )
        
        fitted_var <- t(ridge.var.out$fitted)
        
        res_var = ridge.var.out$res_var
        
        best.hyperparam <- ridge.var.out$best.hyper
        temp.coef.mat   <- ridge.var.out$coefMat
        
        for (n in 1:ncol(data.ts)) {
            
            r2.is_ridge[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2))
            r2.is_ridge_adj[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2)) * (nrow(res_var)-1)/(nrow(res_var)-1-ncol(data.ts))
        }
        
        
          #c(summary(VAR_est$varresult$d_gr_1y)$r.squared,summary(VAR_est$varresult$p_d)$r.squared,summary(VAR_est$varresult$p_e)$r.squared,summary(VAR_est$varresult$ret_1y)$r.squared, NA, NA)

      }else{
        
        ridge.var.out <- estimate_lasso_var( data.ts, lags=lags, intercept=fit.intercept, separate.lambdas=sep.lambdas, alpha=0.000025, pen.search= pen_search, t1 = t1, t2 = t2 )
        
        fitted_var <- t(ridge.var.out$fitted) 
        fitted_var = fitted_var + matrix(rep(uncond.mean, times = nrow(fitted_var)), nrow = nrow(fitted_var), byrow = T)
        
        fitted_t2_var = t(ridge.var.out$fitted_t2) 
        fitted_t2_var = fitted_t2_var + matrix(rep(uncond.mean, times = nrow(fitted_t2_var)), nrow = nrow(fitted_t2_var), byrow = T)
        
        fitted_t3_var = t(ridge.var.out$fitted_t3) 
        fitted_t3_var = fitted_t3_var + matrix(rep(uncond.mean, times = nrow(fitted_t3_var)), nrow = nrow(fitted_t3_var), byrow = T)
        
        fitted_t4_var = t(ridge.var.out$fitted_t4) 
        fitted_t4_var = fitted_t4_var + matrix(rep(uncond.mean, times = nrow(fitted_t4_var)), nrow = nrow(fitted_t4_var), byrow = T)
        
        fitted_t5_var = t(ridge.var.out$fitted_t5)
        fitted_t5_var = fitted_t5_var + matrix(rep(uncond.mean, times = nrow(fitted_t5_var)), nrow = nrow(fitted_t5_var), byrow = T)
        
        best.hyperparam <- ridge.var.out$best.hyper
        temp.coef.mat   <- ridge.var.out$coefMat
        
        
        res_var = ridge.var.out$res_var
        
        for (n in 1:ncol(data.ts)) {
          
          r2.is_ridge[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n])^2))
          r2.is_ridge_adj[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2)) * (nrow(res_var)-1)/(nrow(res_var)-1-ncol(data.ts))
          
        }
      }
      
      
      output = cbind(as.Date(as.character(data[-1,'quarter_id'])), as.data.frame(fitted_var))
      colnames(output) = c('quarter_id', paste0(colnames(data.ts),"_pred"))
      output = merge(data, output, by = "quarter_id")
      
      output[,'e_gr_1y'] = data_temp[-1,'e_gr_1y'] 
      output[,'e_gr_1y_pred'] = output[,'d_gr_1y_pred'] + output[,'p_d_pred'] - output[,'p_e_pred'] + data_temp[1:(nrow(data_temp)-1),'p_e']  - data_temp[1:(nrow(data_temp)-1),'p_d']
      
      
      vars_ridge_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output
      coef_ridge_list = temp.coef.mat
      
  
      output_t1 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_var))
      colnames(output_t1) = c('quarter_id', paste0(colnames(data.ts),"_pred_t1"))
      pred_ridge_t1_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t1
      
      output_t2 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t2_var))
      colnames(output_t2) = c('quarter_id', paste0(colnames(data.ts),"_pred_t2"))
      pred_ridge_t2_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t2
      
      output_t3 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t3_var))
      colnames(output_t3) = c('quarter_id', paste0(colnames(data.ts),"_pred_t3"))
      pred_ridge_t3_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t3
      
      output_t4 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t4_var))
      colnames(output_t4) = c('quarter_id', paste0(colnames(data.ts),"_pred_t4"))
      pred_ridge_t4_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t4
      
      output_t5 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t5_var))
      colnames(output_t5) = c('quarter_id', paste0(colnames(data.ts),"_pred_t5"))
      pred_ridge_t5_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t5
      
      
      ####
      ## Elastic net
      ####
      
      
      if (fit.intercept){
        
        elasticnet.var.out <- estimate_lasso_var( data.ts, lags=lags, intercept=fit.intercept, separate.lambdas=sep.lambdas, alpha=0.5, pen.search= pen_search, t1 = t1, t2 = t2)
        
        fitted_var <- t(elasticnet.var.out$fitted)
        
        fitted_t2_var = t(elasticnet.var.out$fitted_t2) 
        fitted_t2_var = fitted_t2_var + matrix(rep(uncond.mean, times = nrow(fitted_t2_var)), nrow = nrow(fitted_t2_var), byrow = T)
        
        fitted_t3_var = t(elasticnet.var.out$fitted_t3) 
        fitted_t3_var = fitted_t3_var + matrix(rep(uncond.mean, times = nrow(fitted_t3_var)), nrow = nrow(fitted_t3_var), byrow = T)
        
        fitted_t4_var = t(elasticnet.var.out$fitted_t4) 
        fitted_t4_var = fitted_t4_var + matrix(rep(uncond.mean, times = nrow(fitted_t4_var)), nrow = nrow(fitted_t4_var), byrow = T)
        
        fitted_t5_var = t(elasticnet.var.out$fitted_t5)
        fitted_t5_var = fitted_t5_var + matrix(rep(uncond.mean, times = nrow(fitted_t5_var)), nrow = nrow(fitted_t5_var), byrow = T)
        
        
        res_var = elasticnet.var.out$res_var
        
        best.hyperparam <- elasticnet.var.out$best.hyper
        temp.coef.mat   <- elasticnet.var.out$coefMat
        
        for (n in 1:ncol(data.ts)) {
            
            r2.is_elnet[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2))
            r2.is_elnet_adj[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2)) * (nrow(res_var)-1)/(nrow(res_var)-1-ncol(data.ts))
            
        }
        
          
          #c(summary(VAR_est$varresult$d_gr_1y)$r.squared,summary(VAR_est$varresult$p_d)$r.squared,summary(VAR_est$varresult$p_e)$r.squared,summary(VAR_est$varresult$ret_1y)$r.squared, NA, NA)
        
      }else{
        
        elasticnet.var.out <- estimate_lasso_var( data.ts, lags=lags, intercept=fit.intercept, separate.lambdas=sep.lambdas, alpha=0.5, pen.search= pen_search, t1 = t1, t2 = t2)
        
        fitted_var <- t(elasticnet.var.out$fitted) 
        fitted_var = fitted_var + matrix(rep(uncond.mean, times = nrow(fitted_var)), nrow = nrow(fitted_var), byrow = T)
        
        best.hyperparam <- elasticnet.var.out$best.hyper
        temp.coef.mat   <- elasticnet.var.out$coefMat
        
        
        
        
        res_var = elasticnet.var.out$res_var
        
        for (n in 1:ncol(data.ts)) {
          
          r2.is_elnet[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n])^2))
          r2.is_elnet_adj[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2)) * (nrow(res_var)-1)/(nrow(res_var)-1-ncol(data.ts))
          
          
        }
      }
      
      
      output = cbind(as.Date(as.character(data[-1,'quarter_id'])), as.data.frame(fitted_var))
      colnames(output) = c('quarter_id', paste0(colnames(data.ts),"_pred"))
      output = merge(data, output, by = "quarter_id")
      
      output[,'e_gr_1y'] = data_temp[-1,'e_gr_1y'] 
      output[,'e_gr_1y_pred'] = output[,'d_gr_1y_pred'] + output[,'p_d_pred'] - output[,'p_e_pred'] + data_temp[1:(nrow(data_temp)-1),'p_e']  - data_temp[1:(nrow(data_temp)-1),'p_d']
      
      
      vars_elnet_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output
      coef_elnet_list = temp.coef.mat
      
      
      output_t1 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_var))
      colnames(output_t1) = c('quarter_id', paste0(colnames(data.ts),"_pred_t1"))
      pred_elnet_t1_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t1
      
      output_t2 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t2_var))
      colnames(output_t2) = c('quarter_id', paste0(colnames(data.ts),"_pred_t2"))
      pred_elnet_t2_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t2
      
      output_t3 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t3_var))
      colnames(output_t3) = c('quarter_id', paste0(colnames(data.ts),"_pred_t3"))
      pred_elnet_t3_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t3
      
      output_t4 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t4_var))
      colnames(output_t4) = c('quarter_id', paste0(colnames(data.ts),"_pred_t4"))
      pred_elnet_t4_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t4
      
      output_t5 = cbind(as.Date(as.character(data[-nrow(data),'quarter_id'])), as.data.frame(fitted_t5_var))
      colnames(output_t5) = c('quarter_id', paste0(colnames(data.ts),"_pred_t5"))
      pred_elnet_t5_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output_t5
      
      ######################################################################################################
      # Unconstrained Var In sample
      ######################################################################################################
        
      if (fit.intercept){
        
          type="const"
          VAR_est <- VAR(y=data.ts, p=lags, type=type)
          fitted_var = fitted(VAR_est)
          
          res_var <- resid(VAR_est)
          
          pred = predict(VAR_est, n.ahead=1)
          pred_depvar = as.numeric(matrix(as.data.frame(pred$fcst[system.vars]), ncol = length(system.vars), byrow = F)[1,])
          pred_depvar <- as.matrix(pred_depvar)
          
          if(j ==1){
            
            
            for (n in 1:ncol(data.ts)) {
              
              r2.is_uncon[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2))
              r2.is_uncon_adj[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2)) * (nrow(res_var)-1)/(nrow(res_var)-1-ncol(data.ts))
            }
            
            #c(summary(VAR_est$varresult$d_gr_1y)$r.squared,summary(VAR_est$varresult$p_d)$r.squared,summary(VAR_est$varresult$p_e)$r.squared,summary(VAR_est$varresult$ret_1y)$r.squared, NA, NA)
          
          }else{
            
            
            for (n in 1:ncol(data.ts)) {
              
              r2.is_uncon[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2))
              r2.is_uncon_adj[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2)) * (nrow(res_var)-1)/(nrow(res_var)-1-ncol(data.ts))
              
            }
          }
      
      }else{
      
          type="none"
          VAR_est = VAR(y=data.ts, p=lags, type=type)
          fitted_var = fitted(VAR_est) 
          fitted_var = fitted_var + matrix(rep(uncond.mean, times = nrow(fitted_var)), nrow = nrow(fitted_var), byrow = T)
          res_var <- resid(VAR_est)
            
            for (n in 1:ncol(data.ts)) {
              
              r2.is_uncon[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2))
              r2.is_uncon_adj[j,1+n] = 1- sum(res_var[,n]^2)/ sum(((data.ts[-1,n] - mean(data.ts[-1,n]))^2)) * (nrow(res_var)-1)/(nrow(res_var)-1-ncol(data.ts))
              
            }

            #c(summary(VAR_est$varresult$d_gr_1y)$r.squared,summary(VAR_est$varresult$p_d)$r.squared,summary(VAR_est$varresult$p_e)$r.squared,summary(VAR_est$varresult$ret_1y)$r.squared, NA, NA)
            
      }
    
      coef_unc_list = coef(VAR_est)
      output = cbind(as.Date(as.character(data[-1,'quarter_id'])), as.data.frame(fitted_var))
      colnames(output) = c('quarter_id', paste0(colnames(data.ts),"_pred"))
      output = merge(data, output, by = "quarter_id")
      
      output[,'e_gr_1y'] = data_temp[-1,'e_gr_1y'] 
      output[,'e_gr_1y_pred'] = output[,'d_gr_1y_pred'] + output[,'p_d_pred'] - output[,'p_e_pred'] + data_temp[1:(nrow(data_temp)-1),'p_e']  - data_temp[1:(nrow(data_temp)-1),'p_d']
      
      
      vars_unc_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = output
      
  }
    
  return(list(r2.is_uncon=r2.is_uncon, r2.is_lasso = r2.is_lasso, r2.is_ridge = r2.is_ridge, r2.is_elnet = r2.is_elnet, r2.is_uncon_adj=r2.is_uncon_adj, r2.is_lasso_adj = r2.is_lasso_adj, r2.is_ridge_adj = r2.is_ridge_adj, r2.is_elnet_adj = r2.is_elnet_adj,  vars_unc_list = vars_unc_list,  vars_lasso_list = vars_lasso_list,  vars_ridge_list = vars_ridge_list, vars_elnet_list = vars_elnet_list, loadings_spca_full = loadings_spca_full, loadings_pca_full = loadings_pca_full, coef_unc_list = coef_unc_list, coef_lasso_list = coef_lasso_list, coef_ridge_list = coef_ridge_list, coef_elnet_list = coef_elnet_list, pred_lasso_t1_list = pred_lasso_t1_list, pred_lasso_t2_list = pred_lasso_t2_list, pred_lasso_t3_list = pred_lasso_t3_list, pred_lasso_t4_list = pred_lasso_t4_list, pred_lasso_t5_list = pred_lasso_t5_list, pred_ridge_t1_list = pred_ridge_t1_list, pred_ridge_t2_list = pred_ridge_t2_list, pred_ridge_t3_list = pred_ridge_t3_list, pred_ridge_t4_list = pred_ridge_t4_list, pred_ridge_t5_list = pred_ridge_t5_list, pred_elnet_t1_list = pred_elnet_t1_list, pred_elnet_t2_list = pred_elnet_t2_list, pred_elnet_t3_list = pred_elnet_t3_list, pred_elnet_t4_list = pred_elnet_t4_list, pred_elnet_t5_list = pred_elnet_t5_list))
}


data = df.freq
data.pc= df.freq
dep.vars = DEPVAR
add.vars = CHARS
pca.cat.dict = CATEGORIES
time.col = "quarter_id"
data.freq = freq_var_data
lags = lags
model = "all_models"
pca = T
n_pc = n_pc
where.funcs = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\"
optional.args = list(sep.lambdas=sep_lambdas, fit.intercept = fit_intercept, pen.search = pen_search)
sparse.pca = sparse_pca
alpha.pca = optimal_lambda_2_is
beta.pca = beta_pca
t1 = t1
t2 = t2
extra = "profitability"