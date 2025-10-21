library("lubridate") 
library("sparsepca")
library("dplyr")

estimate_model <- function(data, data.pc, dep.vars, add.vars, pca.cat.dict, model, type, time.col, data.freq, split.ratio, lags, pca, rolling.pca=TRUE, where.funcs, optional.args=FALSE, sparse.pca, alpha.pca, beta.pca, t1 = t1, t2 = t2, n_pc = n_pc, extra = NA){
  
  # TODO hyperparameters currently not in output
  # first load all the functions that are needed

  source(file =paste0(where.funcs,"generate_output_table_rolling.R"), chdir = T)
  source(file =paste0(where.funcs,"create_train_test_data.R"), chdir = T)
  source(file =paste0(where.funcs,"estimate_lasso_var_fabio_oos.R"), chdir = T)
  source(file =paste0(where.funcs,"estimate_univariate_var.R"), chdir = T)
  source(file =paste0(where.funcs,"calculate_r2_oos.R"), chdir = T)
  
  # what are the optional arguments (mostly related to LassoVAR)
  
  if (is.list(optional.args)){
    
    sep.lambdas = optional.args$sep.lambdas
    fit.intercept = optional.args$fit.intercept
    alpha = optional.args$alpha
    pen.search = optional.args$pen.search
  
    }
  
  
  data <- data %>% select_at(c(time.col, dep.vars, add.vars)) %>% drop_na() %>% as.data.frame()
  
  data.pc = data.pc %>% select_at(c(time.col, "month", dep.vars, add.vars)) %>% drop_na() %>% as.data.frame()
  
  
  if (data.freq == "y"){
    
    data.pc = data.pc[data.pc[,'month'] == 12,]
    
  }
  
  
  # choose initial split of the data
  train.date <- sort(data[,time.col])[nrow(data)*split.ratio]
  n.test  <- nrow(data %>% filter(!!as.symbol(time.col) > train.date) %>% dplyr::select(all_of(c(time.col, dep.vars, add.vars))) %>% drop_na())
  
  
  # create output tables
  output.tables <- generate_output_table(dep.vars, add.vars, pca, pca.cat.dict, n.test, sep.lambdas, n_pc)
  r2.oos.out <- output.tables$output_table
  hyperparam.out <- output.tables$hyper_table
  
  
  # reset running variables
  j <- 11
  i <- 1

  if (pca){
    
    chars <- add.vars
    add.vars <- names(pca.cat.dict)
    
    # no rolling PCA; note: PCA on quarterly data freq.
    
    if (!rolling.pca){
      
      # initialize output dataframe
      
      df.pc <- data.pc[c(time.col,"month", dep.vars)]
      
      # train data
      
      data.train.temp <- data.pc %>% filter(!!as.symbol(time.col) <= train.date)
      
      for (c in add.vars){
      
        # PCA
        pca.fit <- prcomp(data.train.temp[pca.cat.dict[[c]]], center=TRUE, scale = TRUE)
        # rotate all data to learned rotation
        projected <- predict(pca.fit, data.pc)[, paste0("PC",seq(1:n_pc))] %>% as.data.frame
        colnames(projected) <- paste0("pca_",seq(1:n_pc))
        # add to inital dataframe
        df.pc <- cbind(df.pc, projected)
        
      }
      
      # new data
      
      data <- df.pc %>% filter(month == 12)
    
      }
  }
  
  vars_list = list()
  coefs_list = list()
  
  if(model == "all_models"){
    
    jj = 1
    jjj = (length(add.vars))
    
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
    
    if (j > 1) {
      
      # check if pca characteristics are used
      
      if (pca){
        
        cat <- c(paste0("SPC",seq(1:n_pc),"_",add.vars[10]), paste0("PC",seq(1:n_pc),"_",add.vars[j-1]))
        
        system.vars = c(dep.vars, cat)
        
      }
    }
    

    # create matrix to store the results for model j
    var_hat    <- as.data.frame(matrix(NA, nrow=n.test, ncol=(length(system.vars)+2)))
    colnames(var_hat) = c(time.col, system.vars, "p_d_hist_mean")
    
    # now manually loop forward through the testing set
    
    for (i in 1:n.test){
      
      # iterate forward the train-test-cutoff
      
      if (data.freq == "q") {
        
        test.date = ceiling_date( train.date %m+%  months(3*i), "month") - days(1)
        
      }else if (data.freq == "y"){
        
        test.date = ceiling_date( train.date %m+%  months(12*i), "month") - days(1)
        
      }
      
     
      # rolling PCA
      #! predicting the PCA is strange now: the rotation of the last determines the targets!
      
      if (j == 1 && pca){
        
        df.pc <- data.pc[c(time.col,"month", dep.vars)]
        
      }
      
      if (j > 1 && pca && rolling.pca){
        
        df.pc <- data.pc[c(time.col,"month", dep.vars)]
        data.train.temp <- data.pc %>% filter(!!as.symbol(time.col) < test.date)
        
        if (sparse.pca==F  &  j > 1 ) {
          
          pca.fit <- prcomp(data.train.temp[pca.cat.dict[["full"]]], center=TRUE, scale = TRUE)
          pca.X =  scale(data.train.temp[pca.cat.dict[["full"]]], center=T, scale = F)
          projected <-  (as.matrix(pca.X) %*% pca.fit$rotation)[,1:n_pc]   %>% as.data.frame    
          colnames(projected) <- paste0("PC",seq(1:n_pc),"_full")
          
          
          pca.fit <- prcomp(data.train.temp[pca.cat.dict[[add.vars[j-1]]]], center=TRUE, scale = TRUE)
          pca.X =  scale(data.train.temp[pca.cat.dict[[add.vars[j-1]]]], center=T, scale = F)
          projected1 <-  (as.matrix(pca.X) %*% pca.fit$rotation)[,1:n_pc]   %>% as.data.frame    
          colnames(projected1) = paste0("PC",seq(1:n_pc),"_",add.vars[j-1])
          
          projected = cbind(projected,projected1)
          
        }
        
        
        if (sparse.pca==T  &  j > 1 ) {
          
          pca.fit <- spca(data.train.temp[pca.cat.dict[["full"]]], center=TRUE, scale = TRUE, alpha = alpha.pca, beta = beta.pca, max_iter = 1000, verbose = F)
          pca.X =  scale(data.train.temp[pca.cat.dict[["full"]]], center=T, scale = F)
          projected <- (as.matrix(pca.X) %*% pca.fit$loadings)[,1:n_pc]   %>% as.data.frame 
          colnames(projected) = paste0("SPC",seq(1:n_pc),"_full")
          loadings_spca_full = pca.fit$loadings; 
          
          pca_temp <- prcomp(data.train.temp[pca.cat.dict[["full"]]], center=TRUE, scale = TRUE)
          loadings_pca_full = pca_temp$rotation
          
          
          pca.fit <- prcomp(data.train.temp[pca.cat.dict[[add.vars[j-1]]]], center=TRUE, scale = TRUE)
          pca.X =  scale(data.train.temp[pca.cat.dict[[add.vars[j-1]]]], center=T, scale = F)
          projected1 <-  (as.matrix(pca.X) %*% pca.fit$rotation)[,1:n_pc]   %>% as.data.frame    
          colnames(projected1) = paste0("PC",seq(1:n_pc),"_",add.vars[j-1])
          
          projected = cbind(projected,projected1)
          
        }
        
        
        # rotate all data to learned rotation
        #projected <- predict(pca.fit, data.pc)[, c("PC1", "PC2")] %>% as.data.frame
        #colnames(projected) <- c(paste0(add.vars[j-1],"_pca_", seq(1:n_pc)))
        df.pcc = df.pc %>% filter(!!as.symbol(time.col) < test.date)
        data <- cbind(df.pcc, projected) %>% filter(month == 12)
        
      }
   
      # get train data
      
      train.test.data = create_train_test_data(data, test.date, train.date, time.col, system.vars, as.time.series=TRUE, data.freq=data.freq)
      train.data.ts   = train.test.data$train.data
      
      
      if (!fit.intercept){
        
        uncond.mean <- colMeans(train.data.ts)
        train.data.ts <- train.data.ts - uncond.mean[col(train.data.ts)]
      
        }
      
      # run the model
      
      if (type =="constrained"){
        
        lasso.var.out   <- estimate_lasso_var( train.data.ts, lags=lags, intercept=fit.intercept, separate.lambdas=sep.lambdas, alpha=alpha, pen.search= pen_search, t1 = t1, t2 = t2)
        predictions     <- lasso.var.out$prediction 
        best.hyperparam <- lasso.var.out$best.hyper
        temp.coef.mat   <- lasso.var.out$coefMat
        
      } else if (type =="unconstrained"){
        
        predictions <- estimate_univariate_var(train.data.ts, system.vars, lags=lags, intercept=fit.intercept)
      
      }
      
      if (!fit.intercept){
        
        predictions <- predictions + uncond.mean
        
      }
      
      # write predictions to output matrix
      
      var_hat[i, time.col]    <- as.character(test.date)
      var_hat[i, system.vars] <- predictions
      p_d_hist = data %>% select_at(c(time.col, dep.vars, "p_d")) %>% drop_na() %>% filter(!!as.symbol(time.col) < test.date) %>% select_at(c("p_d"))  %>%  mutate_at("p_d", as.numeric) %>% as.matrix()
      var_hat[i, "p_d_hist_mean"] <- mean(p_d_hist)
      # write coefficients to output matrix
      class(p_d_hist)
      
      if(type == "constrained"){
        
      if (i==1){
        
        coef.mat <- t(temp.coef.mat)
        
        colnames(coef.mat) <- system.vars
      
        } else{
        
          coef.mat <- rbind(coef.mat, t(temp.coef.mat))
          
          }
        }
      
    }
    
    
    data.train.temp <- data.pc %>% filter(!!as.symbol(time.col) <= test.date)
    
    if(j!= 1){
      
      if (sparse.pca==F  &  j > 1 ) {
        
        pca.fit <- prcomp(data.train.temp[pca.cat.dict[["full"]]], center=TRUE, scale = TRUE)
        pca.X =  scale(data.train.temp[pca.cat.dict[["full"]]], center=T, scale = F)
        projected <-  (as.matrix(pca.X) %*% pca.fit$rotation)[,1:n_pc]   %>% as.data.frame    
        colnames(projected) <- paste0("PC",seq(1:n_pc),"_full")
        
        
        pca.fit <- prcomp(data.train.temp[pca.cat.dict[[add.vars[j-1]]]], center=TRUE, scale = TRUE)
        pca.X =  scale(data.train.temp[pca.cat.dict[[add.vars[j-1]]]], center=T, scale = F)
        projected1 <-  (as.matrix(pca.X) %*% pca.fit$rotation)[,1:n_pc]   %>% as.data.frame    
        colnames(projected1) = paste0("PC",seq(1:n_pc),"_",add.vars[j-1])
        
        projected = cbind(projected,projected1)
        
      }
      
      
      if (sparse.pca==T  &  j > 1 ) {
        
        pca.fit <- spca(data.train.temp[pca.cat.dict[["full"]]], center=TRUE, scale = TRUE, alpha = alpha.pca, beta = beta.pca, max_iter = 1000, verbose = F)
        pca.X =  scale(data.train.temp[pca.cat.dict[["full"]]], center=T, scale = F)
        projected <- (as.matrix(pca.X) %*% pca.fit$loadings)[,1:n_pc]   %>% as.data.frame 
        colnames(projected) = paste0("SPC",seq(1:n_pc),"_full")
        loadings_spca_full = pca.fit$loadings; 
        
        pca_temp <- prcomp(data.train.temp[pca.cat.dict[["full"]]], center=TRUE, scale = TRUE)
        loadings_pca_full = pca_temp$rotation
        
        
        pca.fit <- prcomp(data.train.temp[pca.cat.dict[[add.vars[j-1]]]], center=TRUE, scale = TRUE)
        pca.X =  scale(data.train.temp[pca.cat.dict[[add.vars[j-1]]]], center=T, scale = F)
        projected1 <-  (as.matrix(pca.X) %*% pca.fit$rotation)[,1:n_pc]   %>% as.data.frame    
        colnames(projected1) = paste0("PC",seq(1:n_pc),"_",add.vars[j-1])
        
        projected = cbind(projected,projected1)
        
      }
    
    #colnames(projected) <-  paste0("PC",seq(1:n_pc),"_",add.vars[j-1])
    data <- cbind(df.pc, projected) %>% filter(month == 12)
    
    } else {
      
      data = df.pc
      
    }
    
    # get test data
    
    train.test.data <- create_train_test_data(data, test.date, train.date, time.col, system.vars, as.time.series=TRUE, data.freq=data.freq, only.train=F, only.test=T)
    
    test.data   <- train.test.data$test.data
    
    
    var_hat[,time.col]  = as.Date(var_hat[,time.col])
    colnames(var_hat) = c('quarter_id', paste0(colnames(test.data)[-1],"_pred"))
    
    var_hat <- merge(test.data, var_hat, by=time.col)
    
    cor(var_hat[,'ret_1y'],var_hat[,'ret_1y_pred'])
    cor(var_hat[,'d_gr_1y'],var_hat[,'d_gr_1y_pred'])
    # collect individual predictions in output list
    #vars_list[[paste0("var_hat_", if(j==1){"depvar"} else{add.vars[j-1]})]] = var_hat
    vars_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = var_hat
    
    if(type == "constrained"){
    coefs_list[[if(j==1){"no_pc"} else{add.vars[j-1]}]] = coef.mat
    }
    
    # calculate OOS statistics
    oos.r2 <- calculate_r2_oos(var_hat, data, system.vars)
    # now write to output matrix
    if (pca){
      if (j==1){oos.r2 <- c(oos.r2, pca1=NA, pca2=NA)}
      r2.oos.out[j,c(dep.vars, paste0("spc",seq(1:n_pc)), paste0("pc",seq(1:n_pc)))] <- oos.r2    
    }else{
      if (j==1){oos.r2 <- c(oos.r2, extra=NA)}
      r2.oos.out[j,c(dep.vars, "extra")] <- oos.r2    
    }
    
    if(j>1){
    print(add.vars[j-1])
    }
  }
  if(type == "unconstrained"){coefs_list = NULL}
  return(list(r2=r2.oos.out, real.pred.matrix=vars_list, coefs.matrix=coefs_list))
}




#data = df.freq 
#data.pc = df.freq
#dep.vars = DEPVAR
#add.vars = CHARS
#pca.cat.dict = CATEGORIES
#model = "profitability"
#type = "constrained"
#time.col = "quarter_id"
#n_pc = n_pc
#data.freq = freq_var_data 
#model.pc = "all_models"
#split.ratio = splitratio 
#lags = 1 
#pca = TRUE
#rolling.pca = rolling.pca
#where.funcs  = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\"
#optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha= 1, pen.search = pen_search)
#sparse.pca = T 
#alpha.pca = 0.000153
#beta.pca = 0 
#t1 = t1
#t2 = t2
#extra = "profitability"