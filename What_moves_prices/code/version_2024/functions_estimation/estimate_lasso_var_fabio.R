library('matrixcalc')

estimate_lasso_var <- function(data, pen.search="Rolling", penalty.struct="BasicEN", lags=1, t1= 1/3, t2= 2/3,
                              separate.lambdas=FALSE, alpha=0.5, intercept=TRUE, algo.tol=1e-10){
  # some arguments need to be in one list for the construct function
  model.controls <- list(intercept=intercept, alpha=alpha, tol=algo.tol)
  # all parameters not mentioned in constructModel at default, see here:  
  
  if(pen.search == "LOO"){
  mod1 <- constructModel(data,
                          p=lags,
                          struct = penalty.struct,
                          gran=c(10000,300), #If I wish to provide own penalty parameters, I can do through gran, but I need to set the optional argument ownlambdas to TRUE
                          #T1= floor(nrow(data)*t1),
                          T2= floor(nrow(data)*t2)-2, ## subtruching -2 so that if t2=1 there will be only one oos observation 
                          verbose=FALSE,
                          cv=pen.search,
                          rolling_oos=F, ## very critical: changes a lot if I set it to True
                          separate_lambdas=separate.lambdas,
                          model.controls=model.controls)
  }
  
  if(pen.search == "Rolling"){ 
    mod1 <- constructModel(data,
                           p=lags,
                           struct = penalty.struct,
                           gran=c(10000,300), #If I wish to provide own penalty parameters, I can do through gran, but I need to set the optional argument ownlambdas to TRUE
                           T1= floor(nrow(data)*t1),
                           T2= floor(nrow(data)*t2)-1, ## subtruching -2 so that if t2=1 there will be only one oos observation 
                           verbose=FALSE,
                           cv=pen.search,
                           rolling_oos=F, ## very critical: changes a lot if I set it to True
                           separate_lambdas=separate.lambdas,
                           model.controls=model.controls)
  }
  
  # run model specification
  res.lasso.var <- cv.BigVAR(mod1)
  res.lasso.var@betaPred

  # optimal hyperparameters
  best.hyperparam <- res.lasso.var@OptimalLambda
  # refit using the best hyperparameters (only necessary if sep.lambdas == TRUE)
  mod2 <- BigVAR.fit(data, struct=penalty.struct, p=lags, alpha=alpha,
                    lambda=best.hyperparam, 
                    separate_lambdas=separate.lambdas, 
                    intercept=intercept, tol=algo.tol)
  coef2 <- mod2[,,1]
  lasso.var.pred  <- coef2%*%res.lasso.var@Zvals
  
  Z = VARXLagCons(data,p=lags,oos=FALSE)$Z
  # obtain out of sample forecasts
  fitted = mod2[,,1]%*%Z
  res_var = data[-1,]- t(fitted)
  
  if(intercept == FALSE){
    
    fitted_t2 =  matrix.power(coef2[,-1],2) %*% Z[-1,]
    fitted_t3 =  matrix.power(coef2[,-1],3)%*% Z[-1,]
    fitted_t4 =  matrix.power(coef2[,-1],4)%*% Z[-1,]
    fitted_t5 =  matrix.power(coef2[,-1],5)%*% Z[-1,]
      
  }
  
  # what is the difference between CV fit and re-fit?
  #abs(predict(results) - coef2%*%results@Zvals)
  return(list(prediction=lasso.var.pred, best.hyper=best.hyperparam, coefMat=coef2, fitted = fitted, fitted_t2 = fitted_t2, fitted_t3 = fitted_t3, fitted_t4 = fitted_t4, fitted_t5 = fitted_t5, res_var = res_var))   
}

#data = data.ts
#intercept=fit.intercept
#separate.lambdas=sep.lambdas
#pen.search="LOO"
#algo.tol=1e-10
#penalty.struct="BasicEN"

#str(res.lasso.var)
#res.lasso.var@lambda_evolve_path
