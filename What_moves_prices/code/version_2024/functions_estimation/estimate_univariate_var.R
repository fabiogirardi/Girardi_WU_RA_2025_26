estimate_univariate_var <- function(data, system.vars, lags=1, intercept=TRUE){
  # what about the intercept?
  if (intercept){type="const"}else{type="none"}
  # estimate
  VAR_est <- VAR(y=data, p=lags, type=type)
  # predict
  pred = predict(VAR_est, n.ahead=1)
  pred_depvar = as.numeric(matrix(as.data.frame(pred$fcst[system.vars]), ncol = length(system.vars), byrow = F)[1,])
  # output prediction
  pred_depvar <- as.matrix(pred_depvar)
  #rownames(pred_depvar) <- system.vars
  return(pred_depvar)
  }

# data = train.data.ts
# intercept = fit.intercept
