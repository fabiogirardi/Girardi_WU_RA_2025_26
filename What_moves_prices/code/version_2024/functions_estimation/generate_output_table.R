generate_output_table <- function(dep.vars, add.vars, pca,  no.test.samples, sep.lambdas){
  # create matrix for OOS R2 for individual var
  if (pca){
    cat <- c("capitalization", "efficiency", "financial_soundness", "liquidity",
          "profitability", "solvency", "valuation","equity_premium", "other","all")   
    tab.out     <- data.frame(matrix(nrow=(length(cat)+1) , ncol=length(dep.vars)+3))
    tab.out[,1] <-  c('DEPVAR', cat)
    colnames(tab.out) <- c("predictor", dep.vars, "pca1", "pca2")
  } else{
    tab.out     <- data.frame(matrix(nrow=(length(add.vars)+1) , ncol=length(dep.vars)+2))
    tab.out[,1] <-  c('DEPVAR',add.vars)
    colnames(tab.out) <- c("predictor", dep.vars, "extra")
  }

  # create matrix for hyperparameters
  if (sep.lambdas){
    tab.hyperparam <- data.frame(matrix(nrow=no.test.samples , ncol=length(dep.vars)+2))
    colnames(tab.hyperparam) <- c("quarter_id", paste0("lambda_",dep.vars))
  } else{
    tab.hyperparam <- data.frame(matrix(nrow=no.test.samples , ncol=2))
    colnames(tab.hyperparam) <- c("quarter_id", "lambda_")
  }
  return(list(output_table=tab.out, hyper_table=tab.hyperparam))
}