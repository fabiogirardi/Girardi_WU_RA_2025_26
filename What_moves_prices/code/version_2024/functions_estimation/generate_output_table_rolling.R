generate_output_table <- function(dep.vars, add.vars, pca, pca.cat.dict, no.test.samples, sep.lambdas, n_pc){
  # create matrix for OOS R2 for individual var
  if (pca){
    cat <- names(pca.cat.dict)  
    tab.out     <- data.frame(matrix(nrow=(length(cat)+1) , ncol=(length(dep.vars)+2*n_pc+1)))
    tab.out[,1] <-  c('no_pc', cat)
    colnames(tab.out) <- c("predictor", dep.vars, paste0("spc",seq(1:n_pc)), paste0("pc",seq(1:n_pc)))
  } else{
    tab.out     <- data.frame(matrix(nrow=(length(add.vars)+1) , ncol=length(dep.vars)+2))
    tab.out[,1] <-  c('no_pc',add.vars)
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