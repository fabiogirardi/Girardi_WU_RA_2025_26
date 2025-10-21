

cross_validation_spca <- function(data, dep.vars, add.vars, pca.cat.dict, sparsity.params = seq(0, 0.001, length = 101), beta.pca = 0, n_pc = n_pc){
      
    df.freq.cv = data[complete.cases(data[, c(dep.vars, add.vars)]), ]

    n = nrow(df.freq.cv)
    
    add.vars <- names(pca.cat.dict)
    
    # Define a range of sparsity parameters
    
    # Initialize a vector to store mean squared errors for each sparsity parameter
    
    cv_errors <- matrix(data = NA, nrow = length(sparsity_params), ncol = 2)
    cv_errors[,1] = sparsity_params
    
    # Perform LOO-CV
    
    pca.fit <- prcomp(df.freq.cv[pca.cat.dict[[add.vars[6-1]]]], center=TRUE, scale = TRUE)
    pca.X =  scale(df.freq.cv[pca.cat.dict[[add.vars[6-1]]]], center=T, scale = F)
    projected1 <-   (as.matrix(pca.X) %*% pca.fit$rotation)[,1:2]   %>% as.data.frame    
    colnames(projected1) <- paste0(add.vars[6-1],"_pca_", seq(1:2))
      
    for (i in seq_along(sparsity_params)) {
      
      lambda <- sparsity_params[i]
      
      errors <- numeric(n)  # Store errors for each observation
      
      pca.fit = spca(df.freq.cv[pca.cat.dict[["full"]]], center=TRUE, scale = TRUE, alpha = sparsity.params[i], beta = beta.pca, max_iter = 1000, verbose = F)
      pca.X =  scale(df.freq.cv[pca.cat.dict[["full"]]], center=T, scale = F)
      projected = (as.matrix(pca.X) %*% pca.fit$loadings)[,1:2]   %>% as.data.frame 
      colnames(projected) = paste0("SPC",seq(1:n_pc),"_full")
    
      
      X <- cbind(df.freq.cv[,DEPVAR], projected,projected1)
      Y = df.freq.cv[,c("d_gr_1y_lead","p_d_1y_lead","p_e_1y_lead", "ret_1y_lead")]
    
        for (j in 1:n) {
          
          # Leave one observation out
          X_train <- X[-j, ]; rownames(X_train)= NULL
          y_train <- Y[-j,]; rownames(y_train)= NULL
          X_test <- X[j, , drop = FALSE]
          y_test <- Y[j,]
          
          # Fit SPCA
          
        
          
          # Fit regression model using SPCA scores
          lm_fit <- lm(as.matrix(y_train) ~ as.matrix(X_train))
          
          
          # Predict for the left-out observation
          test_data <- data.frame(X_test)
          colnames(test_data) <- colnames(X_train)  # Ensure column names match
          y_pred <- as.numeric(t(lm_fit$coefficients) %*% c(1, as.numeric(test_data)))
          
          # Calculate prediction error
          errors[j] <- sum((y_test - y_pred)^2)
        }
        
        # Calculate mean squared error for this lambda
        cv_errors[i,2] <- mean(errors)
        print(i)
      }
    
    colnames(cv_errors) <- c("Sparsity_Parameter", "Mean_Squared_Error")
    # Select the optimal sparsity parameter
    optimal_lambda <- sparsity_params[which.min(cv_errors[,2])]
    
    # Print the optimal parameter and plot the errors
    cat("Optimal sparsity parameter:", optimal_lambda, "\n")
    
    plot = plot(sparsity_params, cv_errors[,2], type = "b", col = "blue", pch = 19,
         xlab = "Sparsity Parameter of PCs", ylab = "Mean Squared Error",
         main = "LOO-CV for SPCA")
    
    return(list(optimal_lambda = optimal_lambda, cv_errors = cv_errors, plot_cv = plot))
}




#data = df.freq
#dep.vars = DEPVAR
#add.vars = CHARS
#pca.cat.dict = CATEGORIES
#n_pc = n_pc
#sparsity_params = seq(0, 0.001, length = 101)
#beta.pca = 0,