#-----
# OLS Function
#----

#' Ordinary Least Square Estimator
#'
#' @param data_ols This is a function that estimates the OLS estimators and computes
#' other important statistics
#'
#' @return
#' @export
#'
#' @examples
ols <-function(data_ols){


  #Create matrices for variables
  Y <- matrix(0,nrow(data_ols),1)
  X <- matrix(0,nrow(data_ols),ncol(data_ols))

  #Define dependent variable
  Y <- data_ols[,1]

  #Vector of constants
  X[,1] <- 1

  #Fill the matrix with independent variables
  for (i in 2:ncol(data_ols)){
    X[,i] <- data_ols[,i]
  }

  #Number of observations
  n <- nrow(X)

  #Number of parameters
  k <- ncol(X)

  #OLS matrix form
  #\hat \y=(X'X)^{-1}X'Y

  #Solving the matrix
  coef <-t(solve(t(X)%*%X)%*%t(X)%*%Y)
  rownames(coef) <- c("Coefficients") #Rename

  #Vector to store y_hat
  Y_hat <- matrix(NA,n,k)

  #Loop for y_hat
  for (i in 1:k){
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
  }

  #Y_hat for each time
  Y_hat <- apply(Y_hat, MARGIN = 1, sum)

  #Residuals
  res <- Y-Y_hat


  #Nice plot for y_obs, y_hat and residuals
  plot(Y_hat)


  #Variance covariance matrix
  #Var(\hat \beta)|X)=1-(n-k) \hat \epsilon ' \hat \epsilon(X'X)^{-1}
  VCV <- (1/(n-k))*as.numeric(t(res)%*%res)*solve(t(X)%*%X)

  #Diagonal matrix variance-covariance
  se <- t(as.matrix(sqrt(diag(VCV))))
  rownames(se) <- c("s.e.") #rename

  #P-values
  p_value <- coef/se
  rownames(p_value) <- c("p-value") #rename





  #Bind main results
  results <-rbind(coef, se, p_value)


  list_results <- list("Results" = results, "Residuals" = res, "Y estimated" = Y_hat)

  return(list_results)
}

#devtools::document()
