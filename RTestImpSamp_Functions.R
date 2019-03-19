
# -------------------------------------- #
# A "Package" to test finite variance.
# ---------------------------------------#
# Mauricio Olivares-Gonzalez
# First Version: March 2019
# This Version: March 2019
# UIUC
# ---------------------------------------#


# ---------------------------------------- #
# ---------------------------------------- #
# SECTION I: Estimate the shape parameter
# ---------------------------------------- #
# ---------------------------------------- #


hill.est <- function(x,k){
  n <- length(x)
  y <- sort(x)
  h <- (1/k)*(as.numeric(lapply(1:k,function(i) log(y[n-i+1])-log(y[(n-k)]))))
  return(h)
}

mme <- function(x,u){
  z <- gpdFit(x,threshold = u)$exceedances
  xi <- 0.5*(1-mean(z)/var(z))
  return(xi)
}

# ---------------------------------------- #
# ---------------------------------------- #
# SECTION II: Test Statistics
# ---------------------------------------- #
# ---------------------------------------- #


monahan.test <- function(x,k){
  t.stat <- sqrt(k/hill.est(x,k)^2)*(hill.est(x,k)-0.5)
  return(t.stat)
}


t.test.mle <- function(x,u){
  n <- length(x)
  mle <- gpdFit(x,threshold = u)$par.ests
  t.stat <- sqrt(n/(3*mle[1]^2))*(mle[2]-0.5)
  return(t.stat)
}










