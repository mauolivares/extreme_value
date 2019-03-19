
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









# ---------------------------------------- #
# ---------------------------------------- #
# SECTION II: Test Statistics
# ---------------------------------------- #
# ---------------------------------------- #


monahan.test <- function(x,k){
  t.stat <- sqrt(k/hill.est(x,k)^2)*(hill.est(x,k)-0.5)
  return(t.stat)
}



