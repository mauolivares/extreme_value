

# -------------------------------------- #
# A "Package" to test finite variance.
# ---------------------------------------#
# Mauricio Olivares-Gonzalez
# First Version: March 2019
# This Version: March 2019
# UIUC
# ---------------------------------------#


# -------------------------------------- #
# -------------------------------------- #
# SECTION I: Generate different samples
# for parametric families
# -------------------------------------- #
# -------------------------------------- #


#' This generates data under a variety of specified DGPs indexed
#' by "type"
#' 
#' Note: if te.sd == 0, we have constant treatment effect
#' 
gen.data <- function(N, te.mean = te.mean, te.sd = te.sd,
                     type = c( "normal", "lognormal","t", "exponential" ) ){
  
  type = match.arg( type )
  
  # generate Y0
  if(type == "normal"){
    Y0 <- rnorm(N, 0, 1 )
  }
  else if(type == "lognormal"){
    Y0 <- rlnorm(N, 0, 1/4 )
  }
  else if(type == "t"){
    Y0 <- rt(N, df = 5 )
  }
  else if(type == "exponential"){
    Y0 <- rexp(N,3)
  } else {
    stop( paste( "Unrecognized type '", type, "'", sep="" ) )
  }
  
  # generate treatment and Y1
  tau <- te.mean + te.sd * Y0
  Y1 <- Y0 + tau
  
  # generate treatment: factor p is the [(p)*100]%
  # of the sample that receives the treatment.
  p <- .5
  Tx = sample(N) <= p*(N)
  
  # generate Y obs
  Y = ifelse( Tx, Y1, Y0 )
  
  data <- data.frame(Y = Y, Z = as.numeric(Tx))
  
  return(data)
}


# ---------------------------------------- #
# ---------------------------------------- #
# SECTION II: Estimate the shape parameter
# ---------------------------------------- #
# ---------------------------------------- #


hill.est <- function(x,k){
  n <- length(x)
  y <- sort(x)
  h <- (1/k)*(as.numeric(lapply(1:k,function(i) log(y[n-i+1])-log(y[(n-k)]))))
  return(h)
}











