# -------------------------------------- #
# This Script performs the Khmaladze
# transform to construct an ADF test 
# ---------------------------------------#
# Mauricio Olivares-Gonzalez
# October 2016
# UIUC
# ---------------------------------------#


# -----------------------#
#    House Keeping       #    
# -----------------------#

rm(list=ls())
cat("\014") 
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


# -------------------#
# Load Packages

pkg<-list("plyr", "mvtnorm","quantreg","eva")
lapply(pkg, require, character.only=T)
rm(pkg)

# ----------------------#
#Set WD and Pushbullet
setwd("/Users/hoeffding/Google Drive/Research/Importance Sampling/")
set.seed(5)
# -------------------#
# Load Functions
functions<-list("Scripts/Perm_Test_library.R")
lapply(functions, source)
rm(functions)


# -------------------------- #
# Parameters of the DGP
type <-  "normal"
# Sample size. 
N <- 200
u <- 50




# Parameters of the simulation 
n.sims <- 200
alpha <- 0.05
n.perm <- 100
n.tests <- 5 # Number of test statistics

# Storage Matrix: 5 test statistics
out <- matrix(NA,nrow=n.sims,ncol = n.tests)

ptm0 <- proc.time()
pb <- txtProgressBar(min = 0, max = n.sims, style = 3)

for(i in 1:n.sims){
  setTxtProgressBar(pb, i)
  fake <- gen.data(N = N, te.mean = te.mean[j], te.sd = te.sd, type = type )
  # Recentering
    
  # I) Observed Statistic
  # Khmaladze transform based on Kolmogorov-Smirnov type test
  KS.k <- Khm.trans(Y1.star,Y0.star,gdot0,t)
    
  # II) Permutations
  Rn <- perm.dist.khm(Y1.star,Y0.star,KS.k,n.perm,t,gdot0)
  # Rejection
  out[i,j] <- reject.rule(Rn,alpha)
    
}
close(pb)
ptm1 <- proc.time()


# Calculate rejection rates
# Results
rej.rates <-apply(out,2,function(x) mean(x))
print(rej.rates)
