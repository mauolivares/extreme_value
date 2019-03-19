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

pkg<-list("plyr", "mvtnorm","quantreg")
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

