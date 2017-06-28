### ===============================================================================================
### R package rxylib BUILDSCRIPTS
### Rcpp
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2017-06-28
### ===============================================================================================

if(!require("Rcpp"))
  install.packages("Rcpp")

library(Rcpp)

##compile new attributes
Rcpp::compileAttributes()

