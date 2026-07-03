#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#- Example script of how to use the nls_fitting_function() for multiple species and regions
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#- source two libraries
library(data.table)
library(dplyr)

#- source the Rscript where the custom function is defined.
#  run this once so that the function is available to be used
source("R/nls_fitting_functions.R")
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#- read in the two relevant datasets

#- Coefficents reported by Westfall et al. Note that I turned stringsAsFactors to F
#  The problem is that "DIVISION" is text and not numbers
coefs_Westfall_vol <-read.csv(file="Tables/Table S3a_volob_coefs_spcd.csv",stringsAsFactors = F)

#- tree volume dataset
tree <- read.csv("Data/NSVB_TREE.csv", stringsAsFactors = T)
#------------------------------------------------------------------------------
#For looking DO_BH distribution
#hist(tree$DO_BH[tree$SPCD==802])

#------------------------------------------------------------------------------
#- run the custom function for the species and region of interest

#- 
outputOR240_tree <- nls_fitting_function_tree(species=202,region="240",params=coefs_Westfall_vol,tree=tree,plotson=T)

summary(outputOR240_tree)



