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
coefs_Westfall_branch <-read.csv(file="Tables/Table S7a_branch_biomass_coefs_spcd.csv",sep=",", stringsAsFactors = F)

#- stem volume dataset
# tree <- read.csv("Data/NSVB_TREE.csv", stringsAsFactors = T)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#- run the custom function for the species and region of interest

#- loblolly pine in the south
output1_branch <- nls_fitting_function_branch(species=110,region="230",params=coefs_Westfall_branch,tree=tree,plotson=T)
summary(output1_branch)

