#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#- Example script of how to use the nls_fitting_function() for multiple species and regions
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
#- source two libraries
library(data.table)
library(dplyr)

#- source the Rscript where the custom function is defined.
#  run this once so that the function is available to be used
source("R/nls_fitting_function.R")
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
#- read in the two relevant datasets

#- Coefficents reported by Westfall et al. Note that I turned stringsAsFactors to F
#  The problem is that "DIVISION" is text and not numbers
coefs_Westfall <-read.csv(file="Tables/Table S3a_volob_coefs_spcd.csv",stringsAsFactors = F)

#- tree volume dataset
tree <- read.csv("Data/NSVB_TREE.csv",stringsAsFactors = T)
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
#- run the custom function for the species and region of interest

#- loblolly pine in the south
output1 <- nls_fitting_function(species=110,region="230",params=coefs_Westfall,tree=tree,plotson=T)
summary(output1)

#- Atlantic white-cedar in the south
output2 <- nls_fitting_function(species=43,region="230",params=coefs_Westfall,tree=tree,plotson=T)
summary(output2)
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
#- Now, imagine a workflow. You read in a tree list from an inventory (e.g., FIA plot).
#- Then you loop over each tree in the inventory.
#- You fit the model for each tree using a function like nls_fitting_function()
#      Or better yet, call up the stored fitted model so that you don't have to 
#      refit the model for every single tree in the inventory.
#- Then you pass the fitted model on to another custom function that does the MC
#      and returns the variance for each tree.
#- Then you add up the variances for each tree in your plot.
#------------------------------------------------------------------------------
