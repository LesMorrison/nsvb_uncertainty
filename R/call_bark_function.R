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
coefs_Westfall_bark <-read.csv(file="Tables/Table S6a_bark_biomass_coefs_spcd.csv",sep=";", stringsAsFactors = F)

#- stem volume dataset
tree <- read.csv("Data/NSVB_TREE.csv", stringsAsFactors = T)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#- run the custom function for the species and region of interest

#- loblolly pine in the south
output1_bark <- nls_fitting_function_bark(species=110,region="230",params=coefs_Westfall_bark,tree=tree,plotson=T)
summary(output1_bark)


#------------------------------------------------------------------------------
# workflow is, for a tree of a given diameter and height
#  1- predict bark volume using output1_bark
#  2- predict bark biomass using predict_bark_biomass
#  3- calculate density as volume / biomass
#  4- recalculate bark biomass as volume (1) multiplied by density (3)
#  .... use Monte Carlo approach to estimate the uncertainty of bark biomass

#Filtering species
stem_sample<-tree %>% 
  filter(SPCD==110)

#Filtering the coefficients for species
params_species <- coefs_Westfall_bark %>% 
  filter(SPCD==110) %>% 
  filter(DIVISION==230)


# Merging both data bases
sample_sp_110<-merge(stem_sample, params_species, by='SPCD')

# Filtering the columns of interest
sp_110<-sample_sp_110 %>% 
  select(SPCD,AUTHOR,TREENO,DO_BH,HT_TOT,ST_WD_CV_TOT,ST_BK_CV_TOT,DIVISION,model,a,b,c)

# 1- Predicting bark volume using output1_bark
sp_110$pred_barkvol <- predict(output1_bark, 
                                     newdata = sp_110[,1:5] )

# 2- Predicting bark biomass using the equation of Schumacher-Hall model and the 
# ST_WD_CV_TOT Total stem wood cubic foot volume or ST_BK_CV_TOT?

sp_110$bark_biomass <- with(sp_110,a*DO_BH^b*HT_TOT^c)


# 3- Calculate Density as volume/biomass

sp_110$bark_density_sp110 <- with(sp_110,bark_biomass/pred_barkvol)

# 4- Recalculate biomass
sp_110$biomass2 <- with(sp_110,pred_barkvol*mean(bark_density_sp110))
