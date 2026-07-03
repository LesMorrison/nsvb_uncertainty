#- source two libraries
library(data.table)
library(dplyr)

#- source the Rscript where the custom function is defined.
#  run this once so that the function is available to be used
source("R/nls_fitting_functions.R")
source("R/nlsLM_sp221.R")
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#- read in the two relevant datasets

#- Coefficents reported by Westfall et al. Note that I turned stringsAsFactors to F
#  The problem is that "DIVISION" is text and not numbers
coefs_Westfall_bark <-read.csv(file="Tables/Table S6a_bark_biomass_coefs_spcd.csv",sep=";", stringsAsFactors = F)

#- stem volume dataset
#tree <- read.csv("Data/NSVB_TREE.csv", stringsAsFactors = T)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#- run the custom function for the species and region of interest

# Fixing the Westfall dataset, Division 230 is missing in the bark dataset, but is
# present in the vol dataset
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
tree_bark <- tree %>%
  dplyr::filter(
    !(SPCD == 202 & is.na(ST_BK_CV_TOT)),
    !(SPCD == 202 & is.na(DO_BH)),
    !(SPCD == 202 & is.na(HT_TOT)))

coefs_Westfall_bark_fix <- coefs_Westfall_bark

coefs_Westfall_bark_fix$DIVISION[
  coefs_Westfall_bark_fix$SPCD == 221 & 
    coefs_Westfall_bark_fix$DIVISION == ""] <- "230"

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

output5_bark <- nls_function_sp221_bark(species=221,region="230",params=coefs_Westfall_bark_fix,tree=tree_bark,plotson=T)
outputOR240_bark <- nls_fitting_function_tree(species=202,region="240",params=coefs_Westfall_bark,tree=tree,plotson=T)
summary(outputOR240_bark)

#At this point, your checks showed: DO_BH, HT_TOT, and ST_BK_CV_TOT are finite,
#the starting values a, b, c are finite, the model at the starting values is finite
# So the error is happening because base nls() is unstable during the optimization step. 
# While it tries new values of a, b, or c, it reaches a combination where the model 
#evaluation breaks, and numericDeriv() stops.
#What to do? The practical fix is to stop using base nls() here and use nlsLM() instead.



#------------------------------------------------------------------------------
# workflow is, for a tree of a given diameter and height
#  1- predict bark volume using output1_bark
#  2- predict bark biomass using predict_bark_biomass
#  3- calculate density as volume / biomass
#  4- recalculate bark biomass as volume (1) multiplied by density (3)
#  .... use Monte Carlo approach to estimate the uncertainty of bark biomass

#Filtering species
stem_sample<-tree_bark %>% 
  filter(SPCD==202)

#Filtering the coefficients for species
params_species <- coefs_Westfall_bark %>% 
  filter(SPCD==202) %>% 
  filter(DIVISION=='240')


# Merging both data bases
sample_sp_202OR240<-merge(stem_sample, params_species, by='SPCD')

# Filtering the columns of interest
sp_202_OR240<-sample_sp_202OR240 %>% 
  dplyr::select(SPCD,AUTHOR,TREENO,DO_BH,HT_TOT,ST_WD_CV_TOT,ST_BK_CV_TOT,DIVISION,model,a,b,c)

# 1- Predicting bark volume using output1_bark
sp_202_OR240$pred_barkvol <- predict(outputOR240_bark, 
                                     newdata = sp_202_OR240[,1:5] )

# 2- Predicting bark biomass using the equation of Schumacher-Hall model and the 
# ST_WD_CV_TOT Total stem wood cubic foot volume or ST_BK_CV_TOT?

sp_202_OR240$bark_biomass <- with(sp_202_OR240,a*DO_BH^b*HT_TOT^c)


# 3- Calculate Density as volume/biomass

sp_202_OR240$bark_density_sp202 <- with(sp_202_OR240,bark_biomass/pred_barkvol)

# 4- Recalculate biomass
sp_202_OR240$biomass2 <- with(sp_202_OR240,pred_barkvol*mean(bark_density_sp202))

