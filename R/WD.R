### Global Wood Density Database
install.packages("BIOMASS")
library(BIOMASS)

# WD is in g/cm^3, so we transform it into cubic foot volume

WD_sp110 <- getWoodDensity(genus="Pinus", species = "echinata", stand= NULL)
WD_sp110$mean_CV = WD_sp110$meanWD*62.428
WD_sp110$mean_CV_sd = WD_sp110$sdWD*62.428

#####Define the CV for the WD

CV_sp110 <- (WD_sp110$sdWD/WD_sp110$meanWD) *100
