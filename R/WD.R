### Global Wood Density Database
install.packages("BIOMASS")
library(BIOMASS)

WD_sp110 <- getWoodDensity(genus="Pinus", species = "echinata", stand= NULL)

#####Define the CV for the WD

CV_sp110 <- (WD_sp110$sdWD/WD_sp110$meanWD) *100
