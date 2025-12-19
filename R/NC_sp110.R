#install.packages("rFIA")
#library(rFIA)
library(dplyr)

###### Getting NOrth Carolina dataset of lobolly pine
#options(timeout = 3000)
#NC_sp110 <- getFIA('NC', dir = NULL, common = TRUE, tables = NULL, load = TRUE)
#save(NC_sp110, file = 'Data/NC_sp110.RData')

### Load the data
load('Data/NC_sp110.RData')
source("R/nls_fitting_functions.R")
#names(NC_sp110)
tree_NC<- NC_sp110$TREE
#str(tree)

# selecting the columns of interest
tree_sp110<-tree_NC[,c('PLOT', 'SUBP', 'TREE', 'INVYR', 'SPCD', 'DIA', 'ACTUALHT', 'HTCD', 
                    'VOLTSGRS', 'VOLCFGRS_BARK', 'DRYBIO_BRANCH')]

# Filtering only the year 2024 (last measurement) Aand conditon of height
tree_sp110_2024<- tree_sp110 %>% 
  filter(INVYR == 2024)  %>% 
  filter(HTCD == 1)

# Cleaning NAs
tree_sp110_2024cl<- tree_sp110_2024%>%filter(!is.na(DIA)&!is.na(ACTUALHT)) 

#Renaming the columns of interest
sp110_2024cl <- tree_sp110_2024cl%>% 
  rename(DO_BH = DIA,
         HT_TOT = ACTUALHT)

#sp110_2024cl$predvol<-predict(output1_tree, newdata = sp110_2024cl)
#sp110_2024cl$predbark<-predict(output1_bark, newdata = sp110_2024cl)
#sp110_2024cl$predbranch<-predict(output1_branch, newdata = sp110_2024cl)


# Running the function for adding predictions and SD to the data frame
models <- list(
  stem = output1_tree,
  bark = output1_bark,
  branch = output1_branch
)

#sp110_2024cl <- add_vol_pred(sp110_2024cl, models, n_sim = 1000)
#save(sp110_2024cl, file = 'Data/sp110_2024predvol.rds')

load('Data/sp110_2024predvol.rds')

# Bark density calculation and uncertainty
bark_coef <- coef(output1_bark)
a <- bark_coef['a']
b <- bark_coef['b']
c <- bark_coef['c']
# calculating bark biomass with:
sp110_2024cl$bark_biomass <- with(sp110_2024cl,a*DO_BH^b*HT_TOT^c) 
# calculating bark density with:
sp110_2024cl$bark_densi <- with(sp110_2024cl,bark_biomass/pred_barkvol) 

meanbark_WD<- mean(sp110_2024cl$bark_densi, na.rm=TRUE)
sdbark_WD <- sd(sp110_2024cl$bark_densi, na.rm = TRUE)

sp110_2024cl$bark_densi_sd <- sdbark_WD

# ==============================================================================
# Monte Carlo Simulation for Total Biomass Uncertainty
# ==============================================================================

set.seed(123)
n_sims <- 1000
n_trees <- nrow(sp110_2024cl)

biomass_simulations <- matrix(NA, nrow = n_trees, ncol = n_sims)

for(sim in 1:n_sims) {
  
  # Simulate volumes with uncertainty
  sim_stemvol <- rnorm(n_trees, 
                       mean = sp110_2024cl$pred_stemvol, 
                       sd = sp110_2024cl$pred_stemvol_se)
  
  sim_barkvol <- rnorm(n_trees, 
                       mean = sp110_2024cl$pred_barkvol, 
                       sd = sp110_2024cl$pred_barkvol_se)
  
  sim_branchvol <- rnorm(n_trees, 
                         mean = sp110_2024cl$pred_branchvol, 
                         sd = sp110_2024cl$pred_branchvol_se)
 
   # Stem wood density: single value per simulation (species-level
  # mean_CV = WD_sp110$meanWD*62.428 in cubic volume
  sim_stem_WD_sp110 <- rnorm(1, mean = WD_sp110$mean_CV, sd = WD_sp110$mean_CV_sd)
  
  # Bark density: individual values per tree WITH uncertainty
  
  sim_bark_WD_sp110 <- rnorm(n_trees, 
                            mean = sp110_2024cl$bark_densi, 
                            sd = sp110_2024cl$bark_densi_sd)
  
  
  # Ensure biological constraints (no negative values)
  sim_stemvol <- pmax(sim_stemvol, 0)
  sim_barkvol <- pmax(sim_barkvol, 0)
  sim_branchvol <- pmax(sim_branchvol, 0)
  sim_WD_stem <- pmax(sim_stem_WD_sp110, 0)
  sim_Density_bark <- pmax(sim_bark_WD_sp110, 0)
  
  # Calculate total biomass for this simulation
  # stem biomass: stem volume × stem wood density (same WD for all trees)
  # bark biomass: bark volume × bark density (individual per tree)
  # branch biomass: branch volume (already in biomass units)
  biomass_simulations[, sim] <- (sim_stemvol * sim_WD_stem) + 
    (sim_barkvol * sim_Density_bark) + 
    sim_branchvol
  
  if(sim %% 100 == 0) cat("  Completed", sim, "simulations\n")
}



# ==============================================================================
# Calculate uncertainty metrics for individual trees
# ==============================================================================

sp110_2024cl$biomass_mean_sim <- rowMeans(biomass_simulations)
sp110_2024cl$biomass_median_sim <- apply(biomass_simulations, 1, median)
sp110_2024cl$biomass_sd_sim <- apply(biomass_simulations, 1, sd)
sp110_2024cl$biomass_var_sim <- sp110_2024cl$biomass_sd^2
sp110_2024cl$biomass_cv_sim <- (sp110_2024cl$biomass_sd / sp110_2024cl$biomass_mean) * 100
sp110_2024cl$biomass_ci_lower_sim <- apply(biomass_simulations, 1, quantile, probs = 0.025)
sp110_2024cl$biomass_ci_upper_sim <- apply(biomass_simulations, 1, quantile, probs = 0.975)

# Total biomass for each simulation
total_biomass_sims <- colSums(biomass_simulations)

# Uncertainty aorund predictions
plot(sp110_2024cl$DO_BH, sp110_2024cl$biomass_mean_sim,
     pch = 16, col = "blue",
     ylab = "Predicted biomass",
     xlab = "DBH")

arrows(sp110_2024cl$DO_BH, sp110_2024cl$biomass_ci_upper_sim,
       sp110_2024cl$DO_BH, sp110_2024cl$biomass_ci_lower_sim,
       angle = 90, code = 3, length = 0.03)

#Prediction interval as a shaded ribbon
ord <- order(sp110_2024cl$DO_BH)

plot(sp110_2024cl$DO_BH[ord], sp110_2024cl$biomass_ci_lower_sim[ord],
     type = "l", lwd = 2,
     ylim = range(sp110_2024cl$biomass_ci_lower_sim, sp110_2024cl$pred_upr),
     xlab = "DBH", ylab = "Predicted biomass")

polygon(
  c(sp110_2024cl$DO_BH[ord], rev(sp110_2024cl$DO_BH[ord])),
  c(sp110_2024cl$biomass_ci_lower_sim[ord], rev(sp110_2024cl$biomass_ci_upper_sim[ord])),
  col = rgb(0, 0, 0, 0.2),
  border = NA
)

# Heat map
dbh_bin <- cut(sp110_2024cl$DO_BH, breaks = 20)

uncert_by_bin <- tapply(sp110_2024cl$biomass_mean_sim,
                        dbh_bin,
                        median,
                        na.rm = TRUE)

plot(uncert_by_bin,
     type = "b",
     xlab = "DBH class",
     ylab = "Median relative uncertainty")

# Plottong with ribbons

ord <- order(sp110_2024cl$DO_BH)

plot(sp110_2024cl$DO_BH[ord],
     sp110_2024cl$biomass_mean_sim[ord],
     type = "l", lwd = 2,
     ylim = range(sp110_2024cl$biomass_ci_lower_sim,
                  sp110_2024cl$biomass_ci_upper_sim),
     xlab = "DBH",
     ylab = "Predicted biomass")

polygon(
  c(sp110_2024cl$DO_BH[ord],
    rev(sp110_2024cl$DO_BH[ord])),
  c(sp110_2024cl$biomass_ci_lower_sim[ord],
    rev(sp110_2024cl$biomass_ci_upper_sim[ord])),
  col = rgb(0, 0, 0, 0.25),
  border = NA
)

lines(sp110_2024cl$DO_BH[ord],
      sp110_2024cl$biomass_mean_sim[ord],
      lwd = 2)


# ==============================================================================
# Calculating total biomass directly
# ==============================================================================
#sp_110$barkdensity<-(sp_110$Density_sp110*62.428)
sp110_2024cl$totalbiomass=((sp110_2024cl$pred_stemvol*WD_sp110$mean_CV)+
                       (sp110_2024cl$pred_barkvol*sp110_2024cl$bark_densi)+
                       (sp110_2024cl$pred_branchvol))

# Variance of each component
# For stem: (volume * wood density)
var_stem <- (sp110_2024cl$pred_stemvol)^2 * (WD_sp110$mean_CV_sd)^2 + 
  (WD_sp110$mean_CV)^2 * (sp110_2024cl$pred_stemvol_se)^2

# For bark: (volume * density)
var_bark <- (sp110_2024cl$pred_barkvol)^2 * (sp110_2024cl$bark_densi_sd)^2 + 
  (sp110_2024cl$bark_densi)^2 * (sp110_2024cl$pred_barkvol_se)^2

# For branch: (just volume)
var_branch <- (sp110_2024cl$pred_branchvol_se)^2

# Total variance (sum of variances)
sp110_2024cl$var_totalbiomass <- var_stem + var_bark + var_branch

# Total uncertainty (standard deviation)
sp110_2024cl$sd_totalbiomass <- sqrt(sp110_2024cl$var_totalbiomass)




