#install.packages("rFIA")
library(rFIA)
library(dplyr)

###### Getting Oregon dataset of Douglas Fir (Pseudotsuga menziessi)
#options(timeout = 3000)
#OR_sp202 <- getFIA('OR', dir = NULL, common = TRUE, tables = NULL, load = TRUE)
#save(OR_sp202, file = 'Data/OR_sp202.RData')

### Load the data
load('Data/OR_sp202.RData')
source("R/nls_fitting_functions.R")
#names(NC_sp110)
tree_OR<- OR_sp202$TREE

tree_OR_sp202 <- tree_OR[tree_OR$SPCD==202,]

#str(tree)

# selecting the columns of interest: TREE
tree_sp202<-tree_OR_sp202[,c('CN', 'PLT_CN', 'CONDID','STATECD', 'UNITCD', 'COUNTYCD', 'PLOT', 'SUBP', 'CYCLE', 'SUBCYCLE',
                             'TREE', 'TPA_UNADJ','INVYR', 'SPCD', 'DIA', 'ACTUALHT', 'HTCD', 'VOLTSGRS', 
                             'VOLCFGRS_BARK', 'DRYBIO_BRANCH')]

# checking the number of years in each monitoring cycle. Cycle 6 goes from 2010 to 2019 (10 years)
unique(tree_sp202$INVYR[tree_sp202$CYCLE == 6])

#Cheking for the condition of height
unique(tree_sp202$HTCD)

# Filtering only cycle 6 and condition of height
tree_sp202_C6<- tree_sp202 %>% 
  filter(CYCLE == 6)  %>% 
  filter(HTCD == 1)

# Cleaning NAs
tree_sp202_C6<- tree_sp202_C6%>%filter(!is.na(DIA)&!is.na(ACTUALHT)) 

#Renaming the columns of interest: DIA (inches), ACTUALHT (foot)
sp202_C6 <- tree_sp202_C6%>% 
  rename(DO_BH = DIA,
         HT_TOT = ACTUALHT)

hist(sp202_C6$DO_BH,
       breaks = 30,
       freq = FALSE,
       col = "lightgray",
       border = "white",
       main = "Diameter Distribution",
       xlab = "Diameter (inch)")

hist(sp202_C6$HT_TOT,
     breaks = 30,
     freq = FALSE,
     col = "lightgray",
     border = "white",
     main = "Height Distribution",
     xlab = "Height (foot)")

# Looking for duplicates
any(duplicated(sp202_C6[,c('COUNTYCD','PLOT', 'SUBP', 'CYCLE', 'SUBCYCLE','TREE')]))
table(sp202_C6$INVYR[sp202_C6$CYCLE == 6])

# Running the function for adding predictions and SD to the data frame
load("R/output_2.RData")

models <- list(
  stem = output2_tree,
  bark = output2_bark,
  branch = output2_branch)


#sp202_C6 <- add_vol_pred(sp202_C6, models, n_sim = 1000)
#save(sp202_C6, file = 'Data/sp202_C6predvol.rds')

load('Data/sp202_C6predvol.rds')

# Bark density calculation and uncertainty
bark_coef <- coef(output2_bark)
a <- bark_coef['a']
b <- bark_coef['b']
c <- bark_coef['c']

# calculating bark biomass with:
sp202_C6$barkbiomass<-with(sp202_C6,a*DO_BH^b*HT_TOT^c)

# calculating bark density with:
sp202_C6$bark_densi <- with(sp202_C6,barkbiomass/pred_barkvol) 

meanbark_WD<- mean(sp202_C6$bark_densi, na.rm=TRUE)
sdbark_WD <- sd(sp202_C6$bark_densi, na.rm = TRUE)

sp202_C6$bark_densi_sd <- sdbark_WD

# Getting WD
#install.packages("BIOMASS")
library(BIOMASS)

# WD is in g/cm^3, so we transform it into lb/cubic foot volume
WD_sp202 <- getWoodDensity(genus="Pseudotsuga", species = "menziesii", stand= NULL)
WD_sp202$mean_CV = WD_sp202$meanWD*62.428
WD_sp202$mean_CV_sd = WD_sp202$sdWD*62.428

# ==============================================================================
# Monte Carlo Simulation for Total Biomass Uncertainty
# ==============================================================================

set.seed(123)
n_sims <- 1000
n_trees <- nrow(sp202_C6)

# to keep the total too
biomass_simulations  <- matrix(NA, nrow = n_trees, ncol = n_sims)

# store per-tree biomass for each sim (same structure as your total matrix)
stem_sim   <- matrix(NA, nrow = n_trees, ncol = n_sims)
bark_sim   <- matrix(NA, nrow = n_trees, ncol = n_sims)
branch_sim <- matrix(NA, nrow = n_trees, ncol = n_sims)

for(sim in 1:n_sims) {
  
  # Simulate volumes with uncertainty
  sim_stemvol <- rnorm(n_trees, 
                       mean = sp202_C6$pred_stemvol, 
                       sd = sp202_C6$pred_stemvol_se)
  
  sim_barkvol <- rnorm(n_trees, 
                       mean = sp202_C6$pred_barkvol, 
                       sd = sp202_C6$pred_barkvol_se)
  
  sim_branchvol <- rnorm(n_trees, 
                         mean = sp202_C6$pred_branchvol, 
                         sd = sp202_C6$pred_branchvol_se)
  
  # Stem wood density: single value per simulation (species-level
  # mean_CV = WD_sp110$meanWD*62.428 in cubic volume
  sim_stem_WD_sp202 <- rnorm(1, mean = WD_sp202$mean_CV, sd = WD_sp202$mean_CV_sd)
  
  # Bark density: individual values per tree WITH uncertainty
  
  sim_bark_WD_sp202 <- rnorm(n_trees, 
                             mean = sp202_C6$bark_densi, 
                             sd = sp202_C6$bark_densi_sd)
  
  
  # Ensure biological constraints (no negative values)
  sim_stemvol <- pmax(sim_stemvol, 0)
  sim_barkvol <- pmax(sim_barkvol, 0)
  sim_branchvol <- pmax(sim_branchvol, 0)
  sim_WD_stem <- pmax(sim_stem_WD_sp202, 0)
  sim_Density_bark <- pmax(sim_bark_WD_sp202, 0)
  
  # --- component biomasses (per tree) ---
  stem_biomass   <- sim_stemvol   * sim_WD_stem
  bark_biomass   <- sim_barkvol   * sim_Density_bark
  branch_biomass <- sim_branchvol   # (as you noted: already biomass units)
  
  # --- save ---
  stem_sim[, sim]   <- stem_biomass
  bark_sim[, sim]   <- bark_biomass
  branch_sim[, sim] <- branch_biomass
  
 # total_sim[, sim]  <- stem_biomass + bark_biomass + branch_biomass
  
  biomass_simulations[, sim] <- (sim_stemvol * sim_WD_stem) + 
    (sim_barkvol * sim_Density_bark) + 
    sim_branchvol
  
  if(sim %% 100 == 0) cat("  Completed", sim, "simulations\n")
}

# ==============================================================================
# Calculate uncertainty metrics for individual trees
# ==============================================================================
sp202_C6$biomass_mean_sim <- rowMeans(biomass_simulations)
sp202_C6$biomass_median_sim <- apply(biomass_simulations, 1, median)
sp202_C6$biomass_sd_sim <- apply(biomass_simulations, 1, sd)
sp202_C6$biomass_var_sim <- sp202_C6$biomass_sd^2
sp202_C6$biomass_cv_sim <- (sp202_C6$biomass_sd / sp202_C6$biomass_mean) * 100
sp202_C6$biomass_ci_lower_sim <- apply(biomass_simulations, 1, quantile, probs = 0.025)
sp202_C6$biomass_ci_upper_sim <- apply(biomass_simulations, 1, quantile, probs = 0.975)


# ==============================================================================
# Calculating total biomass directly
# ==============================================================================
#sp_110$barkdensity<-(sp_110$Density_sp110*62.428)
sp202_C6$totalbiomass=((sp202_C6$pred_stemvol*WD_sp202$mean_CV)+
                             (sp202_C6$pred_barkvol*sp202_C6$bark_densi)+
                             (sp202_C6$pred_branchvol))

# Variance of each component
# For stem: (volume * wood density)
var_stem <- (sp202_C6$pred_stemvol)^2 * (WD_sp202$mean_CV_sd)^2 + 
  (WD_sp202$mean_CV)^2 * (sp202_C6$pred_stemvol_se)^2

# For bark: (volume * density)
var_bark <- (sp202_C6$pred_barkvol)^2 * (sp202_C6$bark_densi_sd)^2 + 
  (sp202_C6$bark_densi)^2 * (sp202_C6$pred_barkvol_se)^2

# For branch: (just volume)
var_branch <- (sp202_C6$pred_branchvol_se)^2

# Total variance (sum of variances)
sp202_C6$var_totalbiomass <- var_stem + var_bark + var_branch

# Total uncertainty (standard deviation)
sp202_C6$sd_totalbiomass <- sqrt(sp202_C6$var_totalbiomass)

#### Ploting uncertainty at tree level#######

# Uncertainty around predictions

x <- sp202_C6$DO_BH
y <-  sp202_C6$biomass_mean_sim
sd <-  sp202_C6$biomass_sd_sim

plot(y, sd,
     pch = 19,
     xlab = "Biomass at tree level (lb)",
     ylab = "Absolute uncertainty (SD, %)",
     main = "Tree-level relative absolute uncertainty")

summary(sp202_C6$DO_BH)

############## Uncertainty at plot level ######################################
tree_OR_plot <- OR_sp202$PLOT

#Filtering DO_BH > 5 inches subplots
sp202_DOBH_5<- sp202_C6[sp202_C6$DO_BH>=5 & sp202_C6$DO_BH<24,]

# Filtering saplings microplots
sp202_DOBH_1<- sp202_C6[sp202_C6$DO_BH>=1 & sp202_C6$DO_BH<5,]
unique(tree_OR_plot$MICROPLOT_LOC)

#Macroplots
sp202_plot_joined <- sp202_C6 %>% 
  dplyr::left_join(
    tree_OR_plot %>% 
      dplyr::select(STATECD, UNITCD, COUNTYCD, PLOT, CYCLE, MACRO_BREAKPOINT_DIA),
    by = c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "CYCLE"))

sp202_DOBH_24<- sp202_plot_joined[sp202_plot_joined$DO_BH>=24,]


##### Identifying repeteated rows #####
#any(duplicated(sp110_DOBH_5[, c("PLOT", "SUBP", "TREE", "SPCD")]))
any(duplicated(sp202_DOBH_5[, c("CN", "PLT_CN", "CONDID", 'COUNTYCD', "PLOT", "SUBP", "TREE", "SPCD")]))

# Filtering columns of interest
tree_level_5 <- sp202_DOBH_5[,c("CN", "PLT_CN", "CONDID", "COUNTYCD", "PLOT", 'SUBP','INVYR', "TREE", 
                                "TPA_UNADJ","biomass_mean_sim", "biomass_sd_sim", "biomass_var_sim",
                                "biomass_cv_sim","totalbiomass", "sd_totalbiomass")]


tree_level_1 <- sp202_DOBH_1[,c("CN", "PLT_CN", "CONDID", "COUNTYCD", "PLOT", 'SUBP','INVYR', "TREE", 
                                "TPA_UNADJ","biomass_mean_sim", "biomass_sd_sim", "biomass_var_sim",
                                "biomass_cv_sim", "totalbiomass", "sd_totalbiomass")]

tree_level_24 <- sp202_DOBH_24[,c("CN", "PLT_CN", "CONDID", "COUNTYCD", "PLOT",'SUBP','INVYR', "TREE", 
                                "TPA_UNADJ", "biomass_mean_sim", "biomass_sd_sim", "biomass_var_sim",
                                "biomass_cv_sim", "totalbiomass", "sd_totalbiomass")]


# # Expansion factor to trees > 24
# TPA_24 <- 0.99
# TPH_24 <- TPA_24 * 2.47105
# 
# # Expansion factor to trees > 5
# TPA_5 <- 6.018046
# TPH_5 <- TPA_5 * 2.47105 
# 
# # Expansion factor to trees < 5
# TPA_1 <- 74.96
# TPH_1 <- TPA_1 * 2.47105

acre_to_ha <- 2.47105
lb_to_Mg <- 0.00045359237

# Expand biomass at the tree level
tree_level_24$biomass_Mg_ha <-
  tree_level_24$biomass_mean_sim *
  tree_level_24$TPA_UNADJ*
  acre_to_ha * lb_to_Mg

tree_level_5$biomass_Mg_ha <-
  tree_level_5$biomass_mean_sim * 
  tree_level_5$TPA_UNADJ*
  acre_to_ha * lb_to_Mg

tree_level_1$biomass_Mg_ha <-
  tree_level_1$biomass_mean_sim * 
  tree_level_1$TPA_UNADJ*
  acre_to_ha * lb_to_Mg


# Expand uncertainty
tree_level_24$var_Mg2_ha2 <- tree_level_24$biomass_var_sim *
  (tree_level_24$TPA_UNADJ * acre_to_ha)^2 *
  lb_to_Mg^2

tree_level_5$var_Mg2_ha2 <- tree_level_5$biomass_var_sim *
  (tree_level_5$TPA_UNADJ * acre_to_ha)^2 *
  lb_to_Mg^2

tree_level_1$var_Mg2_ha2 <- tree_level_1$biomass_var_sim *
  (tree_level_1$TPA_UNADJ * acre_to_ha)^2 *
  lb_to_Mg^2

#Combining both datasets
tree_level <- rbind(tree_level_24, tree_level_5, tree_level_1)
save(tree_level, file = 'Data/tree_level_OR.rds')


##############Aggregate to plot level######################################
plot_biomass <- aggregate(
  biomass_Mg_ha ~ PLT_CN + CONDID + COUNTYCD + PLOT + INVYR,
  data = tree_level,
  FUN = sum,
  na.rm = TRUE)

plot_var <- aggregate(
  var_Mg2_ha2 ~ PLT_CN + CONDID + COUNTYCD + PLOT + INVYR,
  data = tree_level,
  FUN = sum,
  na.rm = TRUE)

plot_summary <- merge(
  plot_biomass,
  plot_var,
  by = c('PLT_CN','CONDID',"COUNTYCD","PLOT","INVYR"),
  all = TRUE)

plot_summary$sd_Mg_ha <- sqrt(plot_summary$var_Mg2_ha2)
plot_summary$cv_pct <- 100 * plot_summary$sd_Mg_ha /
  plot_summary$biomass_Mg_ha

COND <- OR_sp202$COND
PPSA  <- OR_sp202$POP_PLOT_STRATUM_ASSGN
POP_STRATUM <- OR_sp202$POP_STRATUM
POP_EVAL <- OR_sp202$POP_EVAL
POP_EVAL_TYP <- OR_sp202$POP_EVAL_TYP

sum(plot_summary$PLT_CN %in% PPSA$PLT_CN)
nrow(plot_summary)

plot_summary2 <- merge(
  plot_summary,
  PPSA[, c("PLT_CN","STRATUMCD","EVALID")],
  by = "PLT_CN",
  all.x = TRUE)

plot_summary3 <- merge(
  plot_summary2,
  POP_STRATUM[, c("STRATUMCD","EVALID","EXPNS")],
  by = c("STRATUMCD","EVALID"),
  all.x = TRUE)

COND_small <- COND[, c("PLT_CN", "CONDID", "CONDPROP_UNADJ")]

plot_summary4 <- merge(
  plot_summary3,
  COND_small,
  by = c("PLT_CN", "CONDID"),
  all.x = TRUE)


### Creating another table with all the forest conditions and the correct EVALID
EVALID_OR <- 411901

PPSA_evalid <- subset(PPSA, EVALID == EVALID_OR)
base_cond <- merge(
  PPSA_evalid[, c("PLT_CN", "EVALID", "STRATUMCD")],
  COND[, c("PLT_CN", "CONDID", "CONDPROP_UNADJ", "COND_STATUS_CD")],
  by = "PLT_CN"
)

POP_STRATUM_evalid <- subset(POP_STRATUM, EVALID == EVALID_OR)
base_cond <- merge(
  base_cond,
  POP_STRATUM[, c("EVALID", "STRATUMCD", "EXPNS")],
  by = c("EVALID", "STRATUMCD"),
  all.x = TRUE
)

base_cond <- subset(base_cond, COND_STATUS_CD == 1)

base_biomass <- merge(
  base_cond,
  plot_summary[, c("PLT_CN", "CONDID", "biomass_Mg_ha")],
  by = c("PLT_CN", "CONDID"),
  all.x = TRUE
)

base_biomass$biomass_Mg_ha[is.na(base_biomass$biomass_Mg_ha)] <- 0

mean_DF_OR <- with(
  base_biomass,
  sum(biomass_Mg_ha * CONDPROP_UNADJ * EXPNS, na.rm = TRUE) /
    sum(CONDPROP_UNADJ * EXPNS, na.rm = TRUE))

mean_DF_OR


base_present <- subset(base_biomass, biomass_Mg_ha > 0)

mean_DF_present <- with(
  base_present,
  sum(biomass_Mg_ha * CONDPROP_UNADJ * EXPNS, na.rm = TRUE) /
    sum(CONDPROP_UNADJ * EXPNS, na.rm = TRUE)
)

mean_DF_present
mean(base_present$biomass_Mg_ha, na.rm = TRUE)


saveRDS(plot_summary, file = "Data/sp202_summary.rds")

# Biomass + SD per plot
plot(
  plot_summary$biomass_Mg_ha,
  plot_summary$sd_Mg_ha,
  pch = 19,
  xlab = "Plot biomass (Mg/ha)",
  ylab = "Uncertainty (SD, Mg/ha)",
  main = "Uncertainty vs biomass")

x <- plot_summary$biomass_Mg_ha
y <-  plot_summary$sd_Mg_ha
sd <-  plot_summary$sd_Mg_ha

plot(x, y,
     pch = 19,
     xlab = "Plots (Plot biomass)",
     ylab = "Uncertainty(SD)",
     main = "Plot biomass with uncertainty")

arrows(
  x0 = x, y0 = y - sd,
  x1 = x, y1 = y + sd,
  angle = 90, code = 3, length = 0.03)

# CV across plots
hist(plot_summary$cv_pct,
     breaks = 20,
     xlab = "CV (%)",
     main = "Distribution of plot-level uncertainty")

plot(plot_summary$biomass_Mg_ha,
     plot_summary$cv_pct,
     pch = 19,
     xlab = "Plot biomass (Mg/ha)",
     ylab = "Relative uncertainty (CV %)",
     main = "Plot-level biomass vs relative uncertainty")

################################################################################
################################################################################
#Testing parameters from region M240 and 240
dbh <- seq(5,50,by=1)
h <- 60
# M240 
a1<- 0.002580220
b1<- 1.717064
c1<-1.1645229

# 240 
a2<- 0.002916158
b2<- 1.778796
c2<- 1.0855265

Y1 <- a1 * dbh^b1 * h^c1
Y2 <- a2 * dbh^b2 * h^c2

plot(dbh, Y1, type = "l", lwd = 2,
     xlab = "DBH (cm)",
     ylab = "Predicted Stem volume",
     col = "black")

lines(dbh, Y2, lwd = 2, col = "red")
legend("topleft",
       legend = c("Ecoregion 1", "Ecoregion 2"),
       col = c("black", "red"),
       lwd = 2)

percent_diff <- (Y1 - Y2) / Y2 * 100

plot(dbh, percent_diff, type = "l",
     ylab = "% Difference",
     xlab = "DBH (cm)")
abline(h = 0, lty = 2)
################################################################################
################################################################################

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Components Uncertainty

# mean across trees for each simulation
stem_mean_by_sim   <- colMeans(stem_sim,   na.rm = TRUE)
bark_mean_by_sim   <- colMeans(bark_sim,   na.rm = TRUE)
branch_mean_by_sim <- colMeans(branch_sim, na.rm = TRUE)

# summary stats
means <- c(Stem   = mean(stem_mean_by_sim),
           Bark   = mean(bark_mean_by_sim),
           Branch = mean(branch_mean_by_sim))

sds   <- c(Stem   = sd(stem_mean_by_sim),
           Bark   = sd(bark_mean_by_sim),
           Branch = sd(branch_mean_by_sim))

bp <- barplot(means,
              ylab = "Mean biomass",
              main = "Mean biomass by component (MC uncertainty)",
              col  = c("red","green","blue"),
              ylim = c(0, max(means + sds) * 1.15))

arrows(x0 = bp, y0 = means - sds,
       x1 = bp, y1 = means + sds,
       angle = 90, code = 3, length = 0.05)

# Error bars  = 95% CI
# 95% CI instead of +/- SD

cis <- rbind(
  lower = c(Stem   = quantile(stem_mean_by_sim,   0.025),
            Bark   = quantile(bark_mean_by_sim,   0.025),
            Branch = quantile(branch_mean_by_sim, 0.025)),
  upper = c(Stem   = quantile(stem_mean_by_sim,   0.975),
            Bark   = quantile(bark_mean_by_sim,   0.975),
            Branch = quantile(branch_mean_by_sim, 0.975)))


bp <- barplot(means,
              ylab = "Mean biomass",
              main = "Mean biomass by component (95% CI)",
              col  = c("red","green","blue"),
              ylim = c(0, max(cis["upper",]) * 1.15))

arrows(x0 = bp, y0 = cis["lower",],
       x1 = bp, y1 = cis["upper",],
       angle = 90, code = 3, length = 0.05)



summary_sp202 <- component_summary(stem_mean_by_sim, bark_mean_by_sim, branch_mean_by_sim,
                                   species_code = "sp202")

#Store sp in one place
results_by_species <- readRDS("Data/component_uncertainty_results.rds")
results_by_species[["sp202"]] <- summary_sp202
# Save
saveRDS(results_by_species, file = "Data/component_uncertainty_results.rds")

# Comparison table for plotting
sp_plot <- species_plot_table(results_by_species)
  
library(ggplot2)
ggplot(sp_plot, aes(x = species, y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_wrap(~ component, nrow = 1, scales = "free_y") +
  labs(x = "Species", y = "Mean biomass", title = "Biomass by component (95% MC CI)") +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank())

ggplot(sp_plot, aes(x = species, y = mean, fill = component)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(x = "Species", y = "Mean biomass", fill = "Component",
       title = "Biomass components by species (95% MC CI)") +
  theme_bw()


# Relative uncertainty

sp_plot$cv_pct <- 100 * (sp_plot$upper - sp_plot$lower) / (2 * sp_plot$mean)  # approx from CI width

ggplot(sp_plot, aes(x = species, y = cv_pct)) +
  geom_col() +
  facet_wrap(~ component, nrow = 1, scales = "free_y") +
  labs(x = "Species", y = "Relative uncertainty (approx CV %)",
       title = "Relative uncertainty by component across species") +
  theme_bw() +
  theme(strip.background = element_blank())
