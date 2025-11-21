
install.packages("propagate")
library(propagate)
library(MASS)
#library(dplyr)

# ==============================================================================
# Function that directly adds predictions and SE to your dataframe
# ==============================================================================

add_vol_pred <- function(data, models, n_sim = 1000) {
  
  process_one_model <- function(nls_model, newdata, component_name) {
    cat("Processing", component_name, "volume...\n")
    
    # Extract parameters and covariance matrix
    params <- coef(nls_model)
    vcov_mat <- vcov(nls_model)
    
    # Simulate parameters
    sim_params <- mvrnorm(n_sim, mu = params, Sigma = vcov_mat)
    
    # Compute predictions for each simulated parameter set
    pred_matrix <- apply(sim_params, 1, function(p) {
      a <- p[1]; b <- p[2]; c <- p[3]
      a * newdata$DO_BH^b * newdata$HT_TOT^c
    })
    
    # Calculate statistics
    pred_mean  <- rowMeans(pred_matrix)
    pred_lower <- apply(pred_matrix, 1, quantile, probs = 0.025, na.rm = TRUE)
    pred_upper <- apply(pred_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
    pred_SE <- (pred_upper - pred_lower) / (2 * 1.96)
    
    # Add columns directly to dataframe
    data[[paste0("pred_", component_name, "vol")]] <- pred_mean
    data[[paste0("pred_", component_name, "vol_se")]] <- pred_SE
    data[[paste0("pred_", component_name, "vol_lower")]] <- pred_lower
    data[[paste0("pred_", component_name, "vol_upper")]] <- pred_upper
    
    return(data)
  }
  
  # Process all models
  for(component in names(models)) {
    data <- process_one_model(models[[component]], data, component)
  }
  
  return(data)
}

# ==============================================================================
# Apply to the data
# ==============================================================================

# Create named list of models
models <- list(
  stem = output1_tree,
  bark = output1_bark,
  branch = output1_branch
)

# Process all models at once
sp_110 <- add_vol_pred(sp_110, models, n_sim = 1000)

# Bark density uncertainty 
mean_bark_WD<- mean(sp_110$Density_sp110, na.rm=TRUE)
sd_bark_WD <- sd(sp_110$bark_density_sp110, na.rm = TRUE)
CV_bark<- sd_bark_WD/mean_bark_WD
sp_110$Density_sp110_sd <- sd_bark_WD


# ==============================================================================
# Monte Carlo Simulation for Total Biomass Uncertainty
# ==============================================================================

set.seed(123)
n_sims <- 1000
n_trees <- nrow(sp_110)

biomass_simulations <- matrix(NA, nrow = n_trees, ncol = n_sims)

for(sim in 1:n_sims) {
  
  # Simulate volumes with uncertainty
  sim_stemvol <- rnorm(n_trees, 
                       mean = sp_110$pred_stemvol, 
                       sd = sp_110$pred_stemvol_se)
  
  sim_barkvol <- rnorm(n_trees, 
                       mean = sp_110$pred_barkvol, 
                       sd = sp_110$pred_barkvol_se)

 sim_branchvol <- rnorm(n_trees, 
                         mean = sp_110$pred_branchvol, 
                         sd = sp_110$pred_branchvol_se)
 
 # Stem wood density: single value per simulation (species-level
 # mean_CV = WD_sp110$meanWD*62.428 in cubic volume
  sim_WD_stem_sp110 <- rnorm(1, mean = WD_sp110$mean_CV, sd = WD_sp110$mean_CV_sd)
 
  # Bark density: individual values per tree WITH uncertainty
  
  sim_Density_bark <- rnorm(n_trees, 
                           mean = sp_110$Density_sp110, 
                           sd = sp_110$Density_sp110_sd)
 
  # Ensure biological constraints (no negative values)
  sim_stemvol <- pmax(sim_stemvol, 0)
  sim_barkvol <- pmax(sim_barkvol, 0)
  sim_branchvol <- pmax(sim_branchvol, 0)
  sim_WD_stem <- pmax(sim_WD_stem_sp110, 0)
  sim_Density_bark <- pmax(sim_Density_bark, 0)
  
  # Calculate total biomass for this simulation
  # stem biomass: stem volume × stem wood density (same WD for all trees)
  # bark biomass: bark volume × bark density (individual per tree)
  # branch biomass: branch volume (already in biomass units)
  biomass_simulations[, sim] <- (sim_stemvol * sim_WD_stem) + 
    (sim_barkvol * sim_Density_bark) + 
    sim_branchvol
  
  if(sim %% 100 == 0) cat("  Completed", sim, "simulations\n")
}

sp_110$total_biomass_sim<-rowMeans(biomass_simulations)

# ==============================================================================
# Calculate uncertainty metrics for individual trees
# ==============================================================================

sp_110$biomass_mean_sim <- rowMeans(biomass_simulations)
sp_110$biomass_median_sim <- apply(biomass_simulations, 1, median)
sp_110$biomass_sd_sim <- apply(biomass_simulations, 1, sd)
sp_110$biomass_var_sim <- sp_110$biomass_sd^2
sp_110$biomass_cv_sim <- (sp_110$biomass_sd / sp_110$biomass_mean) * 100
sp_110$biomass_ci_lower_sim <- apply(biomass_simulations, 1, quantile, probs = 0.025)
sp_110$biomass_ci_upper_sim <- apply(biomass_simulations, 1, quantile, probs = 0.975)

# ==============================================================================
# Calculate total biomass across all trees with uncertainty
# ==============================================================================

# Total biomass for each simulation
total_biomass_sims <- colSums(biomass_simulations)


# ==============================================================================
# Calculating total biomass directly
# ==============================================================================
#sp_110$barkdensity<-(sp_110$Density_sp110*62.428)
sp_110$totalbiomass=((sp_110$pred_stemvol*WD_sp110$mean_CV)+
                       (sp_110$pred_barkvol*sp_110$Density_sp110)+
                       (sp_110$pred_branchvol))

# Variance of each component
# For stem: (volume * wood density)
var_stem <- (sp_110$pred_stemvol)^2 * (WD_sp110$mean_CV_sd)^2 + 
  (WD_sp110$mean_CV)^2 * (sp_110$pred_stemvol_se)^2

# For bark: (volume * density)
var_bark <- (sp_110$pred_barkvol)^2 * (sp_110$Density_sp110_sd)^2 + 
  (sp_110$Density_sp110)^2 * (sp_110$pred_barkvol_se)^2

# For branch: (just volume)
var_branch <- (sp_110$pred_branchvol_se)^2

# Total variance (sum of variances)
sp_110$var_totalbiomass <- var_stem + var_bark + var_branch

# Total uncertainty (standard deviation)
sp_110$sd_totalbiomass <- sqrt(sp_110$var_totalbiomass)

