### Load the data
load('Data/VT_sp129.RData')
#source("R/nls_fitting_functions.R")
library(dplyr)

#1. Getting Vermont sp129 (White pine)
tree_VT<- VT_sp129$TREE
tree_VT_plot <- VT_sp129$PLOT #(plot information)
tree_VT_cond <- VT_sp129$COND #(condition information)
tree_VT_pop <- VT_sp129$POP_STRATUM #(population information)
tree_VT_assgn <- VT_sp129$POP_PLOT_STRATUM_ASSGN # (Population Plot Stratum Assignment Table)

tree_VT_sp129 <- tree_VT[tree_VT$SPCD==129,]

# Choosing the EVALID value
sort(unique(tree_VT_pop$EVALID))
names(VT_sp129)
tree_VT_eval <- VT_sp129$POP_EVAL
tree_VT_eval_typ <- VT_sp129$POP_EVAL_TYP
tree_VT_eval[, c("EVALID", "EVAL_DESCR")] # evaluation matching my study period

evalid_selected <- 501901 #defining the population

tree_VT_eval[grepl("2019", tree_VT_eval$EVAL_DESCR) &
               grepl("CURRENT AREA, CURRENT VOLUME", tree_VT_eval$EVAL_DESCR),
             c("EVALID", "EVAL_DESCR")]


tree_VT_pop_sel   <- subset(tree_VT_pop,   EVALID == evalid_selected)
tree_VT_assgn_sel <- subset(tree_VT_assgn, EVALID == evalid_selected)

#1. From plot to stratum (we merge the plot information with pop_stratum that has 
# the expansion factor: column STRATUM_CN)
plot_stratum <- merge(
  tree_VT_plot,
  tree_VT_assgn_sel,
  by.x = "CN",
  by.y = "PLT_CN")

#2. From stratum to expansion factor (This join adds the area expansion factor: EXPNS)
plot_stratum <- merge(
  plot_stratum,
  tree_VT_pop_sel[, c("CN", "EXPNS")],
  by.x = "STRATUM_CN",
  by.y = "CN")

#3. Add condition proportions: PLT_CN adds each condition to its plot
cond_full <- merge(
  tree_VT_cond,
  plot_stratum,
  by.x = "PLT_CN",
  by.y = "CN")

#4. Create weight: condition are weight = stratum expansion factor * condition proportion
cond_full$w <- cond_full$EXPNS * cond_full$CONDPROP_UNADJ

#5. All-condition table *******
all_conditions <- cond_full[, c(
  "PLT_CN", "CONDID", "EXPNS", "CONDPROP_UNADJ", "w")]

#6. upload summary plot of sp 129 with AGB data
plot_summary <- readRDS("Data/sp129_VT.rds")


#6. Attaching the FIA weight to the biomass summary
plot_summary_all <- merge(
  all_conditions,
  plot_summary[, c("PLT_CN", "CONDID", "biomass_Mg_ha", "var_Mg2_ha2")],
  by = c("PLT_CN", "CONDID"),
  all.x = TRUE)

#### Compute design-based biomass estimate ###### 

# Weighted mean biomass density: Mg/ha forest area with biomass density
# Mean biomass where species 129 occurs
mean_biomass_Mgha_sampling <- with(
  subset(plot_summary_all, !is.na(biomass_Mg_ha)),
  sum(w * biomass_Mg_ha) / sum(w)
)

# White pine biomass averagaed over all Vermont forest area, 
# including forest areas with no white pine.

#plot_summary_all$biomass_Mg_ha[is.na(plot_summary_all$biomass_Mg_ha)] <- 0
#plot_summary_all$var_Mg2_ha2[is.na(plot_summary_all$var_Mg2_ha2)] <- 0
#mean_biomass_sampling <- with(plot_summary_all,
#                        sum(w * biomass_Mg_ha)/sum(w))

# Total biomass: Mg
total_biomass_Mg_sampling <- with(
            subset(plot_summary_all,!is.na(biomass_Mg_ha)),
                         sum(w * biomass_Mg_ha))

sum(plot_summary_all$w)   # total forest area represented

###########################Sampling uncertainty################################
################################################################################

# Number of plot-condition observations
n <- nrow(plot_summary_all) #number of independent sampling units

# Weighted mean
y_wsamp <- mean_biomass_Mgha_sampling

# Approximate design-based variance of weighted mean
var2_sampling_mean <- with(plot_summary_all,
                          sum((w^2) * (biomass_Mg_ha - y_wsamp)^2) / (sum(w)^2))

se_sampling_mean <- sqrt(var2_sampling_mean)

cv_sampling_pct <- 100 * se_sampling_mean /mean_biomass_Mgha_sampling

se_sampling_mean #sampling uncertainty
cv_sampling_pct


#######Using biomass from the rFIA data##################################
library(rFIA)

####### White Pine - Vermont ########
load('Data/VT_sp129.RData')
VT_sampling_se <- biomass(VT_sp129,
                          treeType = "live",
                          variance = TRUE,
                          component = "Total",
                          treeDomain = SPCD ==129)
                     
VT_sampling_se$BIO_ACRE_SE <- sqrt(VT_sampling_se$BIO_ACRE_VAR)

VT_2013_2019 <- VT_sampling_se %>%
  filter(YEAR >= 2013, YEAR <= 2019)

var_rFIA_mean_VT129 <- sum(VT_2013_2019$BIO_ACRE_VAR) / (7^2)

se_rFIA_mean_VT129 <- sqrt(var_rFIA_mean_VT129)

mean_biomass_rFIA_VT129 <- mean(VT_2013_2019$BIO_ACRE)

cv_rFIA_VT129 <- 100 * se_rFIA_mean_VT129 / mean_biomass_rFIA_VT129


load('Data/NC_sp110.RData')
NC_sampling_se <- biomass(NC_sp110,
                          treeType = "live",
                          variance = TRUE,
                          component = "Total",
                          treeDomain = SPCD ==110)

NC_sampling_se$BIO_ACRE_SE <- sqrt(NC_sampling_se$BIO_ACRE_VAR)

NC_2016_2021 <- NC_sampling_se %>%
  filter(YEAR >= 2016, YEAR <= 2021)

var_rFIA_mean_NC110 <- sum(NC_2016_2021$BIO_ACRE_VAR) / (6^2)

se_rFIA_mean_NC110 <- sqrt(var_rFIA_mean_NC110)

mean_biomass_rFIA_NC110 <- mean(NC_2016_2021$BIO_ACRE)

cv_rFIA_NC110 <- 100 * se_rFIA_mean_NC110 / mean_biomass_rFIA_NC110


# NC_110 <- biomass(NC_sp110_biomass2,
#                   treeType = "live",
#                   variance = TRUE,
#                   component = "Total")


########## Red maple #############
# load('Data/NY_sp316.RData')
# NY_sampling_se <- biomass(NY_sp316,
#                           treeType = "live",
#                           variance = TRUE,
#                           component = "Total",
#                           treeDomain = SPCD ==316)
# 
# NY_sampling_se$BIO_ACRE_SE <- sqrt(NY_sampling_se$BIO_ACRE_VAR)
# # NY_2013_2019 <- VT_sampling_se %>%
#   filter(YEAR >= 2013, YEAR <= 2019)
# # var_rFIA_mean <- sum(VT_2013_2019$BIO_ACRE_VAR) / (7^2)
# # se_rFIA_mean <- sqrt(var_rFIA_mean)
# # mean_biomass_rFIA <- mean(VT_2013_2019$BIO_ACRE)
# # cv_rFIA <- 100 * se_rFIA_mean / mean_biomass_rFIA


################################################################################

get_sampling_stats <- function(fia_db, spcd, species_name, start_year, end_year){
  
  samp <- biomass(
    fia_db,
    treeType = "live",
    variance = TRUE,
    component = "Total",
    treeDomain = SPCD == spcd
  )
  
  samp <- samp %>%
    filter(YEAR >= start_year, YEAR <= end_year)
  
  mean_biomass <- mean(samp$BIO_ACRE, na.rm = TRUE) * 2.2417 #to convert from short tons/acre to Mg/ha
  
  var_mean <- sum(samp$BIO_ACRE_VAR, na.rm = TRUE) / (nrow(samp)^2)
  
  se_sampling <- (sqrt(var_mean) * 2.2417) #To convert from short tons/acre to Mg/ha
  
  cv_sampling_pct <- 100 * se_sampling / mean_biomass
  
  data.frame(
    species = species_name,
    spcd = spcd,
    start_year = start_year,
    end_year = end_year,
    n_years = nrow(samp),
    mean_biomass_rFIA = mean_biomass,
    var_sampling = var_mean,
    se_sampling = se_sampling,
    cv_sampling_pct = cv_sampling_pct
  )
}

species_info <- list(
  list(db = VT_sp129, spcd = 129, species = "White pine", start = 2013, end = 2019),
  list(db = NY_sp316, spcd = 316, species = "Red maple", start = 2013, end = 2019),
  list(db = NY_sp318, spcd = 318, species = "Sugar maple", start = 2013, end = 2019),
  list(db = OR_sp202, spcd = 202, species = "Douglas-fir", start = 2010, end = 2019),
  list(db = FL_sp221, spcd = 221, species = "Bold Cypress", start = 2014, end = 2018),
  list(db = WI_sp802, spcd = 802, species = "White oak", start = 2015, end = 2022),
  list(db = NC_sp110, spcd = 110, species = "Shortleaf pine", start = 2016, end = 2021)
)

results_sampling <- do.call(
  rbind,
  lapply(species_info, function(x){
    get_sampling_stats(
      fia_db = x$db,
      spcd = x$spcd,
      species_name = x$species,
      start_year = x$start,
      end_year = x$end)})
)

results_sampling
save(results_sampling, file = 'Data/result_sampling.rds')


#### Calling the species biomass results ###########
# CN + PLT_CN + CONDID
NC_sp110_biomass2 <- readRDS("Data/sp110_summary2.rds")

plot_NCsp210_biomass2 <- NC_sp110_biomass2 %>%
   summarise(
    total_biomass_Mg_ha = sum(biomass_Mg_ha, na.rm = TRUE),
    mean_biomass_Mg_ha = mean(biomass_Mg_ha, na.rm = TRUE),
    var_biomass_Mg_ha = sum(var_Mg2_ha2, na.rm = TRUE),
    sd_Mg_ha = sqrt(var_biomass_Mg_ha),
    cv_biomass = 100 * (sd_Mg_ha/total_biomass_Mg_ha),
    n_plots =n()
  )

# PLT_CN + CONDID
# NC_sp110_biomass3 <- readRDS("Data/sp110_summary3.rds")
# 
# plot_NCsp110_biomass3 <- NC_sp110_biomass3 %>%
#   summarise(
#     total_biomass_Mg_ha = sum(biomass_Mg_ha, na.rm = TRUE),
#     mean_biomass_Mg_ha = mean(biomass_Mg_ha, na.rm = TRUE),
#     var_biomass_Mg2_ha2 = sum(var_Mg2_ha2, na.rm = TRUE),
#     sd_Mg_ha = sqrt(var_biomass_Mg2_ha2),
#     cv_biomass = 100 * (sd_Mg_ha/total_biomass_Mg_ha),
#    n_plots = n()
#   )

OR_sp202_biomass <- readRDS("Data/sp202_summary.rds")

plot_ORsp202_biomass <- OR_sp202_biomass %>%
  summarise(
    total_biomass_Mg_ha = sum(biomass_Mg_ha, na.rm = TRUE),
    mean_biomass_Mg_ha = mean(biomass_Mg_ha, na.rm = TRUE),
    var_biomass_Mg_ha = sum(var_Mg2_ha2, na.rm = TRUE),
    sd_Mg_ha = sqrt(var_biomass_Mg_ha),
    cv_biomass = 100 * (sd_Mg_ha/total_biomass_Mg_ha),
    n_plots =n()
  )

OR_sp202_biomass2 <- readRDS("Data/sp202_summary2.rds")

plot_ORsp202_biomass2 <- OR_sp202_biomass2 %>%
  summarise(
    total_biomass_Mg_ha = sum(biomass_Mg_ha, na.rm = TRUE),
    mean_biomass_Mg_ha = mean(biomass_Mg_ha, na.rm = TRUE),
    var_biomass_Mg_ha = sum(var_Mg2_ha2, na.rm = TRUE),
    sd_Mg_ha = sqrt(var_biomass_Mg_ha),
    cv_biomass = 100 * (sd_Mg_ha/total_biomass_Mg_ha),
    n_plots =n()
  )
