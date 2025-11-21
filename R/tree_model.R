
#install.packages("data.table")
library(data.table)
library(openxlsx)
library(dplyr)
library(nlme)

#Loading the dataset

tree<-read.csv(file="Data/NSVB_TREE.csv",stringsAsFactors = T)

ecoregion <- read.csv(file="Data/ECOREGION.csv", sep = ';',stringsAsFactors = T)

s3a_spcd_coef <-read.csv(file="Tables/Table S3a_volob_coefs_spcd.csv",stringsAsFactors = T)

#s3b_jenkins_coef <-read.csv(file="Tables/Table S3b_volob_coefs_jenkins.csv",stringsAsFactors = T)

#Filtering SPCD 110
sample_sp110<-tree %>% 
  filter(SPCD==110)

length(unique(sample_sp110$REGION))
table(sample_sp110$REGION)

#Filtering the coeficients for sp 110
coef_sp110 <- s3a_spcd_coef %>% 
  filter(SPCD==110)  

# Mergin coefficients dataset and ecoregion dataset
sp110_tree_coef <- merge(coef_sp110, ecoregion, by='DIVISION')

#Merging sp110 tree dataset with its coefficients
sp110<- sample_sp110 %>% 
  left_join(sp110_tree_coef, by = c('SPCD','REGION'))

 

#Filtering the columns of interest
sample_sp111_coef_DHHT<-sample_sp111_coef %>% 
  select(SPCD,AUTHOR,TREENO,DO_BH,HT_TOT,REGION,ST_WD_CV_TOT,DIVISION,model,a,b,c)
  
#Plotting
plot(ST_WD_CV_TOT~HT_TOT, data=sample_sp111_coef_DHHT)
plot(ST_WD_CV_TOT~DO_BH, data=sample_sp111_coef_DHHT)

#nls function
nls_fitCV<-nls(ST_WD_CV_TOT~a*DO_BH^b*HT_TOT^c, data = sample_sp111_coef_DHHT,
                      start = list(a=0.003633229,b=1.952409,c=0.965148))
summary(nls_fitCV) #each parameter is statistically significant.

#Predicting the biomass
predbiomass_sample_sp111<-predict(nls_fitCV, newdata = sample_sp111_coef_DHHT)

#For 95% prediction interval
# 1. Extract parameter estimates and variance-covariance matrix
params <- coef(nls_fitCV)
vcov_mat <- vcov(nls_fitCV)

# 2. Simulate parameters 1000 times
library(MASS)
sim_params <- mvrnorm(1000, mu = params, Sigma = vcov_mat)

# 3. Compute predicted biomass for each simulation
pred_matrix <- apply(sim_params, 1, function(p) {
  a <- p[1]; b <- p[2]; c <- p[3]
  a * sample_sp111_coef_DHHT$DO_BH^b * sample_sp111_coef_DHHT$HT_TOT^c
})

# 4. Compute mean prediction and 95% interval
sample_sp111_coef_DHHT$pred_mean  <- rowMeans(pred_matrix)
sample_sp111_coef_DHHT$pred_lower <- apply(pred_matrix, 1, quantile, probs = 0.025, na.rm=TRUE)
sample_sp111_coef_DHHT$pred_upper <- apply(pred_matrix, 1, quantile, probs = 0.975, na.rm=TRUE)

head(sample_sp111_coef_DHHT[, c("DO_BH", "HT_TOT", "pred_mean", "pred_lower", "pred_upper")])

# 5. Converting the 95% prediction interval into a standard error
sample_sp111_coef_DHHT$pred_SE <- 
  (sample_sp111_coef_DHHT$pred_upper - sample_sp111_coef_DHHT$pred_lower) / (2 * 1.96)

# 6. Calculate the variance for each tree
sample_sp111_coef_DHHT$pred_var <- sample_sp111_coef_DHHT$pred_SE^2

# 7. Summing the variance s of all trees
var_total_sp111 <- sum(sample_sp111_coef_DHHT$pred_var, na.rm = TRUE)

# 8. Calculate SE total and CI for plot total
SE_total_sp111<-sqrt(var_total_sp111)
total_biomass<-sum(predbiomass_sample_sp111, na.rm=TRUE)
CI_upper_sp111<-total_biomass + (1.96 * SE_total_sp111)
CI_lower_sp111<-total_biomass - (1.96 * SE_total_sp111)
