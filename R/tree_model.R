
#install.packages("data.table")
library(data.table)
library(openxlsx)
library(dplyr)
library(nlme)


#Importing the data base of tree

setwd('D:/ESF/Research2/')
tree<-read.csv(file="Data/NSVB_TREE.csv",stringsAsFactors = T)
#save(tree, file="tree.RData")
#load("Data/tree.RData")
s3a_spcd_coef <-read.csv(file="Tables/Table S3a_volob_coefs_spcd.csv",stringsAsFactors = T)

#s3b_jenkins_coef <-read.csv(file="Tables/Table S3b_volob_coefs_jenkins.csv",stringsAsFactors = T)

#Exploring the regions, no of sp and no of trees.
reg_sp_notree<-tree %>% 
  count(LOC,SPCD) #to see the no of sp and the no of trees in each region

reg_sp_notree %>% 
  arrange(desc(n)) #for identifying the rows with more number of trees

#Filtering Region 1 
S_LOC1<-tree %>% 
  filter(LOC=="1") %>% 
  mutate(sp=factor(SPCD))

#Explore the data
# head(S_LOC1)
# unique(S_LOC1$DO_BH)
# unique(S_LOC1$HT_TOT)
# unique(S_LOC1$SPCD)
# any(is.na(S_LOC1$HT_TOT))


s3a_sp_unique <- s3a_spcd_coef[!duplicated(s3a_spcd_coef$SPCD), ]

#Merging both data bases
S_LOC1_coef<-merge(S_LOC1, s3a_sp_unique,by='SPCD')

# Step 3: View the result
head(S_LOC1_coef)

#Filtering the columns of interest
S_LOC1_coef_DHHT<-S_LOC1_coef %>% 
  select(SPCD,AUTHOR,LOC,TREENO,DO_BH,HT_TOT,ST_WDBK_CV_TOT,ST_WDBK_DW_TOT,REGION,DIVISION,model,a,b,c)

# which(is.na(S_LOC1_coef_DHHT$ST_WDBK_CV_TOT))
# sum(is.na(S_LOC1_coef_DHHT$ST_WDBK_CV_TOT)) #5873
# which(is.na(S_LOC1_coef_DHHT$ST_WDBK_DW_TOT))
# sum(is.na(S_LOC1_coef_DHHT$ST_WDBK_DW_TOT)) #10932

#Plotting
plot(ST_WDBK_CV_TOT~HT_TOT, data=S_LOC1_coef_DHHT)
plot(ST_WDBK_CV_TOT~DO_BH, data=S_LOC1_coef_DHHT)

#nls function
nls_fitCV<-nls(ST_WDBK_CV_TOT~a*DO_BH^b*HT_TOT^c, data = S_LOC1_coef_DHHT,
                      start = list(a=0.004076385,b=1.855971,c=0.9764992))
summary(nls_fitCV) #each parameter is statistically significant. 

nls_fitDW<-nls(ST_WDBK_DW_TOT~a*DO_BH^b*HT_TOT^c, data = S_LOC1_coef_DHHT,
               start = list(a=0.004076385,b=1.855971,c=0.9764992))
summary(nls_fitDW)

#Extract fitted values and residuals
ST_WDBK_CV_TOT_obs <- nls_fitCV$m$lhs()     # observed values
ST_WDBK_CV_TOT_hat <- fitted(nls_fitCV)     # fitted values
resid <- residuals(nls_fitCV)  
#compute 
SS_res <- sum(resid^2)      # Residual Sum of Squares
SS_tot <- sum((ST_WDBK_CV_TOT_obs - mean(ST_WDBK_CV_TOT_obs))^2)  # Total Sum of Squares
R2 <- 1 - SS_res/SS_tot

R2 #0.986 Closer to 1 better fit

plot(ST_WDBK_CV_TOT_obs, ST_WDBK_CV_TOT_hat,
     xlab = "Observed", ylab = "Fitted",
     main = paste("Observed vs Fitted, R² =", round(R2, 3)))
abline(0, 1, col = "red")

