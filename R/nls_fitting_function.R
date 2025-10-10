#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
#- Function to fit the Schumacher-Hall model to the Westfall et al. 2024 allometric dataset
#  Takes the number of a species and the region as inputs (one each as integers).
#  Also takes the parameter list (Table S3a_volob_coefs_spcd.csv), and the "tree" dataset as arguments.
#  Returns the fitted nls model.
#  If the plotson flag is T, also make some plots
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
nls_fitting_function <- function(species,region,params,tree,plotson=F...){
  
  
  #Filtering species
  tree_sample<-tree %>% 
    filter(SPCD==species)
  
  #Filtering the coefficients for species
  params_species <- params %>% 
    filter(SPCD==species) %>% 
    filter(DIVISION==region)
    
  
  #Merging both data bases
  sample_species<-merge(tree_sample, params_species, by='SPCD')
  
  #Filtering the columns of interest
  sample_species2<-sample_species %>% 
    select(SPCD,AUTHOR,TREENO,DO_BH,HT_TOT,REGION,ST_WD_CV_TOT,DIVISION,model,a,b,c)
  
  #- make plots if requested. Model diagnostic plots may be useful too.
  if(plotson==T){
    plot(ST_WD_CV_TOT~HT_TOT, data=sample_species2)
    plot(ST_WD_CV_TOT~DO_BH, data=sample_species2)
  }
  
  #- Fit the Schumacher-Hall model
  nls_fitCV<-nls(ST_WD_CV_TOT~a*DO_BH^b*HT_TOT^c, data = sample_species2,
                 start = list(a=sample_species2$a[1],b=sample_species2$b[1],c=sample_species2$c[1]))
  return(nls_fitCV)
}
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

