#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# educational attainment [EA]) in risk of 18 common diseases (as
# previously selected in the INTERVENE flagship manuscript:
# https://doi.org/10.1101/2023.06.12.23291186) and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script meta-analysis model 2: Run fixed-effects meta-analysis of ox Proportional-Hazards models with age of onset as
# timescale, where EA is dichotomized into low vs high EA (reference = low EA),
# and include the trait-specific PGS, sex (except for prostate and breast
# cancer), birth decade and the first 10 genetic PCs as covariates across FinnGen
# data release 11 (FGR11), UK Biobank (UKB), and Generation Scotland (GS) (script inspired by the
# INTERVENE Flagship project scripts:
# https://github.com/intervene-EU-H2020/flagship)
#
# required input data: FGR11 + UKB + GS model 2 (from INTERVENE GxE_SESDisease GoogleDrive Folder)
#
# Last edits: 06/11/2024 (edits, FAH: final checks and minor tweaks prior to
# upload to GitHub)
# 
################################################################################

################################################################################
#
# Set up script
#
################################################################################

# function to install (if required) and load R packages
packages<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

# install (if required) and load the following R packages (this uses the
# packages function as specified in the source file): data.table = package for
# efficiently reading in large data sets; dplyr, forcats & stringr = data
# wrangling; metafor = meta-analysis package; googledrive + googlesheets4 =
# read/write to/from GoogleDrive.
packages("data.table","metafor","dplyr","forcats","stringr",
         "googledrive","googlesheets4")

# set working directory
setwd("C:/Users/hagenbee/OneDrive - University of Helsinki/SESdiffDiseaseRisk/")

# run GoogleDrive - set to FALSE if latest results have already been downloaded!
run_googledrive<-FALSE


################################################################################
#
# Read in full sample hazard ratios per standard deviation
#
################################################################################

# download latest version of files to local machine
if (run_googledrive==TRUE) {
  #identify folder
  folder_id = drive_get(as_id("18iI9QxxJ7WXrXO6hcNal_amvGmR1F_jv")) # this ID links to the "INTERVENE flagship/GxE_SESDisease/Output_CoxPHmodels_perBB" folder. ID obtained with drive_find(pattern = "Output_Cox")
  
  #find files in folder
  files = drive_ls(folder_id)
  
  #loop dirs and download files inside them
  for (i in seq_along(files$name)) {
    #list files
    i_dir = drive_ls(files[i, ])
    
    #mkdir
    try({dir.create(paste0("output/GoogleDrive/",files$name[i]))})
    
    #download files
    for (file_i in seq_along(i_dir$name)) {
      #fails if already exists
      try({
        drive_download(
          as_id(i_dir$id[file_i]),
          path = paste0("output/GoogleDrive/",files$name[i], "/",i_dir$name[file_i])
        )
      })
    }
  }
}

# read in model 2
FGR11.2 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt", data.table=FALSE)
FGR11.2$Biobank <- "FinnGen"
#
UKB.2 <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
UKB.2$Biobank <- "UK Biobank"
#
GS.2 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
GS.2$Biobank <- "Generation Scotland"


################################################################################
#
# Combine FGR11 & UKB results
#
################################################################################

# create long format to include both EA and PRS effect
FGR11.2long <- data.frame(trait = c(rep(FGR11.2$trait,2)),
                          beta = c(FGR11.2$EAhigh_beta,FGR11.2$PRS_beta),
                          se = c(FGR11.2$EAhigh_se,FGR11.2$PRS_se),
                          Test = c(rep(c("EA","PRS"),each=nrow(FGR11.2))),
                          Biobank = c(rep(FGR11.2$Biobank,2)))
UKB.2long <- data.frame(trait = c(rep(UKB.2$trait,2)),
                        beta = c(UKB.2$EAhigh_beta,UKB.2$PRS_beta),
                        se = c(UKB.2$EAhigh_se,UKB.2$PRS_se),
                        Test = c(rep(c("EA","PRS"),each=nrow(UKB.2))),
                        Biobank = c(rep(UKB.2$Biobank,2)))
GS.2long <- data.frame(trait = c(rep(GS.2$trait,2)),
                        beta = c(GS.2$EAhigh_beta,GS.2$PRS_beta),
                        se = c(GS.2$EAhigh_se,GS.2$PRS_se),
                        Test = c(rep(c("EA","PRS"),each=nrow(GS.2))),
                        Biobank = c(rep(GS.2$Biobank,2)))
#combine datasets
all.2 <- rbind(FGR11.2long,UKB.2long,GS.2long)


################################################################################
#
# Run meta-analyses
#
################################################################################

# run meta-analysis model 2
metaresults.2 <- c()
for(l in c("EA","PRS")){
  
  quartile <- subset(all.2, Test==l)
  
  for(i in unique(quartile$trait)){
    print(i)
    disease <- subset(quartile, trait==i & !(is.na(beta)))
    
    
    #Meta analysis should be done at the beta level 
    meta <- rma(yi=beta, sei=se, data=disease, method="FE")
    
    metaresults.2 <- rbind(metaresults.2, c(l, i, meta$b, meta$se, meta$pval, meta$QE, meta$QEp))
    
  }
  
}

# reorganize the meta-analysis results 
metaresults.2 <- as.data.frame(metaresults.2)
colnames(metaresults.2) <- c("Test","Phenotype","Beta","SE","Pval","QHet","HetPval")
metaresults.2 <- metaresults.2 %>% mutate_at(c(3:7),as.numeric)
metaresults.2$HR <- exp(metaresults.2$Beta)
metaresults.2$Cipos <- exp(metaresults.2$Beta + (1.96*metaresults.2$SE))
metaresults.2$Cineg <- exp(metaresults.2$Beta - (1.96*metaresults.2$SE))

# write to file
fwrite(metaresults.2, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model2/", as.character(Sys.Date()), 
                           "_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2_anyN.csv",sep=""))


################################################################################
#
# Upload meta-analyzed results to Google Drive 
#
################################################################################

drive_upload(media = "output/2classEA/MetaAnalysis/FGR11_UKB_GS/model2/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2_anyN.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2.csv",
             type = "spreadsheet")
