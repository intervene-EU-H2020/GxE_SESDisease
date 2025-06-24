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
# Script meta-analysis model 1a: Run fixed-effects meta-analysis of Cox
# Proportional-Hazards models with age of onset as timescale, where EA is
# dichotomized into low vs high EA (reference = low EA), and include sex (except
# for prostate and breast cancer), birth decade and the first 5 genetic PCs as
# covariates across FinnGen data release 11 (FGR11), UK Biobank (UKB), and
# Generation Scotland (GS) (script inspired by the INTERVENE Flagship project
# scripts: https://github.com/intervene-EU-H2020/flagship)
#
# required input data: FGR11 + UKB + GS model 1 EA only (from INTERVENE GxE_SESDisease GoogleDrive Folder)
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
packages("data.table","metafor","dplyr","forcats","stringr","googledrive",
         "googlesheets4")

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

# read in model 1a - EA only
FGR11.1a <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt", data.table=FALSE)
FGR11.1a$Biobank <- "FinnGen"
#
UKB.1a <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.1a$Biobank <- "UK Biobank"
#
GS.1a <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
GS.1a$Biobank <- "Generation Scotland"


################################################################################
#
# Combine FGR11 & UKB results
#
################################################################################

all.1a <- rbind(FGR11.1a[,c("trait","EAhigh_beta","EAhigh_se","Biobank")],
                UKB.1a[,c("trait","EAhigh_beta","EAhigh_se","Biobank")],
                GS.1a[,c("trait","EAhigh_beta","EAhigh_se","Biobank")])


################################################################################
#
# Run meta-analyses
#
################################################################################

## model 1a ##

# run meta-analysis model 1a
metaresults.1a <- c()
for(i in unique(all.1a$trait)){
  print(i)
  disease <- subset(all.1a, trait==i & !(is.na(EAhigh_beta)))

  #Meta analysis should be done at the beta level and stratified by ancestry 
  meta <- rma(yi=EAhigh_beta, sei=EAhigh_se, data=disease, method="FE")
  
  metaresults.1a <- rbind(metaresults.1a, c(i, meta$b, meta$se, meta$pval, meta$QE, meta$QEp))
}

# reorganize the meta-analysis results  
metaresults.1a <- as.data.frame(metaresults.1a)
colnames(metaresults.1a) <- c("Phenotype","Beta","SE","Pval","QHet","HetPval")
metaresults.1a <- metaresults.1a %>% mutate_at(c(2:6),as.numeric)
metaresults.1a$HR <- exp(metaresults.1a$Beta)
metaresults.1a$Cipos <- exp(metaresults.1a$Beta + (1.96*metaresults.1a$SE))
metaresults.1a$Cineg <- exp(metaresults.1a$Beta - (1.96*metaresults.1a$SE))

# write to file
fwrite(metaresults.1a, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model1/", as.character(Sys.Date()), 
                             "_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a_anyN.csv",sep=""))


################################################################################
#
# Upload meta-analyzed results to Google Drive
#
################################################################################

# model 1a
drive_upload(media = "output/2classEA/MetaAnalysis/FGR11_UKB_GS/model1/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a_anyN.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv",
             type = "spreadsheet")
