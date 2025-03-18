#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# educational attainment [EA]) in incidences of 18 common diseases (as
# previously selected in the INTERVENE flagship manuscript:
# https://doi.org/10.1101/2023.06.12.23291186) and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Meta-analyse Cox Proportional-Hazards models from R11 FinnGen, UKB,
# and GenScot where EA is dichotomized to low vs high EA ( inspired by the
# INTERVENE Flagship project scripts:
# https://github.com/intervene-EU-H2020/flagship)
#
# Models: 
#         1) EA, disease-specific polygenic score (PGS) sex (except for prostate
#         and breast cancer), first 10 genetic PCs + birth decade as covariate
#         2) EA, disease-specific polygenic score (PGS), EA*PGS, sex (except for prostate
#         and breast cancer), first 10 genetic PCs + birth decade as covariate
# Data: 
#       1) UKB + GS model 2 (from INTERVENE GxE_SESDisease GoogleDrive Folder)
#       2) UKB + GS model 4 (from INTERVENE GxE_SESDisease GoogleDrive Folder)
#
# Start script: 18/03/2025
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
# efficiently reading in large data sets; ggplot2 = versatile visualizations;
# dplyr, forcats & stringr = data wrangling; cowplot = combining plots; metafor
# = meta-analysis package; googledrive + googlesheets4 = read/write to/from
# GoogleDrive.
packages("data.table","ggplot2","metafor","dplyr","forcats","stringr","cowplot",
         "googledrive","googlesheets4")

# set working directory (the working directory is the project folder on my VM on
# the FinnGen Sosioeconomic Data Sandbox)
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
UKB.2 <- fread("output/GoogleDrive/UKB/2025-03-07_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
UKB.2$Biobank <- "UK Biobank"
#
GS.2 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
GS.2$Biobank <- "Generation Scotland"

# read in model 4
UKB.4 <- fread("output/GoogleDrive/UKB/2025-03-10_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
UKB.4$Biobank <- "UK Biobank"
#
GS.4 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
GS.4$Biobank <- "Generation Scotland"


################################################################################
#
# Combine UKB & GenScot results
#
################################################################################

## model 2 ##
# create long format to include both EA and PRS effect
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
all.2 <- rbind(UKB.2long,GS.2long)

## model 4 ##
UKB.4long <- data.frame(trait = c(rep(UKB.4$trait,3)),
                        beta = c(UKB.4$EAhigh_beta,UKB.4$PRS_beta,UKB.4$`PRS:EAhigh_beta`),
                        se = c(UKB.4$EAhigh_se,UKB.4$PRS_se,UKB.4$`PRS:EAhigh_se`),
                        Test = c(rep(c("EA","PRS","EAxPRS"),each=nrow(UKB.4))),
                        Biobank = c(rep(UKB.4$Biobank,3)))
GS.4long <- data.frame(trait = c(rep(GS.4$trait,3)),
                        beta = c(GS.4$EAhigh_beta,GS.4$PRS_beta,GS.4$`PRS:EAhigh_beta`),
                        se = c(GS.4$EAhigh_se,GS.4$PRS_se,GS.4$`PRS:EAhigh_se`),
                        Test = c(rep(c("EA","PRS","EAxPRS"),each=nrow(GS.4))),
                        Biobank = c(rep(GS.4$Biobank,3)))
#combine datasets
all.4 <- rbind(UKB.4long,GS.4long)


################################################################################
#
# Run meta-analyses & generate per model per trait forest plots
#
################################################################################

## model 2 ##

# run meta-analysis model 2 (+ create per trait forest plots)
metaresults.2 <- c()
for(l in c("EA","PRS")){

  quartile <- subset(all.2, Test==l)

  for(i in unique(quartile$trait)){
    print(i)
    disease <- subset(quartile, trait==i & !(is.na(beta)))


    #Meta analysis should be done at the beta level
    meta <- rma(yi=beta, sei=se, data=disease, method="FE")

    png(file=paste0("output/2classEA/MetaAnalysis/UKB_GS/model2/",as.character(Sys.Date()),
                    "_INTERVENE_EducationalAttainment_FEMetaAnalysis_UKB_GenScot_model2_",l,"_",i,"_Forest.png"),
        width = 7,
        height    = 7,
        units     = "in",
        res       = 300)
    forest(meta, slab=disease$Biobank)
    dev.off()

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
fwrite(metaresults.2, paste("output/2classEA/MetaAnalysis/UKB_GS/model2/", as.character(Sys.Date()),
                           "_INTERVENE_EducationalAttainment_FEMetaAnalysis_UKB_GenScot_model2.csv",sep=""))


## model 4 ##

# run meta-analysis model 4 (+ create per trait forest plots)
metaresults.4 <- c()
for(l in c("EA","PRS","EAxPRS")){

  quartile <- subset(all.4, Test==l)

  for(i in unique(quartile$trait)){
    print(i)
    disease <- subset(quartile, trait==i & !(is.na(beta)))


    #Meta analysis should be done at the beta level
    meta <- rma(yi=beta, sei=se, data=disease, method="FE")

    png(file=paste0("output/2classEA/MetaAnalysis/UKB_GS/model4/",as.character(Sys.Date()),
                    "_INTERVENE_EducationalAttainment_FEMetaAnalysis_UKB_GenScot_model4_",l,"_",i,"_Forest.png"),
        width = 7,
        height    = 7,
        units     = "in",
        res       = 300)
    forest(meta, slab=disease$Biobank)
    dev.off()

    metaresults.4 <- rbind(metaresults.4, c(l, i, meta$b, meta$se, meta$pval, meta$QE, meta$QEp))

  }

}

# reorganize the meta-analysis results
metaresults.4 <- as.data.frame(metaresults.4)
colnames(metaresults.4) <- c("Test","Phenotype","Beta","SE","Pval","QHet","HetPval")
metaresults.4 <- metaresults.4 %>% mutate_at(c(3:7),as.numeric)
metaresults.4$HR <- exp(metaresults.4$Beta)
metaresults.4$Cipos <- exp(metaresults.4$Beta + (1.96*metaresults.4$SE))
metaresults.4$Cineg <- exp(metaresults.4$Beta - (1.96*metaresults.4$SE))

# write to file
fwrite(metaresults.4, paste("output/2classEA/MetaAnalysis/UKB_GS/model4/", as.character(Sys.Date()),
                            "_INTERVENE_EducationalAttainment_FEMetaAnalysis_UKB_GenScot_model4.csv",sep=""))


################################################################################
#
# Upload meta-analyzed results to Google Drive (wait with upload until I know
# which meta-analysis to use)
#
################################################################################

# model 2
drive_upload(media = "output/2classEA/MetaAnalysis/UKB_GS/model2/2025-03-18_INTERVENE_EducationalAttainment_FEMetaAnalysis_UKB_GenScot_model2.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2025-03-18_INTERVENE_EducationalAttainment_FEMetaAnalysis_UKB_GenScot_model2.csv",
             type = "spreadsheet")

# model 4
drive_upload(media = "output/2classEA/MetaAnalysis/UKB_GS/model4/2025-03-18_INTERVENE_EducationalAttainment_FEMetaAnalysis_UKB_GenScot_model4.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2025-03-18_INTERVENE_EducationalAttainment_FEMetaAnalysis_UKB_GenScot_model4.csv",
             type = "spreadsheet")
