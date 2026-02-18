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
# Script meta-analysis model 4: Run fixed- and random effects meta-analysis of Cox
# Proportional-Hazards models with age of onset as timescale, EA, trait-specific
# PGS, EA * trait-specific PGS interaction, sex (except for protate and breast
# cancer), first 10 genetics PCs + birth decade as covariates across FinnGen
# data release 11 (FGR11), UK Biobank (UKB), and Generation Scotland (GS)
# (script inspired by the INTERVENE Flagship project scripts:
# https://github.com/intervene-EU-H2020/flagship)
#
# required input data: FGR11 + UKB + GS model 4 (from INTERVENE GxE_SESDisease GoogleDrive Folder)
#
# Last edits: 18/02/2026 (edits, FAH: add random-effects meta-analytical models)
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

# read in model 4
FGR11.4 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt", data.table=FALSE)
FGR11.4$Biobank <- "FinnGen"
#
UKB.4 <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
UKB.4$Biobank <- "UK Biobank"
#
GS.4 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
GS.4$Biobank <- "Generation Scotland"


################################################################################
#
# Combine FGR11 & UKB results
#
################################################################################

FGR11.4long <- data.frame(trait = c(rep(FGR11.4$trait,3)),
                          beta = c(FGR11.4$EAhigh_beta,FGR11.4$PRS_beta,FGR11.4$`PRS:EAhigh_beta`),
                          se = c(FGR11.4$EAhigh_se,FGR11.4$PRS_se,FGR11.4$`PRS:EAhigh_se`),
                          Test = c(rep(c("EA","PRS","EAxPRS"),each=nrow(FGR11.4))),
                          Biobank = c(rep(FGR11.4$Biobank,3)))
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
all.4 <- rbind(FGR11.4long,UKB.4long,GS.4long)



################################################################################
#
# Run meta-analyses - fixed effect
#
################################################################################

# run meta-analysis model 4
metaresults.4 <- c()
for(l in c("EA","PRS","EAxPRS")){
  
  quartile <- subset(all.4, Test==l)
  
  for(i in unique(quartile$trait)){
    print(i)
    disease <- subset(quartile, trait==i & !(is.na(beta)))
    
    
    #Meta analysis should be done at the beta level 
    meta <- rma(yi=beta, sei=se, data=disease, method="FE")
    
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
fwrite(metaresults.4, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model4/", as.character(Sys.Date()), 
                            "_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4_anyN.csv",sep=""))


################################################################################
#
# Run meta-analyses - random effect
#
################################################################################

# run meta-analysis model 4
metaresultsr.4 <- c()
for(l in c("EA","PRS","EAxPRS")){
  
  quartile <- subset(all.4, Test==l)
  
  for(i in unique(quartile$trait)){
    print(i)
    disease <- subset(quartile, trait==i & !(is.na(beta)))
    
    
    #Meta analysis should be done at the beta level 
    metar <- rma(yi=beta, sei=se, data=disease, method="REML")
    
    metaresultsr.4 <- rbind(metaresultsr.4, c(l, i, metar$b, metar$se, metar$pval, metar$QE, metar$QEp))
    
  }
  
}

# reorganize the meta-analysis results 
metaresultsr.4 <- as.data.frame(metaresultsr.4)
colnames(metaresultsr.4) <- c("Test","Phenotype","Beta","SE","Pval","QHet","HetPval")
metaresultsr.4 <- metaresultsr.4 %>% mutate_at(c(3:7),as.numeric)
metaresultsr.4$HR <- exp(metaresultsr.4$Beta)
metaresultsr.4$Cipos <- exp(metaresultsr.4$Beta + (1.96*metaresultsr.4$SE))
metaresultsr.4$Cineg <- exp(metaresultsr.4$Beta - (1.96*metaresultsr.4$SE))

# write to file
fwrite(metaresultsr.4, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model4/", as.character(Sys.Date()), 
                            "_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",sep=""))


################################################################################
#
# Upload meta-analyzed results to Google Drive
#
################################################################################

# fixed effect
drive_upload(media = "output/2classEA/MetaAnalysis/FGR11_UKB_GS/model4/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4_anyN.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",
             type = "spreadsheet")

# random effect
drive_upload(media = "output/2classEA/MetaAnalysis/FGR11_UKB_GS/model4/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",
             type = "spreadsheet")
