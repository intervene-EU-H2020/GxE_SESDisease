#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# occupation) in risk of 18 common diseases (as previously selected in the
# INTERVENE flagship manuscript: https://doi.org/10.1101/2023.06.12.23291186)
# and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script meta-analysis model 4: Run fixed- and random-effects meta-analysis of
# Cox Proportional-Hazards models with age of onset as timescale, occupation,
# trait-specific PGS, occupation * trait-specific PGS interaction, sex (except
# for protate and breast cancer), first 10 genetics PCs + birth decade as
# covariates across FinnGen data release 11 (FGR11), UK Biobank (UKB), and
# Generation Scotland (GS) (script inspired by the INTERVENE Flagship project
# scripts: https://github.com/intervene-EU-H2020/flagship)
#
# required input data: FGR11 + UKB + GS model 4 (from INTERVENE GxE_SESDisease
# GoogleDrive Folder)
#
# Last edits: 16/04/2026 (edits, FAH: gloabalize for Github update)
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
setwd("C:/Users/fhk210/OneDrive - Vrije Universiteit Amsterdam/OngoingProjects/SESDiffDiseaseRisk")

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
FGR11.4 <- fread("output/GoogleDrive/FGR11/2025-01-30_INTERVENE_Occupation_Coeffs_CoxPH_model4_FinnGenR11.txt", data.table=FALSE)
FGR11.4$Biobank <- "FinnGen"
#
UKB.4 <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model4_Coeffs.txt",data.table=FALSE)
UKB.4$Biobank <- "UK Biobank"
#
GS.4 <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model4_Coeffs.txt",data.table=FALSE)
GS.4$Biobank <- "Generation Scotland"


################################################################################
#
# Generation Scotland included "T1D, Rheumatoid arthritis, skin melanoma, and
# epilepsy" which have too small sample sizes to perform the analyses in. Remove
# these traits prior to the meta-analysis
#
################################################################################

GS.4 <- GS.4[-which(GS.4$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                      "C3_MELANOMA_SKIN","G6_EPLEPSY")),]


################################################################################
#
# Combine FGR11 & UKB results
#
################################################################################

FGR11.4long <- data.frame(trait = c(rep(FGR11.4$trait,3)),
                          beta = c(FGR11.4$`OccupationUpper-level_beta`,FGR11.4$PRS_beta,FGR11.4$`PRS:OccupationUpper-level_beta`),
                          se = c(FGR11.4$`OccupationUpper-level_se`,FGR11.4$PRS_se,FGR11.4$`PRS:OccupationUpper-level_se`),
                          Test = c(rep(c("Occupation","PRS","OccupationxPRS"),each=nrow(FGR11.4))),
                          Biobank = c(rep(FGR11.4$Biobank,3)))
UKB.4long <- data.frame(trait = c(rep(UKB.4$trait,3)),
                        beta = c(UKB.4$`OccupationUpper-level_beta`,UKB.4$PRS_beta,UKB.4$`PRS:OccupationUpper-level_beta`),
                        se = c(UKB.4$`OccupationUpper-level_se`,UKB.4$PRS_se,UKB.4$`PRS:OccupationUpper-level_se`),
                        Test = c(rep(c("Occupation","PRS","OccupationxPRS"),each=nrow(UKB.4))),
                        Biobank = c(rep(UKB.4$Biobank,3)))
GS.4long <- data.frame(trait = c(rep(GS.4$trait,3)),
                        beta = c(GS.4$`OccupationUpper-level_beta`,GS.4$PRS_beta,GS.4$`PRS:OccupationUpper-level_beta`),
                        se = c(GS.4$`OccupationUpper-level_se`,GS.4$PRS_se,GS.4$`PRS:OccupationUpper-level_se`),
                        Test = c(rep(c("Occupation","PRS","OccupationxPRS"),each=nrow(GS.4))),
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
for(l in c("Occupation","PRS","OccupationxPRS")){
  
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
fwrite(metaresults.4, paste("output/EmploymentStatus/MetaAnalysis/FGR11_UKB_GS/model4/", as.character(Sys.Date()), 
                            "_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",sep=""))


################################################################################
#
# Run meta-analyses - random effect
#
################################################################################

# run meta-analysis model 4
metaresultsr.4 <- c()
for(l in c("Occupation","PRS","OccupationxPRS")){
  
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
fwrite(metaresultsr.4, paste("output/EmploymentStatus/MetaAnalysis/FGR11_UKB_GS/model4/", as.character(Sys.Date()), 
                            "_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",sep=""))


################################################################################
#
# Upload meta-analyzed results to Google Drive
#
################################################################################

# fixed effect
drive_upload(media = "output/EmploymentStatus/MetaAnalysis/FGR11_UKB_GS/model4/2026-03-13_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2026-03-13_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",
             type = "spreadsheet")

# random effect
drive_upload(media = "output/EmploymentStatus/MetaAnalysis/FGR11_UKB_GS/model4/2026-03-13_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2026-03-13_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",
             type = "spreadsheet")
