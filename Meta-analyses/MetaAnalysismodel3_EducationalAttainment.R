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
# Script meta-analysis model 3: Run fixed-effects meta-analysis of Cox
# Proportional-Hazards models with age of onset as timescale, with
# trait-specific PGS by EA level, sex (except for breast and prostate cancer),
# bith decade, and the first 10 genetic PCS as covariates across FinnGen data
# release 11 (FGR11), UK Biobank (UKB), and Generation Scotland (GS) (script
# inspired by the INTERVENE Flagship project scripts:
# https://github.com/intervene-EU-H2020/flagship)
#
# required input data: FGR11 + UKB + GS model 3 (from INTERVENE GxE_SESDisease GoogleDrive Folder)
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


# read in model 3
FGR11.3 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt", data.table=FALSE)
FGR11.3$Biobank <- "FinnGen"
#
UKB.3 <- fread("output/GoogleDrive/UKB/2024-04-04_UKB_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
UKB.3$Biobank <- "UK Biobank"
#
GS.3 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
GS.3$Biobank <- "Generation Scotland"


################################################################################
#
# As current version of UKB results also includes the traits
# where UKB was in the discovery GWAS, subset those data frames to only include
# the traits where UKB was not in the discovery GWAS: "Type 1
# Diabetes","Prostate Cancer","Gout","Rheumatoid Arthritis","Breast
# Cancer","Epilepsy","Alcohol Use Disorder"
#
################################################################################

UKB.3 <- UKB.3[which(UKB.3$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]


################################################################################
#
# Combine FGR11 & UKB results
#
################################################################################

# create long format to include both EA and PRS effect
FGR11.3long <- data.frame(trait = c(rep(FGR11.3$trait,2)),
                          beta = c(FGR11.3$GxlowEA_beta,FGR11.3$GxhighEA_beta),
                          se = c(FGR11.3$GxlowEA_se,FGR11.3$GxhighEA_se),
                          Test = c(rep(c("lowEA","highEA"),each=nrow(FGR11.3))),
                          Biobank = c(rep(FGR11.3$Biobank,2)))
UKB.3long <- data.frame(trait = c(rep(UKB.3$trait,2)),
                        beta = c(UKB.3$GxlowEA_beta,UKB.3$GxhighEA_beta),
                        se = c(UKB.3$GxlowEA_se,UKB.3$GxhighEA_beta),
                        Test = c(rep(c("lowEA","highEA"),each=nrow(UKB.3))),
                        Biobank = c(rep(UKB.3$Biobank,2)))
GS.3long <- data.frame(trait = c(rep(GS.3$trait,2)),
                        beta = c(GS.3$GxlowEA_beta,GS.3$GxhighEA_beta),
                        se = c(GS.3$GxlowEA_se,GS.3$GxhighEA_beta),
                        Test = c(rep(c("lowEA","highEA"),each=nrow(GS.3))),
                        Biobank = c(rep(GS.3$Biobank,2)))
#combine datasets
all.3 <- rbind(FGR11.3long,UKB.3long,GS.3long)


################################################################################
#
# Run meta-analyses
#
################################################################################

# run meta-analysis model 3
metaresults.3 <- c()
for(l in c("lowEA","highEA")){
  
  quartile <- subset(all.3, Test==l)
  
  for(i in unique(quartile$trait)){
    print(i)
    disease <- subset(quartile, trait==i & !(is.na(beta)))
    
    
    #Meta analysis should be done at the beta level 
    meta <- rma(yi=beta, sei=se, data=disease, method="FE")
    
    metaresults.3 <- rbind(metaresults.3, c(l, i, meta$b, meta$se, meta$pval, meta$QE, meta$QEp))
    
  }
  
}

# reorganize the meta-analysis results 
metaresults.3 <- as.data.frame(metaresults.3)
colnames(metaresults.3) <- c("EA","Phenotype","Beta","SE","Pval","QHet","HetPval")
metaresults.3 <- metaresults.3 %>% mutate_at(c(3:7),as.numeric)
metaresults.3$HR <- exp(metaresults.3$Beta)
metaresults.3$Cipos <- exp(metaresults.3$Beta + (1.96*metaresults.3$SE))
metaresults.3$Cineg <- exp(metaresults.3$Beta - (1.96*metaresults.3$SE))

# write to file
fwrite(metaresults.3, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model3/", as.character(Sys.Date()), 
                            "_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_anyN.csv",sep=""))


################################################################################
#
# Upload meta-analyzed results to Google Drive
#
################################################################################

drive_upload(media = "output/2classEA/MetaAnalysis/FGR11_UKB_GS/model3/2024-07-08_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_anyN.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2024-07-08_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_anyN.csv",
             type = "spreadsheet")

