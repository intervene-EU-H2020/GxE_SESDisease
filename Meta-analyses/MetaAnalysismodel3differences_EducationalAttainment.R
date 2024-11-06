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
# Script: Calculate whether the difference in effect of the disease-specific
# polygenic scores between the low and high education group are significant
#
# required input data: FinnGen data release 11 (FGR11) + UK Biobank (UKB) +
# Generation Scotland (GS) + meta-analysis (FEMA) model 3 (EA models)
#
# Last edits: 06/11/2024 (edits, FAH: add code to download/upload from
# GoogleDrive + final checks and minor tweaks prior to upload to GitHub)
# 
################################################################################

################################################################################
#
# Set up script
#
################################################################################

# set working directory 
setwd("C:/Users/hagenbee/OneDrive - University of Helsinki/SESdiffDiseaseRisk/")

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
# efficiently reading in large data sets; dplyr, forcats, stringr & tidyr = data
# wrangling; googledrive + googlesheets4 = read/write to/from GoogleDrive.
packages("data.table","dplyr","forcats","stringr","tidyr","googledrive",
         "googlesheets4")

# run GoogleDrive - set to FALSE if latest results have already been downloaded!
run_googledrive<-TRUE


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

# read in model 3 - per biobank
FGR11.3 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt", data.table=FALSE)
FGR11.3$Biobank <- "FinnGen"
#
UKB.3 <- fread("output/GoogleDrive/UKB/2024-04-04_UKB_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
UKB.3$Biobank <- "UK Biobank"
#
GS.3 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
GS.3$Biobank <- "Generation Scotland"

# Extract meta-analysis results from Google Drive
if (run_googledrive == TRUE) {
  
  # Identify folder
  folder_id <- drive_get(as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH")) # this ID links to the "INTERVENE flagship/GxE_SESDisease/Output_CoxPHmodels_meta-analysis" folder. ID obtained with drive_find(pattern = "Output_Cox")
  
  # List files in the folder (no subfolders expected)
  files <- drive_ls(folder_id)
  
  # Create local directory for downloads if it doesn't exist
  dir.create("output/GoogleDrive/MetaAnalysis", showWarnings = FALSE, recursive = TRUE)
  
  # Download each file in the folder
  for (file_i in seq_along(files$name)) {
    try({
      drive_download(
        as_id(files$id[file_i]),
        path = paste0("output/GoogleDrive/MetaAnalysis/", files$name[file_i]),
        overwrite = FALSE
      )
    }, silent = TRUE)
  }
}

# read in model 3 - meta-analysis
FEMA.3 <- fread("output/GoogleDrive/MetaAnalysis/2024-07-08_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_anyN.csv",data.table=FALSE)
FEMA.3$Biobank <- "FE meta-analysis"


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
# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits: "Atrial Fibrillation", "Colorectal Cancer",
#
################################################################################

FEMA.3 <- FEMA.3[-which(FEMA.3$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results 
#
################################################################################

# model 3 - also add beta and se (needed to determine significanc of difference
# PGS effect between low/high education groups)
model3 <- data.frame(trait = c(rep(FGR11.3$trait,2),rep(UKB.3$trait,2),
                               rep(GS.3$trait,2),FEMA.3$Phenotype),
                     HR = c(FGR11.3$GxlowEA_HR,FGR11.3$GxhighEA_HR,
                            UKB.3$GxlowEA_HR,UKB.3$GxhighEA_HR,
                            GS.3$GxlowEA_HR,GS.3$GxhighEA_HR,FEMA.3$HR),
                     lb = c(FGR11.3$GxlowEA_HR_lower95,FGR11.3$GxhighEA_HR_lower95,
                            UKB.3$GxlowEA_HR_lower95,UKB.3$GxhighEA_HR_lower95,
                            GS.3$GxlowEA_HR_lower95,GS.3$GxhighEA_HR_lower95,
                            FEMA.3$Cineg),
                     ub = c(FGR11.3$GxlowEA_HR_upper95,FGR11.3$GxhighEA_HR_upper95,
                            UKB.3$GxlowEA_HR_upper95,UKB.3$GxhighEA_HR_upper95,
                            GS.3$GxlowEA_HR_upper95,GS.3$GxhighEA_HR_upper95,
                            FEMA.3$Cipos),
                     beta = c(FGR11.3$GxlowEA_beta,FGR11.3$GxhighEA_beta,
                              UKB.3$GxlowEA_beta,UKB.3$GxhighEA_beta,
                              GS.3$GxlowEA_beta,GS.3$GxhighEA_beta,FEMA.3$Beta),
                     se = c(FGR11.3$GxlowEA_se,FGR11.3$GxhighEA_se,
                            UKB.3$GxlowEA_se,UKB.3$GxhighEA_se,
                            GS.3$GxlowEA_se,GS.3$GxhighEA_se,
                            FEMA.3$SE),
                     Test = c(rep("PRS in low EA",nrow(FGR11.3)), rep("PRS in high EA",nrow(FGR11.3)), 
                              rep("PRS in low EA", nrow(UKB.3)), rep("PRS in high EA",nrow(UKB.3)), 
                              rep("PRS in low EA",nrow(GS.3)), rep("PRS in high EA",nrow(GS.3)),
                              rep("PRS in low EA",0.5*nrow(FEMA.3)), rep("PRS in high EA",0.5*nrow(FEMA.3))),
                     Biobanks = c(rep(FGR11.3$Biobank,2),rep(UKB.3$Biobank,2),
                                  rep(GS.3$Biobank,2),FEMA.3$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model3$trait <- factor(model3$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                "K11_APPENDACUT","AUD_SWEDISH"),
                       labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                                  "* Atrial Fibrillation","* Colorectal Cancer","Asthma",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                  "Major Depression","Any Cancer","Epilepsy",
                                  "Appendicitis","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model3$trait <- fct_rev(model3$trait)
# EA levels as factor to plot them in order of magnitude - adjust labels to
# simple for further data wrangling down the line
model3$Test <- factor(model3$Test, levels = c("PRS in low EA","PRS in high EA"), 
                      labels = c("LowEA","HighEA"))
# Biobank as factor
model3$Biobanks <- factor(model3$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))


# remove UKB and GS estimates
plot3.dat <- model3[-which(model3$Biobanks=="UK Biobank" | model3$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot3.dat <- plot3.dat[-which(plot3.dat$trait[which(plot3.dat$Biobanks=="FinnGen")] %in% 
                                c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                  "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                  "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                  "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                  "Knee Osteoarthritis")),]

# calculate difference in the PGS effect between high and low EA group for each
# trait to inform ordering of the traits in the plot (biggest differences with
# higher PRS effects in low edu group on top, and diseases with biggest
# differences + higher PRS effects in the high edu group at the bottom)
# Load necessary libraries
# arrange dataframe by trait and EA group
plot3.df <- plot3.dat %>%
  arrange(trait, Test)

# Ensure each trait has only two rows (one for each group)
plot3_grouped <- plot3.df %>%
  group_by(trait) %>%
  summarise(
    LowEA = HR[Test == "LowEA"],
    HighEA = HR[Test == "HighEA"],
    lowEAb = beta[Test == "LowEA"],
    highEAb = beta[Test == "HighEA"],
    lowEAse = se[Test == "LowEA"],
    highEAse = se [Test == "HighEA"]
  )

# Calculate the difference in effect size for each trait
plot3_diff <- plot3_grouped %>%
  mutate(difference = LowEA - HighEA,
         Betadiff = lowEAb - highEAb,
         SEDiff = sqrt(lowEAse**2 + highEAse**2))

# Determine whether difference between PGS effect (beta) low and high education
# groups is significant. Code adapted from:
# https://github.com/intervene-EU-H2020/flagship/blob/main/MetaAnalysis/AgeSpecific_MetaAnalysisandHetero.R
# calculate z-value
plot3_diff <- plot3_diff %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot3_diff <- plot3_diff %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# Create a column to guide the order of plotting
plot3_diff <- plot3_diff %>%
  arrange(desc(difference)) %>%
  mutate(order = rank(-difference, ties.method = "first"))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot3_diff <- plot3_diff %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
# manually adjust the order 
plot3_diff$order <- c(1:2,6,3:5,7:14,17,15:16,18:19)

# write to file
fwrite(plot3_diff, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model3/", as.character(Sys.Date()), 
                         "_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_Differences.csv",sep=""))

################################################################################
#
# Upload meta-analyzed results to Google Drive
#
################################################################################

drive_upload(media = "output/2classEA/MetaAnalysis/FGR11_UKB_GS/model3/2024-11-04_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_Differences.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2024-11-04_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_Differences.csv",
             type = "spreadsheet")

