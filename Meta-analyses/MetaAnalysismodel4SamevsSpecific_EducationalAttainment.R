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
# Script: Calculate whether the difference in effect of education, the
# disease-specific polygenic scores, and their interaction between the
# unadjusted (same threshold all cohort) and adjusted (cohort-specific
# thresholds) models are significant
#
# required input data: FinnGen data release 11 (FGR11) + UK Biobanke (UKB) +
# Generation Scotland (GS) + meta-analysis (FEMA & REMA) model 4 with same and
# cohort-specific threshold (i.e., only different threshold used in UKB)
#
# LAst script: 17/04/2026 (edits, FAH: last edits before Github upload)
# 
################################################################################

################################################################################
#
# Set up script
#
################################################################################

# set working directory 
setwd("C:/Users/fhk210/OneDrive - Vrije Universiteit Amsterdam/OngoingProjects/SESDiffDiseaseRisk")

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
# wrangling; googledrive + googlesheets4 = read/write to/from GoogleDrive; readxl = read excel
# files (upload to googledrive converts csv to xlsx).
packages("data.table","dplyr","forcats","stringr","tidyr","googledrive",
         "googlesheets4","readxl")

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
UKB.4a <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
UKB.4a$Biobank <- "UK Biobank"
#
UKB.4b <- fread("output/GoogleDrive/UKB/2026-02-16_UKBiobank_EUR_INTERVENE_EducationalAttainment_CohortSpecificThreshold_CoxPH_model4_Coeffs.txt",data.table=FALSE)
UKB.4b$Biobank <- "UK Biobank"
#
GS.4 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
GS.4$Biobank <- "Generation Scotland"

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

# read in models meta-analysis - fixed effects
FEMA.4a <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4.xlsx"))
FEMA.4a$Biobank <- "FE meta-analysis"
#
FEMA.4b <- fread("output/GoogleDrive/MetaAnalysis/2026-03-11_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4_CohortSpecificThreshold.csv",data.table=FALSE)
FEMA.4b$Biobank <- "FE meta-analysis"

# read in models meta-analysis - fixed effects
REMA.4a <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",data.table=FALSE)
REMA.4a$Biobank <- "FE meta-analysis"
#
REMA.4b <- fread("output/GoogleDrive/MetaAnalysis/2026-03-11_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4_CohortSpecificThreshold.csv",data.table=FALSE)
REMA.4b$Biobank <- "FE meta-analysis"


################################################################################
#
# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits: "Atrial Fibrillation",
# "Colorectal Cancer",
#
################################################################################

# fixed effect
FEMA.4a <- FEMA.4a[-which(FEMA.4a$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.4b <- FEMA.4b[-which(FEMA.4b$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]

# random effect
REMA.4a <- REMA.4a[-which(REMA.4a$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
REMA.4b <- REMA.4b[-which(REMA.4b$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results for plotting - fixed effect
#
################################################################################

# model 4 - original 
model4a <- data.frame(trait = c(rep(FGR11.4$trait,3),rep(UKB.4a$trait,3),
                                rep(GS.4$trait,3),FEMA.4a$Phenotype),
                      HR = c(FGR11.4$EAhigh_HR,FGR11.4$PRS_HR,FGR11.4$`PRS:EAhigh_HR`,
                             UKB.4a$EAhigh_HR,UKB.4a$PRS_HR,UKB.4a$`PRS:EAhigh_HR`,
                             GS.4$EAhigh_HR,GS.4$PRS_HR,GS.4$`PRS:EAhigh_HR`,
                             FEMA.4a$HR),
                      lb = c(FGR11.4$EAhigh_HR_lower95,FGR11.4$PRS_HR_lower95,FGR11.4$`PRS:EAhigh_HR_lower95`,
                             UKB.4a$EAhigh_HR_lower95,UKB.4a$PRS_HR_lower95,UKB.4a$`PRS:EAhigh_HR_lower95`,
                             GS.4$EAhigh_HR_lower95,GS.4$PRS_HR_lower95,GS.4$`PRS:EAhigh_HR_lower95`,
                             FEMA.4a$Cineg),
                      ub = c(FGR11.4$EAhigh_HR_upper95,FGR11.4$PRS_HR_upper95,FGR11.4$`PRS:EAhigh_HR_upper95`,
                             UKB.4a$EAhigh_HR_upper95,UKB.4a$PRS_HR_upper95,UKB.4a$`PRS:EAhigh_HR_upper95`,
                             GS.4$EAhigh_HR_upper95,GS.4$PRS_HR_upper95,GS.4$`PRS:EAhigh_HR_upper95`,
                             FEMA.4a$Cipos),
                      beta = c(FGR11.4$EAhigh_beta,FGR11.4$PRS_beta,FGR11.4$`PRS:EAhigh_beta`,
                               UKB.4a$EAhigh_beta,UKB.4a$PRS_beta,UKB.4a$`PRS:EAhigh_beta`,
                               GS.4$EAhigh_beta,GS.4$PRS_beta,GS.4$`PRS:EAhigh_beta`,
                               FEMA.4a$Beta),
                      se = c(FGR11.4$EAhigh_se,FGR11.4$PRS_se,FGR11.4$`PRS:EAhigh_se`,
                             UKB.4a$EAhigh_se,UKB.4a$PRS_se,UKB.4a$`PRS:EAhigh_se`,
                             GS.4$EAhigh_se,GS.4$PRS_se,GS.4$`PRS:EAhigh_se`,
                             FEMA.4a$SE),
                      Test = c(rep("EA",nrow(FGR11.4)), rep("PRS",nrow(FGR11.4)), rep("EAxPRS",nrow(FGR11.4)),
                               rep("EA", nrow(UKB.4a)), rep("PRS",nrow(UKB.4a)), rep("EAxPRS",nrow(UKB.4a)),
                               rep("EA", nrow(GS.4)), rep("PRS",nrow(GS.4)), rep("EAxPRS",nrow(GS.4)),
                               FEMA.4a$Test),
                      Biobanks = c(rep(FGR11.4$Biobank,3),rep(UKB.4a$Biobank,3),
                                   rep(GS.4$Biobank,3),FEMA.4a$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4a$trait <- factor(model4a$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model4a$trait <- fct_rev(model4a$trait)
# EA levels as factor to plot them in order of magnitude
model4a$Test <- factor(model4a$Test, levels = c("EA","PRS","EAxPRS"), labels = c("High Education","PRS", "Interaction"))
# Biobank as factor
model4a$Biobanks <- factor(model4a$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                           labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

# model 4 adjusted 
model4b <- data.frame(trait = c(rep(FGR11.4$trait,3),rep(UKB.4b$trait,3),
                               rep(GS.4$trait,3),FEMA.4b$Phenotype),
                     HR = c(FGR11.4$EAhigh_HR,FGR11.4$PRS_HR,FGR11.4$`PRS:EAhigh_HR`,
                            UKB.4b$EA2high_HR,UKB.4b$PRS_HR,UKB.4b$`PRS:EA2high_HR`,
                            GS.4$EAhigh_HR,GS.4$PRS_HR,GS.4$`PRS:EAhigh_HR`,
                            FEMA.4b$HR),
                     lb = c(FGR11.4$EAhigh_HR_lower95,FGR11.4$PRS_HR_lower95,FGR11.4$`PRS:EAhigh_HR_lower95`,
                            UKB.4b$EA2high_HR_lower95,UKB.4b$PRS_HR_lower95,UKB.4b$`PRS:EA2high_HR_lower95`,
                            GS.4$EAhigh_HR_lower95,GS.4$PRS_HR_lower95,GS.4$`PRS:EAhigh_HR_lower95`,
                            FEMA.4b$Cineg),
                     ub = c(FGR11.4$EAhigh_HR_upper95,FGR11.4$PRS_HR_upper95,FGR11.4$`PRS:EAhigh_HR_upper95`,
                            UKB.4b$EA2high_HR_upper95,UKB.4b$PRS_HR_upper95,UKB.4b$`PRS:EA2high_HR_upper95`,
                            GS.4$EAhigh_HR_upper95,GS.4$PRS_HR_upper95,GS.4$`PRS:EAhigh_HR_upper95`,
                            FEMA.4b$Cipos),
                     beta = c(FGR11.4$EAhigh_beta,FGR11.4$PRS_beta,FGR11.4$`PRS:EAhigh_beta`,
                              UKB.4b$EA2high_beta,UKB.4b$PRS_beta,UKB.4b$`PRS:EA2high_beta`,
                              GS.4$EAhigh_beta,GS.4$PRS_beta,GS.4$`PRS:EAhigh_beta`,
                              FEMA.4b$Beta),
                     se = c(FGR11.4$EAhigh_se,FGR11.4$PRS_se,FGR11.4$`PRS:EAhigh_se`,
                            UKB.4b$EA2high_se,UKB.4b$PRS_se,UKB.4b$`PRS:EA2high_se`,
                            GS.4$EAhigh_se,GS.4$PRS_se,GS.4$`PRS:EAhigh_se`,
                            FEMA.4b$SE),
                     Test = c(rep("EA",nrow(FGR11.4)), rep("PRS",nrow(FGR11.4)), rep("EAxPRS",nrow(FGR11.4)),
                              rep("EA", nrow(UKB.4b)), rep("PRS",nrow(UKB.4b)), rep("EAxPRS",nrow(UKB.4b)),
                              rep("EA", nrow(GS.4)), rep("PRS",nrow(GS.4)), rep("EAxPRS",nrow(GS.4)),
                              FEMA.4b$Test),
                     Biobanks = c(rep(FGR11.4$Biobank,3),rep(UKB.4b$Biobank,3),
                                  rep(GS.4$Biobank,3),FEMA.4b$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4b$trait <- factor(model4b$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model4b$trait <- fct_rev(model4b$trait)
# EA levels as factor to plot them in order of magnitude
model4b$Test <- factor(model4b$Test, levels = c("EA","PRS","EAxPRS"), labels = c("High Education","PRS", "Interaction"))
# Biobank as factor
model4b$Biobanks <- factor(model4b$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))


## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB estimates
plot4a.dat <- model4a[-which(model4a$Biobanks=="UK Biobank" | model4a$Biobanks=="Generation Scotland"),]
plot4b.dat <- model4b[-which(model4b$Biobanks=="UK Biobank" | model4b$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot4a.dat <- plot4a.dat[-which(plot4a.dat$trait[which(plot4a.dat$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]
plot4b.dat <- plot4b.dat[-which(plot4b.dat$trait[which(plot4b.dat$Biobanks=="FinnGen")] %in% 
                                c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                  "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                  "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                  "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                  "Knee Osteoarthritis")),]

# combine data frames models
df4ab <- rbind(plot4a.dat,plot4b.dat) #row bind
df4ab$Model <- c(rep(c("Original", "Adjusted"),each=nrow(plot4a.dat)))

# split by Test for calculation
df4ab.EA <- df4ab[which(df4ab$Test=="High Education"),]
df4ab.PRS <- df4ab[which(df4ab$Test=="PRS"),]
df4ab.INT <- df4ab[which(df4ab$Test=="Interaction"),]

# calculate difference in the education and PGS effect between models for each
# trait to inform ordering of the traits in the plot arrange dataframe by trait
# and Test group
plot4ab.df.EA <- df4ab.EA %>%
  arrange(trait, Model) 
plot4ab.df.PRS <- df4ab.PRS %>%
  arrange(trait, Model) 
plot4ab.df.INT <- df4ab.INT %>%
  arrange(trait, Model) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot4ab_grouped_EA <- plot4ab.df.EA %>%
  group_by(trait) %>%
  reframe(
    Original_HR = HR[Model == "Original"],
    Adjusted_HR = HR[Model == "Adjusted"],
    Originalb = beta[Model == "Original"],
    Adjustedb = beta[Model == "Adjusted"],
    Originalse = se[Model == "Original"],
    Adjustedse = se[Model == "Adjusted"],
    difference = Adjusted_HR - Original_HR,
    Betadiff = Adjustedb - Originalb,
    SEDiff = sqrt(Adjustedse**2 + Adjustedse**2)
  )
plot4ab_grouped_PRS <- plot4ab.df.PRS %>%
  group_by(trait) %>%
  reframe(
    Original_HR = HR[Model == "Original"],
    Adjusted_HR = HR[Model == "Adjusted"],
    Originalb = beta[Model == "Original"],
    Adjustedb = beta[Model == "Adjusted"],
    Originalse = se[Model == "Original"],
    Adjustedse = se[Model == "Adjusted"],
    difference = Adjusted_HR - Original_HR,
    Betadiff = Adjustedb - Originalb,
    SEDiff = sqrt(Adjustedse**2 + Adjustedse**2)
  )
plot4ab_grouped_INT <- plot4ab.df.INT %>%
  group_by(trait) %>%
  reframe(
    Original_HR = HR[Model == "Original"],
    Adjusted_HR = HR[Model == "Adjusted"],
    Originalb = beta[Model == "Original"],
    Adjustedb = beta[Model == "Adjusted"],
    Originalse = se[Model == "Original"],
    Adjustedse = se[Model == "Adjusted"],
    difference = Adjusted_HR - Original_HR,
    Betadiff = Adjustedb - Originalb,
    SEDiff = sqrt(Adjustedse**2 + Adjustedse**2)
  )

# Determine whether difference between groups is significant. Code adapted from:
# https://github.com/intervene-EU-H2020/flagship/blob/main/MetaAnalysis/AgeSpecific_MetaAnalysisandHetero.R
# calculate z-value
plot4ab_diff_EA <- plot4ab_grouped_EA %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot4ab_diff_PRS <- plot4ab_grouped_PRS %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot4ab_diff_INT <- plot4ab_grouped_INT %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot4ab_diff_EA <- plot4ab_diff_EA %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot4ab_diff_PRS <- plot4ab_diff_PRS %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot4ab_diff_INT <- plot4ab_diff_INT %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot4ab_diff_EA <- plot4ab_diff_EA %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot4ab_diff_PRS <- plot4ab_diff_PRS %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot4ab_diff_INT <- plot4ab_diff_INT %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))

# combine results
plot4ab_diff_EA$Test <- "High Education"
plot4ab_diff_PRS$Test <- "PRS"
plot4ab_diff_INT$Test <- "Interaction"
plot4ab_diff <- rbind(plot4ab_diff_EA,plot4ab_diff_PRS,plot4ab_diff_INT)

# write to file
fwrite(plot4ab_diff, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model4/", as.character(Sys.Date()), 
                         "_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_OrgvsSpecific_Differences.csv",sep=""))


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results for plotting - random effect
#
################################################################################

# model 4 - original 
model4ar <- data.frame(trait = c(rep(FGR11.4$trait,3),rep(UKB.4a$trait,3),
                                rep(GS.4$trait,3),REMA.4a$Phenotype),
                      HR = c(FGR11.4$EAhigh_HR,FGR11.4$PRS_HR,FGR11.4$`PRS:EAhigh_HR`,
                             UKB.4a$EAhigh_HR,UKB.4a$PRS_HR,UKB.4a$`PRS:EAhigh_HR`,
                             GS.4$EAhigh_HR,GS.4$PRS_HR,GS.4$`PRS:EAhigh_HR`,
                             REMA.4a$HR),
                      lb = c(FGR11.4$EAhigh_HR_lower95,FGR11.4$PRS_HR_lower95,FGR11.4$`PRS:EAhigh_HR_lower95`,
                             UKB.4a$EAhigh_HR_lower95,UKB.4a$PRS_HR_lower95,UKB.4a$`PRS:EAhigh_HR_lower95`,
                             GS.4$EAhigh_HR_lower95,GS.4$PRS_HR_lower95,GS.4$`PRS:EAhigh_HR_lower95`,
                             REMA.4a$Cineg),
                      ub = c(FGR11.4$EAhigh_HR_upper95,FGR11.4$PRS_HR_upper95,FGR11.4$`PRS:EAhigh_HR_upper95`,
                             UKB.4a$EAhigh_HR_upper95,UKB.4a$PRS_HR_upper95,UKB.4a$`PRS:EAhigh_HR_upper95`,
                             GS.4$EAhigh_HR_upper95,GS.4$PRS_HR_upper95,GS.4$`PRS:EAhigh_HR_upper95`,
                             REMA.4a$Cipos),
                      beta = c(FGR11.4$EAhigh_beta,FGR11.4$PRS_beta,FGR11.4$`PRS:EAhigh_beta`,
                               UKB.4a$EAhigh_beta,UKB.4a$PRS_beta,UKB.4a$`PRS:EAhigh_beta`,
                               GS.4$EAhigh_beta,GS.4$PRS_beta,GS.4$`PRS:EAhigh_beta`,
                               REMA.4a$Beta),
                      se = c(FGR11.4$EAhigh_se,FGR11.4$PRS_se,FGR11.4$`PRS:EAhigh_se`,
                             UKB.4a$EAhigh_se,UKB.4a$PRS_se,UKB.4a$`PRS:EAhigh_se`,
                             GS.4$EAhigh_se,GS.4$PRS_se,GS.4$`PRS:EAhigh_se`,
                             REMA.4a$SE),
                      Test = c(rep("EA",nrow(FGR11.4)), rep("PRS",nrow(FGR11.4)), rep("EAxPRS",nrow(FGR11.4)),
                               rep("EA", nrow(UKB.4a)), rep("PRS",nrow(UKB.4a)), rep("EAxPRS",nrow(UKB.4a)),
                               rep("EA", nrow(GS.4)), rep("PRS",nrow(GS.4)), rep("EAxPRS",nrow(GS.4)),
                               REMA.4a$Test),
                      Biobanks = c(rep(FGR11.4$Biobank,3),rep(UKB.4a$Biobank,3),
                                   rep(GS.4$Biobank,3),REMA.4a$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4ar$trait <- factor(model4ar$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model4ar$trait <- fct_rev(model4ar$trait)
# EA levels as factor to plot them in order of magnitude
model4ar$Test <- factor(model4ar$Test, levels = c("EA","PRS","EAxPRS"), labels = c("High Education","PRS", "Interaction"))
# Biobank as factor
model4ar$Biobanks <- factor(model4ar$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                           labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

# model 4 adjusted 
model4br <- data.frame(trait = c(rep(FGR11.4$trait,3),rep(UKB.4b$trait,3),
                                rep(GS.4$trait,3),REMA.4b$Phenotype),
                      HR = c(FGR11.4$EAhigh_HR,FGR11.4$PRS_HR,FGR11.4$`PRS:EAhigh_HR`,
                             UKB.4b$EA2high_HR,UKB.4b$PRS_HR,UKB.4b$`PRS:EA2high_HR`,
                             GS.4$EAhigh_HR,GS.4$PRS_HR,GS.4$`PRS:EAhigh_HR`,
                             REMA.4b$HR),
                      lb = c(FGR11.4$EAhigh_HR_lower95,FGR11.4$PRS_HR_lower95,FGR11.4$`PRS:EAhigh_HR_lower95`,
                             UKB.4b$EA2high_HR_lower95,UKB.4b$PRS_HR_lower95,UKB.4b$`PRS:EA2high_HR_lower95`,
                             GS.4$EAhigh_HR_lower95,GS.4$PRS_HR_lower95,GS.4$`PRS:EAhigh_HR_lower95`,
                             REMA.4b$Cineg),
                      ub = c(FGR11.4$EAhigh_HR_upper95,FGR11.4$PRS_HR_upper95,FGR11.4$`PRS:EAhigh_HR_upper95`,
                             UKB.4b$EA2high_HR_upper95,UKB.4b$PRS_HR_upper95,UKB.4b$`PRS:EA2high_HR_upper95`,
                             GS.4$EAhigh_HR_upper95,GS.4$PRS_HR_upper95,GS.4$`PRS:EAhigh_HR_upper95`,
                             REMA.4b$Cipos),
                      beta = c(FGR11.4$EAhigh_beta,FGR11.4$PRS_beta,FGR11.4$`PRS:EAhigh_beta`,
                             UKB.4b$EA2high_beta,UKB.4b$PRS_beta,UKB.4b$`PRS:EA2high_beta`,
                               GS.4$EAhigh_beta,GS.4$PRS_beta,GS.4$`PRS:EAhigh_beta`,
                               REMA.4b$Beta),
                      se = c(FGR11.4$EAhigh_se,FGR11.4$PRS_se,FGR11.4$`PRS:EAhigh_se`,
                             UKB.4b$EA2high_se,UKB.4b$PRS_se,UKB.4b$`PRS:EA2high_se`,
                             GS.4$EAhigh_se,GS.4$PRS_se,GS.4$`PRS:EAhigh_se`,
                             REMA.4b$SE),
                      Test = c(rep("EA",nrow(FGR11.4)), rep("PRS",nrow(FGR11.4)), rep("EAxPRS",nrow(FGR11.4)),
                               rep("EA", nrow(UKB.4b)), rep("PRS",nrow(UKB.4b)), rep("EAxPRS",nrow(UKB.4b)),
                               rep("EA", nrow(GS.4)), rep("PRS",nrow(GS.4)), rep("EAxPRS",nrow(GS.4)),
                               REMA.4b$Test),
                      Biobanks = c(rep(FGR11.4$Biobank,3),rep(UKB.4b$Biobank,3),
                                   rep(GS.4$Biobank,3),REMA.4b$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4br$trait <- factor(model4br$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model4br$trait <- fct_rev(model4br$trait)
# EA levels as factor to plot them in order of magnitude
model4br$Test <- factor(model4br$Test, levels = c("EA","PRS","EAxPRS"), labels = c("High Education","PRS", "Interaction"))
# Biobank as factor
model4br$Biobanks <- factor(model4br$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                           labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))


## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB estimates
plot4ar.dat <- model4ar[-which(model4ar$Biobanks=="UK Biobank" | model4ar$Biobanks=="Generation Scotland"),]
plot4br.dat <- model4br[-which(model4br$Biobanks=="UK Biobank" | model4br$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot4ar.dat <- plot4ar.dat[-which(plot4ar.dat$trait[which(plot4ar.dat$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]
plot4br.dat <- plot4br.dat[-which(plot4br.dat$trait[which(plot4br.dat$Biobanks=="FinnGen")] %in% 
                                   c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                     "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                     "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                     "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                     "Knee Osteoarthritis")),]

# combine data frames models
df4abr <- rbind(plot4ar.dat,plot4br.dat) #row bind
df4abr$Model <- c(rep(c("Original", "Adjusted"),each=nrow(plot4ar.dat)))

# split by Test for calculation
df4abr.EA <- df4abr[which(df4abr$Test=="High Education"),]
df4abr.PRS <- df4abr[which(df4abr$Test=="PRS"),]
df4abr.INT <- df4abr[which(df4abr$Test=="Interaction"),]

# calculate difference in the education and PGS effect between models for each
# trait to inform ordering of the traits in the plot arrange dataframe by trait
# and Test group
plot4abr.df.EA <- df4abr.EA %>%
  arrange(trait, Model) 
plot4abr.df.PRS <- df4abr.PRS %>%
  arrange(trait, Model) 
plot4abr.df.INT <- df4abr.INT %>%
  arrange(trait, Model) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot4abr_grouped_EA <- plot4abr.df.EA %>%
  group_by(trait) %>%
  reframe(
    Original_HR = HR[Model == "Original"],
    Adjusted_HR = HR[Model == "Adjusted"],
    Originalb = beta[Model == "Original"],
    Adjustedb = beta[Model == "Adjusted"],
    Originalse = se[Model == "Original"],
    Adjustedse = se[Model == "Adjusted"],
    difference = Adjusted_HR - Original_HR,
    Betadiff = Adjustedb - Originalb,
    SEDiff = sqrt(Adjustedse**2 + Adjustedse**2)
  )
plot4abr_grouped_PRS <- plot4abr.df.PRS %>%
  group_by(trait) %>%
  reframe(
    Original_HR = HR[Model == "Original"],
    Adjusted_HR = HR[Model == "Adjusted"],
    Originalb = beta[Model == "Original"],
    Adjustedb = beta[Model == "Adjusted"],
    Originalse = se[Model == "Original"],
    Adjustedse = se[Model == "Adjusted"],
    difference = Adjusted_HR - Original_HR,
    Betadiff = Adjustedb - Originalb,
    SEDiff = sqrt(Adjustedse**2 + Adjustedse**2)
  )
plot4abr_grouped_INT <- plot4abr.df.INT %>%
  group_by(trait) %>%
  reframe(
    Original_HR = HR[Model == "Original"],
    Adjusted_HR = HR[Model == "Adjusted"],
    Originalb = beta[Model == "Original"],
    Adjustedb = beta[Model == "Adjusted"],
    Originalse = se[Model == "Original"],
    Adjustedse = se[Model == "Adjusted"],
    difference = Adjusted_HR - Original_HR,
    Betadiff = Adjustedb - Originalb,
    SEDiff = sqrt(Adjustedse**2 + Adjustedse**2)
  )

# Determine whether difference between groups is significant. Code adapted from:
# https://github.com/intervene-EU-H2020/flagship/blob/main/MetaAnalysis/AgeSpecific_MetaAnalysisandHetero.R
# calculate z-value
plot4abr_diff_EA <- plot4abr_grouped_EA %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot4abr_diff_PRS <- plot4abr_grouped_PRS %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot4abr_diff_INT <- plot4abr_grouped_INT %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot4abr_diff_EA <- plot4abr_diff_EA %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot4abr_diff_PRS <- plot4abr_diff_PRS %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot4abr_diff_INT <- plot4abr_diff_INT %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot4abr_diff_EA <- plot4abr_diff_EA %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot4abr_diff_PRS <- plot4abr_diff_PRS %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot4abr_diff_INT <- plot4abr_diff_INT %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))

# combine results
plot4abr_diff_EA$Test <- "High Education"
plot4abr_diff_PRS$Test <- "PRS"
plot4abr_diff_INT$Test <- "Interaction"
plot4abr_diff <- rbind(plot4abr_diff_EA,plot4abr_diff_PRS,plot4abr_diff_INT)

# write to file
fwrite(plot4abr_diff, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model4/", as.character(Sys.Date()), 
                           "_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_OrgvsSpecific_Differences.csv",sep=""))


################################################################################
#
# Upload meta-analyzed results to Google Drive
#
################################################################################

# fixed effect
drive_upload(media = "output/2classEA/MetaAnalysis/FGR11_UKB_GS/model4/2026-03-13_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_OriginalvsThresholdSpecific_Differences.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2026-03-13_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_OriginalvsThresholdSpecific_Differences.csv",
             type = "spreadsheet")

# random effect
drive_upload(media = "output/2classEA/MetaAnalysis/FGR11_UKB_GS/model4/2026-03-13_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_OriginalvsThresholdSpecific_Differences.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2026-03-13_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_OriginalvsThresholdSpecific_Differences.csv",
             type = "spreadsheet")
