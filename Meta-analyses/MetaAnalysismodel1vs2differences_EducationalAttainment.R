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
# Script: Calculate whether the difference in effect of education an the
# disease-specific polygenic scores between the unadjusted (model 1) and
# adjusted (model 2) models are significant
#
# required input data: FinnGen data release 11 (FGR11) + UK Biobanke (UKB) +
# Generation Scotland (GS) + meta-analysis (FEMA + REMA) model 1a+b + 2
#
# Start script: 18/02/2026 (edits, FAH: add random-effects meta-analytical models)
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
# wrangling; googledrive + googlesheets4 = read/write to/from GoogleDrive; readxl = read excel
# files (upload to googledrive converts csv to xlsx).
packages("data.table","dplyr","forcats","stringr","tidyr","googledrive",
         "googlesheets4","readxl")

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


# read in model 1a - EA only - per biobank
FGR11.1a <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt", data.table=FALSE)
FGR11.1a$Biobank <- "FinnGen"
#
UKB.1a <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.1a$Biobank <- "UK Biobank"
#
GS.1a <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
GS.1a$Biobank <- "Generation Scotland"

# read in model 1b - PRS only
FGR11.1b <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt", data.table=FALSE)
FGR11.1b$Biobank <- "FinnGen"
#
UKB.1b <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.1b$Biobank <- "UK Biobank"
#
GS.1b <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
GS.1b$Biobank <- "Generation Scotland"

# read in model 2
FGR11.2 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt", data.table=FALSE)
FGR11.2$Biobank <- "FinnGen"
#
UKB.2 <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
UKB.2$Biobank <- "UK Biobank"
#
GS.2 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
GS.2$Biobank <- "Generation Scotland"

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

# read in models 1a,1b, and 2 - meta-analysis - fixed effects
FEMA.1a <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.xlsx"))
FEMA.1a$Biobank <- "FE meta-analysis"
#
FEMA.1b <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.xlsx"))
FEMA.1b$Biobank <- "FE meta-analysis"
#
FEMA.2 <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2.xlsx"))
FEMA.2$Biobank <- "FE meta-analysis"

# read in models 1a,1b, and 2 - meta-analysis - random effects
REMA.1a <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv",data.table=FALSE)
REMA.1a$Biobank <- "RE meta-analysis"
#
REMA.1b <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv",data.table=FALSE)
REMA.1b$Biobank <- "RE meta-analysis"
#
REMA.2 <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model2.csv",data.table=FALSE)
REMA.2$Biobank <- "RE meta-analysis"


################################################################################
#
# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits: "Atrial Fibrillation", "Colorectal Cancer",
#
################################################################################

# fixed effect
FEMA.1a <- FEMA.1a[-which(FEMA.1a$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.1b <- FEMA.1b[-which(FEMA.1b$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.2 <- FEMA.2[-which(FEMA.2$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]

# random effect
REMA.1a <- REMA.1a[-which(REMA.1a$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
REMA.1b <- REMA.1b[-which(REMA.1b$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
REMA.2 <- REMA.2[-which(REMA.2$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results for plotting - fixed effect
#
################################################################################

# model 1 
model1 <- data.frame(trait = c(FGR11.1a$trait,FGR11.1b$trait,UKB.1a$trait,
                               UKB.1b$trait,GS.1a$trait,GS.1b$trait,
                               FEMA.1a$Phenotype,FEMA.1b$Phenotype),
                     HR = c(FGR11.1a$EAhigh_HR,FGR11.1b$PRS_HR,UKB.1a$EAhigh_HR,
                            UKB.1b$PRS_HR,GS.1a$EAhigh_HR,GS.1b$PRS_HR,FEMA.1a$HR,FEMA.1b$HR),
                     lb = c(FGR11.1a$EAhigh_HR_lower95,FGR11.1b$PRS_HR_lower95,
                            UKB.1a$EAhigh_HR_lower95,UKB.1b$PRS_HR_lower95,
                            GS.1a$EAhigh_HR_lower95,GS.1b$PRS_HR_lower95,
                            FEMA.1a$Cineg,FEMA.1b$Cineg),
                     ub = c(FGR11.1a$EAhigh_HR_upper95,FGR11.1b$PRS_HR_upper95,
                            UKB.1a$EAhigh_HR_upper95,UKB.1b$PRS_HR_upper95,
                            GS.1a$EAhigh_HR_upper95,GS.1b$PRS_HR_upper95,
                            FEMA.1a$Cipos,FEMA.1b$Cipos),
                     beta = c(FGR11.1a$EAhigh_beta,FGR11.1b$PRS_beta,UKB.1a$EAhigh_beta,
                              UKB.1b$PRS_beta,GS.1a$EAhigh_beta,GS.1b$PRS_beta,FEMA.1a$Beta,FEMA.1b$Beta),
                     se = c(FGR11.1a$EAhigh_se,FGR11.1b$PRS_se,UKB.1a$EAhigh_se,
                            UKB.1b$PRS_se,GS.1a$EAhigh_se,GS.1b$PRS_se,FEMA.1a$SE,FEMA.1b$SE),
                     Test = c(rep("high EA",nrow(FGR11.1a)),rep("PRS",nrow(FGR11.1b)), 
                              rep("high EA", nrow(UKB.1a)),rep("PRS",nrow(UKB.1b)), 
                              rep("high EA",nrow(GS.1a)),rep("PRS",nrow(GS.1b)),
                              rep("high EA",nrow(FEMA.1a)), rep("PRS",nrow(FEMA.1b))),
                     Biobanks = c(FGR11.1a$Biobank,FGR11.1b$Biobank,UKB.1a$Biobank,
                                  UKB.1b$Biobank,GS.1a$Biobank,GS.1b$Biobank,
                                  FEMA.1a$Biobank,FEMA.1b$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model1$trait <- factor(model1$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model1$trait <- fct_rev(model1$trait)
# EA levels as factor to plot them in order of magnitude
model1$Test <- factor(model1$Test, levels = c("high EA","PRS"), labels = c("High Education","PRS"))
# Biobank as factor
model1$Biobanks <- factor(model1$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

# model 2
model2 <- data.frame(trait = c(rep(FGR11.2$trait,2),rep(UKB.2$trait,2),
                               rep(GS.2$trait,2),FEMA.2$Phenotype),
                     HR = c(FGR11.2$EAhigh_HR,FGR11.2$PRS_HR,UKB.2$EAhigh_HR,
                            UKB.2$PRS_HR,GS.2$EAhigh_HR,GS.2$PRS_HR,FEMA.2$HR),
                     lb = c(FGR11.2$EAhigh_HR_lower95,FGR11.2$PRS_HR_lower95,
                            UKB.2$EAhigh_HR_lower95,UKB.2$PRS_HR_lower95,
                            GS.2$EAhigh_HR_lower95,GS.2$PRS_HR_lower95,FEMA.2$Cineg),
                     ub = c(FGR11.2$EAhigh_HR_upper95,FGR11.2$PRS_HR_upper95,
                            UKB.2$EAhigh_HR_upper95,UKB.2$PRS_HR_upper95,
                            GS.2$EAhigh_HR_upper95,GS.2$PRS_HR_upper95,FEMA.2$Cipos),
                     beta = c(FGR11.2$EAhigh_beta,FGR11.2$PRS_beta,UKB.2$EAhigh_beta,
                              UKB.2$PRS_beta,GS.2$EAhigh_beta,GS.2$PRS_beta,FEMA.2$Beta),
                     se = c(FGR11.2$EAhigh_se,FGR11.2$PRS_se,UKB.2$EAhigh_se,
                            UKB.2$PRS_se,GS.2$EAhigh_se,GS.2$PRS_se,FEMA.2$SE),
                     Test = c(rep("high EA",nrow(FGR11.2)), rep("PRS",nrow(FGR11.2)), 
                              rep("high EA", nrow(UKB.2)), rep("PRS",nrow(UKB.2)), 
                              rep("high EA", nrow(GS.2)), rep("PRS",nrow(GS.2)),
                              rep("high EA",0.5*nrow(FEMA.2)), rep("PRS",0.5*nrow(FEMA.2))),
                     Biobanks = c(rep(FGR11.2$Biobank,2),rep(UKB.2$Biobank,2),
                                  rep(GS.2$Biobank,2),FEMA.2$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model2$trait <- factor(model2$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model2$trait <- fct_rev(model2$trait)
# EA levels as factor to plot them in order of magnitude
model2$Test <- factor(model2$Test, levels = c("high EA","PRS"), labels = c("High Education","PRS"))
# Biobank as factor
model2$Biobanks <- factor(model2$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))


## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB estimates
plot1a.dat <- model1[-which(model1$Biobanks=="UK Biobank" | model1$Biobanks=="Generation Scotland"),]
plot2.dat <- model2[-which(model2$Biobanks=="UK Biobank" | model2$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot1a.dat <- plot1a.dat[-which(plot1a.dat$trait[which(plot1a.dat$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]
plot2.dat <- plot2.dat[-which(plot2.dat$trait[which(plot2.dat$Biobanks=="FinnGen")] %in% 
                                c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                  "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                  "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                  "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                  "Knee Osteoarthritis")),]

# combine data frames model 1 and 2 for plotting 
df12 <- rbind(plot1a.dat,plot2.dat) #row bind
df12$Model <- c(rep(c("model1", "model2"),each=nrow(plot1a.dat)))

# split by Test for calculation
df12.EA <- df12[which(df12$Test=="High Education"),]
df12.PRS <- df12[which(df12$Test=="PRS"),]

# calculate difference in the education and PGS effect between model 1 and 2 for each
# trait to inform ordering of the traits in the plot
# arrange dataframe by trait and Test group (education vs PGS)
plot12.df.EA <- df12.EA %>%
  arrange(trait, Model) 
plot12.df.PRS <- df12.PRS %>%
  arrange(trait, Model) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot12_grouped_EA <- plot12.df.EA %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Model == "model1"],
    Model2_HR = HR[Model == "model2"],
    Model1b = beta[Model == "model1"],
    Model2b = beta[Model == "model2"],
    Model1se = se[Model == "model1"],
    Model2se = se[Model == "model2"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )
plot12_grouped_PRS <- plot12.df.PRS %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Model == "model1"],
    Model2_HR = HR[Model == "model2"],
    Model1b = beta[Model == "model1"],
    Model2b = beta[Model == "model2"],
    Model1se = se[Model == "model1"],
    Model2se = se[Model == "model2"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )


# Determine whether difference between PGS effect (beta) low and high education
# groups is significant. Code adapted from:
# https://github.com/intervene-EU-H2020/flagship/blob/main/MetaAnalysis/AgeSpecific_MetaAnalysisandHetero.R
# calculate z-value
plot12_diff_EA <- plot12_grouped_EA %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot12_diff_PRS <- plot12_grouped_PRS %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot12_diff_EA <- plot12_diff_EA %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot12_diff_PRS <- plot12_diff_PRS %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))


# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot12_diff_EA <- plot12_diff_EA %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot12_diff_PRS <- plot12_diff_PRS %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))

# combine EA and PRS results
plot12_diff_EA$Test <- "High Education"
plot12_diff_PRS$Test <- "PRS"
plot12_diff <- rbind(plot12_diff_EA,plot12_diff_PRS)

# write to file
fwrite(plot12_diff, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model2/", as.character(Sys.Date()), 
                         "_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv",sep=""))


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results for plotting - random effect
#
################################################################################

# model 1 
model1r <- data.frame(trait = c(FGR11.1a$trait,FGR11.1b$trait,UKB.1a$trait,
                               UKB.1b$trait,GS.1a$trait,GS.1b$trait,
                               REMA.1a$Phenotype,REMA.1b$Phenotype),
                     HR = c(FGR11.1a$EAhigh_HR,FGR11.1b$PRS_HR,UKB.1a$EAhigh_HR,
                            UKB.1b$PRS_HR,GS.1a$EAhigh_HR,GS.1b$PRS_HR,REMA.1a$HR,REMA.1b$HR),
                     lb = c(FGR11.1a$EAhigh_HR_lower95,FGR11.1b$PRS_HR_lower95,
                            UKB.1a$EAhigh_HR_lower95,UKB.1b$PRS_HR_lower95,
                            GS.1a$EAhigh_HR_lower95,GS.1b$PRS_HR_lower95,
                            REMA.1a$Cineg,REMA.1b$Cineg),
                     ub = c(FGR11.1a$EAhigh_HR_upper95,FGR11.1b$PRS_HR_upper95,
                            UKB.1a$EAhigh_HR_upper95,UKB.1b$PRS_HR_upper95,
                            GS.1a$EAhigh_HR_upper95,GS.1b$PRS_HR_upper95,
                            REMA.1a$Cipos,REMA.1b$Cipos),
                     beta = c(FGR11.1a$EAhigh_beta,FGR11.1b$PRS_beta,UKB.1a$EAhigh_beta,
                              UKB.1b$PRS_beta,GS.1a$EAhigh_beta,GS.1b$PRS_beta,REMA.1a$Beta,REMA.1b$Beta),
                     se = c(FGR11.1a$EAhigh_se,FGR11.1b$PRS_se,UKB.1a$EAhigh_se,
                            UKB.1b$PRS_se,GS.1a$EAhigh_se,GS.1b$PRS_se,REMA.1a$SE,REMA.1b$SE),
                     Test = c(rep("high EA",nrow(FGR11.1a)),rep("PRS",nrow(FGR11.1b)), 
                              rep("high EA", nrow(UKB.1a)),rep("PRS",nrow(UKB.1b)), 
                              rep("high EA",nrow(GS.1a)),rep("PRS",nrow(GS.1b)),
                              rep("high EA",nrow(REMA.1a)), rep("PRS",nrow(REMA.1b))),
                     Biobanks = c(FGR11.1a$Biobank,FGR11.1b$Biobank,UKB.1a$Biobank,
                                  UKB.1b$Biobank,GS.1a$Biobank,GS.1b$Biobank,
                                  REMA.1a$Biobank,REMA.1b$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model1r$trait <- factor(model1r$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model1r$trait <- fct_rev(model1r$trait)
# EA levels as factor to plot them in order of magnitude
model1r$Test <- factor(model1r$Test, levels = c("high EA","PRS"), labels = c("High Education","PRS"))
# Biobank as factor
model1r$Biobanks <- factor(model1r$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","RE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland","RE meta-analysis"))

# model 2
modelr2 <- data.frame(trait = c(rep(FGR11.2$trait,2),rep(UKB.2$trait,2),
                               rep(GS.2$trait,2),REMA.2$Phenotype),
                     HR = c(FGR11.2$EAhigh_HR,FGR11.2$PRS_HR,UKB.2$EAhigh_HR,
                            UKB.2$PRS_HR,GS.2$EAhigh_HR,GS.2$PRS_HR,REMA.2$HR),
                     lb = c(FGR11.2$EAhigh_HR_lower95,FGR11.2$PRS_HR_lower95,
                            UKB.2$EAhigh_HR_lower95,UKB.2$PRS_HR_lower95,
                            GS.2$EAhigh_HR_lower95,GS.2$PRS_HR_lower95,REMA.2$Cineg),
                     ub = c(FGR11.2$EAhigh_HR_upper95,FGR11.2$PRS_HR_upper95,
                            UKB.2$EAhigh_HR_upper95,UKB.2$PRS_HR_upper95,
                            GS.2$EAhigh_HR_upper95,GS.2$PRS_HR_upper95,REMA.2$Cipos),
                     beta = c(FGR11.2$EAhigh_beta,FGR11.2$PRS_beta,UKB.2$EAhigh_beta,
                              UKB.2$PRS_beta,GS.2$EAhigh_beta,GS.2$PRS_beta,REMA.2$Beta),
                     se = c(FGR11.2$EAhigh_se,FGR11.2$PRS_se,UKB.2$EAhigh_se,
                            UKB.2$PRS_se,GS.2$EAhigh_se,GS.2$PRS_se,REMA.2$SE),
                     Test = c(rep("high EA",nrow(FGR11.2)), rep("PRS",nrow(FGR11.2)), 
                              rep("high EA", nrow(UKB.2)), rep("PRS",nrow(UKB.2)), 
                              rep("high EA", nrow(GS.2)), rep("PRS",nrow(GS.2)),
                              rep("high EA",0.5*nrow(REMA.2)), rep("PRS",0.5*nrow(REMA.2))),
                     Biobanks = c(rep(FGR11.2$Biobank,2),rep(UKB.2$Biobank,2),
                                  rep(GS.2$Biobank,2),REMA.2$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
modelr2$trait <- factor(modelr2$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
modelr2$trait <- fct_rev(modelr2$trait)
# EA levels as factor to plot them in order of magnitude
modelr2$Test <- factor(modelr2$Test, levels = c("high EA","PRS"), labels = c("High Education","PRS"))
# Biobank as factor
modelr2$Biobanks <- factor(modelr2$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","RE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland","RE meta-analysis"))


## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB estimates
plot1a.datr <- model1r[-which(model1r$Biobanks=="UK Biobank" | model1r$Biobanks=="Generation Scotland"),]
plot2.datr <- modelr2[-which(modelr2$Biobanks=="UK Biobank" | modelr2$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot1a.datr <- plot1a.datr[-which(plot1a.datr$trait[which(plot1a.datr$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]
plot2.datr <- plot2.datr[-which(plot2.datr$trait[which(plot2.datr$Biobanks=="FinnGen")] %in% 
                                c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                  "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                  "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                  "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                  "Knee Osteoarthritis")),]

# combine data frames model 1 and 2 for plotting 
df12r <- rbind(plot1a.datr,plot2.datr) #row bind
df12r$Model <- c(rep(c("model1", "model2"),each=nrow(plot1a.datr)))

# split by Test for calculation
df12r.EA <- df12r[which(df12r$Test=="High Education"),]
df12r.PRS <- df12r[which(df12r$Test=="PRS"),]

# calculate difference in the education and PGS effect between model 1 and 2 for each
# trait to inform ordering of the traits in the plot
# arrange dataframe by trait and Test group (education vs PGS)
plot12.dfr.EA <- df12r.EA %>%
  arrange(trait, Model) 
plot12.dfr.PRS <- df12r.PRS %>%
  arrange(trait, Model) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot12r_grouped_EA <- plot12.dfr.EA %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Model == "model1"],
    Model2_HR = HR[Model == "model2"],
    Model1b = beta[Model == "model1"],
    Model2b = beta[Model == "model2"],
    Model1se = se[Model == "model1"],
    Model2se = se[Model == "model2"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )
plot12r_grouped_PRS <- plot12.dfr.PRS %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Model == "model1"],
    Model2_HR = HR[Model == "model2"],
    Model1b = beta[Model == "model1"],
    Model2b = beta[Model == "model2"],
    Model1se = se[Model == "model1"],
    Model2se = se[Model == "model2"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )

# Determine whether difference between PGS effect (beta) low and high education
# groups is significant. Code adapted from:
# https://github.com/intervene-EU-H2020/flagship/blob/main/MetaAnalysis/AgeSpecific_MetaAnalysisandHetero.R
# calculate z-value
plot12r_diff_EA <- plot12r_grouped_EA %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot12r_diff_PRS <- plot12r_grouped_PRS %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot12r_diff_EA <- plot12r_diff_EA %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot12r_diff_PRS <- plot12r_diff_PRS %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot12r_diff_EA <- plot12r_diff_EA %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot12r_diff_PRS <- plot12r_diff_PRS %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))

# combine EA and PRS results
plot12r_diff_EA$Test <- "High Education"
plot12r_diff_PRS$Test <- "PRS"
plot12r_diff <- rbind(plot12r_diff_EA,plot12r_diff_PRS)

# write to file
fwrite(plot12r_diff, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model2/", as.character(Sys.Date()), 
                          "_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv",sep=""))


################################################################################
#
# Upload meta-analyzed results to Google Drive
#
################################################################################

# fixed effect
drive_upload(media = "output/2classEA/MetaAnalysis/FGR11_UKB_GS/model2/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv",
             type = "spreadsheet")

# random effect
drive_upload(media = "output/2classEA/MetaAnalysis/FGR11_UKB_GS/model2/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv",
             path = as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH"),
             name = "2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv",
             type = "spreadsheet")

