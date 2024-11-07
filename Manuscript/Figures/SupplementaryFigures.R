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
# Script: Create supplementary figures (per cohort + meta-analysis)
#
# Figures this script creates
#       1) Figure S1: model 1 (EA or PRS): per cohort + meta-analysis
#       2) Figure S2: model 2 (EA+PRS): per cohort + meta-analysis
#       3) Figure S3: model 3: per cohort + meta-analysis
#       5) Figure S4: model 5: per cohort + meta-analysis
# required input data:
#       1) FinnGen data release 11 (FGR11) + UK Biobank (UKB) + Generation Scotland (GS) + meta-analysis model 1a+b
#       2) FGR11 + UKB + GS + meta-analysis model 2
#       3) significance test difference effect education and PGS model 1vs2
#       4) FGR11 + UKB + GS + meta-analysis model 3
#       5) significance test difference effect PGS high vs low EA (model 3)
#       6) FGR11 + UKB + GS + meta-analysis model 5
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

# set working directory (the working directory is the project folder on my VM on
# the FinnGen Sosioeconomic Data Sandbox)
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
# efficiently reading in large data sets; ggplot2 = versatile visualizations;
# viridis = color-blind friendly colors; dplyr, forcats & stringr = data
# wrangling; cowplot + grid + gridExtra = combining plots; .
packages("data.table","ggplot2","viridis","dplyr","forcats","stringr","cowplot",
         "grid","gridExtra")

# color blind friendly palette
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


################################################################################
#
# Read in full sample hazard ratios per standard deviation
#
################################################################################

# read in model 1a - EA only
FGR11.1a <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt", data.table=FALSE)
FGR11.1a$Biobank <- "FinnGen"
#
UKB.1a <- fread("output/GoogleDrive/UKB/2024-04-04_UKB_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.1a$Biobank <- "UK Biobank"
#
GS.1a <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
GS.1a$Biobank <- "Generation Scotland"
#
FEMA.1a <- fread("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model1/2024-07-08_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a_anyN.csv",data.table=FALSE)
FEMA.1a$Biobank <- "FE meta-analysis"

# read in model 1b - PRS only
FGR11.1b <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt", data.table=FALSE)
FGR11.1b$Biobank <- "FinnGen"
#
UKB.1b <- fread("output/GoogleDrive/UKB/2024-04-04_UKB_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.1b$Biobank <- "UK Biobank"
#
GS.1b <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
GS.1b$Biobank <- "Generation Scotland"
#
FEMA.1b <- fread("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model1/2024-07-08_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b_anyN.csv",data.table=FALSE)
FEMA.1b$Biobank <- "FE meta-analysis"

# read in model 2
FGR11.2 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt", data.table=FALSE)
FGR11.2$Biobank <- "FinnGen"
#
UKB.2 <- fread("output/GoogleDrive/UKB/2024-04-04_UKB_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
UKB.2$Biobank <- "UK Biobank"
#
GS.2 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
GS.2$Biobank <- "Generation Scotland"
#
FEMA.2 <- fread("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model2/2024-07-08_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2_anyN.csv",data.table=FALSE)
FEMA.2$Biobank <- "FE meta-analysis"

# read in model 3
FGR11.3 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt", data.table=FALSE)
FGR11.3$Biobank <- "FinnGen"
#
UKB.3 <- fread("output/GoogleDrive/UKB/2024-04-04_UKB_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
UKB.3$Biobank <- "UK Biobank"
#
GS.3 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
GS.3$Biobank <- "Generation Scotland"
#
FEMA.3 <- fread("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model3/2024-07-08_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_anyN.csv",data.table=FALSE)
FEMA.3$Biobank <- "FE meta-analysis"

# read in model 5 - per PRS group
FGR11.5a <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGen_INTERVENE_EducationalAttainment_CoxPH_model5_Coeffs_PGSGroup1.txt", data.table=FALSE)
FGR11.5a$Biobank <- "FinnGen"
FGR11.5b <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGen_INTERVENE_EducationalAttainment_CoxPH_model5_Coeffs_PGSGroup2.txt", data.table=FALSE)
FGR11.5b$Biobank <- "FinnGen"
FGR11.5c <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGen_INTERVENE_EducationalAttainment_CoxPH_model5_Coeffs_PGSGroup3.txt", data.table=FALSE)
FGR11.5c$Biobank <- "FinnGen"
#
UKB.5a <- fread("output/GoogleDrive/UKB/2024-04-04_UKB_INTERVENE_EducationalAttainment_CoxPH_model5_PGSGroup1_Coeffs.txt",data.table=FALSE)
UKB.5a$Biobank <- "UK Biobank"
UKB.5b <- fread("output/GoogleDrive/UKB/2024-04-04_UKB_INTERVENE_EducationalAttainment_CoxPH_model5_PGSGroup2_Coeffs.txt",data.table=FALSE)
UKB.5b$Biobank <- "UK Biobank"
UKB.5c <- fread("output/GoogleDrive/UKB/2024-04-04_UKB_INTERVENE_EducationalAttainment_CoxPH_model5_PGSGroup3_Coeffs.txt",data.table=FALSE)
UKB.5c$Biobank <- "UK Biobank"
#
GS.5a <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model5_Coeffs_PGSGroup1.txt",data.table=FALSE)
GS.5a$Biobank <- "Generation Scotland"
GS.5b <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model5_Coeffs_PGSGroup2.txt",data.table=FALSE)
GS.5b$Biobank <- "Generation Scotland"
GS.5c <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model5_Coeffs_PGSGroup3.txt",data.table=FALSE)
GS.5c$Biobank <- "Generation Scotland"
#
FEMA.5 <- fread("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model5/2024-07-08_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model5_anyN.csv",data.table=FALSE)
FEMA.5$Biobank <- "FE meta-analysis"


################################################################################
#
# As current version of UKB results also includes the traits
# where UKB was in the discovery GWAS, subset those data frames to only include
# the traits where UKB was not in the discovery GWAS: "Type 1
# Diabetes","Prostate Cancer","Gout","Rheumatoid Arthritis","Breast
# Cancer","Epilepsy","Alcohol Use Disorder"
#
################################################################################

UKB.1a <- UKB.1a[which(UKB.1a$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]
UKB.1b <- UKB.1b[which(UKB.1b$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]
UKB.2 <- UKB.2[which(UKB.2$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]
UKB.3 <- UKB.3[which(UKB.3$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]
UKB.5a <- UKB.5a[which(UKB.5a$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]
UKB.5b <- UKB.5b[which(UKB.5b$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]
UKB.5c <- UKB.5c[which(UKB.5c$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]


################################################################################
#
# As current version of the meta-analysis also includes traits only available in
# FinnGen, subset those data frames to only include the traits where >=2 cohorts
# were analysed.
#
################################################################################

## remove traits not meta-analyzed ##
FEMA.1a <- FEMA.1a[-which(FEMA.1a$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.1b <- FEMA.1b[-which(FEMA.1b$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.2 <- FEMA.2[-which(FEMA.2$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.3 <- FEMA.3[-which(FEMA.3$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.5 <- FEMA.5[-which(FEMA.5$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]


################################################################################
#
# Combine FGR11 & UKB & GenScot & meta-analysis results
#
################################################################################

# create dataset combining results for model 1a+b 
TS5 <- data.frame(Biobank = c(FEMA.1a$Biobank,FEMA.1b$Biobank,
                              FGR11.1a$Biobank,FGR11.1b$Biobank,
                              UKB.1a$Biobank,UKB.1b$Biobank,
                              GS.1a$Biobank,GS.1b$Biobank),
                  Test = c(rep("high educational attainment",nrow(FEMA.1a)), rep("PRS",nrow(FEMA.1b)),
                           rep("high educational attainment",nrow(FGR11.1a)), rep("PRS",nrow(FGR11.1b)), 
                           rep("high educational attainment", nrow(UKB.1a)), rep("PRS",nrow(UKB.1b)), 
                           rep("high educational attainment",nrow(GS.1a)), rep("PRS",nrow(GS.1b))),
                  Phenotype = c(FEMA.1a$Phenotype,FEMA.1b$Phenotype,
                                FGR11.1a$trait,FGR11.1b$trait,
                                UKB.1a$trait,UKB.1b$trait,
                                GS.1a$trait,GS.1b$trait),
                  Beta = c(FEMA.1a$Beta,FEMA.1b$Beta,
                           FGR11.1a$EAhigh_beta,FGR11.1b$PRS_beta,
                           UKB.1a$EAhigh_beta,UKB.1b$PRS_beta,
                           GS.1a$EAhigh_beta,GS.1b$PRS_beta),
                  SE = c(FEMA.1a$SE,FEMA.1b$SE,
                         FGR11.1a$EAhigh_se,FGR11.1b$PRS_se,
                         UKB.1a$EAhigh_se,UKB.1b$PRS_se,
                         GS.1a$EAhigh_se,GS.1b$PRS_se),
                  Pval = c(FEMA.1a$Pval,FEMA.1b$Pval,
                           FGR11.1a$EAhigh_p,FGR11.1b$PRS_p,
                           UKB.1a$EAhigh_p,UKB.1b$PRS_p,
                           GS.1a$EAhigh_p,GS.1b$PRS_p),
                  HR = c(FEMA.1a$HR,FEMA.1b$HR,
                         FGR11.1a$EAhigh_HR,FGR11.1b$PRS_HR,
                         UKB.1a$EAhigh_HR,UKB.1b$PRS_HR,
                         GS.1a$EAhigh_HR,GS.1b$PRS_HR),
                  Cineg = c(FEMA.1a$Cineg,FEMA.1b$Cineg,
                            FGR11.1a$EAhigh_HR_lower95,FGR11.1b$PRS_HR_lower95,
                            UKB.1a$EAhigh_HR_lower95,UKB.1b$PRS_HR_lower95,
                            GS.1a$EAhigh_HR_lower95,GS.1b$PRS_HR_lower95),
                  Cipos = c(FEMA.1a$Cipos,FEMA.1b$Cipos,
                            FGR11.1a$EAhigh_HR_upper95,FGR11.1b$PRS_HR_upper95,
                            UKB.1a$EAhigh_HR_upper95,UKB.1b$PRS_HR_upper95,
                            GS.1a$EAhigh_HR_upper95,GS.1b$PRS_HR_upper95),
                  QHet = c(FEMA.1a$QHet,FEMA.1b$QHet,
                           rep(NA,nrow(FGR11.1a)),rep(NA,nrow(FGR11.1b)),
                           rep(NA,nrow(UKB.1a)),rep(NA,nrow(UKB.1b)),
                           rep(NA,nrow(GS.1a)),rep(NA,nrow(GS.1b))),
                  HetPval = c(FEMA.1a$HetPval,FEMA.1b$HetPval,
                              rep(NA,nrow(FGR11.1a)),rep(NA,nrow(FGR11.1b)),
                              rep(NA,nrow(UKB.1a)),rep(NA,nrow(UKB.1b)),
                              rep(NA,nrow(GS.1a)),rep(NA,nrow(GS.1b))))
# adjust labels
TS5$Phenotype <- factor(TS5$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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

# create dataset combining results for model 2
TS6 <- data.frame(Biobank = c(FEMA.2$Biobank,
                              FGR11.2$Biobank,FGR11.2$Biobank,
                              UKB.2$Biobank,UKB.2$Biobank,
                              GS.2$Biobank,GS.2$Biobank),
                  Test = c(FEMA.2$Test,
                           rep("high educational attainment",nrow(FGR11.2)), rep("PRS",nrow(FGR11.2)), 
                           rep("high educational attainment", nrow(UKB.2)), rep("PRS",nrow(UKB.2)), 
                           rep("high educational attainment",nrow(GS.2)), rep("PRS",nrow(GS.2))),
                  Phenotype = c(FEMA.2$Phenotype,
                                FGR11.2$trait,FGR11.2$trait,
                                UKB.2$trait,UKB.2$trait,
                                GS.2$trait,GS.2$trait),
                  Beta = c(FEMA.2$Beta,
                           FGR11.2$EAhigh_beta,FGR11.2$PRS_beta,
                           UKB.2$EAhigh_beta,UKB.2$PRS_beta,
                           GS.2$EAhigh_beta,GS.2$PRS_beta),
                  SE = c(FEMA.2$SE,
                         FGR11.2$EAhigh_se,FGR11.2$PRS_se,
                         UKB.2$EAhigh_se,UKB.2$PRS_se,
                         GS.2$EAhigh_se,GS.2$PRS_se),
                  Pval = c(FEMA.2$Pval,
                           FGR11.2$EAhigh_p,FGR11.2$PRS_p,
                           UKB.2$EAhigh_p,UKB.2$PRS_p,
                           GS.2$EAhigh_p,GS.2$PRS_p),
                  HR = c(FEMA.2$HR,
                         FGR11.2$EAhigh_HR,FGR11.2$PRS_HR,
                         UKB.2$EAhigh_HR,UKB.2$PRS_HR,
                         GS.2$EAhigh_HR,GS.2$PRS_HR),
                  Cineg = c(FEMA.2$Cineg,
                            FGR11.2$EAhigh_HR_lower95,FGR11.2$PRS_HR_lower95,
                            UKB.2$EAhigh_HR_lower95,UKB.2$PRS_HR_lower95,
                            GS.2$EAhigh_HR_lower95,GS.2$PRS_HR_lower95),
                  Cipos = c(FEMA.2$Cipos,
                            FGR11.2$EAhigh_HR_upper95,FGR11.2$PRS_HR_upper95,
                            UKB.2$EAhigh_HR_upper95,UKB.2$PRS_HR_upper95,
                            GS.2$EAhigh_HR_upper95,GS.2$PRS_HR_upper95),
                  QHet = c(FEMA.2$QHet,
                           rep(NA,2*nrow(FGR11.2)),
                           rep(NA,2*nrow(UKB.2)),
                           rep(NA,2*nrow(GS.2))),
                  HetPval = c(FEMA.2$HetPval,
                              rep(NA,2*nrow(FGR11.2)),
                              rep(NA,2*nrow(UKB.2)),
                              rep(NA,2*nrow(GS.2))))
# adjust labels
TS6$Test[which(TS6$Test=="EA")] <- "high educational attainment"
#
TS6$Phenotype <- factor(TS6$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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

# create dataset combining results for model 3 
TS7 <- data.frame(Biobank = c(FEMA.3$Biobank,
                              rep(FGR11.3$Biobank,2),
                              rep(UKB.3$Biobank,2),
                              rep(GS.3$Biobank,2)),
                  EducationGroup = c(FEMA.3$EA,
                           rep("low educational attainment",nrow(FGR11.3)), rep("high educational attainment",nrow(FGR11.3)), 
                           rep("low educational attainment", nrow(UKB.3)), rep("high educational attainment",nrow(UKB.3)), 
                           rep("low educational attainment",nrow(GS.3)), rep("high educational attainment",nrow(GS.3))),
                  Phenotype = c(FEMA.3$Phenotype,
                                rep(FGR11.3$trait,2),
                                rep(UKB.3$trait,2),
                                rep(GS.3$trait,2)),
                  Beta = c(FEMA.3$Beta,
                           FGR11.3$GxlowEA_beta,FGR11.3$GxhighEA_beta,
                           UKB.3$GxlowEA_beta,UKB.3$GxhighEA_beta,
                           GS.3$GxlowEA_beta,GS.3$GxhighEA_beta),
                  SE = c(FEMA.3$SE,
                         FGR11.3$GxlowEA_se,FGR11.3$GxhighEA_se,
                         UKB.3$GxlowEA_se,UKB.3$GxhighEA_se,
                         GS.3$GxlowEA_se,GS.3$GxhighEA_se),
                  Pval = c(FEMA.3$Pval,
                           FGR11.3$GxlowEA_p,FGR11.3$GxhighEA_p,
                           UKB.3$GxlowEA_p,UKB.3$GxhighEA_p,
                           GS.3$GxlowEA_p,GS.3$GxhighEA_p),
                  HR = c(FEMA.3$HR,
                         FGR11.3$GxlowEA_HR,FGR11.3$GxhighEA_HR,
                         UKB.3$GxlowEA_HR,UKB.3$GxhighEA_HR,
                         GS.3$GxlowEA_HR,GS.3$GxhighEA_HR),
                  Cineg = c(FEMA.3$Cineg,
                            FGR11.3$GxlowEA_HR_lower95,FGR11.3$GxhighEA_HR_lower95,
                            UKB.3$GxlowEA_HR_lower95,UKB.3$GxhighEA_HR_lower95,
                            GS.3$GxlowEA_HR_lower95,GS.3$GxhighEA_HR_lower95),
                  Cipos = c(FEMA.3$Cipos,
                            FGR11.3$GxlowEA_HR_upper95,FGR11.3$GxhighEA_HR_upper95,
                            UKB.3$GxlowEA_HR_upper95,UKB.3$GxhighEA_HR_upper95,
                            GS.3$GxlowEA_HR_upper95,GS.3$GxhighEA_HR_upper95),
                  QHet = c(FEMA.3$QHet,
                           rep(NA,2*nrow(FGR11.3)),
                           rep(NA,2*nrow(UKB.3)),
                           rep(NA,2*nrow(GS.3))),
                  HetPval = c(FEMA.3$HetPval,
                              rep(NA,2*nrow(FGR11.3)),
                              rep(NA,2*nrow(UKB.3)),
                              rep(NA,2*nrow(GS.3))))
# adjust labels
TS7$EducationGroup[which(TS7$EducationGroup=="lowEA")] <- "low educational attainment"
TS7$EducationGroup[which(TS7$EducationGroup=="highEA")] <- "high educational attainment"
#
TS7$Phenotype <- factor(TS7$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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

# create dataset combining results for model 5
TS8 <- data.frame(Biobank = c(FEMA.5$Biobank,
                              FGR11.5a$Biobank,FGR11.5b$Biobank,FGR11.5c$Biobank,
                              UKB.5a$Biobank,UKB.5b$Biobank,UKB.5c$Biobank,
                              GS.5a$Biobank,GS.5b$Biobank,GS.5c$Biobank),
                  PRSgroup = c(FEMA.5$PRSgroup,
                           rep("low PRS",nrow(FGR11.5a)), rep("medium PRS",nrow(FGR11.5b)), rep("high PRS",nrow(FGR11.5c)), 
                           rep("low PRS", nrow(UKB.5a)), rep("medium PRS",nrow(UKB.5b)), rep("high PRS",nrow(UKB.5c)),  
                           rep("low PRS",nrow(GS.5a)), rep("medium PRS",nrow(GS.5b)), rep("high PRS",nrow(GS.5c))),
                  Phenotype = c(FEMA.5$Phenotype,
                                FGR11.5a$trait,FGR11.5b$trait,FGR11.5c$trait,
                                UKB.5a$trait,UKB.5b$trait,UKB.5c$trait,
                                GS.5a$trait,GS.5b$trait,GS.5c$trait),
                  Beta = c(FEMA.5$Beta,
                           FGR11.5a$EAhigh_beta,FGR11.5b$EAhigh_beta,FGR11.5c$EAhigh_beta,
                           UKB.5a$EAhigh_beta,UKB.5b$EAhigh_beta,UKB.5c$EAhigh_beta,
                           GS.5a$EAhigh_beta,GS.5b$EAhigh_beta,GS.5c$EAhigh_beta),
                  SE = c(FEMA.5$SE,
                         FGR11.5a$EAhigh_se,FGR11.5b$EAhigh_se,FGR11.5c$EAhigh_se,
                         UKB.5a$EAhigh_se, UKB.5b$EAhigh_se, UKB.5c$EAhigh_se,
                         GS.5a$EAhigh_se,GS.5b$EAhigh_se,GS.5c$EAhigh_se),
                  Pval = c(FEMA.5$Pval,
                           FGR11.5a$EAhigh_p,FGR11.5b$EAhigh_p,FGR11.5c$EAhigh_p,
                           UKB.5a$EAhigh_p,UKB.5b$EAhigh_p,UKB.5c$EAhigh_p,
                           GS.5a$EAhigh_p,GS.5b$EAhigh_p,GS.5c$EAhigh_p),
                  HR = c(FEMA.5$HR,
                         FGR11.5a$EAhigh_HR,FGR11.5b$EAhigh_HR,FGR11.5c$EAhigh_HR,
                         UKB.5a$EAhigh_HR,UKB.5b$EAhigh_HR,UKB.5c$EAhigh_HR,
                         GS.5a$EAhigh_HR,GS.5b$EAhigh_HR,GS.5c$EAhigh_HR),
                  Cineg = c(FEMA.5$Cineg,
                            FGR11.5a$EAhigh_HR_lower95, FGR11.5b$EAhigh_HR_lower95, FGR11.5c$EAhigh_HR_lower95,
                            UKB.5a$EAhigh_HR_lower95,UKB.5b$EAhigh_HR_lower95,UKB.5c$EAhigh_HR_lower95,
                            GS.5a$EAhigh_HR_lower95,GS.5b$EAhigh_HR_lower95,GS.5c$EAhigh_HR_lower95),
                  Cipos = c(FEMA.5$Cipos,
                            FGR11.5a$EAhigh_HR_upper95,FGR11.5b$EAhigh_HR_upper95,FGR11.5c$EAhigh_HR_upper95,
                            UKB.5a$EAhigh_HR_upper95,UKB.5b$EAhigh_HR_upper95,UKB.5c$EAhigh_HR_upper95,
                            GS.5a$EAhigh_HR_upper95,GS.5b$EAhigh_HR_upper95,GS.5c$EAhigh_HR_upper95),
                  QHet = c(FEMA.5$QHet,
                           rep(NA,nrow(FGR11.5a)),rep(NA,nrow(FGR11.5b)),rep(NA,nrow(FGR11.5c)),
                           rep(NA,nrow(UKB.5a)),rep(NA,nrow(UKB.5b)),rep(NA,nrow(UKB.5c)),
                           rep(NA,nrow(GS.5a)),rep(NA,nrow(GS.5b)),rep(NA,nrow(GS.5c))),
                  HetPval = c(FEMA.5$HetPval,
                              rep(NA,nrow(FGR11.5a)),rep(NA,nrow(FGR11.5b)),rep(NA,nrow(FGR11.5c)),
                              rep(NA,nrow(UKB.5a)),rep(NA,nrow(UKB.5b)),rep(NA,nrow(UKB.5c)),
                              rep(NA,nrow(GS.5a)),rep(NA,nrow(GS.5b)),rep(NA,nrow(GS.5c))))
# adjust labels
TS8$PRSgroup[which(TS8$PRSgroup=="lowPRS")] <- "low PRS"
TS8$PRSgroup[which(TS8$PRSgroup=="mediumPRS")] <- "medium PRS"
TS8$PRSgroup[which(TS8$PRSgroup=="highPRS")] <- "high PRS"
#
TS8$Phenotype <- factor(TS8$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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


################################################################################
#
# Create Figures 
#
################################################################################

# Figure S1: per cohort + meta-analysed results for model 1a + 1b

# Biobank as factor
TS5$Biobank <- factor(TS5$Biobank, levels = c("FE meta-analysis","FinnGen","UK Biobank", "Generation Scotland"), 
                             labels = c("FE meta-analysis","FinnGen","UK Biobank","Generation Scotland"))


# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
FigS1 <- ggplot(TS5, aes(x = HR, y = Biobank, 
                            xmin = Cineg, xmax = Cipos,
                            group = Test, color = Test)) + 
  geom_point(position = position_dodge(1), size=9.5) + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5) +
  theme_minimal() + scale_colour_manual(values=cbPalette) + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,18.5,1),color="black",size=.5) +
  scale_x_continuous(breaks = round(seq(min(TS5$Cineg)-0.05, max(TS5$Cipos)+0.05, by = 0.35),1)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio per Standard Deviation (95% CI)") + facet_wrap(~Phenotype)

# save figure as tiff
tiff(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_EducationalAttainment_FigureS1.tiff"),
     width=700,height=600,units='mm',res=600,pointsize=6)
FigS1
dev.off()

# Figure S2: per cohort + meta-analysed results for model 2

# Biobank as factor
TS6$Biobank <- factor(TS6$Biobank, levels = c("FE meta-analysis","FinnGen","UK Biobank", "Generation Scotland"), 
                      labels = c("FE meta-analysis","FinnGen","UK Biobank","Generation Scotland"))

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
FigS2 <- ggplot(TS6, aes(x = HR, y = Biobank, 
                               xmin = Cineg, xmax = Cipos,
                               group = Test, color = Test)) + 
  geom_point(position = position_dodge(1), size=9.5) + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5) +
  theme_minimal() + scale_colour_manual(values=cbPalette) + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,18.5,1),color="black",size=.5) +
  scale_x_continuous(breaks = round(seq(min(TS6$Cineg)-0.05, max(TS6$Cipos)+0.05, by = 0.35),1)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio per Standard Deviation (95% CI)") + facet_wrap(~Phenotype)


# save figure as tiff
tiff(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_EducationalAttainment_FigureS2.tiff"),
     width=700,height=600,units='mm',res=600,pointsize=6)
FigS2
dev.off()

# Figure S3: per cohort + meta-analysed results for model 3

# Biobank as factor
TS7$Biobank <- factor(TS7$Biobank, levels = c("FE meta-analysis","FinnGen","UK Biobank", "Generation Scotland"), 
                      labels = c("FE meta-analysis","FinnGen","UK Biobank","Generation Scotland"))

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
FigS3 <- ggplot(TS7, aes(x = HR, y = Biobank, 
                         xmin = Cineg, xmax = Cipos,
                         group = EducationGroup, color = EducationGroup)) + 
  geom_point(position = position_dodge(1), size=9.5) + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5) +
  theme_minimal() + scale_colour_manual(values=cbPalette) + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,18.5,1),color="black",size=.5) +
  scale_x_continuous(breaks = round(seq(min(TS7$Cineg)-0.05, max(TS7$Cipos)+0.05, by = 0.35),1)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio per Standard Deviation (95% CI)") + facet_wrap(~Phenotype)


# save figure as tiff
tiff(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_EducationalAttainment_FigureS3.tiff"),
     width=700,height=600,units='mm',res=600,pointsize=6)
FigS3
dev.off()

# Figure S4: per cohort + meta-analysed results for model 5

# Biobank as factor
TS8$Biobank <- factor(TS8$Biobank, levels = c("FE meta-analysis","FinnGen","UK Biobank", "Generation Scotland"), 
                      labels = c("FE meta-analysis","FinnGen","UK Biobank","Generation Scotland"))

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
FigS4 <- ggplot(TS8, aes(x = HR, y = Biobank, 
                         xmin = Cineg, xmax = Cipos,
                         group = PRSgroup, color = PRSgroup)) + 
  geom_point(position = position_dodge(1), size=9.5) + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5) +
  theme_minimal() + scale_colour_manual(values=cbPalette) + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,18.5,1),color="black",size=.5) +
  scale_x_continuous(breaks = round(seq(min(TS8$Cineg)-0.05, max(TS8$Cipos)+0.05, by = 0.35),1)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio per Standard Deviation (95% CI)") + facet_wrap(~Phenotype)


# save figure as tiff
tiff(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_EducationalAttainment_FigureS4.tiff"),
     width=700,height=600,units='mm',res=600,pointsize=6)
FigS4
dev.off()

