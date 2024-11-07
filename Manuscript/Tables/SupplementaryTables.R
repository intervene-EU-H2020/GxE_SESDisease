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
# Script: Create manuscript supplementary tables (per cohort + meta-analysis)
#
# Tables this script creates:
#       1) Table S4: descriptive statistics per cohohort
#       2) Table S5: results model 1: per cohort + meta-analysis
#       3) Table S6: results model 2: per cohort + meta-analysis
#       4) Table S7: significance test difference effect education and PGS model 1vs2
#       5) Table S8: results model 3 (per cohort + meta-analysis) 
#       6) Table S9 significance test difference effect PGS high vs low EA (model3)
#       7) Table S10: results model 5: per cohort + meta-analysis
#       8) Table S11: descriptive statistics per cohort and polygenic score strata
# required input data:
#       1) FinnGen data release 11 (FGR11) + UK Biobank (UKB) + Generation Scotland (GS) descriptive statistics
#       2) FGR11 + UKB + GS + meta-analysis model 1a+b
#       3) FGR11 + UKB + GS + meta-analysis model 2
#       4) significance test difference effect education and PGS model 1vs2
#       5) FGR11 + UKB + GS + meta-analysis model 3
#       6) significance test difference effect PGS high vs low EA (model 3)
#       7) FGR11 + UKB + GS + meta-analysis model 5
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
# efficiently reading in large data sets; dplyr, forcats & stringr = data
# wrangling.
packages("data.table","dplyr","forcats","stringr")


################################################################################
#
# Read in full sample hazard ratios per standard deviation
#
################################################################################

# read in descriptive statistics
FGR11.N <- fread("output/2classEA/FG11/2024-03-13_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11.txt",data.table=FALSE)
FGR11.N$Biobank <- "FinnGen"
FGR11.N3 <- fread("output/2classEA/FG11/2024-03-13_INTERVENE_SESDiffDiseases_SampleDescriptives_byPGS3group_FinnGenR11.txt",data.table=FALSE)
FGR11.N3$Biobank <- "FinnGen"
#
UKB.N <- fread("output/2classEA/UKB/2024-04-04_INTERVENE_SESDiffDiseases_SampleDescriptives_UKB.txt",data.table=FALSE)
UKB.N$Biobank <- "UK Biobank"
UKB.N3 <- fread("output/2classEA/UKB/2024-04-04_INTERVENE_SESDiffDiseases_SampleDescriptives_byPGS3group_UKB.txt",data.table=FALSE)
UKB.N3$Biobank <- "UK Biobank"
#
GS.N <- fread("output/2classEA/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_SampleDescriptives.txt",data.table=FALSE)
GS.N$Biobank <- "Generation Scotland"
GS.N3 <- fread("output/2classEA/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_SampleDescriptives_byPGS3group.txt",data.table=FALSE)
GS.N3$Biobank <- "Generation Scotland"

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

# significance differences effect education and PRS model 1 vs 2
mod12sign <- fread("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model2/2024-11-04_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv",data.table = FALSE)

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

# significance difference effect PGS high vs low EA 
mod3sign <- fread("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model3/2024-11-04_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_Differences.csv",data.table = FALSE)

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

UKB.N <- UKB.N[which(UKB.N$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]
UKB.N3 <- UKB.N3[which(UKB.N3$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]
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
# Combine FGR11 & UKB & GenScot & meta-analysis results and create supplementary
#       1) Table S4: descriptive statistics per cohohort
#       2) Table S5: results model 1: per cohort + meta-analysis
#       3) Table S6: results model 2: per cohort + meta-analysis
#       4) Table S7: significance test difference effect education and PGS model 1vs2
#       5) Table S8: results model 3 (per cohort + meta-analysis) 
#       6) Table S9 significance test difference effect PGS high vs low EA (model3)
#       7) Table S10: results model 5: per cohort + meta-analysis
#       8) Table S11: descriptive statistics per cohort and polygenic score strata
#
################################################################################

# create Table S4
TS4 <- rbind(FGR11.N,UKB.N,GS.N)
#reorder columns
TS4 <- TS4[,c(1,23,2:22)]
#adjust column names
names(TS4) <- c("Disease","Study",names(TS4[3:23]))
# adjust labels
TS4$Disease <- factor(TS4$Disease, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
# write file 
write.table(TS4, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_TableS4.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

# create Table S5 
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
# write file 
write.table(TS5, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_TableS5.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

# create Table S6 
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
# write file 
write.table(TS6, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_TableS6.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

# create Table S7 
TS7 <- mod12sign[,c(1:12,14)]
# rename columns
names(TS7) <- c("Phenotype", "HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                "beta_difference","se_difference","Zval_difference","Pval_difference","Test")
# reorder
TS7 <- TS7[,c("Phenotype","Test","HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
              "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
              "beta_difference","se_difference","Pval_difference")]
# write file 
write.table(TS7, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_TableS7.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

# Create Table S8
TS8 <- data.frame(Biobank = c(FEMA.3$Biobank,
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
TS8$EducationGroup[which(TS8$EducationGroup=="lowEA")] <- "low educational attainment"
TS8$EducationGroup[which(TS8$EducationGroup=="highEA")] <- "high educational attainment"
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
# write file 
write.table(TS8, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_TableS8.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

# create Table S9
TS9 <- mod3sign[,c(1:12)]
# rename columns
names(TS9) <- c("Phenotype", "HR_LowEducation","HR_HighEducation","beta_LowEducation",
                "beta_HighEducation","se_LowEducation","se_HighEducation","HR_difference",
                "beta_difference","se_difference","Zval_difference","Pval_difference")
# add column with percentage difference HR
TS9$HR_difference_percentage <- abs(TS9$HR_HighEducation - TS9$HR_LowEducation) / TS9$HR_LowEducation * 100
# reorder
TS9 <- TS9[,c("Phenotype", "HR_LowEducation","HR_HighEducation","beta_LowEducation",
              "beta_HighEducation","se_LowEducation","se_HighEducation","HR_difference",
              "HR_difference_percentage","beta_difference","se_difference","Pval_difference")]
# write file 
write.table(TS9, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_TableS9.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


# create Table S10 
TS10 <- data.frame(Biobank = c(FEMA.5$Biobank,
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
TS10$PRSgroup[which(TS10$PRSgroup=="lowPRS")] <- "low PRS"
TS10$PRSgroup[which(TS10$PRSgroup=="mediumPRS")] <- "medium PRS"
TS10$PRSgroup[which(TS10$PRSgroup=="highPRS")] <- "high PRS"
#
TS10$Phenotype <- factor(TS10$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
# write file 
write.table(TS10, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_TableS10.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

# create Table S11
TS11 <- rbind(FGR11.N3,UKB.N3,GS.N3)
#reorder columns
TS11 <- TS11[,c(1,67,2:22,24:44,46:66)]
#adjust column names
names(TS11) <- c("Disease","Study",names(TS11[3:65]))
# adjust labels
TS11$Disease <- factor(TS11$Disease, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
# write file 
write.table(TS11, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_EducationalAttainment_TableS11.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
