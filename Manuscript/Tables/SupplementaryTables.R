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
#       1) eTable 4: descriptive statistics per cohort for EA
#       2) eTable 5: results model 1: per cohort + meta-analysis for EA
#       3) eTable 6: results model 2: per cohort + meta-analysis for EA
#       4) eTable 7: significance test difference effect education and PGS model 1vs2
#       5) eTable 8: results model 3 (per cohort + meta-analysis) for EA
#       6) eTable 9 results model 4 (per cohort + meta-analysis) for EA
#       7) eTable 10: descriptive statistics FinnGen model 6 for EA
#       8) eTable 11: FinnGen results model 6 for EA
#       9) eTable 12: FinnGen results bootstrapped cumulative incidences for EA
#      10) eTable 13: FinnGen descriptives for EA (80%) 
#      11) eTable 14: results model 0 in 80% Finngen for EA
#      12) eTable 15: results model 1 in 80% FinnGen for EA
#      13) eTable 16: results model 2 in 80% FinnGen for EA
#      14) eTable 17: results model 4 in 80% FinnGen for EA
#      15) eTable 18: descriptive statistics 20% FinnGen for EA
#      16) eTable 19: AUC predictive results per cohort  for EA
#      17) eTable 20: NRI/IDI predictive results per cohort for EA
#      18) eTable 21: descriptive statistics FinnGen for Fine-Gray model for EA
#      19) eTable 22: Fine-Gray estimates model 4 in FinnGen for EA
#      20) eTable 23: Compare Cox and FG estimates model 4 in FinnGen for EA
#      21) eTable 24: descriptive statistics non-EU ancestries UKB for EA
#      22) eTable 25: UKB non-EU ancestry results model 1 for EA
#      23) eTable 26: descriptive statistics FinnGen for occupation
#      24) eTable 27: results model 1 FinnGen for occupation
#      25) eTable 28: results model 2 FinnGen for occupation
#      26) eTable 29: significance test differences effect occupation and PGS model 1vs2
#      27) eTable 30: results model 3 FinnGen for occupation
#      28) eTable 31: results model 4 FinnGen for occupation
#      29) eTable 32: descriptive statistics FinnGen model 6 for occupation
#      30) eTable 33: results model 6 FinnGen for occupation
#      31) eTable 34: FinnGen results bootstrapped cumulative incidences for occupation
#
# Data: 
#       1) FGR11 + UKB (incl. all traits) + GS descriptive statistics education
#       2) FGR11 + UKB + GS + meta-analysis model 1a+b for EA
#       3) FGR11 + UKB + GS + meta-analysis model 2 for EA
#       4) significance test difference effect education and PGS model 1vs2
#       5) FGR11 + UKB + GS + meta-analysis model 3 for EA
#       6) FGR11 + UKB + GS + meta-analysis model 4 for EA
#       7) FGR11 descriptive statistics education by PGS strata
#       8) FGR11 results model 6 for EA
#       9) FGR11 cumulative incidences in high vs low EA for each disorder
#      10) FGR11 80% descriptives statistics education
#      11) FGR11 80% resuts model 0a+b for EA
#      12) FGR11 80% results model 1a+b for EA
#      13) FGR11 80% results model 2 for EA
#      14) FGR11 80% results model 4 for EA
#      15) FGR11 20% descriptive statistics education
#      16) FGR11 20% + UKB AUC prediction
#      17) FGR11 20% + UKB NRI/IDI prediction
#      18) FGR11 descriptive statistics education for fine-gray models
#      19) FGR11 model 4 Fine-Gray results for EA
#      20) significance test difference effects Cox vs Fine-Gray in FGR11 for EA
#      21) UKB non-EU ancestries descriptive statistics for EA
#      22) UKB non-EU ancestries results model 1 for EA
#      23) FGR11 descriptive statistics for occupation
#      24) FGR11 model 1a+b for occupation
#      25) FGR11 model 2 for occupation
#      26) FGR11 significance test differences effect occupation and PGS model 1vs2
#      27) FGR111 model 3 for occupation
#      28) FGR11 model 4 for occupation 
#      29) FGR11 descriptive statistics for model 6 for occupation
#      30) FGR11 model 6 for occupation
#      31) FinnGen results bootstrapped cumulative incidences for occupation 
#
# Last edits: 25/06/2025 (edits, FAH: upload to GitHub)
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
# efficiently reading in large data sets; dplyr, forcats & stringr = data
# wrangling; readxl = read excel files (upload to googledrive converts csv to
# xlsx).
packages("data.table","dplyr","forcats","stringr","readxl")


################################################################################
#
# Read in files
#
################################################################################

## read in descriptive statistics ##
# FinnGen
FGR11.N <- fread("output/2classEA/FG11/2024-03-13_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11.txt",data.table=FALSE) # descriptive for analyses on education
FGR11.N$Biobank <- "FinnGen"
FGR11.N6 <- fread("output/2classEA/FG11/2025-03-14_INTERVENE_SESDiffDiseases_SampleDescriptives_mod6_FinnGenR11.txt",data.table=FALSE) # descriptive for analyses on education by PGS strata
FGR11.NFG <- fread("output/2classEA/FG11/2025-03-25_INTERVENE_SESDiffDiseases_SampleDescriptives_FineGray_FinnGenR11.txt",data.table=FALSE) # descriptives for analyses on education with Fine-Gray models
FGR11.N80 <- fread("output/2classEA/FG11/2025-06-03_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11_80percent.txt",data.table=FALSE) # descriptives for analyses on education in random 80% sample
FGR11.N20 <- fread("output/2classEA/FG11/2025-06-03_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11_20percent.txt",data.table=FALSE) # descriptives for analyses on education in random 20% sample
FGR11.N20$Biobank <- "FinnGen (20%)"
FGR11.Nocc <- fread("output/EmploymentStatus/FG11/2025-01-30_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11_Occupation_MANLOWCOMB.txt",data.table=FALSE) # descriptives for analyses on occupation
FGR11.N6occ <- fread("output/EmploymentStatus/FG11/2025-03-14_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11_Occupation_mod6.txt",data.table=FALSE) # descriptives for analyses on occupation by PGS strata
# UK Biobank
UKB.N.EUR <- fread("output/2classEA/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_SESDiffDiseases_SampleDescriptives_EducationalAttainment.txt",data.table=FALSE) # descriptives for analyses on education in EU ancestry
UKB.N.EUR$Biobank <- "UK Biobank"
UKB.N.SAS <- fread("output/2classEA/UKB/2025-05-22_UKBiobank_SAS_INTERVENE_SESDiffDiseases_SampleDescriptives_EducationalAttainment.txt",data.table=FALSE) # descriptives for analyses on education in SAS ancestry
UKB.N.SAS$Biobank <- "SAS"
UKB.N.EAS <- fread("output/2classEA/UKB/2025-05-22_UKBiobank_EAS_INTERVENE_SESDiffDiseases_SampleDescriptives_EducationalAttainment.txt",data.table=FALSE) # descriptives for analyses on education in EAS ancestry
UKB.N.EAS$Biobank <- "EAS"
UKB.N.AFR <- fread("output/2classEA/UKB/2025-05-22_UKBiobank_AFR_INTERVENE_SESDiffDiseases_SampleDescriptives_EducationalAttainment.txt",data.table=FALSE) # descriptives for analyses on education in AFR ancestry
UKB.N.AFR$Biobank <- "AFR"
UKB.N.EURall <- fread("output/2classEA/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_SESDiffDiseases_SampleDescriptivesAll_EducationalAttainment.txt",data.table=FALSE) # descriptives for analyses on education in EU ancestry for all 19 traits
UKB.N.EURall$Biobank <- "UK Biobank"
# Generation Scotland
GS.N <- fread("output/2classEA/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_SampleDescriptives.txt",data.table=FALSE) # descriptives for analyes on education
GS.N$Biobank <- "Generation Scotland"

## read in model 0a or ob in 80% FinnGen
FGR11.0a80 <- fread("output/2classEA/FG11/LogReg/2025-06-13_INTERVENE_SESDiffDiseases_Coeffs_glm_by_model0a_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample
FGR11.0b80 <- fread("output/2classEA/FG11/LogReg/2025-06-13_INTERVENE_SESDiffDiseases_Coeffs_glm_by_model0b_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample

## read in model 1a - EA or occupation only ##
# FinnGen
FGR11.1a <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt", data.table=FALSE) # results for analyses on education
FGR11.1a$Biobank <- "FinnGen"
FGR11.1a80 <- fread("output/2classEA/FG11/LogReg/2025-06-13_INTERVENE_SESDiffDiseases_Coeffs_glm_by_model1a_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample
FGR11.1aocc <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model1/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model1a_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation
# UK Biobank
UKB.1a.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on education in EU ancestry
UKB.1a.EUR$Biobank <- "UK Biobank"
UKB.1a.SAS <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_SAS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on education in SAS ancestry
UKB.1a.SAS$Biobank <- "SAS"
UKB.1a.EAS <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EAS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on education in EAS ancestry
UKB.1a.EAS$Biobank <- "EAS"
UKB.1a.AFR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_AFR_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on education in AFR ancestry
UKB.1a.AFR$Biobank <- "AFR"
# Generation Scotland
GS.1a <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on education
GS.1a$Biobank <- "Generation Scotland"
# Meta-analysis FinnGen + UK Biobank + Generation Scotland
FEMA.1a <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.xlsx")) # results for analyses on education
FEMA.1a$Biobank <- "FE meta-analysis"

## read in model 1b - PGS only (education or occupation models) ##
# FinnGen
FGR11.1b <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt", data.table=FALSE) # results for analyses on education
FGR11.1b$Biobank <- "FinnGen"
FGR11.1b80 <- fread("output/2classEA/FG11/LogReg/2025-06-13_INTERVENE_SESDiffDiseases_Coeffs_glm_by_model1b_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample
FGR11.1bocc <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model1/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model1b_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation
# UK Biobank
UKB.1b.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on education in EU ancestry
UKB.1b.EUR$Biobank <- "UK Biobank"
UKB.1b.SAS <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on education in SAS ancestry
UKB.1b.SAS$Biobank <- "SAS"
UKB.1b.EAS <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on education in EAS ancestry
UKB.1b.EAS$Biobank <- "EAS"
UKB.1b.AFR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on education in AFR ancestry
UKB.1b.AFR$Biobank <- "AFR"
# Generation Scotland
GS.1b <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on education
GS.1b$Biobank <- "Generation Scotland"
# Meta-analysis FinnGen + UK Biobank + Generation Scotland
FEMA.1b <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.xlsx")) # results for analyses on education
FEMA.1b$Biobank <- "FE meta-analysis"

## read in model 2 education or occupation ##
# FinnGen
FGR11.2 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt", data.table=FALSE) # results for analyses on education
FGR11.2$Biobank <- "FinnGen"
FGR11.280 <- fread("output/2classEA/FG11/LogReg/2025-06-13_INTERVENE_SESDiffDiseases_Coeffs_glm_by_model2_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample
FGR11.2occ <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model2/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model2_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation
# UK Biobank
UKB.2.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE) # results for analyses on education in EU ancestry
UKB.2.EUR$Biobank <- "UK Biobank"
# Generation Scotland
GS.2 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE) # results for analyses on education
GS.2$Biobank <- "Generation Scotland"
# Meta-analysis FinnGen + UK Biobank + Generation Scotland
FEMA.2 <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2.xlsx")) # results for analyses on education
FEMA.2$Biobank <- "FE meta-analysis"

## significance differences effect education/occupation and PGS model 1 vs 2 meta-analysis/FinnGen ##
mod12sign.EA <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.xlsx")) # results for models on education meta-analyses
mod12sign.OCC <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model2/2025-01-31_INTERVENE_Occupation_FinnGenR11_model1vs2_Differences.csv", data.table=FALSE) # results for models on occupation in FinnGen only

## read in model 3 education or occupation ##
# FinnGen
FGR11.3 <- fread("output/GoogleDrive/FGR11/2025-02-07_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt", data.table=FALSE) # results for analyses on education
FGR11.3$Biobank <- "FinnGen"
FGR11.3occ <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model3/2025-02-12_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model3b_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation
# UK Biobank
UKB.3.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model3b_Coeffs.txt",data.table=FALSE) # results for analyses on education in EU ancestry
UKB.3.EUR$Biobank <- "UK Biobank"
# Generation Scotland
GS.3 <- fread("output/GoogleDrive/GS/2025-02-24_GS_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE) # results for analyses on education
GS.3$Test <- c(rep("LowEA",length(unique(GS.3$trait))),rep("HighEA",length(unique(GS.3$trait)))) # in initial script shared with GS forgot a line of code to add this, so now add it manually. 
GS.3$Biobank <- "Generation Scotland"
# Meta-analysis FinnGen + UK Biobank + Generation Scotland
FEMA.3 <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3.xlsx")) # results for analyses on education
FEMA.3$Biobank <- "FE meta-analysis"

## read in model 4 - traditional GxEA/OCC interaction model ##
# FinnGen
FGR11.4 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt", data.table=FALSE) # results for analyses on education
FGR11.4$Biobank <- "FinnGen"
FGR11.480 <- fread("output/2classEA/FG11/LogReg/2025-06-13_INTERVENE_SESDiffDiseases_Coeffs_glm_by_model4_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample
FGR11.4FG <- fread("output/2classEA/FG11/CoxPropHaz_model4/2025-03-14_INTERVENE_SESDiffDiseases_Coeffs_FineGray_model4_FinnGenR11.txt", data.table=FALSE) # results for Fine-Gray analyses on education 
FGR11.4occ <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model4/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model4_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation
# Uk Biobank
UKB.4.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
UKB.4.EUR$Biobank <- "UK Biobank"
# Generation Scotland
GS.4 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
GS.4$Biobank <- "Generation Scotland"
# Meta-analysis FinnGen + UK Biobank + Generation Scotland
FEMA.4 <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4.xlsx"))
FEMA.4$Biobank <- "FE meta-analysis"

## significance differences model 4 Cox vs Fine-Gray in FinnGen ##
modCFsign <- fread("output/2classEA/FG11/CoxPropHaz_model4/2025-04-11_INTERVENE_SESDiffDiseases_Differences_CoxvsFG_model4_FinnGenR11.csv", data.table=FALSE) # results for models on occupation in FinnGen only

## read in model 6 education or occupation - FinnGen only ##
FGR11.6 <- fread("output/2classEA/FG11/CoxPropHaz_model6/2025-03-14_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model6_FinnGenR11.txt", data.table=FALSE) # results for analyses on education
FGR11.6occ <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model6/2025-03-14_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model6_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation

## read in bootstrapped cumulative incidence results model 6 education or occupation - FinnGen only ##
# low education
LE.PC <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_PROSTATE_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on low education for prostate cancer
LE.AC <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_CANCER_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on low education for any cancer
LE.KO <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_KNEE_ARTHROSIS_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on low education for knee osteoarthritis
LE.HO <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_COX_ARTHROSIS_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on low education for hip osteoarthritis
LE.CHD <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-26_I9_CHD_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on low education for coronary heart disease
LE.AS <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-26_J10_ASTHMA_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on low education for asthma
LE.AF <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-26_I9_AF_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on low education for atrial fibrillation
LE.T2D <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-26_T2D_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on low education for type 2 diabetes
LE.T1D <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_T1D_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on low education for type 1 diabetes
# high education
HE.PC <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_PROSTATE_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on high education for prostate cancer
HE.AC <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_CANCER_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on high education for any cancer
HE.KO <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_KNEE_ARTHROSIS_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on high education for knee osteoarthritis
HE.HO <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_COX_ARTHROSIS_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on high education for hip osteoarthritis
HE.CHD <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_I9_CHD_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on high education for coronary heart disease
HE.AS <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_J10_ASTHMA_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on high education for asthma
HE.AF <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_I9_AF_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on high education for atrial fibrillation
HE.T2D <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_T2D_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on high education for type 2 diabetes
HE.T1D <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_T1D_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on high education for type 1 diabetes
# lower-level occupation
LO.PC <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_PROSTATE_LifetimeRisk_LowerlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on lower-level occupation for prostate cancer
LO.AC <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_CANCER_LifetimeRisk_LowerlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on lower-level occupation for any cancer
LO.KO <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_KNEE_ARTHROSIS_LifetimeRisk_LowerlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on lower-level occupation for knee osteoarthritis
LO.HO <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_COX_ARTHROSIS_LifetimeRisk_LowerlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on lower-level occupation for hip osteoarthritis
LO.CHD <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_I9_CHD_LifetimeRisk_LowerlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on lower-level occupation for coronary heart disease
LO.AS <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_J10_ASTHMA_LifetimeRisk_LowerlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on lower-level occupation for asthma
LO.AF <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_I9_AF_LifetimeRisk_LowerlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on lower-level occupation for atrial fibrillation
LO.T2D <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_T2D_LifetimeRisk_LowerlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on lower-level occupation for type 2 diabetes
# upper-level occupation
UL.PC <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_PROSTATE_LifetimeRisk_UpperlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on upper-level occupation for prostate cancer
UL.AC <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_CANCER_LifetimeRisk_UpperlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on upper-level occupation for any cancer
UL.KO <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_KNEE_ARTHROSIS_LifetimeRisk_UpperlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on upper-level occupation for knee osteoarthritis
UL.HO <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_COX_ARTHROSIS_LifetimeRisk_UpperlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on upper-level occupation for hip osteoarthritis
UL.CHD <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_I9_CHD_LifetimeRisk_UpperlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on upper-level occupation for coronary heart disease
UL.AS <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_J10_ASTHMA_LifetimeRisk_UpperlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on upper-level occupation for asthma
UL.AF <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_I9_AF_LifetimeRisk_UpperlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on upper-level occupation for atrial fibrillation
UL.T2D <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_T2D_LifetimeRisk_UpperlevelOccupation_Bootstrapped_FinnGen.csv", data.table=FALSE) # results for analyses on upper-level occupation for type 2 diabetes

## read in AUC per cohort and UK (UK biobank + Generation Scotland) meta-analyses ##
# FinnGen
FGR11.0a1aAUC <- fread("output/Prediction/FinnGen/2025-06-16_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparison_Model0a-1a_by.txt",data.table=FALSE) # results model 1a vs 2
FGR11.0a1aAUC$Biobank <- "FinnGen"
FGR11.0b1bAUC <- fread("output/Prediction/FinnGen/2025-06-13_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparison_Model0b-1b_by.txt",data.table=FALSE) # results model 1b vs 2
FGR11.0b1bAUC$Biobank <- "FinnGen"
FGR11.1a2AUC <- fread("output/Prediction/FinnGen/2025-06-16_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparison_Model1a-2_by.txt",data.table=FALSE) # results model 1a vs 2
FGR11.1a2AUC$Biobank <- "FinnGen"
FGR11.1b2AUC <- fread("output/Prediction/FinnGen/2025-06-13_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparison_Model1b-2_by.txt",data.table=FALSE) # results model 1b vs 2
FGR11.1b2AUC$Biobank <- "FinnGen"
FGR11.24AUC <- fread("output/Prediction/FinnGen/2025-06-13_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparison_Model2-4_by.txt",data.table=FALSE) # results model 2 vs 4
FGR11.24AUC$Biobank <- "FinnGen"
# UK Biobank
UKB.0a1aAUC.EUR <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model0a-1a_by.txt",data.table=FALSE) # results model 1a vs 2 in EU ancestry
UKB.0a1aAUC.EUR$Biobank <- "UKBiobank"
UKB.0b1bAUC.EUR <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model0b-1b_by.txt",data.table=FALSE) # results model 1b vs 2 in EU ancestry
UKB.0b1bAUC.EUR$Biobank <- "UKBiobank"
UKB.1a2AUC.EUR <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model1a-2_by.txt",data.table=FALSE) # results model 1a vs 2 in EU ancestry
UKB.1a2AUC.EUR$Biobank <- "UKBiobank"
UKB.1b2AUC.EUR <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model1b-2_by.txt",data.table=FALSE) # results model 1b vs 2 in EU ancestry
UKB.1b2AUC.EUR$Biobank <- "UKBiobank"
UKB.24AUC.EUR <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model2-4_by.txt",data.table=FALSE) # results model 2 vs 2 in EU ancestry
UKB.24AUC.EUR$Biobank <- "UKBiobank"

## read in NRI/IDI per cohort ##
# FinnGen
FGR11.0a1aNRIIDI <- fread("output/Prediction/FinnGen/2025-06-13_FinnGen_20percent_INTERVENE_EducationalAttainment_NRI_IDI_Model0a-1a_by.txt",data.table=FALSE) # results model 1a vs 2
FGR11.0a1aNRIIDI$Biobank <- "FinnGen"
FGR11.0b1bNRIIDI <- fread("output/Prediction/FinnGen/2025-06-13_FinnGen_20percent_INTERVENE_EducationalAttainment_NRI_IDI_Model0b-1b_by.txt",data.table=FALSE) # results model 1b vs 2
FGR11.0b1bNRIIDI$Biobank <- "FinnGen"
FGR11.1a2NRIIDI <- fread("output/Prediction/FinnGen/2025-06-13_FinnGen_20percent_INTERVENE_EducationalAttainment_NRI_IDI_Model1a-2_by.txt",data.table=FALSE) # results model 1a vs 2
FGR11.1a2NRIIDI$Biobank <- "FinnGen"
FGR11.1b2NRIIDI <- fread("output/Prediction/FinnGen/2025-06-13_FinnGen_20percent_INTERVENE_EducationalAttainment_NRI_IDI_Model1b-2_by.txt",data.table=FALSE) # results model 1b vs 2
FGR11.1b2NRIIDI$Biobank <- "FinnGen"
FGR11.24NRIIDI <- fread("output/Prediction/FinnGen/2025-06-13_FinnGen_20percent_INTERVENE_EducationalAttainment_NRI_IDI_Model2-4_by.txt",data.table=FALSE) # results model 2 vs 4
FGR11.24NRIIDI$Biobank <- "FinnGen"
# UK Biobank
UKB.0a1aNRIIDI.EUR <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model0a-1a_by.txt",data.table=FALSE) # results model 1a vs 2 in EU ancestry
UKB.0a1aNRIIDI.EUR$Biobank <- "UK Biobank"
UKB.0b1bNRIIDI.EUR <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model0b-1b_by.txt",data.table=FALSE) # results model 1b vs 2 in EU ancestry
UKB.0b1bNRIIDI.EUR$Biobank <- "UK Biobank"
UKB.1a2NRIIDI.EUR <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model1a-2_by.txt",data.table=FALSE) # results model 1a vs 2 in EU ancestry
UKB.1a2NRIIDI.EUR$Biobank <- "UK Biobank"
UKB.1b2NRIIDI.EUR <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model1b-2_by.txt",data.table=FALSE) # results model 1b vs 2 in EU ancestry
UKB.1b2NRIIDI.EUR$Biobank <- "UK Biobank"
UKB.24NRIIDI.EUR <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model2-4_by.txt",data.table=FALSE) # results model 2 vs 2 in EU ancestry
UKB.24NRIIDI.EUR$Biobank <- "UK Biobank"


################################################################################
#
# In Generation Scotland the analyses for Rheumatoid Arthritis have too few
# individuals; for those analyses were the results were still generated, remove
# this trait
#
################################################################################

GS.3 <- GS.3[-which(GS.3$trait %in% c("RHEUMA_SEROPOS_OTH")),]


################################################################################
#
# As current version of the meta-analyses of Cox models also includes traits
# only available in FinnGen (atrial fibrillation and colorectal cancer), subset
# those data frames to only include the traits where >=2 cohorts were analysed.
# Similarly, in the UK cohort meta-analyses for the prediction, some traits are
# only available in the UK Biobank (atrial fibrillation, colorectal cancer,
# gout, and rheumatoid arthritis), subset those data frames to only include the
# traits where 2 cohorts were analysed
#
################################################################################

## remove traits not meta-analyzed Cox models ##
FEMA.1a <- FEMA.1a[-which(FEMA.1a$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.1b <- FEMA.1b[-which(FEMA.1b$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.2 <- FEMA.2[-which(FEMA.2$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.3 <- FEMA.3[-which(FEMA.3$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.4 <- FEMA.4[-which(FEMA.4$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]


################################################################################
#
# Create eTable 4: descriptive statistics per cohort for EA
#
################################################################################

# create Table S4
TS4 <- rbind(FGR11.N,UKB.N.EUR,GS.N)
#reorder columns
TS4 <- TS4[,c(1,23,2:22)]
#adjust column names
names(TS4) <- c("Phenotype","Study",names(TS4[3:23]))
# adjust labels
TS4$Phenotype <- factor(TS4$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
                               "_INTERVENE_EducationalAttainment_eTable4.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# Create eTable 5: results model 1: per cohort + meta-analysis for EA
#
################################################################################

# create Table S5 
TS5 <- data.frame(Biobank = c(FEMA.1a$Biobank,FEMA.1b$Biobank,
                              FGR11.1a$Biobank,FGR11.1b$Biobank,
                              UKB.1a.EUR$Biobank,UKB.1b.EUR$Biobank,
                              GS.1a$Biobank,GS.1b$Biobank),
                  Test = c(rep("high educational attainment",nrow(FEMA.1a)), rep("PGS",nrow(FEMA.1b)),
                           rep("high educational attainment",nrow(FGR11.1a)), rep("PGS",nrow(FGR11.1b)), 
                           rep("high educational attainment", nrow(UKB.1a.EUR)), rep("PGS",nrow(UKB.1b.EUR)), 
                           rep("high educational attainment",nrow(GS.1a)), rep("PGS",nrow(GS.1b))),
                  Phenotype = c(FEMA.1a$Phenotype,FEMA.1b$Phenotype,
                                FGR11.1a$trait,FGR11.1b$trait,
                                UKB.1a.EUR$trait,UKB.1b.EUR$trait,
                                GS.1a$trait,GS.1b$trait),
                  Beta = c(FEMA.1a$Beta,FEMA.1b$Beta,
                           FGR11.1a$EAhigh_beta,FGR11.1b$PRS_beta,
                           UKB.1a.EUR$EAhigh_beta,UKB.1b.EUR$PRS_beta,
                           GS.1a$EAhigh_beta,GS.1b$PRS_beta),
                  SE = c(FEMA.1a$SE,FEMA.1b$SE,
                         FGR11.1a$EAhigh_se,FGR11.1b$PRS_se,
                         UKB.1a.EUR$EAhigh_se,UKB.1b.EUR$PRS_se,
                         GS.1a$EAhigh_se,GS.1b$PRS_se),
                  Pval = c(FEMA.1a$Pval,FEMA.1b$Pval,
                           FGR11.1a$EAhigh_p,FGR11.1b$PRS_p,
                           UKB.1a.EUR$EAhigh_p,UKB.1b.EUR$PRS_p,
                           GS.1a$EAhigh_p,GS.1b$PRS_p),
                  HR = c(FEMA.1a$HR,FEMA.1b$HR,
                         FGR11.1a$EAhigh_HR,FGR11.1b$PRS_HR,
                         UKB.1a.EUR$EAhigh_HR,UKB.1b.EUR$PRS_HR,
                         GS.1a$EAhigh_HR,GS.1b$PRS_HR),
                  Cineg = c(FEMA.1a$Cineg,FEMA.1b$Cineg,
                            FGR11.1a$EAhigh_HR_lower95,FGR11.1b$PRS_HR_lower95,
                            UKB.1a.EUR$EAhigh_HR_lower95,UKB.1b.EUR$PRS_HR_lower95,
                            GS.1a$EAhigh_HR_lower95,GS.1b$PRS_HR_lower95),
                  Cipos = c(FEMA.1a$Cipos,FEMA.1b$Cipos,
                            FGR11.1a$EAhigh_HR_upper95,FGR11.1b$PRS_HR_upper95,
                            UKB.1a.EUR$EAhigh_HR_upper95,UKB.1b.EUR$PRS_HR_upper95,
                            GS.1a$EAhigh_HR_upper95,GS.1b$PRS_HR_upper95),
                  QHet = c(FEMA.1a$QHet,FEMA.1b$QHet,
                           rep(NA,nrow(FGR11.1a)),rep(NA,nrow(FGR11.1b)),
                           rep(NA,nrow(UKB.1a.EUR)),rep(NA,nrow(UKB.1b.EUR)),
                           rep(NA,nrow(GS.1a)),rep(NA,nrow(GS.1b))),
                  HetPval = c(FEMA.1a$HetPval,FEMA.1b$HetPval,
                              rep(NA,nrow(FGR11.1a)),rep(NA,nrow(FGR11.1b)),
                              rep(NA,nrow(UKB.1a.EUR)),rep(NA,nrow(UKB.1b.EUR)),
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
                               "_INTERVENE_EducationalAttainment_eTable5.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 6: results model 2: per cohort + meta-analysis for EA
#
################################################################################

# create Table S6 
TS6 <- data.frame(Biobank = c(FEMA.2$Biobank,
                              FGR11.2$Biobank,FGR11.2$Biobank,
                              UKB.2.EUR$Biobank,UKB.2.EUR$Biobank,
                              GS.2$Biobank,GS.2$Biobank),
                  Test = c(FEMA.2$Test,
                           rep("high educational attainment",nrow(FGR11.2)), rep("PGS",nrow(FGR11.2)), 
                           rep("high educational attainment", nrow(UKB.2.EUR)), rep("PGS",nrow(UKB.2.EUR)), 
                           rep("high educational attainment",nrow(GS.2)), rep("PGS",nrow(GS.2))),
                  Phenotype = c(FEMA.2$Phenotype,
                                FGR11.2$trait,FGR11.2$trait,
                                UKB.2.EUR$trait,UKB.2.EUR$trait,
                                GS.2$trait,GS.2$trait),
                  Beta = c(FEMA.2$Beta,
                           FGR11.2$EAhigh_beta,FGR11.2$PRS_beta,
                           UKB.2.EUR$EAhigh_beta,UKB.2.EUR$PRS_beta,
                           GS.2$EAhigh_beta,GS.2$PRS_beta),
                  SE = c(FEMA.2$SE,
                         FGR11.2$EAhigh_se,FGR11.2$PRS_se,
                         UKB.2.EUR$EAhigh_se,UKB.2.EUR$PRS_se,
                         GS.2$EAhigh_se,GS.2$PRS_se),
                  Pval = c(FEMA.2$Pval,
                           FGR11.2$EAhigh_p,FGR11.2$PRS_p,
                           UKB.2.EUR$EAhigh_p,UKB.2.EUR$PRS_p,
                           GS.2$EAhigh_p,GS.2$PRS_p),
                  HR = c(FEMA.2$HR,
                         FGR11.2$EAhigh_HR,FGR11.2$PRS_HR,
                         UKB.2.EUR$EAhigh_HR,UKB.2.EUR$PRS_HR,
                         GS.2$EAhigh_HR,GS.2$PRS_HR),
                  Cineg = c(FEMA.2$Cineg,
                            FGR11.2$EAhigh_HR_lower95,FGR11.2$PRS_HR_lower95,
                            UKB.2.EUR$EAhigh_HR_lower95,UKB.2.EUR$PRS_HR_lower95,
                            GS.2$EAhigh_HR_lower95,GS.2$PRS_HR_lower95),
                  Cipos = c(FEMA.2$Cipos,
                            FGR11.2$EAhigh_HR_upper95,FGR11.2$PRS_HR_upper95,
                            UKB.2.EUR$EAhigh_HR_upper95,UKB.2.EUR$PRS_HR_upper95,
                            GS.2$EAhigh_HR_upper95,GS.2$PRS_HR_upper95),
                  QHet = c(FEMA.2$QHet,
                           rep(NA,2*nrow(FGR11.2)),
                           rep(NA,2*nrow(UKB.2.EUR)),
                           rep(NA,2*nrow(GS.2))),
                  HetPval = c(FEMA.2$HetPval,
                              rep(NA,2*nrow(FGR11.2)),
                              rep(NA,2*nrow(UKB.2.EUR)),
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
                               "_INTERVENE_EducationalAttainment_eTable6.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 7: significance test difference effect education and PGS model 1vs2
#
################################################################################

# create Table S7 
TS7 <- mod12sign.EA[,c(1:12,14)]
# rename columns
names(TS7) <- c("Phenotype", "HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                "beta_difference","se_difference","Zval_difference","Pval_difference","Test")
# reorder
TS7 <- TS7[,c("Phenotype","Test","HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
              "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
              "beta_difference","se_difference","Pval_difference")]

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS7$lb_beta_difference <- TS7$beta_difference - 1.96*TS7$se_difference
TS7$ub_beta_difference <- TS7$beta_difference + 1.96*TS7$se_difference
#
TS7$lb_difference <- exp(TS7$lb_beta_difference)
TS7$ub_difference <- exp(TS7$ub_beta_difference)

# adjust label Test
TS7$Test[which(TS7$Test=="PRS")] <- "PGS"

# write file 
write.table(TS7, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable7.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 8: results model 3 (per cohort + meta-analysis) for EA
#
################################################################################

# Create Table S8
TS8 <- data.frame(Biobank = c(FEMA.3$Biobank,FGR11.3$Biobank,UKB.3.EUR$Biobank,
                              GS.3$Biobank),
                  EducationGroup = c(FEMA.3$EA,FGR11.3$Test,UKB.3.EUR$Test,
                                     GS.3$Test),
                  Phenotype = c(FEMA.3$Phenotype,FGR11.3$trait,UKB.3.EUR$trait,
                                GS.3$trait),
                  Beta = c(FEMA.3$Beta,FGR11.3$PRS_beta,UKB.3.EUR$PRS_beta,
                           GS.3$PRS_beta),
                  SE = c(FEMA.3$SE,FGR11.3$PRS_se,UKB.3.EUR$PRS_se,GS.3$PRS_se),
                  Pval = c(FEMA.3$Pval,FGR11.3$PRS_p,UKB.3.EUR$PRS_p,GS.3$PRS_p),
                  HR = c(FEMA.3$HR,FGR11.3$PRS_HR,UKB.3.EUR$PRS_HR,GS.3$PRS_HR),
                  Cineg = c(FEMA.3$Cineg,FGR11.3$PRS_HR_lower95,
                            UKB.3.EUR$PRS_HR_lower95,GS.3$PRS_HR_lower95),
                  Cipos = c(FEMA.3$Cipos,FGR11.3$PRS_HR_upper95,
                            UKB.3.EUR$PRS_HR_upper95,GS.3$PRS_HR_upper95),
                  QHet = c(FEMA.3$QHet,
                           rep(NA,nrow(FGR11.3)),
                           rep(NA,nrow(UKB.3.EUR)),
                           rep(NA,nrow(GS.3))),
                  HetPval = c(FEMA.3$HetPval,
                              rep(NA,nrow(FGR11.3)),
                              rep(NA,nrow(UKB.3.EUR)),
                              rep(NA,nrow(GS.3))))
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
                               "_INTERVENE_EducationalAttainment_eTable8.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 9 results model 4 (per cohort + meta-analysis) for EA
#
################################################################################

# create combined table with model 4 results
TS9 <- data.frame(Biobank = c(FEMA.4$Biobank,
                               rep(FGR11.4$Biobank,3),
                               rep(UKB.4.EUR$Biobank,3),
                               rep(GS.4$Biobank,3)),
                   Test = c(FEMA.4$Test,
                            rep("high educational attainment",nrow(FGR11.4)), rep("EAxPGS",nrow(FGR11.4)), rep("PGS",nrow(FGR11.4)), 
                            rep("high educational attainment", nrow(UKB.4.EUR)), rep("EAxPGS",nrow(UKB.4.EUR)), rep("PGS",nrow(UKB.4.EUR)),  
                            rep("high educational attainment",nrow(GS.4)), rep("EAxPGS",nrow(GS.4)), rep("PGS",nrow(GS.4))),
                   Phenotype = c(FEMA.4$Phenotype,
                                 rep(FGR11.4$trait,3),
                                 rep(UKB.4.EUR$trait,3),
                                 rep(GS.4$trait,3)),
                   Beta = c(FEMA.4$Beta,
                            FGR11.4$EAhigh_beta,FGR11.4$`PRS:EAhigh_beta`,FGR11.4$PRS_beta,
                            UKB.4.EUR$EAhigh_beta,UKB.4.EUR$`PRS:EAhigh_beta`,UKB.4.EUR$PRS_beta,
                            GS.4$EAhigh_beta,GS.4$`PRS:EAhigh_beta`,GS.4$PRS_beta),
                   SE = c(FEMA.4$SE,
                          FGR11.4$EAhigh_se,FGR11.4$`PRS:EAhigh_se`,FGR11.4$PRS_se,
                          UKB.4.EUR$EAhigh_se,UKB.4.EUR$`PRS:EAhigh_se`,UKB.4.EUR$PRS_se,
                          GS.4$EAhigh_se,GS.4$`PRS:EAhigh_se`,GS.4$PRS_se),
                   Pval = c(FEMA.4$Pval,
                            FGR11.4$EAhigh_p,FGR11.4$`PRS:EAhigh_p`,FGR11.4$PRS_p,
                            UKB.4.EUR$EAhigh_p,UKB.4.EUR$`PRS:EAhigh_p`,UKB.4.EUR$PRS_p,
                            GS.4$EAhigh_p,GS.4$`PRS:EAhigh_p`,GS.4$PRS_p),
                   HR = c(FEMA.4$HR,
                          FGR11.4$EAhigh_HR,FGR11.4$`PRS:EAhigh_HR`,FGR11.4$PRS_HR,
                          UKB.4.EUR$EAhigh_HR,UKB.4.EUR$`PRS:EAhigh_HR`,UKB.4.EUR$PRS_HR,
                          GS.4$EAhigh_HR,GS.4$`PRS:EAhigh_HR`,GS.4$PRS_HR),
                   Cineg = c(FEMA.4$Cineg,
                             FGR11.4$EAhigh_HR_lower95,FGR11.4$`PRS:EAhigh_HR_lower95`,FGR11.4$PRS_HR_lower95,
                             UKB.4.EUR$EAhigh_HR_lower95,UKB.4.EUR$`PRS:EAhigh_HR_lower95`,UKB.4.EUR$PRS_HR_lower95,
                             GS.4$EAhigh_HR_lower95,GS.4$`PRS:EAhigh_HR_lower95`,GS.4$PRS_HR_lower95),
                   Cipos = c(FEMA.4$Cipos,
                             FGR11.4$EAhigh_HR_upper95,FGR11.4$`PRS:EAhigh_HR_upper95`,FGR11.4$PRS_HR_upper95,
                             UKB.4.EUR$EAhigh_HR_upper95,UKB.4.EUR$`PRS:EAhigh_HR_upper95`,UKB.4.EUR$PRS_HR_upper95,
                             GS.4$EAhigh_HR_upper95,GS.4$`PRS:EAhigh_HR_upper95`,GS.4$PRS_HR_upper95),
                   QHet = c(FEMA.4$QHet,
                            rep(NA,3*nrow(FGR11.4)),
                            rep(NA,3*nrow(UKB.4.EUR)),
                            rep(NA,3*nrow(GS.4))),
                   HetPval = c(FEMA.4$HetPval,
                               rep(NA,3*nrow(FGR11.4)),
                               rep(NA,3*nrow(UKB.4.EUR)),
                               rep(NA,3*nrow(GS.4))))
# adjust labels
TS9$Test[which(TS9$Test=="EA")] <- "high educational attainment"
#
TS9$Phenotype <- factor(TS9$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS9, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable9.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 10: descriptive statistics FinnGen model 6 for EA
#
################################################################################

# create Table S10
TS10 <- FGR11.N6
#adjust column names
names(TS10) <- c("Phenotype",names(TS10[2:82])) # trait = Phenotype
names(TS10) <- gsub("Group1","0_20p", names(TS10)) # PGS Group 1 = 0-20%
names(TS10) <- gsub("Group2","20_40p", names(TS10)) # PGS Group 2 = 20-40%
names(TS10) <- gsub("Group3","40_60p", names(TS10)) # PGS Group 3 = 40-60%
names(TS10) <- gsub("Group4","60_95p", names(TS10)) # PGS Group 4 = 60-95%
names(TS10) <- gsub("Group5","95_100p", names(TS10)) # PGS Group 5 = 95-100%
# adjust labels
TS10$Phenotype <- factor(TS10$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                              "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                              "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                              "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                              "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                              "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                              "K11_APPENDACUT","AUD_SWEDISH"),
                      labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                 "Gout","Rheumatoid Arthritis","Breast Cancer",
                                 "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                 "Coronary Heart Disease","Hip Osteoarthritis",
                                 "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                 "Major Depression","Any Cancer","Epilepsy",
                                 "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS10, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable10.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 11: FinnGen results model 6 for EA
#
################################################################################

# create table S11
TS11 <- data.frame(Test = c(rep("0_20p",nrow(FGR11.6)), rep("20_40p",nrow(FGR11.6)), 
                            rep("60_95p",nrow(FGR11.6)), rep("95_1000p",nrow(FGR11.6))),
                   Phenotype = c(rep(FGR11.6$trait,4)),
                   Beta = c(FGR11.6$`PGS_groupGroup 1_beta`,FGR11.6$`PGS_groupGroup 2_beta`,
                            FGR11.6$`PGS_groupGroup 4_beta`,FGR11.6$`PGS_groupGroup 5_beta`),
                   SE = c(FGR11.6$`PGS_groupGroup 1_se`,FGR11.6$`PGS_groupGroup 2_se`,
                          FGR11.6$`PGS_groupGroup 4_se`,FGR11.6$`PGS_groupGroup 5_se`),
                   Pval = c(FGR11.6$`PGS_groupGroup 1_p`,FGR11.6$`PGS_groupGroup 2_p`,
                            FGR11.6$`PGS_groupGroup 4_p`,FGR11.6$`PGS_groupGroup 5_p`),
                   HR = c(FGR11.6$`PGS_groupGroup 1_HR`,FGR11.6$`PGS_groupGroup 2_HR`,
                          FGR11.6$`PGS_groupGroup 4_HR`, FGR11.6$`PGS_groupGroup 5_HR`),
                   Cineg = c(FGR11.6$`PGS_groupGroup 1_HR_lower95`, FGR11.6$`PGS_groupGroup 2_HR_lower95`,
                             FGR11.6$`PGS_groupGroup 4_HR_lower95`, FGR11.6$`PGS_groupGroup 5_HR_lower95`),
                   Cipos = c(FGR11.6$`PGS_groupGroup 1_HR_upper95`,FGR11.6$`PGS_groupGroup 2_HR_upper95`,
                             FGR11.6$`PGS_groupGroup 4_HR_upper95`,FGR11.6$`PGS_groupGroup 5_HR_upper95`))
# adjust labels
TS10$Phenotype <- factor(TS10$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                "K11_APPENDACUT","AUD_SWEDISH"),
                       labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                                  "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                  "Major Depression","Any Cancer","Epilepsy",
                                  "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS11, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable11.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 12: FinnGen results bootstrapped cumulative incidences for EA
#
################################################################################

# create Table S12
TS12 <- data.frame(Test = c(rep("low educational attainment",720), 
                            rep("high educational attainment",720)),
                   Phenotype = c(rep(rep(c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                     "Atrial Fibrillation","Asthma","Coronary Heart Disease",
                                     "Hip Osteoarthritis","Knee Osteoarthritis",
                                     "Any Cancer"),each = 80),2)),
                   Age = c(LE.T1D$age,LE.PC$age, LE.T2D$age, LE.AF$age, LE.AS$age,
                           LE.CHD$age, LE.HO$age, LE.KO$age, LE.AC$age, 
                           HE.T1D$age,HE.PC$age, HE.T2D$age, HE.AF$age, HE.AS$age,
                           HE.CHD$age, HE.HO$age, HE.KO$age, HE.AC$age),
                   Group = c(LE.T1D$Group,LE.PC$Group, LE.T2D$Group, LE.AF$Group, 
                             LE.AS$Group,LE.CHD$Group, LE.HO$Group, LE.KO$Group,
                             LE.AC$Group,HE.T1D$Group,HE.PC$Group, HE.T2D$Group, 
                             HE.AF$Group, HE.AS$Group,HE.CHD$Group, HE.HO$Group, 
                             HE.KO$Group, HE.AC$Group),
                   LifetimeRisk = c(LE.T1D$LifetimeRisk,LE.PC$LifetimeRisk, 
                                    LE.T2D$LifetimeRisk, LE.AF$LifetimeRisk, 
                                    LE.AS$LifetimeRisk,LE.CHD$LifetimeRisk, 
                                    LE.HO$LifetimeRisk, LE.KO$LifetimeRisk, 
                                    LE.AC$LifetimeRisk,HE.T1D$LifetimeRisk,
                                    HE.PC$LifetimeRisk, HE.T2D$LifetimeRisk, 
                                    HE.AF$LifetimeRisk, HE.AS$LifetimeRisk,
                                    HE.CHD$LifetimeRisk, HE.HO$LifetimeRisk, 
                                    HE.KO$LifetimeRisk, HE.AC$LifetimeRisk),
                   CIneg = c(LE.T1D$CIneg,LE.PC$CIneg, LE.T2D$CIneg, LE.AF$CIneg, 
                             LE.AS$CIneg,LE.CHD$CIneg, LE.HO$CIneg, LE.KO$CIneg, 
                             LE.AC$CIneg,HE.T1D$CIneg,HE.PC$CIneg, HE.T2D$CIneg, 
                             HE.AF$CIneg, HE.AS$CIneg,HE.CHD$CIneg, HE.HO$CIneg, 
                             HE.KO$CIneg, HE.AC$CIneg),
                   CIpos = c(LE.T1D$CIpos,LE.PC$CIpos, LE.T2D$CIpos, LE.AF$CIpos, 
                             LE.AS$CIpos,LE.CHD$CIpos, LE.HO$CIpos, LE.KO$CIpos, 
                             LE.AC$CIpos,HE.T1D$CIpos,HE.PC$CIpos, HE.T2D$CIpos, 
                             HE.AF$CIpos, HE.AS$CIpos,HE.CHD$CIpos, HE.HO$CIpos, 
                             HE.KO$CIpos, HE.AC$CIpos))

# write file 
write.table(TS12, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable12.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 13: FinnGen descriptives for EA (80%) 
#
################################################################################

# create Table TS13
TS13 <- FGR11.N80
#adjust column names
names(TS13) <- c("Phenotype",names(TS13[2:22])) # trait = Phenotype
# adjust labels
TS13$Phenotype <- factor(TS13$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                "K11_APPENDACUT","AUD_SWEDISH"),
                       labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                                  "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                  "Major Depression","Any Cancer","Epilepsy",
                                  "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS13, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable13.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 14: results model 0 in 80% FinnGen for EA
#
################################################################################

# create Table S14 
TS14 <- data.frame(Model = c(rep("5PCs",2*nrow(FGR11.0a80)), rep("10PCs",2*nrow(FGR11.0b80))),
                   Test = c(rep(c("Sex","Birth year"),each=nrow(FGR11.0a80)),
                            rep(c("Sex","Birth year"),each=nrow(FGR11.0b80))),
                   Phenotype = c(rep(FGR11.0a80$trait,2),rep(FGR11.0b80$trait,2)),
                   Beta = c(FGR11.0a80$SEX_beta,FGR11.0a80$birth_year_beta,
                            FGR11.0b80$SEX_beta,FGR11.0b80$birth_year_beta),
                   SE = c(FGR11.0a80$SEX_se,FGR11.0a80$birth_year_se,
                          FGR11.0b80$SEX_se,FGR11.0b80$birth_year_se),
                   Pval = c(FGR11.0a80$SEX_p,FGR11.0a80$birth_year_p,
                            FGR11.0b80$SEX_p,FGR11.0b80$birth_year_p))
# adjust labels
TS14$Phenotype <- factor(TS14$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                    "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                    "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                    "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                    "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                    "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                    "K11_APPENDACUT","AUD_SWEDISH"),
                         labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                    "Gout","Rheumatoid Arthritis","Breast Cancer",
                                    "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                    "Coronary Heart Disease","Hip Osteoarthritis",
                                    "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                    "Major Depression","Any Cancer","Epilepsy",
                                    "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS14, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_EducationalAttainment_eTable14.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 15: results model 1 in 80% FinnGen for EA
#
################################################################################

# create Table S15 
TS15 <- data.frame(Model = c(rep("Education",3*nrow(FGR11.1a80)), rep("PGS",3*nrow(FGR11.1b80))),
                   Test = c(rep(c("Sex","Birth year"),each=nrow(FGR11.1a80)),
                            rep("high educational attainment",nrow(FGR11.1a80)), 
                            rep(c("Sex","Birth year"),each=nrow(FGR11.1b80)),
                            rep("PGS",nrow(FGR11.1b80))),
                   Phenotype = c(rep(FGR11.1a80$trait,3),rep(FGR11.1b80$trait,3)),
                   Beta = c(FGR11.1a80$SEX_beta,FGR11.1a80$birth_year_beta,
                            FGR11.1a80$EAhigh_beta,FGR11.1b80$SEX_beta,
                            FGR11.1b80$birth_year_beta,FGR11.1b80$PRS_beta),
                   SE = c(FGR11.1a80$SEX_se,FGR11.1a80$birth_year_se,
                          FGR11.1a80$EAhigh_se,FGR11.1b80$SEX_se,
                          FGR11.1b80$birth_year_se,FGR11.1b80$PRS_se),
                   Pval = c(FGR11.1a80$SEX_p,FGR11.1a80$birth_year_p,
                            FGR11.1a80$EAhigh_p,FGR11.1b80$SEX_p,
                            FGR11.1b80$birth_year_p,FGR11.1b80$PRS_p))
# adjust labels
TS15$Phenotype <- factor(TS15$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                  "K11_APPENDACUT","AUD_SWEDISH"),
                        labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                   "Gout","Rheumatoid Arthritis","Breast Cancer",
                                   "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                   "Major Depression","Any Cancer","Epilepsy",
                                   "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS15, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable15.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 16: results model 2 in 80% FinnGen for EA
#
################################################################################

# create Table S16 
TS16 <- data.frame(Test = c(rep(c("Sex","Birth year"),each=nrow(FGR11.280)),
                            rep("high educational attainment",nrow(FGR11.280)), 
                            rep("PGS",nrow(FGR11.280))),
                  Phenotype = c(rep(FGR11.280$trait,4)),
                  Beta = c(FGR11.280$SEX_beta,FGR11.280$birth_year_beta,
                           FGR11.280$EAhigh_beta,FGR11.280$PRS_beta),
                  SE = c(FGR11.280$SEX_se,FGR11.280$birth_year_se,
                         FGR11.280$EAhigh_se,FGR11.280$PRS_se),
                  Pval = c(FGR11.280$SEX_p,FGR11.280$birth_year_p,
                           FGR11.280$EAhigh_p,FGR11.280$PRS_p))
# adjust labels
TS16$Phenotype <- factor(TS16$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                  "K11_APPENDACUT","AUD_SWEDISH"),
                        labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                   "Gout","Rheumatoid Arthritis","Breast Cancer",
                                   "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                   "Major Depression","Any Cancer","Epilepsy",
                                   "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS16, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable16.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 17: results model 4 in 80% FinnGen for EA
#
################################################################################

# create combined table with model 4 results
TS17 <- data.frame(Test = c(rep(c("Sex","Birth year"),each=nrow(FGR11.480)),
                            rep("high educational attainment",nrow(FGR11.480)), 
                            rep("EAxPGS",nrow(FGR11.480)), rep("PGS",nrow(FGR11.480))),
                  Phenotype = c(rep(FGR11.480$trait,5)),
                  Beta = c(FGR11.480$SEX_beta,FGR11.480$birth_year_beta,
                           FGR11.480$EAhigh_beta,FGR11.480$`PRS:EAhigh_beta`,
                           FGR11.480$PRS_beta),
                  SE = c(FGR11.480$SEX_se,FGR11.480$birth_year_se,
                         FGR11.480$EAhigh_se,FGR11.480$`PRS:EAhigh_se`,
                         FGR11.480$PRS_se),
                  Pval = c(FGR11.480$SEX_p,FGR11.480$birth_year_p,
                           FGR11.480$EAhigh_p,FGR11.480$`PRS:EAhigh_p`,
                           FGR11.480$PRS_p))
# adjust labels
TS17$Phenotype <- factor(TS17$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                  "K11_APPENDACUT","AUD_SWEDISH"),
                        labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                   "Gout","Rheumatoid Arthritis","Breast Cancer",
                                   "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                   "Major Depression","Any Cancer","Epilepsy",
                                   "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS17, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable17.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 18: descriptive statistics 20% FinnGen for EA and all 19 UKB traits
#
################################################################################

# create Table TS17
TS18 <- rbind(FGR11.N20,UKB.N.EURall)
#reorder columns
TS18 <- TS18[,c(1,23,2:22)]
#adjust column names
names(TS18) <- c("Phenotype","Study",names(TS18[3:23]))
# adjust labels
TS18$Phenotype <- factor(TS18$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                    "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                    "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                    "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                    "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                    "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                    "K11_APPENDACUT","AUD_SWEDISH"),
                         labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                    "Gout","Rheumatoid Arthritis","Breast Cancer",
                                    "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                    "Coronary Heart Disease","Hip Osteoarthritis",
                                    "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                    "Major Depression","Any Cancer","Epilepsy",
                                    "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS18, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable18.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 19: AUC predictive results per cohort + UK meta-analysis for EA
#
################################################################################

# function to set labels for traits
set_trait_labels <- function(df, trait_col = "trait") {
  levels_vec <- c("T1D","C3_PROSTATE","T2D","GOUT",
                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                  "K11_APPENDACUT","AUD_SWEDISH")
  
  labels_vec <- c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                  "Atrial Fibrillation","Colorectal Cancer","Asthma",
                  "Coronary Heart Disease","Hip Osteoarthritis",
                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                  "Major Depression","Any Cancer","Epilepsy",
                  "Appendicitis","Alcohol Use Disorder")
  
  df[[trait_col]] <- factor(df[[trait_col]], levels = levels_vec, labels = labels_vec)
  return(df)
}

# adjust labels diseases - FinnGen
FGR11.0a1aAUC <- set_trait_labels(FGR11.0a1aAUC)
FGR11.0b1bAUC <- set_trait_labels(FGR11.0b1bAUC)
FGR11.1a2AUC <- set_trait_labels(FGR11.1a2AUC)
FGR11.1b2AUC <- set_trait_labels(FGR11.1b2AUC)
FGR11.24AUC <- set_trait_labels(FGR11.24AUC)

# adjust labels diseases - UK Biobank
UKB.0a1aAUC.EUR <- set_trait_labels(UKB.0a1aAUC.EUR)
UKB.0b1bAUC.EUR <- set_trait_labels(UKB.0b1bAUC.EUR)
UKB.1a2AUC.EUR <- set_trait_labels(UKB.1a2AUC.EUR)
UKB.1b2AUC.EUR <- set_trait_labels(UKB.1b2AUC.EUR)
UKB.24AUC.EUR <- set_trait_labels(UKB.24AUC.EUR)

# change column names so function to reorder and combine will work for the model
# 2 vs 4 comparison also (and not just for the model 1a/b vs 2)
names(FGR11.24AUC) <- names(FGR11.1a2AUC)
names(UKB.24AUC.EUR) <- names(UKB.1a2AUC.EUR)

# function to format AUC + CI
format_auc <- function(auc, lower, upper) {
  sprintf("%.3f (%.3f–%.3f)", auc, lower, upper)
}

# Function to clean a single result df
# tag = which model comparison this result represents (e.g. "1a_vs_2")
clean_results <- function(df, comparison) {
  df %>%
    mutate(
      AUC_model1_fmt = format_auc(AUC_model1, CI_model1_lower, CI_model1_upper),
      AUC_model2_fmt = format_auc(AUC_model2, CI_model2_lower, CI_model2_upper),
      comparison = comparison
    ) %>%
    select(trait, Biobank, comparison, AUC_model1_fmt, AUC_model2_fmt, AUC_p_value)
}

# Clean each dataset with the appropriate model comparison tag
# FinnGen
FGR11.0a1a.clean <- clean_results(FGR11.0a1aAUC, "0a_vs_1a")
FGR11.0b1b.clean <- clean_results(FGR11.0b1bAUC, "0b_vs_1b")
FGR11.1a2.clean <- clean_results(FGR11.1a2AUC, "1a_vs_2")
FGR11.1b2.clean <- clean_results(FGR11.1b2AUC, "1b_vs_2")
FGR11.24.clean <- clean_results(FGR11.24AUC, "2_vs_4")

# UK Biobank
UKB.0a1a.clean <- clean_results(UKB.0a1aAUC.EUR, "0a_vs_1a")
UKB.0b1b.clean <- clean_results(UKB.0b1bAUC.EUR, "0b_vs_1b")
UKB.1a2.clean <- clean_results(UKB.1a2AUC.EUR, "1a_vs_2")
UKB.1b2.clean <- clean_results(UKB.1b2AUC.EUR, "1b_vs_2")
UKB.24.clean <- clean_results(UKB.24AUC.EUR, "2_vs_4")

# combine all data in long format
auc_combined_long <- bind_rows(FGR11.0a1a.clean,FGR11.0b1b.clean,FGR11.1a2.clean,FGR11.1b2.clean,FGR11.24.clean,
                               UKB.0a1a.clean,UKB.0b1b.clean,UKB.1a2.clean,UKB.1b2.clean,UKB.24.clean)

# Combine all cleaned results into one wide format
combined_wide <- auc_combined_long %>%
  select(trait, Biobank, comparison,AUC_model1_fmt,AUC_model2_fmt,AUC_p_value) %>%
  distinct() %>% #ensure one row per trait-Biobank-model-comparison
  pivot_wider(
    id_cols = trait,
    names_from = c(Biobank,comparison),
    values_from = c(AUC_model1_fmt, AUC_model2_fmt, AUC_p_value),
    names_glue = "{Biobank}_{comparison}_{.value}"
  )

# Define custom column order:
biobanks <- c("FinnGen","UKBiobank")  # adjust as needed
comparisons <- c("0b_vs_1b","0a_vs_1a","1b_vs_2","1a_vs_2", "2_vs_4")
metrics <- c("AUC_model1_fmt", "AUC_model2_fmt", "AUC_p_value")

# Build desired column names
desired_order <- c("trait")  # always start with trait
for (b in biobanks) {
  for (c in comparisons) {
    for (m in metrics) {
      col_name <- paste(b, c, m, sep = "_")
      if (col_name %in% colnames(combined_wide)) {
        desired_order <- c(desired_order, col_name)
      }
    }
  }
}


# Apply column order
TS19 <- combined_wide %>%
  select(all_of(desired_order))

# as dataframe
TS19 <- as.data.frame(TS19)

# write file 
write.table(TS19, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable19.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 20: NRI/IDI predictive results per cohort for EA
#
################################################################################

# function to set labels for traits
set_trait_labels2 <- function(df, trait_col = "Trait") {
  levels_vec <- c("T1D","C3_PROSTATE","T2D","GOUT",
                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                  "K11_APPENDACUT","AUD_SWEDISH")
  
  labels_vec <- c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                  "Atrial Fibrillation","Colorectal Cancer","Asthma",
                  "Coronary Heart Disease","Hip Osteoarthritis",
                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                  "Major Depression","Any Cancer","Epilepsy",
                  "Appendicitis","Alcohol Use Disorder")
  
  df[[trait_col]] <- factor(df[[trait_col]], levels = levels_vec, labels = labels_vec)
  return(df)
}


# adjust labels diseases - FinnGen
FGR11.0a1aNRIIDI <- set_trait_labels2(FGR11.0a1aNRIIDI)
FGR11.0b1bNRIIDI <- set_trait_labels2(FGR11.0b1bNRIIDI)
FGR11.1a2NRIIDI <- set_trait_labels2(FGR11.1a2NRIIDI)
FGR11.1b2NRIIDI <- set_trait_labels2(FGR11.1b2NRIIDI)
FGR11.24NRIIDI <- set_trait_labels2(FGR11.24NRIIDI)

# adjust labels diseases - UK Biobank
UKB.0a1aNRIIDI.EUR <- set_trait_labels2(UKB.0a1aNRIIDI.EUR)
UKB.0b1bNRIIDI.EUR <- set_trait_labels2(UKB.0b1bNRIIDI.EUR)
UKB.1a2NRIIDI.EUR <- set_trait_labels2(UKB.1a2NRIIDI.EUR)
UKB.1b2NRIIDI.EUR <- set_trait_labels2(UKB.1b2NRIIDI.EUR)
UKB.24NRIIDI.EUR <- set_trait_labels2(UKB.24NRIIDI.EUR)

# Function to clean a single result df for NRI/IDI
clean_results2 <- function(df, comparison) {
  df %>%
    mutate(
      Events_NRI = format_auc(Events_NRI, Events_NRI_Lower, Events_NRI_Upper),
      NonEvents_NRI = format_auc(NonEvents_NRI, NonEvents_NRI_Lower, NonEvents_NRI_Upper),
      Overall_NRI = format_auc(Overall_NRI, Overall_NRI_Lower, Overall_NRI_Upper),
      IDI = format_auc(IDI, IDI_Lower, IDI_Upper),
      comparison = comparison
    ) %>%
    select(Trait, Biobank, comparison, Events_NRI, NonEvents_NRI, Overall_NRI,IDI)
}

# Clean each dataset with the appropriate model comparison tag
FGR11.0a1a.clean2 <- clean_results2(FGR11.0a1aNRIIDI, "0a_vs_1a")
FGR11.0b1b.clean2 <- clean_results2(FGR11.0b1bNRIIDI, "0b_vs_1b")
FGR11.1a2.clean2 <- clean_results2(FGR11.1a2NRIIDI, "1a_vs_2")
FGR11.1b2.clean2 <- clean_results2(FGR11.1b2NRIIDI, "1b_vs_2")
FGR11.24.clean2 <- clean_results2(FGR11.24NRIIDI, "2_vs_4")
#
UKB.0a1a.clean2 <- clean_results2(UKB.0a1aNRIIDI.EUR, "0a_vs_1a")
UKB.0b1b.clean2 <- clean_results2(UKB.0b1bNRIIDI.EUR, "0b_vs_1b")
UKB.1a2.clean2 <- clean_results2(UKB.1a2NRIIDI.EUR, "1a_vs_2")
UKB.1b2.clean2 <- clean_results2(UKB.1b2NRIIDI.EUR, "1b_vs_2")
UKB.24.clean2 <- clean_results2(UKB.24NRIIDI.EUR, "2_vs_4")

# combine all data in long format
TS20 <- bind_rows(FGR11.0b1b.clean2,FGR11.0a1a.clean2,FGR11.1b2.clean2,
                  FGR11.1a2.clean2,FGR11.24.clean2,UKB.0b1b.clean2,
                  UKB.0a1a.clean2,UKB.1b2.clean2,UKB.1a2.clean2,UKB.24.clean2)

# write file 
write.table(TS20, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable20.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 21: descriptive statistics FinnGen for Fine-Gray model for EA
#
################################################################################

# create Table TS21
TS21 <- FGR11.NFG
#adjust column names
names(TS21) <- c("Phenotype",names(TS21[2:19])) # trait = Phenotype
# adjust labels
TS21$Phenotype <- factor(TS21$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                    "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                    "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                    "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                    "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                    "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                    "K11_APPENDACUT","AUD_SWEDISH"),
                         labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                    "Gout","Rheumatoid Arthritis","Breast Cancer",
                                    "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                    "Coronary Heart Disease","Hip Osteoarthritis",
                                    "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                    "Major Depression","Any Cancer","Epilepsy",
                                    "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS21, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable21.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 22: Fine-Gray estimates model 4 in FinnGen for EA
#
################################################################################

# create table with model 4 results
TS22 <- data.frame(Test = c(rep("high educational attainment",nrow(FGR11.4FG)), rep("EAxPGS",nrow(FGR11.4FG)), rep("PGS",nrow(FGR11.4FG))),
                  Phenotype = c(rep(FGR11.4FG$trait,3)),
                  Beta = c(FGR11.4FG$EAhigh_beta,FGR11.4FG$interaction_beta,FGR11.4FG$PRS_beta),
                  SE = c(FGR11.4FG$EAhigh_se,FGR11.4FG$interaction_se,FGR11.4FG$PRS_se),
                  Pval = c(FGR11.4FG$EAhigh_p,FGR11.4FG$interaction_p,FGR11.4FG$PRS_p),
                  HR = c(FGR11.4FG$EAhigh_HR,FGR11.4FG$interaction_HR,FGR11.4FG$PRS_HR),
                  Cineg = c(FGR11.4FG$EAhigh_HR_lower95,FGR11.4FG$interaction_HR_lower,FGR11.4FG$PRS_HR_lower95),
                  Cipos = c(FGR11.4FG$EAhigh_HR_upper95,FGR11.4FG$interaction_HR_upper,FGR11.4FG$PRS_HR_upper95))
# adjust labels
TS22$Phenotype <- factor(TS22$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                  "K11_APPENDACUT","AUD_SWEDISH"),
                        labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                   "Gout","Rheumatoid Arthritis","Breast Cancer",
                                   "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                   "Major Depression","Any Cancer","Epilepsy",
                                   "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS22, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable22.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 23: Compare Cox and FG estimates model 4 in FinnGen for EA
#
################################################################################

# Create table 23
TS23 <- data.frame(Phenotype = c(rep(modCFsign$trait,3)),
                   Test = c(rep("High Education",nrow(modCFsign)),
                            rep("PGS",nrow(modCFsign)),
                            rep("EAxPGS",nrow(modCFsign))),
                   HR_CoxPH = c(modCFsign$Model1_HR),
                   HR_FG = c(modCFsign$Model2_HR),
                   beta_CoxPH = c(modCFsign$Model1b),
                   beta_FG = c(modCFsign$Model2b),
                   se_CoxPH = c(modCFsign$Model1se),
                   se_FG = c(modCFsign$Model2se),
                   HR_difference = c(modCFsign$difference),
                   beta_difference = c(modCFsign$Betadiff),
                   se_difference = c(modCFsign$SEDiff),
                   Pval_difference = c(modCFsign$PvalDiff))

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS23$lb_beta_difference <- TS23$beta_difference - 1.96*TS23$se_difference
TS23$ub_beta_difference <- TS23$beta_difference + 1.96*TS23$se_difference
#
TS23$lb_difference <- exp(TS23$lb_beta_difference)
TS23$ub_difference <- exp(TS23$ub_beta_difference)

# write file 
write.table(TS23, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable23.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 24: descriptive statistics non-EU ancestries UKB for EA
#
################################################################################

# combine ancestries
TS24 <- rbind(UKB.N.SAS,UKB.N.EAS,UKB.N.AFR)
#reorder columns
TS24 <- TS24[,c(1,23,2:22)]
#adjust column names
names(TS24) <- c("Phenotype","Ancestry",names(TS24[3:23])) # trait = Phenotype
# adjust labels
TS24$Phenotype <- factor(TS24$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                    "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                    "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                    "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                    "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                    "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                    "K11_APPENDACUT","AUD_SWEDISH"),
                         labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                    "Gout","Rheumatoid Arthritis","Breast Cancer",
                                    "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                    "Coronary Heart Disease","Hip Osteoarthritis",
                                    "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                    "Major Depression","Any Cancer","Epilepsy",
                                    "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS24, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable24.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 25: UKB non-EU ancestry results model 1 for EA
#
################################################################################

# create Table S25 
TS25 <- data.frame(Ancestry = c(UKB.1a.SAS$Biobank,UKB.1b.SAS$Biobank,
                                UKB.1a.EAS$Biobank,UKB.1b.EAS$Biobank,
                                UKB.1a.AFR$Biobank,UKB.1b.AFR$Biobank),
                  Test = c(rep("high educational attainment", nrow(UKB.1a.SAS)), 
                           rep("PGS",nrow(UKB.1b.SAS)), 
                           rep("high educational attainment", nrow(UKB.1a.EAS)), 
                           rep("PGS",nrow(UKB.1b.EAS)),
                           rep("high educational attainment", nrow(UKB.1a.AFR)), 
                           rep("PGS",nrow(UKB.1b.AFR))),
                  Phenotype = c(UKB.1a.SAS$trait,UKB.1b.SAS$trait,
                                UKB.1a.EAS$trait,UKB.1b.EAS$trait,
                                UKB.1a.AFR$trait,UKB.1b.AFR$trait),
                  Beta = c(UKB.1a.SAS$EAhigh_beta,UKB.1b.SAS$PRS_beta,
                           UKB.1a.EAS$EAhigh_beta,UKB.1b.EAS$PRS_beta,
                           UKB.1a.AFR$EAhigh_beta,UKB.1b.AFR$PRS_beta),
                  SE = c(UKB.1a.SAS$EAhigh_se,UKB.1b.SAS$PRS_se,
                         UKB.1a.EAS$EAhigh_se,UKB.1b.EAS$PRS_se,
                         UKB.1a.AFR$EAhigh_se,UKB.1b.AFR$PRS_se),
                  Pval = c(UKB.1a.SAS$EAhigh_p,UKB.1b.SAS$PRS_p,
                           UKB.1a.EAS$EAhigh_p,UKB.1b.EAS$PRS_p,
                           UKB.1a.AFR$EAhigh_p,UKB.1b.AFR$PRS_p),
                  HR = c(UKB.1a.SAS$EAhigh_HR,UKB.1b.SAS$PRS_HR,
                         UKB.1a.EAS$EAhigh_HR,UKB.1b.EAS$PRS_HR,
                         UKB.1a.AFR$EAhigh_HR,UKB.1b.AFR$PRS_HR),
                  Cineg = c(UKB.1a.SAS$EAhigh_HR_lower95,UKB.1b.SAS$PRS_HR_lower95,
                            UKB.1a.EAS$EAhigh_HR_lower95,UKB.1b.EAS$PRS_HR_lower95,
                            UKB.1a.AFR$EAhigh_HR_lower95,UKB.1b.AFR$PRS_HR_lower95),
                  Cipos = c(UKB.1a.SAS$EAhigh_HR_upper95,UKB.1b.SAS$PRS_HR_upper95,
                            UKB.1a.EAS$EAhigh_HR_upper95,UKB.1b.EAS$PRS_HR_upper95,
                            UKB.1a.AFR$EAhigh_HR_upper95,UKB.1b.AFR$PRS_HR_upper95))
# adjust labels
TS25$Phenotype <- factor(TS25$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                  "K11_APPENDACUT","AUD_SWEDISH"),
                        labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                   "Gout","Rheumatoid Arthritis","Breast Cancer",
                                   "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                   "Major Depression","Any Cancer","Epilepsy",
                                   "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS25, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable25.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 26: descriptive statistics FinnGen for occupation
#
################################################################################

# create Table TS26
TS26 <- FGR11.Nocc
#adjust column names
names(TS26) <- c("Phenotype",names(TS26[2:22])) # trait = Phenotype
# adjust labels
TS26$Phenotype <- factor(TS26$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                    "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                    "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                    "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                    "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                    "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                    "K11_APPENDACUT","AUD_SWEDISH"),
                         labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                    "Gout","Rheumatoid Arthritis","Breast Cancer",
                                    "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                    "Coronary Heart Disease","Hip Osteoarthritis",
                                    "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                    "Major Depression","Any Cancer","Epilepsy",
                                    "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS26, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_Occupation_eTable26.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 27: results model 1 FinnGen for occupation
#
################################################################################

# create Table S27 
TS27 <- data.frame(Test = c(rep("upper-level occupation",nrow(FGR11.1aocc)), rep("PGS",nrow(FGR11.1bocc))),
                   Phenotype = c(FGR11.1aocc$trait,FGR11.1bocc$trait),
                   Beta = c(FGR11.1aocc$`OccupationUpper-level_beta`,FGR11.1bocc$PRS_beta),
                   SE = c(FGR11.1aocc$`OccupationUpper-level_se`,FGR11.1bocc$PRS_se),
                   Pval = c(FGR11.1aocc$`OccupationUpper-level_p`,FGR11.1bocc$PRS_p),
                   HR = c(FGR11.1aocc$`OccupationUpper-level_HR`,FGR11.1bocc$PRS_HR),
                   Cineg = c(FGR11.1aocc$`OccupationUpper-level_HR_lower95`,FGR11.1bocc$PRS_HR_lower95),
                   Cipos = c(FGR11.1aocc$`OccupationUpper-level_HR_upper95`,FGR11.1bocc$PRS_HR_upper95))
# adjust labels
TS27$Phenotype <- factor(TS27$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                    "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                    "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                    "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                    "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                    "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                    "K11_APPENDACUT","AUD_SWEDISH"),
                         labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                    "Gout","Rheumatoid Arthritis","Breast Cancer",
                                    "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                    "Coronary Heart Disease","Hip Osteoarthritis",
                                    "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                    "Major Depression","Any Cancer","Epilepsy",
                                    "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS27, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable27.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 28: results model 2 FinnGen for occupation
#
################################################################################

# create Table S28 
TS28 <- data.frame(Test = c(rep("upper-level occupation",nrow(FGR11.2occ)), rep("PGS",nrow(FGR11.2occ))),
                  Phenotype = c(FGR11.2occ$trait,FGR11.2occ$trait),
                  Beta = c(FGR11.2occ$`OccupationUpper-level_beta`,FGR11.2occ$PRS_beta),
                  SE = c(FGR11.2occ$`OccupationUpper-level_se`,FGR11.2occ$PRS_se),
                  Pval = c(FGR11.2occ$`OccupationUpper-level_p`,FGR11.2occ$PRS_p),
                  HR = c(FGR11.2occ$`OccupationUpper-level_HR`,FGR11.2occ$PRS_HR),
                  Cineg = c(FGR11.2occ$`OccupationUpper-level_HR_lower95`,FGR11.2occ$PRS_HR_lower95),
                  Cipos = c(FGR11.2occ$`OccupationUpper-level_HR_upper95`,FGR11.2occ$PRS_HR_upper95))
# adjust labels
TS28$Phenotype <- factor(TS28$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                  "K11_APPENDACUT","AUD_SWEDISH"),
                        labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                   "Gout","Rheumatoid Arthritis","Breast Cancer",
                                   "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                   "Major Depression","Any Cancer","Epilepsy",
                                   "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS28, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable28.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 29: significance test differences effect occupation and PGS model 1vs2
#
################################################################################

# create Table S29
TS29 <- mod12sign.OCC[,c(1:12,14)]
# rename columns
names(TS29) <- c("Phenotype", "HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                "beta_difference","se_difference","Zval_difference","Pval_difference","Test")
# reorder
TS29 <- TS29[,c("Phenotype","Test","HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
              "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
              "beta_difference","se_difference","Pval_difference")]

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS29$lb_beta_difference <- TS29$beta_difference - 1.96*TS29$se_difference
TS29$ub_beta_difference <- TS29$beta_difference + 1.96*TS29$se_difference
#
TS29$lb_difference <- exp(TS29$lb_beta_difference)
TS29$ub_difference <- exp(TS29$ub_beta_difference)

# adjust label Test
TS29$Test[which(TS29$Test=="PRS")] <- "PGS"

# write file 
write.table(TS29, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable29.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 30: results model 3 FinnGen for occupation
#
################################################################################

# Create Table S30
TS30 <- data.frame(OccupationGroup = c(FGR11.3occ$Test),
                  Phenotype = c(FGR11.3occ$trait),
                  Beta = c(FGR11.3occ$PRS_beta),
                  SE = c(FGR11.3occ$PRS_se),
                  Pval = c(FGR11.3occ$PRS_p),
                  HR = c(FGR11.3occ$PRS_HR),
                  Cineg = c(FGR11.3occ$PRS_HR_lower95),
                  Cipos = c(FGR11.3occ$PRS_HR_upper95))
# adjust labels
TS30$OccupationGroup[which(TS30$OccupationGroup=="Lower-level")] <- "Lower-level occupation"
TS30$OccupationGroup[which(TS30$OccupationGroup=="Upper-level")] <- "Upper-level occupation"
#
TS30$Phenotype <- factor(TS30$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                  "K11_APPENDACUT","AUD_SWEDISH"),
                        labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                   "Gout","Rheumatoid Arthritis","Breast Cancer",
                                   "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                   "Major Depression","Any Cancer","Epilepsy",
                                   "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS30, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable30.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 31: results model 4 FinnGen for occupation
#
################################################################################

# create combined table with model 4 results
TS31 <- data.frame(Test = c(rep("Upper-level occupation",nrow(FGR11.4occ)), 
                            rep("OccupationxPGS",nrow(FGR11.4occ)), 
                            rep("PGS",nrow(FGR11.4occ))),
                  Phenotype = c(rep(FGR11.4occ$trait,3)),
                  Beta = c(FGR11.4occ$`OccupationUpper-level_beta`,FGR11.4occ$`PRS:OccupationUpper-level_beta`,FGR11.4occ$PRS_beta),
                  SE = c(FGR11.4occ$`OccupationUpper-level_se`,FGR11.4occ$`PRS:OccupationUpper-level_se`,FGR11.4occ$PRS_se),
                  Pval = c(FGR11.4occ$`OccupationUpper-level_p`,FGR11.4occ$`PRS:OccupationUpper-level_p`,FGR11.4occ$PRS_p),
                  HR = c(FGR11.4occ$`OccupationUpper-level_HR`,FGR11.4occ$`PRS:OccupationUpper-level_HR`,FGR11.4occ$PRS_HR),
                  Cineg = c(FGR11.4occ$`OccupationUpper-level_HR_lower95`,FGR11.4occ$`PRS:OccupationUpper-level_HR_lower95`,FGR11.4occ$PRS_HR_lower95),
                  Cipos = c(FGR11.4occ$`OccupationUpper-level_HR_upper95`,FGR11.4occ$`PRS:OccupationUpper-level_HR_upper95`,FGR11.4occ$PRS_HR_upper95))
# adjust labels
TS31$Phenotype <- factor(TS31$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                  "K11_APPENDACUT","AUD_SWEDISH"),
                        labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                   "Gout","Rheumatoid Arthritis","Breast Cancer",
                                   "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                   "Major Depression","Any Cancer","Epilepsy",
                                   "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS31, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable31.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 32: descriptive statistics FinnGen model 6 for occupation
#
################################################################################

# create Table TS25
TS32 <- FGR11.N6occ
#adjust column names
names(TS32) <- c("Phenotype",names(TS32[2:82])) # trait = Phenotype
# adjust labels
TS32$Phenotype <- factor(TS32$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                    "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                    "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                    "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                    "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                    "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                    "K11_APPENDACUT","AUD_SWEDISH"),
                         labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                    "Gout","Rheumatoid Arthritis","Breast Cancer",
                                    "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                    "Coronary Heart Disease","Hip Osteoarthritis",
                                    "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                    "Major Depression","Any Cancer","Epilepsy",
                                    "Appendicitis","Alcohol Use Disorder"))

# write file 
write.table(TS32, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable32.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 33: results model 6 FinnGen for occupation
#
################################################################################

# create table S33
TS33 <- data.frame(Test = c(rep("0_20p",nrow(FGR11.6occ)), rep("20_40p",nrow(FGR11.6occ)), 
                            rep("60_95p",nrow(FGR11.6occ)), rep("95_1000p",nrow(FGR11.6occ))),
                   Phenotype = c(rep(FGR11.6occ$trait,4)),
                   Beta = c(FGR11.6occ$`PGS_groupGroup 1_beta`,FGR11.6occ$`PGS_groupGroup 2_beta`,
                            FGR11.6occ$`PGS_groupGroup 4_beta`,FGR11.6occ$`PGS_groupGroup 5_beta`),
                   SE = c(FGR11.6occ$`PGS_groupGroup 1_se`,FGR11.6occ$`PGS_groupGroup 2_se`,
                          FGR11.6occ$`PGS_groupGroup 4_se`,FGR11.6occ$`PGS_groupGroup 5_se`),
                   Pval = c(FGR11.6occ$`PGS_groupGroup 1_p`,FGR11.6occ$`PGS_groupGroup 2_p`,
                            FGR11.6occ$`PGS_groupGroup 4_p`,FGR11.6occ$`PGS_groupGroup 5_p`),
                   HR = c(FGR11.6occ$`PGS_groupGroup 1_HR`,FGR11.6occ$`PGS_groupGroup 2_HR`,
                          FGR11.6occ$`PGS_groupGroup 4_HR`, FGR11.6occ$`PGS_groupGroup 5_HR`),
                   Cineg = c(FGR11.6occ$`PGS_groupGroup 1_HR_lower95`, FGR11.6occ$`PGS_groupGroup 2_HR_lower95`,
                             FGR11.6occ$`PGS_groupGroup 4_HR_lower95`, FGR11.6occ$`PGS_groupGroup 5_HR_lower95`),
                   Cipos = c(FGR11.6occ$`PGS_groupGroup 1_HR_upper95`,FGR11.6occ$`PGS_groupGroup 2_HR_upper95`,
                             FGR11.6occ$`PGS_groupGroup 4_HR_upper95`,FGR11.6occ$`PGS_groupGroup 5_HR_upper95`))
# adjust labels
TS33$Phenotype <- factor(TS33$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                    "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                    "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                    "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                    "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                    "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                    "K11_APPENDACUT","AUD_SWEDISH"),
                         labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                    "Gout","Rheumatoid Arthritis","Breast Cancer",
                                    "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                    "Coronary Heart Disease","Hip Osteoarthritis",
                                    "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                    "Major Depression","Any Cancer","Epilepsy",
                                    "Appendicitis","Alcohol Use Disorder"))


# write file 
write.table(TS33, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable33.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 34: FinnGen results bootstrapped cumulative incidences for occupation
#
################################################################################

# create Table S34
TS34 <- data.frame(Test = c(rep("Lower-level occupation",640), 
                            rep("Upper-level occupation",640)),
                   Phenotype = c(rep(rep(c("Prostate Cancer","Type 2 Diabetes",
                                           "Atrial Fibrillation","Asthma","Coronary Heart Disease",
                                           "Hip Osteoarthritis","Knee Osteoarthritis",
                                           "Any Cancer"),each = 80),2)),
                   Age = c(LO.PC$age, LO.T2D$age, LO.AF$age, LO.AS$age,
                           LO.CHD$age, LO.HO$age, LO.KO$age, LO.AC$age, 
                           UL.PC$age, UL.T2D$age, UL.AF$age, UL.AS$age,
                           UL.CHD$age, UL.HO$age, UL.KO$age, UL.AC$age),
                   Group = c(LO.PC$Group, LO.T2D$Group, LO.AF$Group, 
                             LO.AS$Group,LO.CHD$Group, LO.HO$Group, LO.KO$Group,
                             LO.AC$Group,UL.PC$Group, UL.T2D$Group, 
                             UL.AF$Group, UL.AS$Group,UL.CHD$Group, UL.HO$Group, 
                             UL.KO$Group, UL.AC$Group),
                   LifetimeRisk = c(LO.PC$LifetimeRisk, 
                                    LO.T2D$LifetimeRisk, LO.AF$LifetimeRisk, 
                                    LO.AS$LifetimeRisk,LO.CHD$LifetimeRisk, 
                                    LO.HO$LifetimeRisk, LO.KO$LifetimeRisk, 
                                    LO.AC$LifetimeRisk,
                                    UL.PC$LifetimeRisk, UL.T2D$LifetimeRisk, 
                                    UL.AF$LifetimeRisk, UL.AS$LifetimeRisk,
                                    UL.CHD$LifetimeRisk, UL.HO$LifetimeRisk, 
                                    UL.KO$LifetimeRisk, UL.AC$LifetimeRisk),
                   CIneg = c(LO.PC$CIneg, LO.T2D$CIneg, LO.AF$CIneg, 
                             LO.AS$CIneg,LO.CHD$CIneg, LO.HO$CIneg, LO.KO$CIneg, 
                             LO.AC$CIneg,UL.PC$CIneg, UL.T2D$CIneg, 
                             UL.AF$CIneg, UL.AS$CIneg,UL.CHD$CIneg, UL.HO$CIneg, 
                             UL.KO$CIneg, UL.AC$CIneg),
                   CIpos = c(LO.PC$CIpos, LO.T2D$CIpos, LO.AF$CIpos, 
                             LO.AS$CIpos,LO.CHD$CIpos, LO.HO$CIpos, LO.KO$CIpos, 
                             LO.AC$CIpos,UL.PC$CIpos, UL.T2D$CIpos, 
                             UL.AF$CIpos, UL.AS$CIpos,UL.CHD$CIpos, UL.HO$CIpos, 
                             UL.KO$CIpos, UL.AC$CIpos))


# write file 
write.table(TS34, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable34.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
