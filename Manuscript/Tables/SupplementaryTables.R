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
#      19) eTable 22: results model 1: per cohort + meta-analysis for occupation
#      20) eTable 23: results model 2: per cohort + meta-analysis for occupation
#      21) eTable 24: significance test differences effect occupation and PGS model 1vs2
#      22) eTable 25: significance test differences EA vs occupation model 1
#      23) eTable 26: results model 3: per cohort + meta-analysis for occupation
#      24) eTable 27: results model 4: per cohort + meta-analysis for occupation
#      25) eTable 28: significance test differences EA vs occupation model 3
#      26) eTable 29: descriptive statistics FinnGen model 6 for occupation
#      27) eTable 30: results model 6 FinnGen for occupation
#      28) eTable 31: FinnGen results bootstrapped cumulative incidences for occupation
#      29) eTable 32: descriptive statistics per cohort for EA with cohort-specific cutoff
#      30) eTable 33: results model 4 (per cohort + meta-analysis) for EA with cohort-specific cutoff
#      31) eTable 34: significance test differences model 4 EA with same vs cohort-specific cuttoff
#      32) eTable 35: results model 4 (per cohort + meta-analysis) for EA without dichotomization
#      33) eTable 36: significance test differences model 4 EA with and without dichotomization
#      34) eTable 37: descriptive statistics FinnGen when excl. cases before cohort entry for EA
#      35) eTable 38: results model 4 FinnGen EA when excl. cases before cohort entry
#      36) eTable 39: significance test difference model 4 EA in full FinnGen vs after excl. cases before cohort entry
#      37) eTable 40: descriptive statistics FinnGen with mutual exclussive cases for EA
#      38) eTable 41: results model 4 FinnGen EA with mutual exclussive cases
#      39) eTable 42: significance test difference model 4 EA in full FinnGen vs with mutaual exclussive cases
#      40) eTable 43: results model 4 FinnGen EA when including EA PGS + its interaction with disease PGS
#      41) eTable 44: significance test difference model 4 EA in original FinnGen vs including EA PGS + interaction
#      42) eTable 45: descriptive statistics FinnGen for Fine-Gray model for EA
#      43) eTable 46: Fine-Gray estimates model 4 in FinnGen for EA
#      44) eTable 47: Compare Cox and FG estimates model 4 in FinnGen for EA
#      45) eTable 48: descriptive statistics non-EU ancestries UKB for EA
#      46) eTable 49: UKB non-EU ancestry results model 1 for EA
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
#      18) FGR11 + UKB + GS descriptive statistics for occupation
#      19) FGR11 + UKB + GS + meta-analysis model 1a+b for occupation
#      20) FGR11 + UKB + GS + meta-analysis model 2 for occupation
#      21) significance test differences effect occupation and PGS model 1vs2
#      22) FGR111 + UKB + GS + meta-analysis model 3 for occupation
#      23) FGR11 + UKB + GS + meta-analysis model 4 for occupation 
#      24) significance test differences effect PGS in education vs occupation model
#      25) FGR11 descriptive statistics for model 6 for occupation
#      26) FGR11 model 6 for occupation
#      27) FinnGen results bootstrapped cumulative incidences for occupation 
#      28) UKB descriptive statistics education with cohort-specific cut-off
#      29) UKB + meta-analysis model 4 for EA with cohort-specific cutoff
#      30) significance test differences model 4 education with same vs cohort-specific cutoff
#      31) FGR11 + UKB + GS + meta-analysis model 4 for EA without dichotomization
#      32) significance test difference model 4 education with and without dichotomization
#      33) FGR11 descriptive statistics education when excl. cases before cohort entry
#      34) FGR11 model 4 results for EA when excl. cases before cohort entry
#      35) significance test differences model 4 education full FinnGen vs after excl. cases before cohort entry
#      36) FGR11 descriptive statistics education with mutual exclusive cases
#      37) FGR11 model 4 results for EA with mutual exclusive cases
#      38) significance test differences model 4 education full FinnGen vs with mutual exclusive cases only 
#      39) FGR11 model 4 results for EA including EA PGS + interaction
#      40) significance test differences model 4 education original FinnGen vs including EA PGS + interaction
#      41) FGR11 descriptive statistics education for fine-gray models
#      42) FGR11 model 4 Fine-Gray results for EA
#      43) significance test difference effects Cox vs Fine-Gray in FGR11 for EA
#      44) UKB non-EU ancestries descriptive statistics for EA
#      45) UKB non-EU ancestries results model 1 for EA 
#
# Last edits: 20/04/2026 (edits, FAH: add additional sensitivity analyses)
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
# efficiently reading in large data sets; dplyr, tidyr, forcats & stringr = data
# wrangling; readxl = read excel files (upload to googledrive converts csv to
# xlsx).
packages("data.table","dplyr","forcats","stringr","readxl","tidyr")


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
FGR11.N80 <- fread("output/2classEA/FG11/2026-02-25_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11_80percent.txt",data.table=FALSE) # descriptives for analyses on education in random 80% sample
FGR11.N20 <- fread("output/2classEA/FG11/2026-02-25_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11_20percent.txt",data.table=FALSE) # descriptives for analyses on education in random 20% sample
FGR11.N20$Biobank <- "FinnGen (20%)"
FGR11.Nocc <- fread("output/EmploymentStatus/FG11/2025-01-30_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11_Occupation_MANLOWCOMB.txt",data.table=FALSE) # descriptives for analyses on occupation
FGR11.Nocc$Biobank <- "FinnGen"
FGR11.N6occ <- fread("output/EmploymentStatus/FG11/2025-03-14_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11_Occupation_mod6.txt",data.table=FALSE) # descriptives for analyses on occupation by PGS strata
FGR11.Nbb <- fread("output/2classEA/FG11/2026-02-10_INTERVENE_SESDiffDiseases_SampleDescriptives_nocasesbeforeBlood_FinnGenR11.txt",data.table=FALSE) # descriptive for analyses on education after excl. cases before cohort entry
FGR11.Nbb$Biobank <- "FinnGen"
FGR11.Nmc <- fread("output/2classEA/FG11/2026-02-11_INTERVENE_SESDiffDiseases_SampleDescriptives_NoMutCases_FinnGenR11.txt",data.table=FALSE) # descriptive for analyses on education with mututal exclussive cases
FGR11.Nmc$Biobank <- "FinnGen"
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
UKB.N.EURocc <- fread("output/EmploymentStatus/UKB/2026-02-17_UKBiobank_INTERVENE_SESDiffDiseases_SampleDescriptives_Occupation.txt",data.table=FALSE) # descriptives for analyses on occupation in EU ancestry
UKB.N.EURocc$Biobank <- "UK Biobank"
UKB.N.EURcs <- fread("output/2classEA/UKB/2026-02-16_UKBiobank_EUR_INTERVENE_SESDiffDiseases_SampleDescriptives_CohortSpecificThreshold_EducationalAttainment.txt",data.table=FALSE) # descriptives for analyses on education with cohort-specific cutoff in EU ancestry
UKB.N.EURcs$Biobank <- "UK Biobank"
# Generation Scotland
GS.N <- fread("output/2classEA/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_SampleDescriptives.txt",data.table=FALSE) # descriptives for analyes on education
GS.N$Biobank <- "Generation Scotland"
GS.Nocc <- fread("output/EmploymentStatus/GS/2026-02-27_GS_INTERVENE_Occupation_SampleDescriptives.txt",data.table=FALSE) # descriptives for analyes on occupation
GS.Nocc$Biobank <- "Generation Scotland"

## read in model 0a or ob in 80% FinnGen
FGR11.0a80 <- fread("output/2classEA/FG11/LogReg/2025-06-13_INTERVENE_SESDiffDiseases_Coeffs_glm_by_model0a_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample
FGR11.0b80 <- fread("output/2classEA/FG11/LogReg/2025-06-13_INTERVENE_SESDiffDiseases_Coeffs_glm_by_model0b_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample

## read in model 1a - EA or occupation only ##
# FinnGen
FGR11.1a <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt", data.table=FALSE) # results for analyses on education
FGR11.1a$Biobank <- "FinnGen"
FGR11.1a80 <- fread("output/2classEA/FG11/CoxPropHaz_model1/2026-02-25_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model1a_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample
FGR11.1aocc <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model1/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model1a_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation
FGR11.1aocc$Biobank <- "FinnGen"
# UK Biobank
UKB.1a.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on education in EU ancestry
UKB.1a.EUR$Biobank <- "UK Biobank"
UKB.1a.SAS <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_SAS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on education in SAS ancestry
UKB.1a.SAS$Biobank <- "SAS"
UKB.1a.EAS <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EAS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on education in EAS ancestry
UKB.1a.EAS$Biobank <- "EAS"
UKB.1a.AFR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_AFR_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on education in AFR ancestry
UKB.1a.AFR$Biobank <- "AFR"
UKB.1a.EURocc <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on occupation in EU ancestry
UKB.1a.EURocc$Biobank <- "UK Biobank"
# Generation Scotland
GS.1a <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on education
GS.1a$Biobank <- "Generation Scotland"
GS.1aocc <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model1a_Coeffs.txt",data.table=FALSE) # results for analyses on occupation
GS.1aocc$Biobank <- "Generation Scotland"
# Meta-analysis FinnGen + UK Biobank + Generation Scotland
FEMA.1a <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.xlsx")) # results for analyses on education
FEMA.1a$Biobank <- "FE meta-analysis"
FEMA.1aocc <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv",data.table = FALSE) # results for analyses on occupation
FEMA.1aocc$Biobank <- "FE meta-analysis"
#
REMA.1a <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv",data.table = FALSE)
REMA.1a$Biobank <- "RE meta-analysis"
REMA.1aocc <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv",data.table = FALSE) # results for analyses on occupation
REMA.1aocc$Biobank <- "RE meta-analysis"

## significance differences education vs occupation model 1 meta-analysis ##
mod1sign.FE.EAOCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-24_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1_EAvsOccupationDifferences.csv", data.table=FALSE) 
mod1sign.RE.EAOCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-24_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1_EAvsOccupationDifferences.csv", data.table=FALSE) 

## read in model 1b - PGS only (education or occupation models) ##
# FinnGen
FGR11.1b <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt", data.table=FALSE) # results for analyses on education
FGR11.1b$Biobank <- "FinnGen"
FGR11.1b80 <- fread("output/2classEA/FG11/CoxPropHaz_model1/2026-02-25_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model1b_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample
FGR11.1bocc <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model1/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model1b_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation
FGR11.1bocc$Biobank <- "FinnGen"
# UK Biobank
UKB.1b.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on education in EU ancestry
UKB.1b.EUR$Biobank <- "UK Biobank"
UKB.1b.SAS <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on education in SAS ancestry
UKB.1b.SAS$Biobank <- "SAS"
UKB.1b.EAS <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on education in EAS ancestry
UKB.1b.EAS$Biobank <- "EAS"
UKB.1b.AFR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on education in AFR ancestry
UKB.1b.AFR$Biobank <- "AFR"
UKB.1b.EURocc <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on occupation in EU ancestry
UKB.1b.EURocc$Biobank <- "UK Biobank"
# Generation Scotland
GS.1b <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on education
GS.1b$Biobank <- "Generation Scotland"
GS.1bocc <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model1b_Coeffs.txt",data.table=FALSE) # results for analyses on occupation 
GS.1bocc$Biobank <- "Generation Scotland"
# Meta-analysis FinnGen + UK Biobank + Generation Scotland
FEMA.1b <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.xlsx")) # results for analyses on education
FEMA.1b$Biobank <- "FE meta-analysis"
FEMA.1bocc <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv", data.table = FALSE) # results for analyses on occupation
FEMA.1bocc$Biobank <- "FE meta-analysis"
#
REMA.1b <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv", data.table = FALSE)
REMA.1b$Biobank <- "RE meta-analysis"
REMA.1bocc <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv", data.table = FALSE) # results for analyses on occupation
REMA.1bocc$Biobank <- "RE meta-analysis"

## read in model 2 education or occupation ##
# FinnGen
FGR11.2 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt", data.table=FALSE) # results for analyses on education
FGR11.2$Biobank <- "FinnGen"
FGR11.280 <- fread("output/2classEA/FG11/CoxPropHaz_model2/2026-02-25_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model2c_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample
FGR11.2occ <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model2/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model2_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation
FGR11.2occ$Biobank <- "FinnGen"
# UK Biobank
UKB.2.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE) # results for analyses on education in EU ancestry
UKB.2.EUR$Biobank <- "UK Biobank"
UKB.2.EURocc <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model2_Coeffs.txt",data.table=FALSE) # results for analyses on occupation in EU ancestry
UKB.2.EURocc$Biobank <- "UK Biobank"
# Generation Scotland
GS.2 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE) # results for analyses on education
GS.2$Biobank <- "Generation Scotland"
GS.2occ <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model2_Coeffs.txt",data.table=FALSE) # results for analyses on occupation
GS.2occ$Biobank <- "Generation Scotland"
# Meta-analysis FinnGen + UK Biobank + Generation Scotland
FEMA.2 <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2.xlsx")) # results for analyses on education
FEMA.2$Biobank <- "FE meta-analysis"
FEMA.2occ <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2.csv", data.table = FALSE) # results for analyses on occupation
FEMA.2occ$Biobank <- "FE meta-analysis"
#
REMA.2 <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model2.csv", data.table = FALSE)
REMA.2$Biobank <- "RE meta-analysis"
REMA.2occ <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model2.csv", data.table = FALSE) # results for analyses on occupation
REMA.2occ$Biobank <- "RE meta-analysis"

## significance differences effect education/occupation and PGS model 1 vs 2 meta-analysis ##
mod12sign.EA <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.xlsx")) # results for models on education meta-analyses
mod12sign.OCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv", data.table=FALSE) # results for models on occupation meta-analyses
#
mod12sign.RE.EA <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv", data.table = FALSE)
mod12sign.RE.OCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv", data.table=FALSE) # results for models on occupation meta-analyses

## read in model 3 education or occupation ##
# FinnGen
FGR11.3 <- fread("output/GoogleDrive/FGR11/2025-02-07_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt", data.table=FALSE) # results for analyses on education
FGR11.3$Biobank <- "FinnGen"
FGR11.3occ <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model3/2025-02-12_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model3b_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation
FGR11.3occ$Biobank <- "FinnGen"
# UK Biobank
UKB.3.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE) # results for analyses on education in EU ancestry
UKB.3.EUR$Biobank <- "UK Biobank"
UKB.3.EURocc <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model3_Coeffs.txt",data.table=FALSE) # results for analyses on occupation in EU ancestry
UKB.3.EURocc$Biobank <- "UK Biobank"
# Generation Scotland
GS.3 <- fread("output/GoogleDrive/GS/2025-02-24_GS_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE) # results for analyses on education
GS.3$Test <- c(rep("LowEA",length(unique(GS.3$trait))),rep("HighEA",length(unique(GS.3$trait)))) # in initial script shared with GS forgot a line of code to add this, so now add it manually. 
GS.3$Biobank <- "Generation Scotland"
GS.3occ <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model3_Coeffs.txt",data.table=FALSE) # results for analyses on occupation
GS.3occ$Biobank <- "Generation Scotland"
# Meta-analysis FinnGen + UK Biobank + Generation Scotland
FEMA.3 <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3.xlsx")) # results for analyses on education
FEMA.3$Biobank <- "FE meta-analysis"
FEMA.3occ <- fread("output/GoogleDrive/MetaAnalysis/2026-03-13_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3.csv", data.table = FALSE) # results for analyses on occupation
FEMA.3occ$Biobank <- "FE meta-analysis"
#
REMA.3 <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model3.csv", data.table = FALSE)
REMA.3$Biobank <- "RE meta-analysis"
REMA.3occ <- fread("output/GoogleDrive/MetaAnalysis/2026-03-13_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model3.csv", data.table = FALSE) # results for analyses on occupation
REMA.3occ$Biobank <- "RE meta-analysis"

## significance differences education vs occupation model 3 meta-analysis ##
mod3sign.FE.EAOCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-24_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_EAvsOccupationDifferences.csv", data.table=FALSE) 
mod3sign.RE.EAOCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-24_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model3_EAvsOccupationDifferences.csv", data.table=FALSE) 

## read in model 4 - traditional GxEA/OCC interaction model ##
# FinnGen
FGR11.4 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt", data.table=FALSE) # results for analyses on education
FGR11.4$Biobank <- "FinnGen"
FGR11.480 <- fread("output/2classEA/FG11/CoxPropHaz_model4/2026-02-25_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model4c_FinnGenR11_80percent.txt", data.table=FALSE) # results for analyses on education in random 80% sample
FGR11.4FG <- fread("output/2classEA/FG11/CoxPropHaz_model4/2025-03-14_INTERVENE_SESDiffDiseases_Coeffs_FineGray_model4_FinnGenR11.txt", data.table=FALSE) # results for Fine-Gray analyses on education 
FGR11.4occ <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model4/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model4_FinnGenR11.txt", data.table=FALSE) # results for analyses on occupation
FGR11.4occ$Biobank <- "FinnGen"
FGR11.4all <- fread("output/GoogleDrive/FGR11/2026-02-10_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model4_ISCEDcont_FinnGenR11.txt", data.table=FALSE) # results for analyses on education without dichotomization
FGR11.4all$Biobank <- "FinnGen"
FGR11.4bb <- fread("output/GoogleDrive/FGR11/2026-02-10_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model4_nocasesbeforeBlood_FinnGenR11.txt", data.table=FALSE) # results for analyses on education after excl. cases before cohort entry
FGR11.4bb$Biobank <- "FinnGen"
FGR11.4mc <- fread("output/GoogleDrive/FGR11/2026-02-11_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model4c_NoMutCases_FinnGenR11.txt", data.table=FALSE) # results for analyses on education with mutual exclusive cases
FGR11.4mc$Biobank <- "FinnGen"
FGR11.4gg <- fread("output/GoogleDrive/FGR11/2026-02-11_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model4_GxG_FinnGenR11.txt", data.table=FALSE) # results for analyses on education incl. EA PGS + interaction with disease-specific PGS
FGR11.4gg$Biobank <- "FinnGen"
# Uk Biobank
UKB.4.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
UKB.4.EUR$Biobank <- "UK Biobank"
UKB.4.EURocc <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model4_Coeffs.txt",data.table=FALSE) # results for analyses on occupation
UKB.4.EURocc$Biobank <- "UK Biobank"
UKB.4.EURcs <- fread("output/GoogleDrive/UKB/2026-02-16_UKBiobank_EUR_INTERVENE_EducationalAttainment_CohortSpecificThreshold_CoxPH_model4_Coeffs.txt",data.table=FALSE) # results for analyses on education with cohort-specific cutoff
UKB.4.EURcs$Biobank <- "UK Biobank"
UKB.4.EURall <- fread("output/GoogleDrive/UKB/2026-02-16_UKBiobank_EUR_INTERVENE_EducationalAttainment_allISCED_CoxPH_model4_Coeffs.txt",data.table=FALSE) # results for analyses on education without dichotomization
UKB.4.EURall$Biobank <- "UK Biobank"
# Generation Scotland
GS.4 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
GS.4$Biobank <- "Generation Scotland"
GS.4occ <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model4_Coeffs.txt",data.table=FALSE) # results for analyses on occupation
GS.4occ$Biobank <- "Generation Scotland"
GS.4all <- fread("output/GoogleDrive/GS/2026-03-02_GS_INTERVENE_EducationalAttainment_allISCED_CoxPH_model4_Coeffs.txt",data.table=FALSE) # results for analyses on education without dichotomization
GS.4all$Biobank <- "Generation Scotland"
# Meta-analysis FinnGen + UK Biobank + Generation Scotland
FEMA.4 <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4.xlsx"))
FEMA.4$Biobank <- "FE meta-analysis"
FEMA.4occ <- fread("output/GoogleDrive/MetaAnalysis/2026-03-13_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv", data.table = FALSE) # results for analyses on occupation
FEMA.4occ$Biobank <- "FE meta-analysis"
FEMA.4cs <- fread("output/GoogleDrive/MetaAnalysis/2026-03-11_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4_CohortSpecificThreshold.csv", data.table = FALSE) # results for analyses on education with cohort-specific cutoff
FEMA.4cs$Biobank <- "FE meta-analysis"
FEMA.4all <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4_allISCED.csv", data.table = FALSE) # results for analyses on education without dichotomization
FEMA.4all$Biobank <- "FE meta-analysis"
#
REMA.4 <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv", data.table = FALSE)
REMA.4$Biobank <- "RE meta-analysis"
REMA.4occ <- fread("output/GoogleDrive/MetaAnalysis/2026-03-13_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv", data.table = FALSE) # results for analyses on occupation
REMA.4occ$Biobank <- "RE meta-analysis"
REMA.4cs <- fread("output/GoogleDrive/MetaAnalysis/2026-03-11_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4_CohortSpecificThreshold.csv", data.table = FALSE) # results for analyses on education with cohort-specific cutoff
REMA.4cs$Biobank <- "RE meta-analysis"
REMA.4all <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4_allISCED.csv", data.table = FALSE) # results for analyses on education without dichotomization
REMA.4all$Biobank <- "RE meta-analysis"

## significance differences model 4 Educational Attainment Meta-analysis with and without cohort-specific cutoff ##
FEmod4cssign <- fread("output/GoogleDrive/MetaAnalysis/2026-03-18_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_OrgvsSpecific_Differences.csv", data.table=FALSE)
REmod4cssign <- fread("output/GoogleDrive/MetaAnalysis/2026-03-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_OrgvsSpecific_Differences.csv", data.table=FALSE)

## significance differences model 4 Educational Attainment Meta-analysis with and without dichotomization ##
FEmod4allsign <- fread("output/GoogleDrive/MetaAnalysis/2026-03-18_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_BinvsCont_Differences.csv", data.table=FALSE)
REmod4allsign <- fread("output/GoogleDrive/MetaAnalysis/2026-03-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_BinvsCont_Differences.csv", data.table=FALSE)

## significance differences model 4 FinnGen full sample vs after excl. cases before cohort entry ##
modbbsign <- fread("output/2classEA/FG11/CoxPropHaz_model4/2026-03-03_INTERVENE_SESDiffDiseases_Differences_FullFinnGenR11vsNocasebeforeBlood_model4.csv", data.table=FALSE)

## significance differences model 4 FinnGen full sample vs mutual exclusive cases ##
modmcsign <- fread("output/2classEA/FG11/CoxPropHaz_model4/2026-03-24_INTERVENE_SESDiffDiseases_Differences_FullFinnGenR11vsNoMutcases_model4.csv", data.table=FALSE)

## significance differences model 4 FinnGen original results vs incl. EA PGS + its interaction with disease-specific PGS ##
modggsign <- fread("output/2classEA/FG11/CoxPropHaz_model4/2026-03-03_INTERVENE_SESDiffDiseases_Differences_NoGxGvsGxG_model4.csv", data.table=FALSE)

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
FGR11.0a1aAUC <- fread("output/Prediction/FinnGen/2026-03-31_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparisonCox_Model0a-1a_by.txt",data.table=FALSE) # results model 1a vs 2
FGR11.0a1aAUC$Biobank <- "FinnGen"
FGR11.0b1bAUC <- fread("output/Prediction/FinnGen/2026-03-31_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparisonCox_Model0b-1b_by.txt",data.table=FALSE) # results model 1b vs 2
FGR11.0b1bAUC$Biobank <- "FinnGen"
FGR11.1a2AUC <- fread("output/Prediction/FinnGen/2026-03-31_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparisonCox_Model1a-2_by.txt",data.table=FALSE) # results model 1a vs 2
FGR11.1a2AUC$Biobank <- "FinnGen"
FGR11.1b2AUC <- fread("output/Prediction/FinnGen/2026-03-31_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparisonCox_Model1b-2_by.txt",data.table=FALSE) # results model 1b vs 2
FGR11.1b2AUC$Biobank <- "FinnGen"
FGR11.24AUC <- fread("output/Prediction/FinnGen/2026-03-31_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparisonCox_Model2-4_by.txt",data.table=FALSE) # results model 2 vs 4
FGR11.24AUC$Biobank <- "FinnGen"
# UK Biobank
UKB.0a1aAUC.EUR <- fread("output/Prediction/UKB/2026-04-02_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model0a-1a_by.txt",data.table=FALSE) # results model 1a vs 2 in EU ancestry
UKB.0a1aAUC.EUR$Biobank <- "UKBiobank"
UKB.0b1bAUC.EUR <- fread("output/Prediction/UKB/2026-04-02_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model0b-1b_by.txt",data.table=FALSE) # results model 1b vs 2 in EU ancestry
UKB.0b1bAUC.EUR$Biobank <- "UKBiobank"
UKB.1a2AUC.EUR <- fread("output/Prediction/UKB/2026-04-01_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model1a-2_by.txt",data.table=FALSE) # results model 1a vs 2 in EU ancestry
UKB.1a2AUC.EUR$Biobank <- "UKBiobank"
UKB.1b2AUC.EUR <- fread("output/Prediction/UKB/2026-04-01_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model1b-2_by.txt",data.table=FALSE) # results model 1b vs 2 in EU ancestry
UKB.1b2AUC.EUR$Biobank <- "UKBiobank"
UKB.24AUC.EUR <- fread("output/Prediction/UKB/2026-04-01_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model2-4_by.txt",data.table=FALSE) # results model 2 vs 2 in EU ancestry
UKB.24AUC.EUR$Biobank <- "UKBiobank"

## read in NRI/IDI per cohort ##
# FinnGen
FGR11.0a1aNRIIDI <- fread("output/Prediction/FinnGen/2026-03-31_FinnGen_20percent_INTERVENE_EducationalAttainment_Cox_NRI_IDI_Model0a-1a_by.txt",data.table=FALSE) # results model 1a vs 2
FGR11.0a1aNRIIDI$Biobank <- "FinnGen"
FGR11.0b1bNRIIDI <- fread("output/Prediction/FinnGen/2026-03-31_FinnGen_20percent_INTERVENE_EducationalAttainment_Cox_NRI_IDI_Model0b-1b_by.txt",data.table=FALSE) # results model 1b vs 2
FGR11.0b1bNRIIDI$Biobank <- "FinnGen"
FGR11.1a2NRIIDI <- fread("output/Prediction/FinnGen/2026-03-31_FinnGen_20percent_INTERVENE_EducationalAttainment_Cox_NRI_IDI_Model1a-2_by.txt",data.table=FALSE) # results model 1a vs 2
FGR11.1a2NRIIDI$Biobank <- "FinnGen"
FGR11.1b2NRIIDI <- fread("output/Prediction/FinnGen/2026-03-31_FinnGen_20percent_INTERVENE_EducationalAttainment_Cox_NRI_IDI_Model1b-2_by.txt",data.table=FALSE) # results model 1b vs 2
FGR11.1b2NRIIDI$Biobank <- "FinnGen"
FGR11.24NRIIDI <- fread("output/Prediction/FinnGen/2026-03-31_FinnGen_20percent_INTERVENE_EducationalAttainment_Cox_NRI_IDI_Model2-4_by.txt",data.table=FALSE) # results model 2 vs 4
FGR11.24NRIIDI$Biobank <- "FinnGen"
# UK Biobank
UKB.0a1aNRIIDI.EUR <- fread("output/Prediction/UKB/2026-04-02_UKBiobank_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model0a-1a_by.txt",data.table=FALSE) # results model 1a vs 2 in EU ancestry
UKB.0a1aNRIIDI.EUR$Biobank <- "UK Biobank"
UKB.0b1bNRIIDI.EUR <- fread("output/Prediction/UKB/2026-04-02_UKBiobank_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model0b-1b_by.txt",data.table=FALSE) # results model 1b vs 2 in EU ancestry
UKB.0b1bNRIIDI.EUR$Biobank <- "UK Biobank"
UKB.1a2NRIIDI.EUR <- fread("output/Prediction/UKB/2026-04-01_UKBiobank_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model1a-2_by.txt",data.table=FALSE) # results model 1a vs 2 in EU ancestry
UKB.1a2NRIIDI.EUR$Biobank <- "UK Biobank"
UKB.1b2NRIIDI.EUR <- fread("output/Prediction/UKB/2026-04-01_UKBiobank_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model1b-2_by.txt",data.table=FALSE) # results model 1b vs 2 in EU ancestry
UKB.1b2NRIIDI.EUR$Biobank <- "UK Biobank"
UKB.24NRIIDI.EUR <- fread("output/Prediction/UKB/2026-04-01_UKBiobank_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model2-4_by.txt",data.table=FALSE) # results model 2 vs 2 in EU ancestry
UKB.24NRIIDI.EUR$Biobank <- "UK Biobank"


################################################################################
#
# In Generation Scotland the Education analyses for Rheumatoid Arthritis have too few
# individuals; for those analyses were the results were still generated, remove
# this trait
# In Generation Scotland the Occupation analyses for Rheumatoid Arthritis, skin
# melanoma, T1D, and epilepsy have too few individuals; remove these traits.
#
################################################################################

GS.3 <- GS.3[-which(GS.3$trait %in% c("RHEUMA_SEROPOS_OTH")),]
GS.1aocc <- GS.1aocc[-which(GS.1aocc$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                                  "C3_MELANOMA_SKIN","G6_EPLEPSY")),]
GS.1bocc <- GS.1bocc[-which(GS.1bocc$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                                  "C3_MELANOMA_SKIN","G6_EPLEPSY")),]
GS.2occ <- GS.2occ[-which(GS.2occ$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                               "C3_MELANOMA_SKIN","G6_EPLEPSY")),]
GS.3occ <- GS.3occ[-which(GS.3occ$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                               "C3_MELANOMA_SKIN","G6_EPLEPSY")),]
GS.4occ <- GS.4occ[-which(GS.4occ$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                               "C3_MELANOMA_SKIN","G6_EPLEPSY")),]


################################################################################
#
# The current version of the Education meta-analyses of Cox models also includes traits
# only available in FinnGen (atrial fibrillation and colorectal cancer), subset
# those data frames to only include the traits where >=2 cohorts were analysed.
# The current version of the Occupation meta-analyses of Cox models also includes
# traits only available in FinnGen (atrial fibrillation, colorectal cancer, and
# skin melanoma), subset those data frames to only include the traits where >=2
# cohorts were analysed.
#
################################################################################

## remove traits not meta-analyzed Cox models Education ##
# fixed effect
FEMA.1a <- FEMA.1a[-which(FEMA.1a$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.1b <- FEMA.1b[-which(FEMA.1b$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.2 <- FEMA.2[-which(FEMA.2$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.3 <- FEMA.3[-which(FEMA.3$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.4 <- FEMA.4[-which(FEMA.4$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.4cs <- FEMA.4cs[-which(FEMA.4cs$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
FEMA.4all <- FEMA.4all[-which(FEMA.4all$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]

# random effect
REMA.1a <- REMA.1a[-which(REMA.1a$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
REMA.1b <- REMA.1b[-which(REMA.1b$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
REMA.2 <- REMA.2[-which(REMA.2$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
REMA.3 <- REMA.3[-which(REMA.3$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
REMA.4 <- REMA.4[-which(REMA.4$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
REMA.4cs <- REMA.4cs[-which(REMA.4cs$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]
REMA.4all <- REMA.4all[-which(REMA.4all$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]

## remove traits not meta-analyzed Cox models Occupation ##
# fixed effect
FEMA.1aocc <- FEMA.1aocc[-which(FEMA.1aocc$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                            "C3_MELANOMA_SKIN")),]
FEMA.1bocc <- FEMA.1bocc[-which(FEMA.1bocc$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                            "C3_MELANOMA_SKIN")),]
FEMA.2occ <- FEMA.2occ[-which(FEMA.2occ$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                         "C3_MELANOMA_SKIN")),]
FEMA.3occ <- FEMA.3occ[-which(FEMA.3occ$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                         "C3_MELANOMA_SKIN")),]
FEMA.4occ <- FEMA.4occ[-which(FEMA.4occ$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                         "C3_MELANOMA_SKIN")),]

# random effect
REMA.1aocc <- REMA.1aocc[-which(REMA.1aocc$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                            "C3_MELANOMA_SKIN")),]
REMA.1bocc <- REMA.1bocc[-which(REMA.1bocc$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                            "C3_MELANOMA_SKIN")),]
REMA.2occ <- REMA.2occ[-which(REMA.2occ$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                         "C3_MELANOMA_SKIN")),]
REMA.3occ <- REMA.3occ[-which(REMA.3occ$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                         "C3_MELANOMA_SKIN")),]
REMA.4occ <- REMA.4occ[-which(REMA.4occ$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                         "C3_MELANOMA_SKIN")),]


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
                              REMA.1a$Biobank,REMA.1b$Biobank,
                              FGR11.1a$Biobank,FGR11.1b$Biobank,
                              UKB.1a.EUR$Biobank,UKB.1b.EUR$Biobank,
                              GS.1a$Biobank,GS.1b$Biobank),
                  Test = c(rep("high educational attainment",nrow(FEMA.1a)), rep("PGS",nrow(FEMA.1b)),
                           rep("high educational attainment",nrow(REMA.1a)), rep("PGS",nrow(REMA.1b)),
                           rep("high educational attainment",nrow(FGR11.1a)), rep("PGS",nrow(FGR11.1b)), 
                           rep("high educational attainment", nrow(UKB.1a.EUR)), rep("PGS",nrow(UKB.1b.EUR)), 
                           rep("high educational attainment",nrow(GS.1a)), rep("PGS",nrow(GS.1b))),
                  Phenotype = c(FEMA.1a$Phenotype,FEMA.1b$Phenotype,
                                REMA.1a$Phenotype,REMA.1b$Phenotype,
                                FGR11.1a$trait,FGR11.1b$trait,
                                UKB.1a.EUR$trait,UKB.1b.EUR$trait,
                                GS.1a$trait,GS.1b$trait),
                  Beta = c(FEMA.1a$Beta,FEMA.1b$Beta,
                           REMA.1a$Beta,REMA.1b$Beta,
                           FGR11.1a$EAhigh_beta,FGR11.1b$PRS_beta,
                           UKB.1a.EUR$EAhigh_beta,UKB.1b.EUR$PRS_beta,
                           GS.1a$EAhigh_beta,GS.1b$PRS_beta),
                  SE = c(FEMA.1a$SE,FEMA.1b$SE,
                         REMA.1a$SE,REMA.1b$SE,
                         FGR11.1a$EAhigh_se,FGR11.1b$PRS_se,
                         UKB.1a.EUR$EAhigh_se,UKB.1b.EUR$PRS_se,
                         GS.1a$EAhigh_se,GS.1b$PRS_se),
                  Pval = c(FEMA.1a$Pval,FEMA.1b$Pval,
                           REMA.1a$Pval,REMA.1b$Pval,
                           FGR11.1a$EAhigh_p,FGR11.1b$PRS_p,
                           UKB.1a.EUR$EAhigh_p,UKB.1b.EUR$PRS_p,
                           GS.1a$EAhigh_p,GS.1b$PRS_p),
                  HR = c(FEMA.1a$HR,FEMA.1b$HR,
                         REMA.1a$HR,REMA.1b$HR,
                         FGR11.1a$EAhigh_HR,FGR11.1b$PRS_HR,
                         UKB.1a.EUR$EAhigh_HR,UKB.1b.EUR$PRS_HR,
                         GS.1a$EAhigh_HR,GS.1b$PRS_HR),
                  Cineg = c(FEMA.1a$Cineg,FEMA.1b$Cineg,
                            REMA.1a$Cineg,REMA.1b$Cineg,
                            FGR11.1a$EAhigh_HR_lower95,FGR11.1b$PRS_HR_lower95,
                            UKB.1a.EUR$EAhigh_HR_lower95,UKB.1b.EUR$PRS_HR_lower95,
                            GS.1a$EAhigh_HR_lower95,GS.1b$PRS_HR_lower95),
                  Cipos = c(FEMA.1a$Cipos,FEMA.1b$Cipos,
                            REMA.1a$Cipos,REMA.1b$Cipos,
                            FGR11.1a$EAhigh_HR_upper95,FGR11.1b$PRS_HR_upper95,
                            UKB.1a.EUR$EAhigh_HR_upper95,UKB.1b.EUR$PRS_HR_upper95,
                            GS.1a$EAhigh_HR_upper95,GS.1b$PRS_HR_upper95),
                  QHet = c(FEMA.1a$QHet,FEMA.1b$QHet,
                           REMA.1a$QHet,REMA.1b$QHet,
                           rep(NA,nrow(FGR11.1a)),rep(NA,nrow(FGR11.1b)),
                           rep(NA,nrow(UKB.1a.EUR)),rep(NA,nrow(UKB.1b.EUR)),
                           rep(NA,nrow(GS.1a)),rep(NA,nrow(GS.1b))),
                  HetPval = c(FEMA.1a$HetPval,FEMA.1b$HetPval,
                              REMA.1a$HetPval,REMA.1b$HetPval,
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
TS6 <- data.frame(Biobank = c(FEMA.2$Biobank,REMA.2$Biobank,
                              FGR11.2$Biobank,FGR11.2$Biobank,
                              UKB.2.EUR$Biobank,UKB.2.EUR$Biobank,
                              GS.2$Biobank,GS.2$Biobank),
                  Test = c(FEMA.2$Test,REMA.2$Test,
                           rep("high educational attainment",nrow(FGR11.2)), rep("PGS",nrow(FGR11.2)), 
                           rep("high educational attainment", nrow(UKB.2.EUR)), rep("PGS",nrow(UKB.2.EUR)), 
                           rep("high educational attainment",nrow(GS.2)), rep("PGS",nrow(GS.2))),
                  Phenotype = c(FEMA.2$Phenotype,REMA.2$Phenotype,
                                FGR11.2$trait,FGR11.2$trait,
                                UKB.2.EUR$trait,UKB.2.EUR$trait,
                                GS.2$trait,GS.2$trait),
                  Beta = c(FEMA.2$Beta,REMA.2$Beta,
                           FGR11.2$EAhigh_beta,FGR11.2$PRS_beta,
                           UKB.2.EUR$EAhigh_beta,UKB.2.EUR$PRS_beta,
                           GS.2$EAhigh_beta,GS.2$PRS_beta),
                  SE = c(FEMA.2$SE,REMA.2$SE,
                         FGR11.2$EAhigh_se,FGR11.2$PRS_se,
                         UKB.2.EUR$EAhigh_se,UKB.2.EUR$PRS_se,
                         GS.2$EAhigh_se,GS.2$PRS_se),
                  Pval = c(FEMA.2$Pval,REMA.2$Pval,
                           FGR11.2$EAhigh_p,FGR11.2$PRS_p,
                           UKB.2.EUR$EAhigh_p,UKB.2.EUR$PRS_p,
                           GS.2$EAhigh_p,GS.2$PRS_p),
                  HR = c(FEMA.2$HR,REMA.2$HR,
                         FGR11.2$EAhigh_HR,FGR11.2$PRS_HR,
                         UKB.2.EUR$EAhigh_HR,UKB.2.EUR$PRS_HR,
                         GS.2$EAhigh_HR,GS.2$PRS_HR),
                  Cineg = c(FEMA.2$Cineg,REMA.2$Cineg,
                            FGR11.2$EAhigh_HR_lower95,FGR11.2$PRS_HR_lower95,
                            UKB.2.EUR$EAhigh_HR_lower95,UKB.2.EUR$PRS_HR_lower95,
                            GS.2$EAhigh_HR_lower95,GS.2$PRS_HR_lower95),
                  Cipos = c(FEMA.2$Cipos,REMA.2$Cipos,
                            FGR11.2$EAhigh_HR_upper95,FGR11.2$PRS_HR_upper95,
                            UKB.2.EUR$EAhigh_HR_upper95,UKB.2.EUR$PRS_HR_upper95,
                            GS.2$EAhigh_HR_upper95,GS.2$PRS_HR_upper95),
                  QHet = c(FEMA.2$QHet,REMA.2$QHet,
                           rep(NA,2*nrow(FGR11.2)),
                           rep(NA,2*nrow(UKB.2.EUR)),
                           rep(NA,2*nrow(GS.2))),
                  HetPval = c(FEMA.2$HetPval,REMA.2$HetPval,
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
TS7 <- rbind(mod12sign.EA[,c(1:12,14)],mod12sign.RE.EA[,c(1:12,14)])
# rename columns
names(TS7) <- c("Phenotype", "HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                "beta_difference","se_difference","Zval_difference","Pval_difference","Test")
# reorder
TS7 <- TS7[,c("Phenotype","Test","HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
              "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
              "beta_difference","se_difference","Pval_difference")]

# add column to indicate kind of meta-analysis
TS7$MetaAnalysis <- NA
TS7$MetaAnalysis[1:nrow(mod12sign.EA)] <- "FE meta-analysis"
TS7$MetaAnalysis[which(is.na(TS7$MetaAnalysis))] <- "RE meta-analysis"

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
TS8 <- data.frame(Biobank = c(FEMA.3$Biobank,REMA.3$Biobank,FGR11.3$Biobank,
                              UKB.3.EUR$Biobank,GS.3$Biobank),
                  EducationGroup = c(FEMA.3$EA,REMA.3$EA,FGR11.3$Test,
                                     UKB.3.EUR$Test,GS.3$Test),
                  Phenotype = c(FEMA.3$Phenotype,REMA.3$Phenotype,FGR11.3$trait,
                                UKB.3.EUR$trait,GS.3$trait),
                  Beta = c(FEMA.3$Beta,REMA.3$Beta,FGR11.3$PRS_beta,
                           UKB.3.EUR$PRS_beta,GS.3$PRS_beta),
                  SE = c(FEMA.3$SE,REMA.3$SE,FGR11.3$PRS_se,UKB.3.EUR$PRS_se,
                         GS.3$PRS_se),
                  Pval = c(FEMA.3$Pval,REMA.3$Pval,FGR11.3$PRS_p,UKB.3.EUR$PRS_p,
                           GS.3$PRS_p),
                  HR = c(FEMA.3$HR,REMA.3$HR,FGR11.3$PRS_HR,UKB.3.EUR$PRS_HR,
                         GS.3$PRS_HR),
                  Cineg = c(FEMA.3$Cineg,REMA.3$Cineg,FGR11.3$PRS_HR_lower95,
                            UKB.3.EUR$PRS_HR_lower95,GS.3$PRS_HR_lower95),
                  Cipos = c(FEMA.3$Cipos,REMA.3$Cipos,FGR11.3$PRS_HR_upper95,
                            UKB.3.EUR$PRS_HR_upper95,GS.3$PRS_HR_upper95),
                  QHet = c(FEMA.3$QHet,REMA.3$QHet,
                           rep(NA,nrow(FGR11.3)),
                           rep(NA,nrow(UKB.3.EUR)),
                           rep(NA,nrow(GS.3))),
                  HetPval = c(FEMA.3$HetPval,REMA.3$HetPval,
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
TS9 <- data.frame(Biobank = c(FEMA.4$Biobank,REMA.4$Biobank,
                               rep(FGR11.4$Biobank,3),
                               rep(UKB.4.EUR$Biobank,3),
                               rep(GS.4$Biobank,3)),
                   Test = c(FEMA.4$Test,REMA.4$Test,
                            rep("high educational attainment",nrow(FGR11.4)), rep("EAxPGS",nrow(FGR11.4)), rep("PGS",nrow(FGR11.4)), 
                            rep("high educational attainment", nrow(UKB.4.EUR)), rep("EAxPGS",nrow(UKB.4.EUR)), rep("PGS",nrow(UKB.4.EUR)),  
                            rep("high educational attainment",nrow(GS.4)), rep("EAxPGS",nrow(GS.4)), rep("PGS",nrow(GS.4))),
                   Phenotype = c(FEMA.4$Phenotype,REMA.4$Phenotype,
                                 rep(FGR11.4$trait,3),
                                 rep(UKB.4.EUR$trait,3),
                                 rep(GS.4$trait,3)),
                   Beta = c(FEMA.4$Beta,REMA.4$Beta,
                            FGR11.4$EAhigh_beta,FGR11.4$`PRS:EAhigh_beta`,FGR11.4$PRS_beta,
                            UKB.4.EUR$EAhigh_beta,UKB.4.EUR$`PRS:EAhigh_beta`,UKB.4.EUR$PRS_beta,
                            GS.4$EAhigh_beta,GS.4$`PRS:EAhigh_beta`,GS.4$PRS_beta),
                   SE = c(FEMA.4$SE,REMA.4$SE,
                          FGR11.4$EAhigh_se,FGR11.4$`PRS:EAhigh_se`,FGR11.4$PRS_se,
                          UKB.4.EUR$EAhigh_se,UKB.4.EUR$`PRS:EAhigh_se`,UKB.4.EUR$PRS_se,
                          GS.4$EAhigh_se,GS.4$`PRS:EAhigh_se`,GS.4$PRS_se),
                   Pval = c(FEMA.4$Pval,REMA.4$Pval,
                            FGR11.4$EAhigh_p,FGR11.4$`PRS:EAhigh_p`,FGR11.4$PRS_p,
                            UKB.4.EUR$EAhigh_p,UKB.4.EUR$`PRS:EAhigh_p`,UKB.4.EUR$PRS_p,
                            GS.4$EAhigh_p,GS.4$`PRS:EAhigh_p`,GS.4$PRS_p),
                   HR = c(FEMA.4$HR,REMA.4$HR,
                          FGR11.4$EAhigh_HR,FGR11.4$`PRS:EAhigh_HR`,FGR11.4$PRS_HR,
                          UKB.4.EUR$EAhigh_HR,UKB.4.EUR$`PRS:EAhigh_HR`,UKB.4.EUR$PRS_HR,
                          GS.4$EAhigh_HR,GS.4$`PRS:EAhigh_HR`,GS.4$PRS_HR),
                   Cineg = c(FEMA.4$Cineg,REMA.4$Cineg,
                             FGR11.4$EAhigh_HR_lower95,FGR11.4$`PRS:EAhigh_HR_lower95`,FGR11.4$PRS_HR_lower95,
                             UKB.4.EUR$EAhigh_HR_lower95,UKB.4.EUR$`PRS:EAhigh_HR_lower95`,UKB.4.EUR$PRS_HR_lower95,
                             GS.4$EAhigh_HR_lower95,GS.4$`PRS:EAhigh_HR_lower95`,GS.4$PRS_HR_lower95),
                   Cipos = c(FEMA.4$Cipos,REMA.4$Cipos,
                             FGR11.4$EAhigh_HR_upper95,FGR11.4$`PRS:EAhigh_HR_upper95`,FGR11.4$PRS_HR_upper95,
                             UKB.4.EUR$EAhigh_HR_upper95,UKB.4.EUR$`PRS:EAhigh_HR_upper95`,UKB.4.EUR$PRS_HR_upper95,
                             GS.4$EAhigh_HR_upper95,GS.4$`PRS:EAhigh_HR_upper95`,GS.4$PRS_HR_upper95),
                   QHet = c(FEMA.4$QHet,REMA.4$QHet,
                            rep(NA,3*nrow(FGR11.4)),
                            rep(NA,3*nrow(UKB.4.EUR)),
                            rep(NA,3*nrow(GS.4))),
                   HetPval = c(FEMA.4$HetPval,REMA.4$HetPval,
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
# eTable 21: descriptive statistics per cohort for occupation
#
################################################################################

# create Table TS21
TS21 <- rbind(FGR11.Nocc,UKB.N.EURocc,GS.Nocc)
#reorder columns
TS21 <- TS21[,c(1,23,2:22)]
#adjust column names
names(TS21) <- c("Phenotype","Study",names(TS21[3:23])) # trait = Phenotype
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
                               "_INTERVENE_Occupation_eTable21.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 22: results model 1: per cohort + meta-analysis for occupation
#
################################################################################

# create Table S22
TS22 <- data.frame(Biobank = c(FEMA.1aocc$Biobank,FEMA.1bocc$Biobank,
                                   REMA.1aocc$Biobank,REMA.1bocc$Biobank,
                                   FGR11.1aocc$Biobank,FGR11.1bocc$Biobank,
                                   UKB.1a.EURocc$Biobank,UKB.1b.EURocc$Biobank,
                                   GS.1aocc$Biobank,GS.1bocc$Biobank),
                       Test = c(rep("upper-level occupation",nrow(FEMA.1aocc)), rep("PGS",nrow(FEMA.1bocc)),
                                rep("upper-level occupation",nrow(REMA.1aocc)), rep("PGS",nrow(REMA.1bocc)),
                                rep("upper-level occupation",nrow(FGR11.1aocc)), rep("PGS",nrow(FGR11.1bocc)), 
                                rep("upper-level occupation", nrow(UKB.1a.EURocc)), rep("PGS",nrow(UKB.1b.EURocc)), 
                                rep("upper-level occupation",nrow(GS.1aocc)), rep("PGS",nrow(GS.1bocc))),
                       Phenotype = c(FEMA.1aocc$Phenotype,FEMA.1bocc$Phenotype,
                                     REMA.1aocc$Phenotype,REMA.1bocc$Phenotype,
                                     FGR11.1aocc$trait,FGR11.1bocc$trait,
                                     UKB.1a.EURocc$trait,UKB.1b.EURocc$trait,
                                     GS.1aocc$trait,GS.1bocc$trait),
                       Beta = c(FEMA.1aocc$Beta,FEMA.1bocc$Beta,
                                REMA.1aocc$Beta,REMA.1bocc$Beta,
                                FGR11.1aocc$`OccupationUpper-level_beta`,FGR11.1bocc$PRS_beta,
                                UKB.1a.EURocc$`OccupationUpper-level_beta`,UKB.1b.EURocc$PRS_beta,
                                GS.1aocc$`OccupationUpper-level_beta`,GS.1bocc$PRS_beta),
                       SE = c(FEMA.1aocc$SE,FEMA.1bocc$SE,
                              REMA.1aocc$SE,REMA.1bocc$SE,
                              FGR11.1aocc$`OccupationUpper-level_se`,FGR11.1bocc$PRS_se,
                              UKB.1a.EURocc$`OccupationUpper-level_se`,UKB.1b.EURocc$PRS_se,
                              GS.1aocc$`OccupationUpper-level_se`,GS.1bocc$PRS_se),
                       Pval = c(FEMA.1aocc$Pval,FEMA.1bocc$Pval,
                                REMA.1aocc$Pval,REMA.1bocc$Pval,
                                FGR11.1aocc$`OccupationUpper-level_p`,FGR11.1bocc$PRS_p,
                                UKB.1a.EURocc$`OccupationUpper-level_p`,UKB.1b.EURocc$PRS_p,
                                GS.1aocc$`OccupationUpper-level_p`,GS.1bocc$PRS_p),
                       HR = c(FEMA.1aocc$HR,FEMA.1bocc$HR,
                              REMA.1aocc$HR,REMA.1bocc$HR,
                              FGR11.1aocc$`OccupationUpper-level_HR`,FGR11.1bocc$PRS_HR,
                              UKB.1a.EURocc$`OccupationUpper-level_HR`,UKB.1b.EURocc$PRS_HR,
                              GS.1aocc$`OccupationUpper-level_HR`,GS.1bocc$PRS_HR),
                       Cineg = c(FEMA.1aocc$Cineg,FEMA.1bocc$Cineg,
                                 REMA.1aocc$Cineg,REMA.1bocc$Cineg,
                                 FGR11.1aocc$`OccupationUpper-level_HR_lower95`,FGR11.1bocc$PRS_HR_lower95,
                                 UKB.1a.EURocc$`OccupationUpper-level_HR_lower95`,UKB.1b.EURocc$PRS_HR_lower95,
                                 GS.1aocc$`OccupationUpper-level_HR_lower95`,GS.1bocc$PRS_HR_lower95),
                       Cipos = c(FEMA.1aocc$Cipos,FEMA.1bocc$Cipos,
                                 REMA.1aocc$Cipos,REMA.1bocc$Cipos,
                                 FGR11.1aocc$`OccupationUpper-level_HR_upper95`,FGR11.1bocc$PRS_HR_upper95,
                                 UKB.1a.EURocc$`OccupationUpper-level_HR_upper95`,UKB.1b.EURocc$PRS_HR_upper95,
                                 GS.1aocc$`OccupationUpper-level_HR_upper95`,GS.1bocc$PRS_HR_upper95),
                       QHet = c(FEMA.1aocc$QHet,FEMA.1bocc$QHet,
                                REMA.1aocc$QHet,REMA.1bocc$QHet,
                                rep(NA,nrow(FGR11.1aocc)),rep(NA,nrow(FGR11.1bocc)),
                                rep(NA,nrow(UKB.1a.EURocc)),rep(NA,nrow(UKB.1b.EURocc)),
                                rep(NA,nrow(GS.1aocc)),rep(NA,nrow(GS.1bocc))),
                       HetPval = c(FEMA.1aocc$HetPval,FEMA.1bocc$HetPval,
                                   REMA.1aocc$HetPval,REMA.1bocc$HetPval,
                                   rep(NA,nrow(FGR11.1aocc)),rep(NA,nrow(FGR11.1bocc)),
                                   rep(NA,nrow(UKB.1a.EURocc)),rep(NA,nrow(UKB.1b.EURocc)),
                                   rep(NA,nrow(GS.1aocc)),rep(NA,nrow(GS.1bocc))))
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
                                "_INTERVENE_Occupation_eTable22.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 23: results model 2: per cohort + meta-analysis for occupation
#
################################################################################

# create Table S23
TS23 <- data.frame(Biobank = c(FEMA.2occ$Biobank,REMA.2occ$Biobank,
                              FGR11.2occ$Biobank,FGR11.2occ$Biobank,
                              UKB.2.EURocc$Biobank,UKB.2.EURocc$Biobank,
                              GS.2occ$Biobank,GS.2occ$Biobank),
                  Test = c(FEMA.2occ$Test,REMA.2occ$Test,
                           rep("Occupation",nrow(FGR11.2occ)), rep("PRS",nrow(FGR11.2occ)), 
                           rep("Occupation", nrow(UKB.2.EURocc)), rep("PRS",nrow(UKB.2.EURocc)), 
                           rep("Occupation",nrow(GS.2occ)), rep("PRS",nrow(GS.2occ))),
                  Phenotype = c(FEMA.2occ$Phenotype,REMA.2occ$Phenotype,
                                FGR11.2occ$trait,FGR11.2occ$trait,
                                UKB.2.EURocc$trait,UKB.2.EURocc$trait,
                                GS.2occ$trait,GS.2occ$trait),
                  Beta = c(FEMA.2occ$Beta,REMA.2occ$Beta,
                           FGR11.2occ$`OccupationUpper-level_beta`,FGR11.2occ$PRS_beta,
                           UKB.2.EURocc$`OccupationUpper-level_beta`,UKB.2.EURocc$PRS_beta,
                           GS.2occ$`OccupationUpper-level_beta`,GS.2occ$PRS_beta),
                  SE = c(FEMA.2occ$SE,REMA.2occ$SE,
                         FGR11.2occ$`OccupationUpper-level_se`,FGR11.2occ$PRS_se,
                         UKB.2.EURocc$`OccupationUpper-level_se`,UKB.2.EURocc$PRS_se,
                         GS.2occ$`OccupationUpper-level_se`,GS.2occ$PRS_se),
                  Pval = c(FEMA.2occ$Pval,REMA.2occ$Pval,
                           FGR11.2occ$`OccupationUpper-level_p`,FGR11.2occ$PRS_p,
                           UKB.2.EURocc$`OccupationUpper-level_p`,UKB.2.EURocc$PRS_p,
                           GS.2occ$`OccupationUpper-level_p`,GS.2occ$PRS_p),
                  HR = c(FEMA.2occ$HR,REMA.2occ$HR,
                         FGR11.2occ$`OccupationUpper-level_HR`,FGR11.2occ$PRS_HR,
                         UKB.2.EURocc$`OccupationUpper-level_HR`,UKB.2.EURocc$PRS_HR,
                         GS.2occ$`OccupationUpper-level_HR`,GS.2occ$PRS_HR),
                  Cineg = c(FEMA.2occ$Cineg,REMA.2occ$Cineg,
                            FGR11.2occ$`OccupationUpper-level_HR_lower95`,FGR11.2occ$PRS_HR_lower95,
                            UKB.2.EURocc$`OccupationUpper-level_HR_lower95`,UKB.2.EURocc$PRS_HR_lower95,
                            GS.2occ$`OccupationUpper-level_HR_lower95`,GS.2occ$PRS_HR_lower95),
                  Cipos = c(FEMA.2occ$Cipos,REMA.2occ$Cipos,
                            FGR11.2occ$`OccupationUpper-level_HR_upper95`,FGR11.2occ$PRS_HR_upper95,
                            UKB.2.EURocc$`OccupationUpper-level_HR_upper95`,UKB.2.EURocc$PRS_HR_upper95,
                            GS.2occ$`OccupationUpper-level_HR_upper95`,GS.2occ$PRS_HR_upper95),
                  QHet = c(FEMA.2occ$QHet,REMA.2occ$QHet,
                           rep(NA,2*nrow(FGR11.2occ)),
                           rep(NA,2*nrow(UKB.2.EURocc)),
                           rep(NA,2*nrow(GS.2occ))),
                  HetPval = c(FEMA.2occ$HetPval,REMA.2occ$HetPval,
                              rep(NA,2*nrow(FGR11.2occ)),
                              rep(NA,2*nrow(UKB.2.EURocc)),
                              rep(NA,2*nrow(GS.2occ))))
# adjust labels
TS23$Phenotype <- factor(TS23$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
TS23$Test <- factor(TS23$Test, levels = c("Occupation", "PRS"),
                    labels = c("upper-level occupation", "PGS"))

# write file
write.table(TS23, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable23.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 24: significance test differences effect occupation and PGS model 1vs2
#
################################################################################

# create Table S24 
TS24 <- rbind(mod12sign.OCC[,c(1:12,14)],mod12sign.RE.OCC[,c(1:12,14)])
# rename columns
names(TS24) <- c("Phenotype", "HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                "beta_difference","se_difference","Zval_difference","Pval_difference","Test")
# reorder
TS24 <- TS24[,c("Phenotype","Test","HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
              "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
              "beta_difference","se_difference","Pval_difference")]

# add column to indicate kind of meta-analysis
TS24$MetaAnalysis <- NA
TS24$MetaAnalysis[1:nrow(mod12sign.OCC)] <- "FE meta-analysis"
TS24$MetaAnalysis[which(is.na(TS24$MetaAnalysis))] <- "RE meta-analysis"

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS24$lb_beta_difference <- TS24$beta_difference - 1.96*TS24$se_difference
TS24$ub_beta_difference <- TS24$beta_difference + 1.96*TS24$se_difference
#
TS24$lb_difference <- exp(TS24$lb_beta_difference)
TS24$ub_difference <- exp(TS24$ub_beta_difference)

# adjust label Test
TS24$Test[which(TS24$Test=="PRS")] <- "PGS"

# write file
write.table(TS24, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable24.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 25: significance test differences model 1 education vs occupation
#
################################################################################

# create Table S25 
TS25 <- rbind(mod1sign.FE.EAOCC[,c(1:3,8:16,18)],mod1sign.RE.EAOCC[,c(1:3,8:16,18)])

# rename columns
names(TS25) <- c("Phenotype", "HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                 "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                 "beta_difference","se_difference","Zval_difference","Pval_difference","Test")
# reorder
TS25 <- TS25[,c("Phenotype","Test","HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                "beta_difference","se_difference","Pval_difference")]

# add column to indicate kind of meta-analysis
TS25$MetaAnalysis <- NA
TS25$MetaAnalysis[1:nrow(mod1sign.FE.EAOCC)] <- "FE meta-analysis"
TS25$MetaAnalysis[which(is.na(TS25$MetaAnalysis))] <- "RE meta-analysis"

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS25$lb_beta_difference <- TS25$beta_difference - 1.96*TS25$se_difference
TS25$ub_beta_difference <- TS25$beta_difference + 1.96*TS25$se_difference
#
TS25$lb_difference <- exp(TS25$lb_beta_difference)
TS25$ub_difference <- exp(TS25$ub_beta_difference)

# adjust label Test
TS25$Test[which(TS25$Test=="PRS")] <- "PGS"

# write file
write.table(TS25, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable25.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 26: results model 3: per cohort + meta-analysis for occupation
#
################################################################################

# Create Table S26
TS26 <- data.frame(Biobank = c(FEMA.3occ$Biobank,REMA.3occ$Biobank,FGR11.3occ$Biobank,
                              UKB.3.EURocc$Biobank,GS.3occ$Biobank),
                  OccupationGroup = c(FEMA.3occ$EA,REMA.3occ$EA,FGR11.3occ$Test,
                                     UKB.3.EURocc$Test,GS.3occ$Test),
                  Phenotype = c(FEMA.3occ$Phenotype,REMA.3occ$Phenotype,FGR11.3occ$trait,
                                UKB.3.EURocc$trait,GS.3occ$trait),
                  Beta = c(FEMA.3occ$Beta,REMA.3occ$Beta,FGR11.3occ$PRS_beta,
                           UKB.3.EURocc$PRS_beta,GS.3occ$PRS_beta),
                  SE = c(FEMA.3occ$SE,REMA.3occ$SE,FGR11.3occ$PRS_se,UKB.3.EURocc$PRS_se,
                         GS.3occ$PRS_se),
                  Pval = c(FEMA.3occ$Pval,REMA.3occ$Pval,FGR11.3occ$PRS_p,UKB.3.EURocc$PRS_p,
                           GS.3occ$PRS_p),
                  HR = c(FEMA.3occ$HR,REMA.3occ$HR,FGR11.3occ$PRS_HR,UKB.3.EURocc$PRS_HR,
                         GS.3occ$PRS_HR),
                  Cineg = c(FEMA.3occ$Cineg,REMA.3occ$Cineg,FGR11.3occ$PRS_HR_lower95,
                            UKB.3.EURocc$PRS_HR_lower95,GS.3occ$PRS_HR_lower95),
                  Cipos = c(FEMA.3occ$Cipos,REMA.3occ$Cipos,FGR11.3occ$PRS_HR_upper95,
                            UKB.3.EURocc$PRS_HR_upper95,GS.3occ$PRS_HR_upper95),
                  QHet = c(FEMA.3occ$QHet,REMA.3occ$QHet,
                           rep(NA,nrow(FGR11.3occ)),
                           rep(NA,nrow(UKB.3.EURocc)),
                           rep(NA,nrow(GS.3occ))),
                  HetPval = c(FEMA.3occ$HetPval,REMA.3occ$HetPval,
                              rep(NA,nrow(FGR11.3occ)),
                              rep(NA,nrow(UKB.3.EURocc)),
                              rep(NA,nrow(GS.3occ))))
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
                                   "* Atrial Fibrillation","* Colorectal Cancer","Asthma",
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
# eTable 27: results model 4: per cohort + meta-analysis for occupation
#
################################################################################

# create combined table with model 4 results
TS27 <- data.frame(Biobank = c(FEMA.4occ$Biobank,REMA.4occ$Biobank,
                              rep(FGR11.4occ$Biobank,3),
                              rep(UKB.4.EURocc$Biobank,3),
                              rep(GS.4occ$Biobank,3)),
                  Test = c(FEMA.4occ$Test,REMA.4occ$Test,
                           rep("Occupation",nrow(FGR11.4occ)), rep("OccupationxPRS",nrow(FGR11.4occ)), rep("PRS",nrow(FGR11.4occ)), 
                           rep("Occupation", nrow(UKB.4.EURocc)), rep("OccupationxPRS",nrow(UKB.4.EURocc)), rep("PRS",nrow(UKB.4.EURocc)),  
                           rep("Occupation",nrow(GS.4occ)), rep("OccupationxPRS",nrow(GS.4occ)), rep("PRS",nrow(GS.4occ))),
                  Phenotype = c(FEMA.4occ$Phenotype,REMA.4occ$Phenotype,
                                rep(FGR11.4occ$trait,3),
                                rep(UKB.4.EURocc$trait,3),
                                rep(GS.4occ$trait,3)),
                  Beta = c(FEMA.4occ$Beta,REMA.4occ$Beta,
                           FGR11.4occ$`OccupationUpper-level_beta`,FGR11.4occ$`PRS:OccupationUpper-level_beta`,FGR11.4occ$PRS_beta,
                           UKB.4.EURocc$`OccupationUpper-level_beta`,UKB.4.EURocc$`PRS:OccupationUpper-level_beta`,UKB.4.EURocc$PRS_beta,
                           GS.4occ$`OccupationUpper-level_beta`,GS.4occ$`PRS:OccupationUpper-level_beta`,GS.4occ$PRS_beta),
                  SE = c(FEMA.4occ$SE,REMA.4occ$SE,
                         FGR11.4occ$`PRS:OccupationUpper-level_se`,FGR11.4occ$`PRS:OccupationUpper-level_se`,FGR11.4occ$PRS_se,
                         UKB.4.EURocc$`PRS:OccupationUpper-level_se`,UKB.4.EURocc$`PRS:OccupationUpper-level_se`,UKB.4.EURocc$PRS_se,
                         GS.4occ$`PRS:OccupationUpper-level_se`,GS.4occ$`PRS:OccupationUpper-level_se`,GS.4occ$PRS_se),
                  Pval = c(FEMA.4occ$Pval,REMA.4occ$Pval,
                           FGR11.4occ$`PRS:OccupationUpper-level_p`,FGR11.4occ$`PRS:OccupationUpper-level_p`,FGR11.4occ$PRS_p,
                           UKB.4.EURocc$`PRS:OccupationUpper-level_p`,UKB.4.EURocc$`PRS:OccupationUpper-level_p`,UKB.4.EURocc$PRS_p,
                           GS.4occ$`PRS:OccupationUpper-level_p`,GS.4occ$`PRS:OccupationUpper-level_p`,GS.4occ$PRS_p),
                  HR = c(FEMA.4occ$HR,REMA.4occ$HR,
                         FGR11.4occ$`PRS:OccupationUpper-level_HR`,FGR11.4occ$`PRS:OccupationUpper-level_HR`,FGR11.4occ$PRS_HR,
                         UKB.4.EURocc$`PRS:OccupationUpper-level_HR`,UKB.4.EURocc$`PRS:OccupationUpper-level_HR`,UKB.4.EURocc$PRS_HR,
                         GS.4occ$`PRS:OccupationUpper-level_HR`,GS.4occ$`PRS:OccupationUpper-level_HR`,GS.4occ$PRS_HR),
                  Cineg = c(FEMA.4occ$Cineg,REMA.4occ$Cineg,
                            FGR11.4occ$`PRS:OccupationUpper-level_HR_lower95`,FGR11.4occ$`PRS:OccupationUpper-level_HR_lower95`,FGR11.4occ$PRS_HR_lower95,
                            UKB.4.EURocc$`PRS:OccupationUpper-level_HR_lower95`,UKB.4.EURocc$`PRS:OccupationUpper-level_HR_lower95`,UKB.4.EURocc$PRS_HR_lower95,
                            GS.4occ$`PRS:OccupationUpper-level_HR_lower95`,GS.4occ$`PRS:OccupationUpper-level_HR_lower95`,GS.4occ$PRS_HR_lower95),
                  Cipos = c(FEMA.4occ$Cipos,REMA.4occ$Cipos,
                            FGR11.4occ$`PRS:OccupationUpper-level_HR_upper95`,FGR11.4occ$`PRS:OccupationUpper-level_HR_upper95`,FGR11.4occ$PRS_HR_upper95,
                            UKB.4.EURocc$`PRS:OccupationUpper-level_HR_upper95`,UKB.4.EURocc$`PRS:OccupationUpper-level_HR_upper95`,UKB.4.EURocc$PRS_HR_upper95,
                            GS.4occ$`PRS:OccupationUpper-level_HR_upper95`,GS.4occ$`PRS:OccupationUpper-level_HR_upper95`,GS.4occ$PRS_HR_upper95),
                  QHet = c(FEMA.4occ$QHet,REMA.4occ$QHet,
                           rep(NA,3*nrow(FGR11.4occ)),
                           rep(NA,3*nrow(UKB.4.EURocc)),
                           rep(NA,3*nrow(GS.4occ))),
                  HetPval = c(FEMA.4occ$HetPval,REMA.4occ$HetPval,
                              rep(NA,3*nrow(FGR11.4occ)),
                              rep(NA,3*nrow(UKB.4.EURocc)),
                              rep(NA,3*nrow(GS.4occ))))
# adjust labels
TS27$Test <- factor(TS27$Test, levels = c("Occupation", "OccupationxPRS", "PRS"),
                    labels = c("Occupation", "OccupationxPGS", "PGS"))
#
TS27$Phenotype <- factor(TS27$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS27, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable27.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 28: significance test differences model 1 education vs occupation
#
################################################################################

# create Table S28 
TS28 <- rbind(mod3sign.FE.EAOCC[,c(1:3,8:16,18)],mod3sign.RE.EAOCC[,c(1:3,8:16,18)])

# rename columns
names(TS28) <- c("Phenotype", "HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                 "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                 "beta_difference","se_difference","Zval_difference","Pval_difference","Test")
# reorder
TS28 <- TS28[,c("Phenotype","Test","HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                "beta_difference","se_difference","Pval_difference")]

# add column to indicate kind of meta-analysis
TS28$MetaAnalysis <- NA
TS28$MetaAnalysis[1:nrow(mod3sign.FE.EAOCC)] <- "FE meta-analysis"
TS28$MetaAnalysis[which(is.na(TS28$MetaAnalysis))] <- "RE meta-analysis"

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS28$lb_beta_difference <- TS28$beta_difference - 1.96*TS28$se_difference
TS28$ub_beta_difference <- TS28$beta_difference + 1.96*TS28$se_difference
#
TS28$lb_difference <- exp(TS28$lb_beta_difference)
TS28$ub_difference <- exp(TS28$ub_beta_difference)

# adjust label Test
TS28$Test <- factor(TS28$Test, levels = c("low","high"), 
                    labels = c("Low educational attainment","High educational attainment"))

# write file
write.table(TS28, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable28.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 29: descriptive statistics FinnGen model 6 for occupation
#
################################################################################

# create Table TS29
TS29 <- FGR11.N6occ
#adjust column names
names(TS29) <- c("Phenotype",names(TS29[2:82])) # trait = Phenotype
# adjust labels
TS29$Phenotype <- factor(TS29$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS29, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable29.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 30: results model 6 FinnGen for occupation
#
################################################################################

# create table S30
TS30 <- data.frame(Test = c(rep("0_20p",nrow(FGR11.6occ)), rep("20_40p",nrow(FGR11.6occ)),
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
# eTable 31: FinnGen results bootstrapped cumulative incidences for occupation
#
################################################################################

# create Table S31
TS31 <- data.frame(Test = c(rep("Lower-level occupation",640),
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
write.table(TS31, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_Occupation_eTable31.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# Create eTable 32: descriptive statistics per cohort for EA with
# cohort-specific cutoff
#
################################################################################

# create Table S32
TS32 <- rbind(FGR11.N,UKB.N.EURcs,GS.N)
#reorder columns
TS32 <- TS32[,c(1,23,2:22)]
#adjust column names
names(TS32) <- c("Phenotype","Study",names(TS32[3:23]))
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
                                   "* Atrial Fibrillation","* Colorectal Cancer","Asthma",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                   "Major Depression","Any Cancer","Epilepsy",
                                   "Appendicitis","Alcohol Use Disorder"))
# write file 
write.table(TS32, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable32.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 33 results model 4 (per cohort + meta-analysis) for EA with
# cohort-specific cutoff
#
################################################################################

# create combined table with model 4 results
TS33 <- data.frame(Biobank = c(FEMA.4cs$Biobank,REMA.4cs$Biobank,
                              rep(FGR11.4$Biobank,3),
                              rep(UKB.4.EURcs$Biobank,3),
                              rep(GS.4$Biobank,3)),
                  Test = c(FEMA.4cs$Test,REMA.4cs$Test,
                           rep("high educational attainment",nrow(FGR11.4)), rep("EAxPGS",nrow(FGR11.4)), rep("PGS",nrow(FGR11.4)), 
                           rep("high educational attainment", nrow(UKB.4.EURcs)), rep("EAxPGS",nrow(UKB.4.EURcs)), rep("PGS",nrow(UKB.4.EURcs)),  
                           rep("high educational attainment",nrow(GS.4)), rep("EAxPGS",nrow(GS.4)), rep("PGS",nrow(GS.4))),
                  Phenotype = c(FEMA.4cs$Phenotype,REMA.4cs$Phenotype,
                                rep(FGR11.4$trait,3),
                                rep(UKB.4.EURcs$trait,3),
                                rep(GS.4$trait,3)),
                  Beta = c(FEMA.4cs$Beta,REMA.4cs$Beta,
                           FGR11.4$EAhigh_beta,FGR11.4$`PRS:EAhigh_beta`,FGR11.4$PRS_beta,
                           UKB.4.EURcs$EA2high_beta,UKB.4.EURcs$`PRS:EA2high_beta`,UKB.4.EURcs$PRS_beta,
                           GS.4$EAhigh_beta,GS.4$`PRS:EAhigh_beta`,GS.4$PRS_beta),
                  SE = c(FEMA.4cs$SE,REMA.4cs$SE,
                         FGR11.4$EAhigh_se,FGR11.4$`PRS:EAhigh_se`,FGR11.4$PRS_se,
                         UKB.4.EURcs$EA2high_se,UKB.4.EURcs$`PRS:EA2high_se`,UKB.4.EURcs$PRS_se,
                         GS.4$EAhigh_se,GS.4$`PRS:EAhigh_se`,GS.4$PRS_se),
                  Pval = c(FEMA.4cs$Pval,REMA.4cs$Pval,
                           FGR11.4$EAhigh_p,FGR11.4$`PRS:EAhigh_p`,FGR11.4$PRS_p,
                           UKB.4.EURcs$EA2high_p,UKB.4.EURcs$`PRS:EA2high_p`,UKB.4.EURcs$PRS_p,
                           GS.4$EAhigh_p,GS.4$`PRS:EAhigh_p`,GS.4$PRS_p),
                  HR = c(FEMA.4cs$HR,REMA.4cs$HR,
                         FGR11.4$EAhigh_HR,FGR11.4$`PRS:EAhigh_HR`,FGR11.4$PRS_HR,
                         UKB.4.EURcs$EA2high_HR,UKB.4.EURcs$`PRS:EA2high_HR`,UKB.4.EURcs$PRS_HR,
                         GS.4$EAhigh_HR,GS.4$`PRS:EAhigh_HR`,GS.4$PRS_HR),
                  Cineg = c(FEMA.4cs$Cineg,REMA.4cs$Cineg,
                            FGR11.4$EAhigh_HR_lower95,FGR11.4$`PRS:EAhigh_HR_lower95`,FGR11.4$PRS_HR_lower95,
                            UKB.4.EURcs$EA2high_HR_lower95,UKB.4.EURcs$`PRS:EA2high_HR_lower95`,UKB.4.EURcs$PRS_HR_lower95,
                            GS.4$EAhigh_HR_lower95,GS.4$`PRS:EAhigh_HR_lower95`,GS.4$PRS_HR_lower95),
                  Cipos = c(FEMA.4cs$Cipos,REMA.4cs$Cipos,
                            FGR11.4$EAhigh_HR_upper95,FGR11.4$`PRS:EAhigh_HR_upper95`,FGR11.4$PRS_HR_upper95,
                            UKB.4.EURcs$EA2high_HR_upper95,UKB.4.EURcs$`PRS:EA2high_HR_upper95`,UKB.4.EURcs$PRS_HR_upper95,
                            GS.4$EAhigh_HR_upper95,GS.4$`PRS:EAhigh_HR_upper95`,GS.4$PRS_HR_upper95),
                  QHet = c(FEMA.4cs$QHet,REMA.4cs$QHet,
                           rep(NA,3*nrow(FGR11.4)),
                           rep(NA,3*nrow(UKB.4.EURcs)),
                           rep(NA,3*nrow(GS.4))),
                  HetPval = c(FEMA.4cs$HetPval,REMA.4cs$HetPval,
                              rep(NA,3*nrow(FGR11.4)),
                              rep(NA,3*nrow(UKB.4.EURcs)),
                              rep(NA,3*nrow(GS.4))))
# adjust labels
TS33$Test[which(TS33$Test=="EA")] <- "high educational attainment"
TS33$Test[which(TS33$Test=="EAxPRS")] <- "EAxPGS"
TS33$Test[which(TS33$Test=="PRS")] <- "PGS"
#
TS33$Phenotype <- factor(TS33$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS33, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable33.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 34: significance test differences model 1 education vs occupation
#
################################################################################

# create Table S34 
TS34 <- rbind(FEmod4cssign[,c(1:3,8:16,18)],REmod4cssign[,c(1:3,8:16,18)])

# rename columns
names(TS34) <- c("Phenotype", "HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                 "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                 "beta_difference","se_difference","Zval_difference","Pval_difference","Test")
# reorder
TS34 <- TS34[,c("Phenotype","Test","HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                "beta_difference","se_difference","Pval_difference")]

# add column to indicate kind of meta-analysis
TS34$MetaAnalysis <- NA
TS34$MetaAnalysis[1:nrow(FEmod4cssign)] <- "FE meta-analysis"
TS34$MetaAnalysis[which(is.na(TS34$MetaAnalysis))] <- "RE meta-analysis"

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS34$lb_beta_difference <- TS34$beta_difference - 1.96*TS34$se_difference
TS34$ub_beta_difference <- TS34$beta_difference + 1.96*TS34$se_difference
#
TS34$lb_difference <- exp(TS34$lb_beta_difference)
TS34$ub_difference <- exp(TS34$ub_beta_difference)

# write file
write.table(TS34, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_EducationalAttainment_eTable34.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 35 results model 4 (per cohort + meta-analysis) for EA without
# dichotomization
#
################################################################################

# create combined table with model 4 results
TS35 <- data.frame(Biobank = c(FEMA.4all$Biobank,REMA.4all$Biobank,
                               rep(FGR11.4all$Biobank,3),
                               rep(UKB.4.EURall$Biobank,3),
                               rep(GS.4all$Biobank,3)),
                   Test = c(FEMA.4all$Test,REMA.4all$Test,
                            rep("EA",nrow(FGR11.4all)), rep("EAxPRS",nrow(FGR11.4all)), rep("PRS",nrow(FGR11.4all)), 
                            rep("EA", nrow(UKB.4.EURall)), rep("EAxPRS",nrow(UKB.4.EURall)), rep("PRS",nrow(UKB.4.EURall)),  
                            rep("EA",nrow(GS.4all)), rep("EAxPRS",nrow(GS.4all)), rep("PRS",nrow(GS.4all))),
                   Phenotype = c(FEMA.4all$Phenotype,REMA.4all$Phenotype,
                                 rep(FGR11.4all$trait,3),
                                 rep(UKB.4.EURall$trait,3),
                                 rep(GS.4all$trait,3)),
                   Beta = c(FEMA.4all$Beta,REMA.4all$Beta,
                            FGR11.4all$EDUCATION_97_beta,FGR11.4all$`PRS:EDUCATION_97_beta`,FGR11.4all$PRS_beta,
                            UKB.4.EURall$ISCED97_beta,UKB.4.EURall$`PRS:ISCED97_beta`,UKB.4.EURall$PRS_beta,
                            GS.4all$ISCED97_beta,GS.4all$`PRS:ISCED97_beta`,GS.4all$PRS_beta),
                   SE = c(FEMA.4all$SE,REMA.4all$SE,
                          FGR11.4all$EDUCATION_97_se,FGR11.4all$`PRS:EDUCATION_97_se`,FGR11.4all$PRS_se,
                          UKB.4.EURall$ISCED97_se,UKB.4.EURall$`PRS:ISCED97_se`,UKB.4.EURall$PRS_se,
                          GS.4all$ISCED97_se,GS.4all$`PRS:ISCED97_se`,GS.4all$PRS_se),
                   Pval = c(FEMA.4all$Pval,REMA.4all$Pval,
                            FGR11.4all$EDUCATION_97_p,FGR11.4all$`PRS:EDUCATION_97_p`,FGR11.4all$PRS_p,
                            UKB.4.EURall$ISCED97_p,UKB.4.EURall$`PRS:ISCED97_p`,UKB.4.EURall$PRS_p,
                            GS.4all$ISCED97_p,GS.4all$`PRS:ISCED97_p`,GS.4all$PRS_p),
                   HR = c(FEMA.4all$HR,REMA.4all$HR,
                          FGR11.4all$EDUCATION_97_HR,FGR11.4all$`PRS:EDUCATION_97_HR`,FGR11.4all$PRS_HR,
                          UKB.4.EURall$ISCED97_HR,UKB.4.EURall$`PRS:ISCED97_HR`,UKB.4.EURall$PRS_HR,
                          GS.4all$ISCED97_HR,GS.4all$`PRS:ISCED97_HR`,GS.4all$PRS_HR),
                   Cineg = c(FEMA.4all$Cineg,REMA.4all$Cineg,
                             FGR11.4all$EDUCATION_97_HR_lower95,FGR11.4all$`PRS:EDUCATION_97_HR_lower95`,FGR11.4all$PRS_HR_lower95,
                             UKB.4.EURall$ISCED97_HR_lower95,UKB.4.EURall$`PRS:ISCED97_HR_lower95`,UKB.4.EURall$PRS_HR_lower95,
                             GS.4all$ISCED97_HR_lower95,GS.4all$`PRS:ISCED97_HR_lower95`,GS.4all$PRS_HR_lower95),
                   Cipos = c(FEMA.4all$Cipos,REMA.4all$Cipos,
                             FGR11.4all$EDUCATION_97_HR_upper95,FGR11.4all$`PRS:EDUCATION_97_HR_upper95`,FGR11.4all$PRS_HR_upper95,
                             UKB.4.EURall$ISCED97_HR_upper95,UKB.4.EURall$`PRS:ISCED97_HR_upper95`,UKB.4.EURall$PRS_HR_upper95,
                             GS.4all$ISCED97_HR_upper95,GS.4all$`PRS:ISCED97_HR_upper95`,GS.4all$PRS_HR_upper95),
                   QHet = c(FEMA.4all$QHet,REMA.4all$QHet,
                            rep(NA,3*nrow(FGR11.4all)),
                            rep(NA,3*nrow(UKB.4.EURall)),
                            rep(NA,3*nrow(GS.4all))),
                   HetPval = c(FEMA.4all$HetPval,REMA.4all$HetPval,
                               rep(NA,3*nrow(FGR11.4all)),
                               rep(NA,3*nrow(UKB.4.EURall)),
                               rep(NA,3*nrow(GS.4all))))
#
TS35$Phenotype <- factor(TS35$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS35, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_EducationalAttainment_eTable35.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 36: significance test differences model 1 education vs occupation
#
################################################################################

# create Table S36 
TS36 <- rbind(FEmod4allsign[,c(1:3,8:16,18)],REmod4allsign[,c(1:3,8:16,18)])

# rename columns
names(TS36) <- c("Phenotype", "HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                 "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                 "beta_difference","se_difference","Zval_difference","Pval_difference","Test")
# reorder
TS36 <- TS36[,c("Phenotype","Test","HR_Unadjusted","HR_Adjusted","beta_Unadjusted",
                "beta_Adjusted","se_Unadjusted","se_Adjusted","HR_difference",
                "beta_difference","se_difference","Pval_difference")]

# add column to indicate kind of meta-analysis
TS36$MetaAnalysis <- NA
TS36$MetaAnalysis[1:nrow(FEmod4allsign)] <- "FE meta-analysis"
TS36$MetaAnalysis[which(is.na(TS36$MetaAnalysis))] <- "RE meta-analysis"

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS36$lb_beta_difference <- TS36$beta_difference - 1.96*TS36$se_difference
TS36$ub_beta_difference <- TS36$beta_difference + 1.96*TS36$se_difference
#
TS36$lb_difference <- exp(TS36$lb_beta_difference)
TS36$ub_difference <- exp(TS36$ub_beta_difference)

# write file
write.table(TS36, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_EducationalAttainment_eTable36.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

################################################################################
#
# eTable 37: descriptive statistics FinnGen after excl. cases before cohort entry
#
################################################################################

# create Table TS37
TS37 <- FGR11.Nbb
#adjust column names
names(TS37) <- c("Phenotype",names(TS37[2:23])) # trait = Phenotype
# adjust labels
TS37$Phenotype <- factor(TS37$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS37, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable37.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

################################################################################
#
# eTable 38: Cox estimates model 4 in FinnGen for EA after excl. cases before
# cohort entry
#
################################################################################

# create table with model 4 results
TS38 <- data.frame(Test = c(rep("high educational attainment",nrow(FGR11.4bb)), rep("EAxPGS",nrow(FGR11.4bb)), rep("PGS",nrow(FGR11.4bb))),
                  Phenotype = c(rep(FGR11.4bb$trait,3)),
                  Beta = c(FGR11.4bb$EAhigh_beta,FGR11.4bb$`PRS:EAhigh_beta`,FGR11.4bb$PRS_beta),
                  SE = c(FGR11.4bb$EAhigh_se,FGR11.4bb$`PRS:EAhigh_se`,FGR11.4bb$PRS_se),
                  Pval = c(FGR11.4bb$EAhigh_p,FGR11.4bb$`PRS:EAhigh_p`,FGR11.4bb$PRS_p),
                  HR = c(FGR11.4bb$EAhigh_HR,FGR11.4bb$`PRS:EAhigh_HR`,FGR11.4bb$PRS_HR),
                  Cineg = c(FGR11.4bb$EAhigh_HR_lower95,FGR11.4bb$`PRS:EAhigh_HR_lower95`,FGR11.4bb$PRS_HR_lower95),
                  Cipos = c(FGR11.4bb$EAhigh_HR_upper95,FGR11.4bb$`PRS:EAhigh_HR_upper95`,FGR11.4bb$PRS_HR_upper95))
# adjust labels
TS38$Phenotype <- factor(TS38$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS38, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable38.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 39: Compare Cox estimates model 4 in full FinnGen vs after excl. cases
# before cohort entry for EA
#
################################################################################

# Create table 39
TS39 <- data.frame(Phenotype = c(rep(modbbsign$trait,3)),
                   Test = c(rep("High Education",nrow(modbbsign)),
                            rep("PGS",nrow(modbbsign)),
                            rep("EAxPGS",nrow(modbbsign))),
                   HR_Full = c(modbbsign$Model1_HR),
                   HR_Subset = c(modbbsign$Model2_HR),
                   beta_Full = c(modbbsign$Model1b),
                   beta_Subset = c(modbbsign$Model2b),
                   se_Full = c(modbbsign$Model1se),
                   se_Subset = c(modbbsign$Model2se),
                   HR_difference = c(modbbsign$difference),
                   beta_difference = c(modbbsign$Betadiff),
                   se_difference = c(modbbsign$SEDiff),
                   Pval_difference = c(modbbsign$PvalDiff))

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS39$lb_beta_difference <- TS39$beta_difference - 1.96*TS39$se_difference
TS39$ub_beta_difference <- TS39$beta_difference + 1.96*TS39$se_difference
#
TS39$lb_difference <- exp(TS39$lb_beta_difference)
TS39$ub_difference <- exp(TS39$ub_beta_difference)

# write file
write.table(TS39, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable39.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 40: descriptive statistics FinnGen with mutual exclussive cases
#
################################################################################

# create Table TS40
TS40 <- FGR11.Nmc
#adjust column names
names(TS40) <- c("Phenotype",names(TS40[2:23])) # trait = Phenotype
# adjust labels
TS40$Phenotype <- factor(TS40$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS40, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_EducationalAttainment_eTable40.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 41: Cox estimates model 4 in FinnGen for EA with mutual exclussive cases
#
################################################################################

# create table with model 4 results
TS41 <- data.frame(Test = c(rep("high educational attainment",nrow(FGR11.4mc)), rep("EAxPGS",nrow(FGR11.4mc)), rep("PGS",nrow(FGR11.4mc))),
                   Phenotype = c(rep(FGR11.4mc$trait,3)),
                   Beta = c(FGR11.4mc$EAhigh_beta,FGR11.4mc$`PRS:EAhigh_beta`,FGR11.4mc$PRS_beta),
                   SE = c(FGR11.4mc$EAhigh_se,FGR11.4mc$`PRS:EAhigh_se`,FGR11.4mc$PRS_se),
                   Pval = c(FGR11.4mc$EAhigh_p,FGR11.4mc$`PRS:EAhigh_p`,FGR11.4mc$PRS_p),
                   HR = c(FGR11.4mc$EAhigh_HR,FGR11.4mc$`PRS:EAhigh_HR`,FGR11.4mc$PRS_HR),
                   Cineg = c(FGR11.4mc$EAhigh_HR_lower95,FGR11.4mc$`PRS:EAhigh_HR_lower95`,FGR11.4mc$PRS_HR_lower95),
                   Cipos = c(FGR11.4mc$EAhigh_HR_upper95,FGR11.4mc$`PRS:EAhigh_HR_upper95`,FGR11.4mc$PRS_HR_upper95))
# adjust labels
TS41$Phenotype <- factor(TS41$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS41, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_EducationalAttainment_eTable41.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 42: Compare Cox estimates model 4 in full FinnGen vs with mututal
# excussive cases for EA
#
################################################################################

# Create table 42
TS42 <- data.frame(Phenotype = c(rep(modmcsign$trait,3)),
                   Test = c(rep("High Education",nrow(modmcsign)),
                            rep("PGS",nrow(modmcsign)),
                            rep("EAxPGS",nrow(modmcsign))),
                   HR_Full = c(modmcsign$Model1_HR),
                   HR_Subset = c(modmcsign$Model2_HR),
                   beta_Full = c(modmcsign$Model1b),
                   beta_Subset = c(modmcsign$Model2b),
                   se_Full = c(modmcsign$Model1se),
                   se_Subset = c(modmcsign$Model2se),
                   HR_difference = c(modmcsign$difference),
                   beta_difference = c(modmcsign$Betadiff),
                   se_difference = c(modmcsign$SEDiff),
                   Pval_difference = c(modmcsign$PvalDiff))

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS42$lb_beta_difference <- TS42$beta_difference - 1.96*TS42$se_difference
TS42$ub_beta_difference <- TS42$beta_difference + 1.96*TS42$se_difference
#
TS42$lb_difference <- exp(TS42$lb_beta_difference)
TS42$ub_difference <- exp(TS42$ub_beta_difference)

# write file
write.table(TS42, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_EducationalAttainment_eTable42.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 43: Cox estimates model 4 in FinnGen for EA including EA PGS + its
# interaction with the disease-specific PGSs
#
################################################################################

# create table with model 4 results
TS43 <- data.frame(Test = c(rep("high educational attainment",nrow(FGR11.4gg)), rep("EAxPGS",nrow(FGR11.4gg)), rep("Disease PGS",nrow(FGR11.4gg)),
                            rep("EA PGS",nrow(FGR11.4gg)), rep("EA PGSxDisease PGS",nrow(FGR11.4gg))),
                   Phenotype = c(rep(FGR11.4gg$trait,5)),
                   Beta = c(FGR11.4gg$EAhigh_beta,FGR11.4gg$`PRS:EAhigh_beta`,FGR11.4gg$PRS_beta,
                            FGR11.4gg$EAPGS_beta,FGR11.4gg$`PRS:EAPGS_beta`),
                   SE = c(FGR11.4gg$EAhigh_se,FGR11.4gg$`PRS:EAhigh_se`,FGR11.4gg$PRS_se,
                          FGR11.4gg$EAPGS_se, FGR11.4gg$`PRS:EAPGS_se`),
                   Pval = c(FGR11.4gg$EAhigh_p,FGR11.4gg$`PRS:EAhigh_p`,FGR11.4gg$PRS_p,
                            FGR11.4gg$EAPGS_p, FGR11.4gg$`PRS:EAPGS_p`),
                   HR = c(FGR11.4gg$EAhigh_HR,FGR11.4gg$`PRS:EAhigh_HR`,FGR11.4gg$PRS_HR,
                          FGR11.4gg$EAPGS_HR, FGR11.4gg$`PRS:EAPGS_HR`),
                   Cineg = c(FGR11.4gg$EAhigh_HR_lower95,FGR11.4gg$`PRS:EAhigh_HR_lower95`,FGR11.4gg$PRS_HR_lower95,
                             FGR11.4gg$EAPGS_HR_lower95, FGR11.4gg$`PRS:EAPGS_HR_lower95`),
                   Cipos = c(FGR11.4gg$EAhigh_HR_upper95,FGR11.4gg$`PRS:EAhigh_HR_upper95`,FGR11.4gg$PRS_HR_upper95,
                             FGR11.4gg$EAPGS_HR_upper95, FGR11.4gg$`PRS:EAPGS_HR_upper95`))
# adjust labels
TS43$Phenotype <- factor(TS43$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS43, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_EducationalAttainment_eTable43.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 44: Compare Cox estimates model 4 in original FinnGen vs including EA
# PGS + its interaction with disease-specific PGSs
#
################################################################################

# Create table 44
TS44 <- data.frame(Phenotype = c(rep(modggsign$trait,3)),
                   Test = c(rep("High Education",nrow(modggsign)),
                            rep("PGS",nrow(modggsign)),
                            rep("EAxPGS",nrow(modggsign))),
                   HR_Full = c(modggsign$Model1_HR),
                   HR_Subset = c(modggsign$Model2_HR),
                   beta_Full = c(modggsign$Model1b),
                   beta_Subset = c(modggsign$Model2b),
                   se_Full = c(modggsign$Model1se),
                   se_Subset = c(modggsign$Model2se),
                   HR_difference = c(modggsign$difference),
                   beta_difference = c(modggsign$Betadiff),
                   se_difference = c(modggsign$SEDiff),
                   Pval_difference = c(modggsign$PvalDiff))

# add columns with 95% CIs (first calculate based on beta than convert to HR scale)
TS44$lb_beta_difference <- TS44$beta_difference - 1.96*TS44$se_difference
TS44$ub_beta_difference <- TS44$beta_difference + 1.96*TS44$se_difference
#
TS44$lb_difference <- exp(TS44$lb_beta_difference)
TS44$ub_difference <- exp(TS44$ub_beta_difference)

# write file
write.table(TS44, file = paste0("output/Tables/",as.character(Sys.Date()),
                                "_INTERVENE_EducationalAttainment_eTable44.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 45: descriptive statistics FinnGen for Fine-Gray model for EA
#
################################################################################

# create Table TS21
TS45 <- FGR11.NFG
#adjust column names
names(TS45) <- c("Phenotype",names(TS45[2:19])) # trait = Phenotype
# adjust labels
TS45$Phenotype <- factor(TS45$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS45, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable45.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 46: Fine-Gray estimates model 4 in FinnGen for EA
#
################################################################################

# create table with model 4 results
TS46 <- data.frame(Test = c(rep("high educational attainment",nrow(FGR11.4FG)), rep("EAxPGS",nrow(FGR11.4FG)), rep("PGS",nrow(FGR11.4FG))),
                  Phenotype = c(rep(FGR11.4FG$trait,3)),
                  Beta = c(FGR11.4FG$EAhigh_beta,FGR11.4FG$interaction_beta,FGR11.4FG$PRS_beta),
                  SE = c(FGR11.4FG$EAhigh_se,FGR11.4FG$interaction_se,FGR11.4FG$PRS_se),
                  Pval = c(FGR11.4FG$EAhigh_p,FGR11.4FG$interaction_p,FGR11.4FG$PRS_p),
                  HR = c(FGR11.4FG$EAhigh_HR,FGR11.4FG$interaction_HR,FGR11.4FG$PRS_HR),
                  Cineg = c(FGR11.4FG$EAhigh_HR_lower95,FGR11.4FG$interaction_HR_lower,FGR11.4FG$PRS_HR_lower95),
                  Cipos = c(FGR11.4FG$EAhigh_HR_upper95,FGR11.4FG$interaction_HR_upper,FGR11.4FG$PRS_HR_upper95))
# adjust labels
TS46$Phenotype <- factor(TS46$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS46, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable46.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 47: Compare Cox and FG estimates model 4 in FinnGen for EA
#
################################################################################

# Create table 47
TS47 <- data.frame(Phenotype = c(rep(modCFsign$trait,3)),
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
TS47$lb_beta_difference <- TS47$beta_difference - 1.96*TS47$se_difference
TS47$ub_beta_difference <- TS47$beta_difference + 1.96*TS47$se_difference
#
TS47$lb_difference <- exp(TS47$lb_beta_difference)
TS47$ub_difference <- exp(TS47$ub_beta_difference)

# write file
write.table(TS47, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable47.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 48: descriptive statistics non-EU ancestries UKB for EA
#
################################################################################

# combine ancestries
TS48 <- rbind(UKB.N.SAS,UKB.N.EAS,UKB.N.AFR)
#reorder columns
TS48 <- TS48[,c(1,23,2:22)]
#adjust column names
names(TS48) <- c("Phenotype","Ancestry",names(TS48[3:23])) # trait = Phenotype
# adjust labels
TS48$Phenotype <- factor(TS48$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS48, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable48.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# eTable 49: UKB non-EU ancestry results model 1 for EA
#
################################################################################

# create Table S49
TS49 <- data.frame(Ancestry = c(UKB.1a.SAS$Biobank,UKB.1b.SAS$Biobank,
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
TS49$Phenotype <- factor(TS49$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
write.table(TS49, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_eTable49.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

