#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# education [EA] and occuption) in risk of 18 common diseases (as previously
# selected in the INTERVENE flagship manuscript:
# https://doi.org/10.1101/2023.06.12.23291186) and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Compare results model 1 in Educational Attainment and Occupation
# analyses
#
# Data: 
#       1) FGR11 + UKB + GS + fixed-effect meta-analysis model 1a (EA + occupation)
#       2) FGR11 + UKB + GS + fixed-effect meta-analysis model 1b (PGS)
#
# Last edits: 17/04/2026 (edits, FAH: last edits before GitHub Upload)
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
# wrangling; readxl = read excel files (upload to googledrive converts csv to
# xlsx.
packages("data.table","dplyr","forcats","stringr","tidyr","readxl")


################################################################################
#
# Read in full sample hazard ratios per standard deviation
#
################################################################################

# read in model 1a - EA/OCC only
FGR11.1a.EA <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt", data.table=FALSE)
FGR11.1a.EA$Biobank <- "FinnGen"
FGR11.1a.OCC <- fread("output/GoogleDrive/FGR11/2025-01-30_INTERVENE_Occupation_Coeffs_CoxPH_model1a_FinnGenR11.txt", data.table=FALSE)
FGR11.1a.OCC$ Biobank <- "FinnGen"
#
UKB.1a.EA <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.1a.EA$Biobank <- "UK Biobank"
UKB.1a.OCC <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.1a.OCC$Biobank <- "UK Biobank"
#
GS.1a.EA <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
GS.1a.EA$Biobank <- "Generation Scotland"
GS.1a.OCC <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
GS.1a.OCC$Biobank <- "Generation Scotland"
#
FEMA.1a.EA <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.xlsx"))
FEMA.1a.EA$Biobank <- "FE meta-analysis"
FEMA.1a.OCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv",data.table=FALSE)
FEMA.1a.OCC$Biobank <- "FE meta-analysis"
#
REMA.1a.EA <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv",data.table=FALSE)
REMA.1a.EA$Biobank <- "RE meta-analysis"
REMA.1a.OCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv",data.table=FALSE)
REMA.1a.OCC$Biobank <- "RE meta-analysis"

# read in model 1b - PRS only
FGR11.1b.EA <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt", data.table=FALSE)
FGR11.1b.EA$Biobank <- "FinnGen"
FGR11.1b.OCC <- fread("output/GoogleDrive/FGR11/2025-01-30_INTERVENE_Occupation_Coeffs_CoxPH_model1b_FinnGenR11.txt", data.table=FALSE)
FGR11.1b.OCC$Biobank <- "FinnGen"
#
UKB.1b.EA <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.1b.EA$Biobank <- "UK Biobank"
UKB.1b.OCC <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.1b.OCC$Biobank <- "UK Biobank"
#
GS.1b.EA <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
GS.1b.EA$Biobank <- "Generation Scotland"
GS.1b.OCC <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
GS.1b.OCC$Biobank <- "Generation Scotland"
#
FEMA.1b.EA <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.xlsx"))
FEMA.1b.EA$Biobank <- "FE meta-analysis"
FEMA.1b.OCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv",data.table=FALSE)
FEMA.1b.OCC$Biobank <- "FE meta-analysis"
#
REMA.1b.EA <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv",data.table=FALSE)
REMA.1b.EA$Biobank <- "RE meta-analysis"
REMA.1b.OCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv",data.table=FALSE)
REMA.1b.OCC$Biobank <- "RE meta-analysis"


################################################################################
#
# Generation Scotland includes traits which have too small sample sizes to
# perform the analyses in. Remove these traits.
#
################################################################################

GS.1a.OCC <- GS.1a.OCC[-which(GS.1a.OCC$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                                  "C3_MELANOMA_SKIN","G6_EPLEPSY")),]
GS.1b.OCC <- GS.1b.OCC[-which(GS.1b.OCC$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                                  "C3_MELANOMA_SKIN","G6_EPLEPSY")),]


################################################################################
#
# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits
#
################################################################################

# Educational attainment
FEMA.1a.EA <- FEMA.1a.EA[-which(FEMA.1a.EA$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
FEMA.1b.EA <- FEMA.1b.EA[-which(FEMA.1b.EA$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
#
REMA.1a.EA <- REMA.1a.EA[-which(REMA.1a.EA$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
REMA.1b.EA <- REMA.1b.EA[-which(REMA.1b.EA$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]

# Occupation
FEMA.1a.OCC <- FEMA.1a.OCC[-which(FEMA.1a.OCC$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                            "C3_MELANOMA_SKIN")),]
FEMA.1b.OCC <- FEMA.1b.OCC[-which(FEMA.1b.OCC$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                            "C3_MELANOMA_SKIN")),]
#
REMA.1a.OCC <- REMA.1a.OCC[-which(REMA.1a.OCC$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                               "C3_MELANOMA_SKIN")),]
REMA.1b.OCC <- REMA.1b.OCC[-which(REMA.1b.OCC$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                               "C3_MELANOMA_SKIN")),]


################################################################################
#
# Rename "OccupationUpper-level" because this crashes R
#
################################################################################

names(FGR11.1a.OCC) <- gsub("-", "", names(FGR11.1a.OCC))
names(FGR11.1b.OCC) <- gsub("-", "", names(FGR11.1b.OCC))
#
names(UKB.1a.OCC) <- gsub("-","",names(UKB.1a.OCC))
names(UKB.1b.OCC) <- gsub("-","",names(UKB.1b.OCC))
#
names(GS.1a.OCC) <- gsub("-","", names(GS.1a.OCC))
names(GS.1b.OCC) <- gsub("-","", names(GS.1b.OCC))


################################################################################
#
# Reorganize results for plotting - fixed effect
#
################################################################################

# model 1: EA 
model1.EA <- data.frame(trait = c(FGR11.1a.EA$trait,FGR11.1b.EA$trait,
                               UKB.1a.EA$trait,UKB.1b.EA$trait,
                               GS.1a.EA$trait,GS.1b.EA$trait,
                               FEMA.1a.EA$Phenotype,FEMA.1b.EA$Phenotype),
                     HR = c(FGR11.1a.EA$EAhigh_HR,FGR11.1b.EA$PRS_HR,
                            UKB.1a.EA$EAhigh_HR,UKB.1b.EA$PRS_HR,
                            GS.1a.EA$EAhigh_HR,GS.1b.EA$PRS_HR,
                            FEMA.1a.EA$HR,FEMA.1b.EA$HR),
                     lb = c(FGR11.1a.EA$EAhigh_HR_lower95,FGR11.1b.EA$PRS_HR_lower95,
                            UKB.1a.EA$EAhigh_HR_lower95,UKB.1b.EA$PRS_HR_lower95,
                            GS.1a.EA$EAhigh_HR_lower95,GS.1b.EA$PRS_HR_lower95,
                            FEMA.1a.EA$Cineg,FEMA.1b.EA$Cineg),
                     ub = c(FGR11.1a.EA$EAhigh_HR_upper95,FGR11.1b.EA$PRS_HR_upper95,
                            UKB.1a.EA$EAhigh_HR_upper95,UKB.1b.EA$PRS_HR_upper95,
                            GS.1a.EA$EAhigh_HR_upper95,GS.1b.EA$PRS_HR_upper95,
                            FEMA.1a.EA$Cipos,FEMA.1b.EA$Cipos),
                     beta = c(FGR11.1a.EA$EAhigh_beta,FGR11.1b.EA$PRS_beta,
                              UKB.1a.EA$EAhigh_beta,UKB.1b.EA$PRS_beta,
                              GS.1a.EA$EAhigh_beta,GS.1b.EA$PRS_beta,
                              FEMA.1a.EA$Beta,FEMA.1b.EA$Beta),
                     se = c(FGR11.1a.EA$EAhigh_se,FGR11.1b.EA$PRS_se,
                            UKB.1a.EA$EAhigh_se,UKB.1b.EA$PRS_se,
                            GS.1a.EA$EAhigh_se,GS.1b.EA$PRS_se,
                            FEMA.1a.EA$SE,FEMA.1b.EA$SE),
                     Test = c(rep("High Education",nrow(FGR11.1a.EA)),rep("PRS",nrow(FGR11.1b.EA)),
                              rep("High Education",nrow(UKB.1a.EA)),rep("PRS",nrow(UKB.1b.EA)),
                              rep("High Education",nrow(GS.1a.EA)),rep("PRS",nrow(GS.1b.EA)),
                              rep("High Education",nrow(FEMA.1a.EA)),rep("PRS",nrow(FEMA.1b.EA))),
                     Biobanks = c(FGR11.1a.EA$Biobank,FGR11.1b.EA$Biobank,
                                  UKB.1a.EA$Biobank,UKB.1b.EA$Biobank,
                                  GS.1a.EA$Biobank,GS.1b.EA$Biobank,
                                  FEMA.1a.EA$Biobank,FEMA.1b.EA$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model1.EA$trait <- factor(model1.EA$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                      "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                      "C3_COLORECTAL","I9_AF","I9_CHD",
                                                      "COX_ARTHROSIS","C3_MELANOMA_SKIN",
                                                      "J10_ASTHMA","KNEE_ARTHROSIS",
                                                      "F5_DEPRESSIO","C3_BRONCHUS_LUNG",
                                                      "C3_CANCER","K11_APPENDACUT","G6_EPLEPSY",
                                                      "AUD_SWEDISH"),
                       labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                                  "Colorectal Cancer","Atrial Fibrillation",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                  "Major Depression","Lung Cancer","Any Cancer",
                                  "Appendicitis","Epilepsy","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model1.EA$trait <- fct_rev(model1.EA$trait)
# EA levels as factor to plot them in order of magnitude
model1.EA$Test <- factor(model1.EA$Test, levels = c("High Education","PRS"), 
                      labels = c("High Education","PRS"))
model1.EA$Biobanks <- factor(model1.EA$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                             labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB & GS estimates
model1.EA <- model1.EA[-which(model1.EA$Biobanks=="UK Biobank" | model1.EA$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
model1.EA <- model1.EA[-which(model1.EA$trait[which(model1.EA$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]

# model 1: OCC 
model1.OCC <- data.frame(trait = c(FGR11.1a.OCC$trait,FGR11.1b.OCC$trait,
                               UKB.1a.OCC$trait,UKB.1b.OCC$trait,
                               GS.1a.OCC$trait,GS.1b.OCC$trait,
                               FEMA.1a.OCC$Phenotype,FEMA.1b.OCC$Phenotype),
                     HR = c(FGR11.1a.OCC$OccupationUpperlevel_HR,FGR11.1b.OCC$PRS_HR,
                            UKB.1a.OCC$OccupationUpperlevel_HR,UKB.1b.OCC$PRS_HR,
                            GS.1a.OCC$OccupationUpperlevel_HR,GS.1b.OCC$PRS_HR,
                            FEMA.1a.OCC$HR, FEMA.1b.OCC$HR),
                     lb = c(FGR11.1a.OCC$OccupationUpperlevel_HR_lower95,FGR11.1b.OCC$PRS_HR_lower95,
                            UKB.1a.OCC$OccupationUpperlevel_HR_lower95,UKB.1b.OCC$PRS_HR_lower95,
                            GS.1a.OCC$OccupationUpperlevel_HR_lower95,GS.1b.OCC$PRS_HR_lower95,
                            FEMA.1a.OCC$Cineg, FEMA.1b.OCC$Cineg),
                     ub = c(FGR11.1a.OCC$OccupationUpperlevel_HR_upper95,FGR11.1b.OCC$PRS_HR_upper95,
                            UKB.1a.OCC$OccupationUpperlevel_HR_upper95,UKB.1b.OCC$PRS_HR_upper95,
                            GS.1a.OCC$OccupationUpperlevel_HR_upper95,GS.1b.OCC$PRS_HR_upper95,
                            FEMA.1a.OCC$Cipos,FEMA.1b.OCC$Cipos),
                     beta = c(FGR11.1a.OCC$OccupationUpperlevel_beta,FGR11.1b.OCC$PRS_beta,
                              UKB.1a.OCC$OccupationUpperlevel_beta,UKB.1b.OCC$PRS_beta,
                              GS.1a.OCC$OccupationUpperlevel_beta,GS.1b.OCC$PRS_beta,
                              FEMA.1a.OCC$Beta,FEMA.1b.OCC$Beta),
                     se = c(FGR11.1a.OCC$OccupationUpperlevel_se,FGR11.1b.OCC$PRS_se,
                            UKB.1a.OCC$OccupationUpperlevel_se,UKB.1b.OCC$PRS_se,
                            GS.1a.OCC$OccupationUpperlevel_se,GS.1b.OCC$PRS_se,
                            FEMA.1a.OCC$SE,FEMA.1b.OCC$SE),
                     Test = c(rep("Upper level occupation",nrow(FGR11.1a.OCC)),
                              rep("PRS",nrow(FGR11.1b.OCC)),
                              rep("Upper level occupation",nrow(UKB.1a.OCC)),
                              rep("PRS",nrow(UKB.1b.OCC)),
                              rep("Upper level occupation",nrow(GS.1a.OCC)),
                              rep("PRS",nrow(GS.1b.OCC)),
                              rep("Upper level occupation",nrow(FEMA.1a.OCC)),
                              rep("PRS",nrow(FEMA.1b.OCC))),
                     Biobanks = c(FGR11.1a.OCC$Biobank,FGR11.1b.OCC$Biobank,
                                  UKB.1a.OCC$Biobank,UKB.1b.OCC$Biobank,
                                  GS.1a.OCC$Biobank,GS.1b.OCC$Biobank,
                                  FEMA.1a.OCC$Biobank,FEMA.1b.OCC$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model1.OCC$trait <- factor(model1.OCC$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                        "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                        "C3_COLORECTAL","I9_AF","I9_CHD",
                                                        "COX_ARTHROSIS","C3_MELANOMA_SKIN",
                                                        "J10_ASTHMA","KNEE_ARTHROSIS",
                                                        "F5_DEPRESSIO","C3_BRONCHUS_LUNG",
                                                        "C3_CANCER","K11_APPENDACUT","G6_EPLEPSY",
                                                        "AUD_SWEDISH"),
                       labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                                  "Colorectal Cancer","Atrial Fibrillation",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                  "Major Depression","Lung Cancer","Any Cancer",
                                  "Appendicitis","Epilepsy","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model1.OCC$trait <- fct_rev(model1.OCC$trait)
# OCC levels as factor to plot them in order of magnitude
model1.OCC$Test <- factor(model1.OCC$Test, levels = c("Upper level occupation","PRS"), 
                      labels = c("Upper-level occupation","PRS"))
model1.OCC$Biobanks <- factor(model1.OCC$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                             labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB & GS estimates
model1.OCC <- model1.OCC[-which(model1.OCC$Biobanks=="UK Biobank" | model1.OCC$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
model1.OCC <- model1.OCC[-which(model1.OCC$trait[which(model1.OCC$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]

# combine data frames model 1 Ea and OCC for plotting 
df12 <- rbind(model1.EA,model1.OCC) #row bind
df12$Model <- c(rep(c("model1", "model2"),each=nrow(model1.EA)))



################################################################################
#
# Calculate significance differences between EA and OCC model 1a/b - fixed effects
#
################################################################################

# split by Test for calculation
df12.SES <- df12[which(df12$Test=="High Education" | df12$Test=="Upper-level occupation"),]
df12.PRS <- df12[which(df12$Test=="PRS"),]

# calculate difference in the education and PGS effect between model 1 and 2 for each
# trait to inform ordering of the traits in the plot
# arrange dataframe by trait and Test group (education vs PGS)
plot12.df.SES <- df12.SES %>%
  arrange(trait, Model) 
plot12.df.PRS <- df12.PRS %>%
  arrange(trait, Model) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot12_grouped_SES <- plot12.df.SES %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Model == "model1"],
    Model2_HR = HR[Model == "model2"],
    Model1_HR_lb = lb[Model == "model1"],
    Model2_HR_lb = lb[Model == "model2"],
    Model1_HR_ub = ub[Model == "model1"],
    Model2_HR_ub = ub[Model == "model2"],
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
    Model1_HR_lb = lb[Model == "model1"],
    Model2_HR_lb = lb[Model == "model2"],
    Model1_HR_ub = ub[Model == "model1"],
    Model2_HR_ub = ub[Model == "model2"],
    Model1b = beta[Model == "model1"],
    Model2b = beta[Model == "model2"],
    Model1se = se[Model == "model1"],
    Model2se = se[Model == "model2"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )

# Determine whether difference between PGS effect (beta) low and Upper-level occupation
# groups is significant. Code adapted from:
# https://github.com/intervene-EU-H2020/flagship/blob/main/MetaAnalysis/AgeSpecific_MetaAnalysisandHetero.R
# calculate z-value
plot12_diff_SES <- plot12_grouped_SES %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot12_diff_PRS <- plot12_grouped_PRS %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot12_diff_SES <- plot12_diff_SES %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot12_diff_PRS <- plot12_diff_PRS %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot12_diff_SES <- plot12_diff_SES %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot12_diff_PRS <- plot12_diff_PRS %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))

# combine OCC and PRS results
plot12_diff_SES$Test <- "SES"
plot12_diff_PRS$Test <- "PRS"
plot12_diff <- rbind(plot12_diff_SES,plot12_diff_PRS)

# write to file
fwrite(plot12_diff, paste("output/EmploymentStatus/MetaAnalysis/FGR11_UKB_GS/model1/", as.character(Sys.Date()),
                          "_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1_EAvsOccupationDifferences.csv",sep=""))


################################################################################
#
# Reorganize results for plotting - random effect
#
################################################################################

# model 1: EA 
model1r.EA <- data.frame(trait = c(FGR11.1a.EA$trait,FGR11.1b.EA$trait,
                                   UKB.1a.EA$trait,UKB.1b.EA$trait,
                                   GS.1a.EA$trait,GS.1b.EA$trait,
                                   REMA.1a.EA$Phenotype,REMA.1b.EA$Phenotype),
                         HR = c(FGR11.1a.EA$EAhigh_HR,FGR11.1b.EA$PRS_HR,
                                UKB.1a.EA$EAhigh_HR,UKB.1b.EA$PRS_HR,
                                GS.1a.EA$EAhigh_HR,GS.1b.EA$PRS_HR,
                                REMA.1a.EA$HR,REMA.1b.EA$HR),
                         lb = c(FGR11.1a.EA$EAhigh_HR_lower95,FGR11.1b.EA$PRS_HR_lower95,
                                UKB.1a.EA$EAhigh_HR_lower95,UKB.1b.EA$PRS_HR_lower95,
                                GS.1a.EA$EAhigh_HR_lower95,GS.1b.EA$PRS_HR_lower95,
                                REMA.1a.EA$Cineg,REMA.1b.EA$Cineg),
                         ub = c(FGR11.1a.EA$EAhigh_HR_upper95,FGR11.1b.EA$PRS_HR_upper95,
                                UKB.1a.EA$EAhigh_HR_upper95,UKB.1b.EA$PRS_HR_upper95,
                                GS.1a.EA$EAhigh_HR_upper95,GS.1b.EA$PRS_HR_upper95,
                                REMA.1a.EA$Cipos,REMA.1b.EA$Cipos),
                         beta = c(FGR11.1a.EA$EAhigh_beta,FGR11.1b.EA$PRS_beta,
                                  UKB.1a.EA$EAhigh_beta,UKB.1b.EA$PRS_beta,
                                  GS.1a.EA$EAhigh_beta,GS.1b.EA$PRS_beta,
                                  REMA.1a.EA$Beta,REMA.1b.EA$Beta),
                         se = c(FGR11.1a.EA$EAhigh_se,FGR11.1b.EA$PRS_se,
                                UKB.1a.EA$EAhigh_se,UKB.1b.EA$PRS_se,
                                GS.1a.EA$EAhigh_se,GS.1b.EA$PRS_se,
                                REMA.1a.EA$SE,REMA.1b.EA$SE),
                         Test = c(rep("High Education",nrow(FGR11.1a.EA)),rep("PRS",nrow(FGR11.1b.EA)),
                                  rep("High Education",nrow(UKB.1a.EA)),rep("PRS",nrow(UKB.1b.EA)),
                                  rep("High Education",nrow(GS.1a.EA)),rep("PRS",nrow(GS.1b.EA)),
                                  rep("High Education",nrow(REMA.1a.EA)),rep("PRS",nrow(REMA.1b.EA))),
                         Biobanks = c(FGR11.1a.EA$Biobank,FGR11.1b.EA$Biobank,
                                      UKB.1a.EA$Biobank,UKB.1b.EA$Biobank,
                                      GS.1a.EA$Biobank,GS.1b.EA$Biobank,
                                      REMA.1a.EA$Biobank,REMA.1b.EA$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model1r.EA$trait <- factor(model1r.EA$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                        "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                        "C3_COLORECTAL","I9_AF","I9_CHD",
                                                        "COX_ARTHROSIS","C3_MELANOMA_SKIN",
                                                        "J10_ASTHMA","KNEE_ARTHROSIS",
                                                        "F5_DEPRESSIO","C3_BRONCHUS_LUNG",
                                                        "C3_CANCER","K11_APPENDACUT","G6_EPLEPSY",
                                                        "AUD_SWEDISH"),
                           labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                      "Gout","Rheumatoid Arthritis","Breast Cancer",
                                      "Colorectal Cancer","Atrial Fibrillation",
                                      "Coronary Heart Disease","Hip Osteoarthritis",
                                      "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                      "Major Depression","Lung Cancer","Any Cancer",
                                      "Appendicitis","Epilepsy","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model1r.EA$trait <- fct_rev(model1r.EA$trait)
# EA levels as factor to plot them in order of magnitude
model1r.EA$Test <- factor(model1r.EA$Test, levels = c("High Education","PRS"), 
                          labels = c("High Education","PRS"))
model1r.EA$Biobanks <- factor(model1r.EA$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                              labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB & GS estimates
model1r.EA <- model1r.EA[-which(model1r.EA$Biobanks=="UK Biobank" | model1r.EA$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
model1r.EA <- model1r.EA[-which(model1r.EA$trait[which(model1r.EA$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]

# model 1: OCC 
model1r.OCC <- data.frame(trait = c(FGR11.1a.OCC$trait,FGR11.1b.OCC$trait,
                                    UKB.1a.OCC$trait,UKB.1b.OCC$trait,
                                    GS.1a.OCC$trait,GS.1b.OCC$trait,
                                    REMA.1a.OCC$Phenotype,REMA.1b.OCC$Phenotype),
                          HR = c(FGR11.1a.OCC$OccupationUpperlevel_HR,FGR11.1b.OCC$PRS_HR,
                                 UKB.1a.OCC$OccupationUpperlevel_HR,UKB.1b.OCC$PRS_HR,
                                 GS.1a.OCC$OccupationUpperlevel_HR,GS.1b.OCC$PRS_HR,
                                 REMA.1a.OCC$HR, REMA.1b.OCC$HR),
                          lb = c(FGR11.1a.OCC$OccupationUpperlevel_HR_lower95,FGR11.1b.OCC$PRS_HR_lower95,
                                 UKB.1a.OCC$OccupationUpperlevel_HR_lower95,UKB.1b.OCC$PRS_HR_lower95,
                                 GS.1a.OCC$OccupationUpperlevel_HR_lower95,GS.1b.OCC$PRS_HR_lower95,
                                 REMA.1a.OCC$Cineg, REMA.1b.OCC$Cineg),
                          ub = c(FGR11.1a.OCC$OccupationUpperlevel_HR_upper95,FGR11.1b.OCC$PRS_HR_upper95,
                                 UKB.1a.OCC$OccupationUpperlevel_HR_upper95,UKB.1b.OCC$PRS_HR_upper95,
                                 GS.1a.OCC$OccupationUpperlevel_HR_upper95,GS.1b.OCC$PRS_HR_upper95,
                                 REMA.1a.OCC$Cipos,REMA.1b.OCC$Cipos),
                          beta = c(FGR11.1a.OCC$OccupationUpperlevel_beta,FGR11.1b.OCC$PRS_beta,
                                   UKB.1a.OCC$OccupationUpperlevel_beta,UKB.1b.OCC$PRS_beta,
                                   GS.1a.OCC$OccupationUpperlevel_beta,GS.1b.OCC$PRS_beta,
                                   REMA.1a.OCC$Beta,REMA.1b.OCC$Beta),
                          se = c(FGR11.1a.OCC$OccupationUpperlevel_se,FGR11.1b.OCC$PRS_se,
                                 UKB.1a.OCC$OccupationUpperlevel_se,UKB.1b.OCC$PRS_se,
                                 GS.1a.OCC$OccupationUpperlevel_se,GS.1b.OCC$PRS_se,
                                 REMA.1a.OCC$SE,REMA.1b.OCC$SE),
                          Test = c(rep("Upper level occupation",nrow(FGR11.1a.OCC)),
                                   rep("PRS",nrow(FGR11.1b.OCC)),
                                   rep("Upper level occupation",nrow(UKB.1a.OCC)),
                                   rep("PRS",nrow(UKB.1b.OCC)),
                                   rep("Upper level occupation",nrow(GS.1a.OCC)),
                                   rep("PRS",nrow(GS.1b.OCC)),
                                   rep("Upper level occupation",nrow(REMA.1a.OCC)),
                                   rep("PRS",nrow(REMA.1b.OCC))),
                          Biobanks = c(FGR11.1a.OCC$Biobank,FGR11.1b.OCC$Biobank,
                                       UKB.1a.OCC$Biobank,UKB.1b.OCC$Biobank,
                                       GS.1a.OCC$Biobank,GS.1b.OCC$Biobank,
                                       REMA.1a.OCC$Biobank,REMA.1b.OCC$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model1r.OCC$trait <- factor(model1r.OCC$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                          "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                          "C3_COLORECTAL","I9_AF","I9_CHD",
                                                          "COX_ARTHROSIS","C3_MELANOMA_SKIN",
                                                          "J10_ASTHMA","KNEE_ARTHROSIS",
                                                          "F5_DEPRESSIO","C3_BRONCHUS_LUNG",
                                                          "C3_CANCER","K11_APPENDACUT","G6_EPLEPSY",
                                                          "AUD_SWEDISH"),
                            labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                       "Gout","Rheumatoid Arthritis","Breast Cancer",
                                       "Colorectal Cancer","Atrial Fibrillation",
                                       "Coronary Heart Disease","Hip Osteoarthritis",
                                       "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                       "Major Depression","Lung Cancer","Any Cancer",
                                       "Appendicitis","Epilepsy","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model1r.OCC$trait <- fct_rev(model1r.OCC$trait)
# OCC levels as factor to plot them in order of magnitude
model1r.OCC$Test <- factor(model1r.OCC$Test, levels = c("Upper level occupation","PRS"), 
                           labels = c("Upper-level occupation","PRS"))
model1r.OCC$Biobanks <- factor(model1r.OCC$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                               labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB & GS estimates
model1r.OCC <- model1r.OCC[-which(model1r.OCC$Biobanks=="UK Biobank" | model1r.OCC$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
model1r.OCC <- model1r.OCC[-which(model1r.OCC$trait[which(model1r.OCC$Biobanks=="FinnGen")] %in% 
                                    c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease",
                                      "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                      "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                      "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                      "Knee Osteoarthritis")),]

# combine data frames model 1 Ea and OCC for plotting 
df12r <- rbind(model1r.EA,model1r.OCC) #row bind
df12r$Model <- c(rep(c("model1", "model2"),each=nrow(model1r.EA)))


################################################################################
#
# Calculate significance differences between EA and OCC model 1a/b - random effects
#
################################################################################

# split by Test for calculation
df12r.SES <- df12r[which(df12r$Test=="High Education" | df12r$Test=="Upper-level occupation"),]
df12r.PRS <- df12r[which(df12r$Test=="PRS"),]

# calculate difference in the education and PGS effect between model 1 and 2 for each
# trait to inform ordering of the traits in the plot
# arrange dataframe by trait and Test group (education vs PGS)
plot12r.df.SES <- df12r.SES %>%
  arrange(trait, Model) 
plot12r.df.PRS <- df12r.PRS %>%
  arrange(trait, Model) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot12r_grouped_SES <- plot12r.df.SES %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Model == "model1"],
    Model2_HR = HR[Model == "model2"],
    Model1_HR_lb = lb[Model == "model1"],
    Model2_HR_lb = lb[Model == "model2"],
    Model1_HR_ub = ub[Model == "model1"],
    Model2_HR_ub = ub[Model == "model2"],
    Model1b = beta[Model == "model1"],
    Model2b = beta[Model == "model2"],
    Model1se = se[Model == "model1"],
    Model2se = se[Model == "model2"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )
plot12r_grouped_PRS <- plot12r.df.PRS %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Model == "model1"],
    Model2_HR = HR[Model == "model2"],
    Model1_HR_lb = lb[Model == "model1"],
    Model2_HR_lb = lb[Model == "model2"],
    Model1_HR_ub = ub[Model == "model1"],
    Model2_HR_ub = ub[Model == "model2"],
    Model1b = beta[Model == "model1"],
    Model2b = beta[Model == "model2"],
    Model1se = se[Model == "model1"],
    Model2se = se[Model == "model2"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )

# Determine whether difference between PGS effect (beta) low and Upper-level occupation
# groups is significant. Code adapted from:
# https://github.com/intervene-EU-H2020/flagship/blob/main/MetaAnalysis/AgeSpecific_MetaAnalysisandHetero.R
# calculate z-value
plot12r_diff_SES <- plot12r_grouped_SES %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot12r_diff_PRS <- plot12r_grouped_PRS %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot12r_diff_SES <- plot12r_diff_SES %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot12r_diff_PRS <- plot12r_diff_PRS %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot12r_diff_SES <- plot12r_diff_SES %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot12r_diff_PRS <- plot12r_diff_PRS %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))

# combine OCC and PRS results
plot12r_diff_SES$Test <- "SES"
plot12r_diff_PRS$Test <- "PRS"
plot12r_diff <- rbind(plot12r_diff_SES,plot12r_diff_PRS)

# write to file
fwrite(plot12r_diff, paste("output/EmploymentStatus/MetaAnalysis/FGR11_UKB_GS/model1/", as.character(Sys.Date()),
                           "_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1_EAvsOccupationDifferences.csv",sep=""))
