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
# Script: Compare results for model 4 (Education + PGS + interaction +
# covariates) using Cox Proportional Hazard Model with EA as binary (original
# meta-analysis) and EA without dichotomization
#
# Data: required input data: FinnGen data release 11 (FGR11) + UK Biobank (UKB) +
# Generation Scotland (GS) + meta-analysis (FEMA & REMA) model 4 where EA is
# binary or continuous
#
# Last edits: 17/04/2026 (edits, FAH: last edits before GitHub upload)
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
# wrangling.
packages("data.table","dplyr","forcats","stringr","tidyr")


################################################################################
#
# Read in full sample and subset hazard ratios per standard deviation
#
################################################################################

# read in model 4 - EA as binary - per biobank
FGR11.4a <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt", data.table=FALSE)
FGR11.4a$Biobank <- "FinnGen"
#
UKB.4a <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
UKB.4a$Biobank <- "UK Biobank"
#
GS.4a <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt",data.table=FALSE)
GS.4a$Biobank <- "Generation Scotland"

# read in model 4 - EA as continuous - per biobank
FGR11.4b <- fread("output/GoogleDrive/FGR11/2026-02-10_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model4_ISCEDcont_FinnGenR11.txt", data.table=FALSE)
FGR11.4b$Biobank <- "FinnGen"
#
UKB.4b <- fread("output/GoogleDrive/UKB/2026-02-16_UKBiobank_EUR_INTERVENE_EducationalAttainment_allISCED_CoxPH_model4_Coeffs.txt",data.table=FALSE)
UKB.4b$Biobank <- "UK Biobank"
#
GS.4b <- fread("output/GoogleDrive/GS/2026-03-02_GS_INTERVENE_EducationalAttainment_allISCED_CoxPH_model4_Coeffs.txt",data.table=FALSE)
GS.4b$Biobank <- "Generation Scotland"

# read in models meta-analysis - fixed effects
FEMA.4a <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4.xlsx"))
FEMA.4a$Biobank <- "FE meta-analysis"
#
FEMA.4b <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4_allISCED.csv",data.table=FALSE)
FEMA.4b$Biobank <- "FE meta-analysis"

# read in models meta-analysis - fixed effects
REMA.4a <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4.csv",data.table=FALSE)
REMA.4a$Biobank <- "FE meta-analysis"
#
REMA.4b <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model4_allISCED.csv",data.table=FALSE)
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

# model 4 - Binary 
model4a <- data.frame(trait = c(rep(FGR11.4a$trait,3),rep(UKB.4a$trait,3),
                                rep(GS.4a$trait,3),FEMA.4a$Phenotype),
                      HR = c(FGR11.4a$EAhigh_HR,FGR11.4a$PRS_HR,FGR11.4a$`PRS:EAhigh_HR`,
                             UKB.4a$EAhigh_HR,UKB.4a$PRS_HR,UKB.4a$`PRS:EAhigh_HR`,
                             GS.4a$EAhigh_HR,GS.4a$PRS_HR,GS.4a$`PRS:EAhigh_HR`,
                             FEMA.4a$HR),
                      lb = c(FGR11.4a$EAhigh_HR_lower95,FGR11.4a$PRS_HR_lower95,FGR11.4a$`PRS:EAhigh_HR_lower95`,
                             UKB.4a$EAhigh_HR_lower95,UKB.4a$PRS_HR_lower95,UKB.4a$`PRS:EAhigh_HR_lower95`,
                             GS.4a$EAhigh_HR_lower95,GS.4a$PRS_HR_lower95,GS.4a$`PRS:EAhigh_HR_lower95`,
                             FEMA.4a$Cineg),
                      ub = c(FGR11.4a$EAhigh_HR_upper95,FGR11.4a$PRS_HR_upper95,FGR11.4a$`PRS:EAhigh_HR_upper95`,
                             UKB.4a$EAhigh_HR_upper95,UKB.4a$PRS_HR_upper95,UKB.4a$`PRS:EAhigh_HR_upper95`,
                             GS.4a$EAhigh_HR_upper95,GS.4a$PRS_HR_upper95,GS.4a$`PRS:EAhigh_HR_upper95`,
                             FEMA.4a$Cipos),
                      beta = c(FGR11.4a$EAhigh_beta,FGR11.4a$PRS_beta,FGR11.4a$`PRS:EAhigh_beta`,
                               UKB.4a$EAhigh_beta,UKB.4a$PRS_beta,UKB.4a$`PRS:EAhigh_beta`,
                               GS.4a$EAhigh_beta,GS.4a$PRS_beta,GS.4a$`PRS:EAhigh_beta`,
                               FEMA.4a$Beta),
                      se = c(FGR11.4a$EAhigh_se,FGR11.4a$PRS_se,FGR11.4a$`PRS:EAhigh_se`,
                             UKB.4a$EAhigh_se,UKB.4a$PRS_se,UKB.4a$`PRS:EAhigh_se`,
                             GS.4a$EAhigh_se,GS.4a$PRS_se,GS.4a$`PRS:EAhigh_se`,
                             FEMA.4a$SE),
                      Test = c(rep("EA",nrow(FGR11.4a)), rep("PRS",nrow(FGR11.4a)), rep("EAxPRS",nrow(FGR11.4a)),
                               rep("EA", nrow(UKB.4a)), rep("PRS",nrow(UKB.4a)), rep("EAxPRS",nrow(UKB.4a)),
                               rep("EA", nrow(GS.4a)), rep("PRS",nrow(GS.4a)), rep("EAxPRS",nrow(GS.4a)),
                               FEMA.4a$Test),
                      Biobanks = c(rep(FGR11.4a$Biobank,3),rep(UKB.4a$Biobank,3),
                                   rep(GS.4a$Biobank,3),FEMA.4a$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4a$trait <- factor(model4a$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                  "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                  "C3_COLORECTAL","I9_AF","I9_CHD",
                                                  "COX_ARTHROSIS","C3_MELANOMA_SKIN",
                                                  "J10_ASTHMA","KNEE_ARTHROSIS",
                                                  "F5_DEPRESSIO","C3_BRONCHUS_LUNG",
                                                  "C3_CANCER","K11_APPENDACUT",
                                                  "G6_EPLEPSY","AUD_SWEDISH"),
                        labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                   "Gout","Rheumatoid Arthritis","Breast Cancer",
                                   "* Colorectal Cancer","* Atrial Fibrillation",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                   "Major Depression","Lung Cancer","Any Cancer",
                                   "Appendicitis","Epilepsy","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model4a$trait <- fct_rev(model4a$trait)
# EA levels as factor to plot them in order of magnitude
model4a$Test <- factor(model4a$Test, levels = c("EA","PRS","EAxPRS"), labels = c("Education","PRS", "Interaction"))
# Biobank as factor
model4a$Biobanks <- factor(model4a$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                           labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

# model 4 Continuous 
model4b <- data.frame(trait = c(rep(FGR11.4b$trait,3),rep(UKB.4b$trait,3),
                                rep(GS.4b$trait,3),FEMA.4b$Phenotype),
                      HR = c(FGR11.4b$EDUCATION_97_HR,FGR11.4b$PRS_HR,FGR11.4b$`PRS:EDUCATION_97_HR`,
                             UKB.4b$ISCED97_HR,UKB.4b$PRS_HR,UKB.4b$`PRS:ISCED97_HR`,
                             GS.4b$ISCED97_HR,GS.4b$PRS_HR,GS.4b$`PRS:ISCED97_HR`,
                             FEMA.4b$HR),
                      lb = c(FGR11.4b$EDUCATION_97_HR_lower95,FGR11.4b$PRS_HR_lower95,FGR11.4b$`PRS:EDUCATION_97_HR_lower95`,
                             UKB.4b$ISCED97_HR_lower95,UKB.4b$PRS_HR_lower95,UKB.4b$`PRS:ISCED97_HR_lower95`,
                             GS.4b$ISCED97_HR_lower95,GS.4b$PRS_HR_lower95,GS.4b$`PRS:ISCED97_HR_lower95`,
                             FEMA.4b$Cineg),
                      ub = c(FGR11.4b$EDUCATION_97_HR_upper95,FGR11.4b$PRS_HR_upper95,FGR11.4b$`PRS:EDUCATION_97_HR_upper95`,
                             UKB.4b$ISCED97_HR_upper95,UKB.4b$PRS_HR_upper95,UKB.4b$`PRS:ISCED97_HR_upper95`,
                             GS.4b$ISCED97_HR_upper95,GS.4b$PRS_HR_upper95,GS.4b$`PRS:ISCED97_HR_upper95`,
                             FEMA.4b$Cipos),
                      beta = c(FGR11.4b$EDUCATION_97_beta,FGR11.4b$PRS_beta,FGR11.4b$`PRS:EDUCATION_97_beta`,
                               UKB.4b$ISCED97_beta,UKB.4b$PRS_beta,UKB.4b$`PRS:ISCED97_beta`,
                               GS.4b$ISCED97_beta,GS.4b$PRS_beta,GS.4b$`PRS:ISCED97_beta`,
                               FEMA.4b$Beta),
                      se = c(FGR11.4b$EDUCATION_97_se,FGR11.4b$PRS_se,FGR11.4b$`PRS:EDUCATION_97_se`,
                             UKB.4b$ISCED97_se,UKB.4b$PRS_se,UKB.4b$`PRS:ISCED97_se`,
                             GS.4b$ISCED97_se,GS.4b$PRS_se,GS.4b$`PRS:ISCED97_se`,
                             FEMA.4b$SE),
                      Test = c(rep("EA",nrow(FGR11.4b)), rep("PRS",nrow(FGR11.4b)), rep("EAxPRS",nrow(FGR11.4b)),
                               rep("EA", nrow(UKB.4b)), rep("PRS",nrow(UKB.4b)), rep("EAxPRS",nrow(UKB.4b)),
                               rep("EA", nrow(GS.4b)), rep("PRS",nrow(GS.4b)), rep("EAxPRS",nrow(GS.4b)),
                               FEMA.4b$Test),
                      Biobanks = c(rep(FGR11.4b$Biobank,3),rep(UKB.4b$Biobank,3),
                                   rep(GS.4b$Biobank,3),FEMA.4b$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4b$trait <- factor(model4b$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                  "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                  "C3_COLORECTAL","I9_AF","I9_CHD",
                                                  "COX_ARTHROSIS","C3_MELANOMA_SKIN",
                                                  "J10_ASTHMA","KNEE_ARTHROSIS",
                                                  "F5_DEPRESSIO","C3_BRONCHUS_LUNG",
                                                  "C3_CANCER","K11_APPENDACUT",
                                                  "G6_EPLEPSY","AUD_SWEDISH"),
                        labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                   "Gout","Rheumatoid Arthritis","Breast Cancer",
                                   "* Colorectal Cancer","* Atrial Fibrillation",
                                   "Coronary Heart Disease","Hip Osteoarthritis",
                                   "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                   "Major Depression","Lung Cancer","Any Cancer",
                                   "Appendicitis","Epilepsy","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model4b$trait <- fct_rev(model4b$trait)
# EA levels as factor to plot them in order of magnitude
model4b$Test <- factor(model4b$Test, levels = c("EA","PRS","EAxPRS"), labels = c("Education","PRS", "Interaction"))
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
df4ab$Model <- c(rep(c("Binary", "Continuous"),each=nrow(plot4a.dat)))

# split by Test for calculation
df4ab.EA <- df4ab[which(df4ab$Test=="Education"),]
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
    Binary_HR = HR[Model == "Binary"],
    Continuous_HR = HR[Model == "Continuous"],
    Binary_HR_lb = lb[Model == "Binary"],
    Continuous_HR_lb = lb[Model == "Continuous"],
    Binary_HR_ub = ub[Model == "Binary"],
    Continuous_HR_ub = ub[Model == "Continuous"],
    Binaryb = beta[Model == "Binary"],
    Continuousb = beta[Model == "Continuous"],
    Binaryse = se[Model == "Binary"],
    Continuousse = se[Model == "Continuous"],
    difference = Continuous_HR - Binary_HR,
    Betadiff = Continuousb - Binaryb,
    SEDiff = sqrt(Continuousse**2 + Continuousse**2)
  )
plot4ab_grouped_PRS <- plot4ab.df.PRS %>%
  group_by(trait) %>%
  reframe(
    Binary_HR = HR[Model == "Binary"],
    Continuous_HR = HR[Model == "Continuous"],
    Binary_HR_lb = lb[Model == "Binary"],
    Continuous_HR_lb = lb[Model == "Continuous"],
    Binary_HR_ub = ub[Model == "Binary"],
    Continuous_HR_ub = ub[Model == "Continuous"],
    Binaryb = beta[Model == "Binary"],
    Continuousb = beta[Model == "Continuous"],
    Binaryse = se[Model == "Binary"],
    Continuousse = se[Model == "Continuous"],
    difference = Continuous_HR - Binary_HR,
    Betadiff = Continuousb - Binaryb,
    SEDiff = sqrt(Continuousse**2 + Continuousse**2)
  )
plot4ab_grouped_INT <- plot4ab.df.INT %>%
  group_by(trait) %>%
  reframe(
    Binary_HR = HR[Model == "Binary"],
    Continuous_HR = HR[Model == "Continuous"],
    Binary_HR_lb = lb[Model == "Binary"],
    Continuous_HR_lb = lb[Model == "Continuous"],
    Binary_HR_ub = ub[Model == "Binary"],
    Continuous_HR_ub = ub[Model == "Continuous"],
    Binaryb = beta[Model == "Binary"],
    Continuousb = beta[Model == "Continuous"],
    Binaryse = se[Model == "Binary"],
    Continuousse = se[Model == "Continuous"],
    difference = Continuous_HR - Binary_HR,
    Betadiff = Continuousb - Binaryb,
    SEDiff = sqrt(Continuousse**2 + Continuousse**2)
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
plot4ab_diff_EA$Test <- "Education"
plot4ab_diff_PRS$Test <- "PRS"
plot4ab_diff_INT$Test <- "Interaction"
plot4ab_diff <- rbind(plot4ab_diff_EA,plot4ab_diff_PRS,plot4ab_diff_INT)

# write to file
fwrite(plot4ab_diff, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model4/", as.character(Sys.Date()), 
                           "_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_BinvsCont_Differences.csv",sep=""))


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results for plotting - random effect
#
################################################################################

# model 4 - Binary 
model4ar <- data.frame(trait = c(rep(FGR11.4a$trait,3),rep(UKB.4a$trait,3),
                                 rep(GS.4a$trait,3),REMA.4a$Phenotype),
                       HR = c(FGR11.4a$EAhigh_HR,FGR11.4a$PRS_HR,FGR11.4a$`PRS:EAhigh_HR`,
                              UKB.4a$EAhigh_HR,UKB.4a$PRS_HR,UKB.4a$`PRS:EAhigh_HR`,
                              GS.4a$EAhigh_HR,GS.4a$PRS_HR,GS.4a$`PRS:EAhigh_HR`,
                              REMA.4a$HR),
                       lb = c(FGR11.4a$EAhigh_HR_lower95,FGR11.4a$PRS_HR_lower95,FGR11.4a$`PRS:EAhigh_HR_lower95`,
                              UKB.4a$EAhigh_HR_lower95,UKB.4a$PRS_HR_lower95,UKB.4a$`PRS:EAhigh_HR_lower95`,
                              GS.4a$EAhigh_HR_lower95,GS.4a$PRS_HR_lower95,GS.4a$`PRS:EAhigh_HR_lower95`,
                              REMA.4a$Cineg),
                       ub = c(FGR11.4a$EAhigh_HR_upper95,FGR11.4a$PRS_HR_upper95,FGR11.4a$`PRS:EAhigh_HR_upper95`,
                              UKB.4a$EAhigh_HR_upper95,UKB.4a$PRS_HR_upper95,UKB.4a$`PRS:EAhigh_HR_upper95`,
                              GS.4a$EAhigh_HR_upper95,GS.4a$PRS_HR_upper95,GS.4a$`PRS:EAhigh_HR_upper95`,
                              REMA.4a$Cipos),
                       beta = c(FGR11.4a$EAhigh_beta,FGR11.4a$PRS_beta,FGR11.4a$`PRS:EAhigh_beta`,
                                UKB.4a$EAhigh_beta,UKB.4a$PRS_beta,UKB.4a$`PRS:EAhigh_beta`,
                                GS.4a$EAhigh_beta,GS.4a$PRS_beta,GS.4a$`PRS:EAhigh_beta`,
                                REMA.4a$Beta),
                       se = c(FGR11.4a$EAhigh_se,FGR11.4a$PRS_se,FGR11.4a$`PRS:EAhigh_se`,
                              UKB.4a$EAhigh_se,UKB.4a$PRS_se,UKB.4a$`PRS:EAhigh_se`,
                              GS.4a$EAhigh_se,GS.4a$PRS_se,GS.4a$`PRS:EAhigh_se`,
                              REMA.4a$SE),
                       Test = c(rep("EA",nrow(FGR11.4a)), rep("PRS",nrow(FGR11.4a)), rep("EAxPRS",nrow(FGR11.4a)),
                                rep("EA", nrow(UKB.4a)), rep("PRS",nrow(UKB.4a)), rep("EAxPRS",nrow(UKB.4a)),
                                rep("EA", nrow(GS.4a)), rep("PRS",nrow(GS.4a)), rep("EAxPRS",nrow(GS.4a)),
                                REMA.4a$Test),
                       Biobanks = c(rep(FGR11.4a$Biobank,3),rep(UKB.4a$Biobank,3),
                                    rep(GS.4a$Biobank,3),REMA.4a$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4ar$trait <- factor(model4ar$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                    "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                    "C3_COLORECTAL","I9_AF","I9_CHD",
                                                    "COX_ARTHROSIS","C3_MELANOMA_SKIN",
                                                    "J10_ASTHMA","KNEE_ARTHROSIS",
                                                    "F5_DEPRESSIO","C3_BRONCHUS_LUNG",
                                                    "C3_CANCER","K11_APPENDACUT",
                                                    "G6_EPLEPSY","AUD_SWEDISH"),
                         labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                    "Gout","Rheumatoid Arthritis","Breast Cancer",
                                    "* Colorectal Cancer","* Atrial Fibrillation",
                                    "Coronary Heart Disease","Hip Osteoarthritis",
                                    "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                    "Major Depression","Lung Cancer","Any Cancer",
                                    "Appendicitis","Epilepsy","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model4ar$trait <- fct_rev(model4ar$trait)
# EA levels as factor to plot them in order of magnitude
model4ar$Test <- factor(model4ar$Test, levels = c("EA","PRS","EAxPRS"), labels = c("Education","PRS", "Interaction"))
# Biobank as factor
model4ar$Biobanks <- factor(model4ar$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                            labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

# model 4 Continuous 
model4br <- data.frame(trait = c(rep(FGR11.4b$trait,3),rep(UKB.4b$trait,3),
                                 rep(GS.4b$trait,3),REMA.4b$Phenotype),
                       HR = c(FGR11.4b$EDUCATION_97_HR,FGR11.4b$PRS_HR,FGR11.4b$`PRS:EDUCATION_97_HR`,
                              UKB.4b$ISCED97_HR,UKB.4b$PRS_HR,UKB.4b$`PRS:ISCED97_HR`,
                              GS.4b$ISCED97_HR,GS.4b$PRS_HR,GS.4b$`PRS:ISCED97_HR`,
                              REMA.4b$HR),
                       lb = c(FGR11.4b$EDUCATION_97_HR_lower95,FGR11.4b$PRS_HR_lower95,FGR11.4b$`PRS:EDUCATION_97_HR_lower95`,
                              UKB.4b$ISCED97_HR_lower95,UKB.4b$PRS_HR_lower95,UKB.4b$`PRS:ISCED97_HR_lower95`,
                              GS.4b$ISCED97_HR_lower95,GS.4b$PRS_HR_lower95,GS.4b$`PRS:ISCED97_HR_lower95`,
                              REMA.4b$Cineg),
                       ub = c(FGR11.4b$EDUCATION_97_HR_upper95,FGR11.4b$PRS_HR_upper95,FGR11.4b$`PRS:EDUCATION_97_HR_upper95`,
                              UKB.4b$ISCED97_HR_upper95,UKB.4b$PRS_HR_upper95,UKB.4b$`PRS:ISCED97_HR_upper95`,
                              GS.4b$ISCED97_HR_upper95,GS.4b$PRS_HR_upper95,GS.4b$`PRS:ISCED97_HR_upper95`,
                              REMA.4b$Cipos),
                       beta = c(FGR11.4b$EDUCATION_97_beta,FGR11.4b$PRS_beta,FGR11.4b$`PRS:EDUCATION_97_beta`,
                                UKB.4b$ISCED97_beta,UKB.4b$PRS_beta,UKB.4b$`PRS:ISCED97_beta`,
                                GS.4b$ISCED97_beta,GS.4b$PRS_beta,GS.4b$`PRS:ISCED97_beta`,
                                REMA.4b$Beta),
                       se = c(FGR11.4b$EDUCATION_97_se,FGR11.4b$PRS_se,FGR11.4b$`PRS:EDUCATION_97_se`,
                              UKB.4b$ISCED97_se,UKB.4b$PRS_se,UKB.4b$`PRS:ISCED97_se`,
                              GS.4b$ISCED97_se,GS.4b$PRS_se,GS.4b$`PRS:ISCED97_se`,
                              REMA.4b$SE),
                       Test = c(rep("EA",nrow(FGR11.4b)), rep("PRS",nrow(FGR11.4b)), rep("EAxPRS",nrow(FGR11.4b)),
                                rep("EA", nrow(UKB.4b)), rep("PRS",nrow(UKB.4b)), rep("EAxPRS",nrow(UKB.4b)),
                                rep("EA", nrow(GS.4b)), rep("PRS",nrow(GS.4b)), rep("EAxPRS",nrow(GS.4b)),
                                REMA.4b$Test),
                       Biobanks = c(rep(FGR11.4b$Biobank,3),rep(UKB.4b$Biobank,3),
                                    rep(GS.4b$Biobank,3),REMA.4b$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4br$trait <- factor(model4br$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                    "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                    "C3_COLORECTAL","I9_AF","I9_CHD",
                                                    "COX_ARTHROSIS","C3_MELANOMA_SKIN",
                                                    "J10_ASTHMA","KNEE_ARTHROSIS",
                                                    "F5_DEPRESSIO","C3_BRONCHUS_LUNG",
                                                    "C3_CANCER","K11_APPENDACUT",
                                                    "G6_EPLEPSY","AUD_SWEDISH"),
                         labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                    "Gout","Rheumatoid Arthritis","Breast Cancer",
                                    "* Colorectal Cancer","* Atrial Fibrillation",
                                    "Coronary Heart Disease","Hip Osteoarthritis",
                                    "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                    "Major Depression","Lung Cancer","Any Cancer",
                                    "Appendicitis","Epilepsy","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model4br$trait <- fct_rev(model4br$trait)
# EA levels as factor to plot them in order of magnitude
model4br$Test <- factor(model4br$Test, levels = c("EA","PRS","EAxPRS"), labels = c("Education","PRS", "Interaction"))
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
df4abr$Model <- c(rep(c("Binary", "Continuous"),each=nrow(plot4ar.dat)))

# split by Test for calculation
df4abr.EA <- df4abr[which(df4abr$Test=="Education"),]
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
    Binary_HR = HR[Model == "Binary"],
    Continuous_HR = HR[Model == "Continuous"],
    Binary_HR_lb = lb[Model == "Binary"],
    Continuous_HR_lb = lb[Model == "Continuous"],
    Binary_HR_ub = ub[Model == "Binary"],
    Continuous_HR_ub = ub[Model == "Continuous"],
    Binaryb = beta[Model == "Binary"],
    Continuousb = beta[Model == "Continuous"],
    Binaryse = se[Model == "Binary"],
    Continuousse = se[Model == "Continuous"],
    difference = Continuous_HR - Binary_HR,
    Betadiff = Continuousb - Binaryb,
    SEDiff = sqrt(Continuousse**2 + Continuousse**2)
  )
plot4abr_grouped_PRS <- plot4abr.df.PRS %>%
  group_by(trait) %>%
  reframe(
    Binary_HR = HR[Model == "Binary"],
    Continuous_HR = HR[Model == "Continuous"],
    Binary_HR_lb = lb[Model == "Binary"],
    Continuous_HR_lb = lb[Model == "Continuous"],
    Binary_HR_ub = ub[Model == "Binary"],
    Continuous_HR_ub = ub[Model == "Continuous"],
    Binaryb = beta[Model == "Binary"],
    Continuousb = beta[Model == "Continuous"],
    Binaryse = se[Model == "Binary"],
    Continuousse = se[Model == "Continuous"],
    difference = Continuous_HR - Binary_HR,
    Betadiff = Continuousb - Binaryb,
    SEDiff = sqrt(Continuousse**2 + Continuousse**2)
  )
plot4abr_grouped_INT <- plot4abr.df.INT %>%
  group_by(trait) %>%
  reframe(
    Binary_HR = HR[Model == "Binary"],
    Continuous_HR = HR[Model == "Continuous"],
    Binary_HR_lb = lb[Model == "Binary"],
    Continuous_HR_lb = lb[Model == "Continuous"],
    Binary_HR_ub = ub[Model == "Binary"],
    Continuous_HR_ub = ub[Model == "Continuous"],
    Binaryb = beta[Model == "Binary"],
    Continuousb = beta[Model == "Continuous"],
    Binaryse = se[Model == "Binary"],
    Continuousse = se[Model == "Continuous"],
    difference = Continuous_HR - Binary_HR,
    Betadiff = Continuousb - Binaryb,
    SEDiff = sqrt(Continuousse**2 + Continuousse**2)
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
plot4abr_diff_EA$Test <- "Education"
plot4abr_diff_PRS$Test <- "PRS"
plot4abr_diff_INT$Test <- "Interaction"
plot4abr_diff <- rbind(plot4abr_diff_EA,plot4abr_diff_PRS,plot4abr_diff_INT)

# write to file
fwrite(plot4abr_diff, paste("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model4/", as.character(Sys.Date()), 
                           "_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_BinvsCont_Differences.csv",sep=""))
