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
# Script: Compare results model 3 in Educational Attainment and Occupation
# analyses 
#
# Data: FGR11 + UKB + GS + fixed-effect meta-analysis model 3b (EA + occupation)
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

# read in model 3 - PGS stratified by EA/OCC
FGR11.3.EA <- fread("output/GoogleDrive/FGR11/2025-02-07_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt", data.table=FALSE)
FGR11.3.EA$Biobank <- "FinnGen"
FGR11.3.OCC <- fread("output/GoogleDrive/FGR11/2025-02-12_INTERVENE_Occupation_Coeffs_CoxPH_model3_FinnGenR11.txt", data.table=FALSE)
FGR11.3.OCC$Biobank <- "FinnGen"
#
UKB.3.EA <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
UKB.3.EA$Biobank <- "UK Biobank"
UKB.3.OCC <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model3_Coeffs.txt",data.table=FALSE)
UKB.3.OCC$Biobank <- "UK Biobank"
#
GS.3.EA <- fread("output/GoogleDrive/GS/2025-02-24_GS_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
GS.3.EA$Test <- c(rep("LowEA",length(unique(GS.3.EA$trait))),rep("HighEA",length(unique(GS.3.EA$trait)))) # in initial script shared with GS forgot a line of code to add this, so now add it manually. 
GS.3.EA$Biobank <- "Generation Scotland"
GS.3.OCC <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model3_Coeffs.txt",data.table=FALSE)
GS.3.OCC$Biobank <- "Generation Scotland"
#
FEMA.3.EA <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3.xlsx"))
FEMA.3.EA$Biobank <- "FE meta-analysis"
FEMA.3.OCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-13_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3.csv",data.table=FALSE)
FEMA.3.OCC$Biobank <- "FE meta-analysis"
#
REMA.3.EA <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model3.csv",data.table=FALSE)
REMA.3.EA$Biobank <- "RE meta-analysis"
REMA.3.OCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-13_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model3.csv",data.table=FALSE)
REMA.3.OCC$Biobank <- "RE meta-analysis"

################################################################################
#
# Generation Scotland includes traits which have too small sample sizes to
# perform the analyses in. Remove these traits.
#
################################################################################

# Educational Attainment
GS.3.EA <- GS.3.EA[-which(GS.3.EA$trait %in% c("RHEUMA_SEROPOS_OTH")),]

# Occupation
GS.3.OCC <- GS.3.OCC[-which(GS.3.OCC$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                      "C3_MELANOMA_SKIN","G6_EPLEPSY")),]


################################################################################
#
# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits
#
################################################################################

# Educational attainment
FEMA.3.EA <- FEMA.3.EA[-which(FEMA.3.EA$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
REMA.3.EA <- REMA.3.EA[-which(REMA.3.EA$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]

# Occupation
FEMA.3.OCC <- FEMA.3.OCC[-which(FEMA.3.OCC$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                         "C3_MELANOMA_SKIN")),]
REMA.3.OCC <- REMA.3.OCC[-which(REMA.3.OCC$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                            "C3_MELANOMA_SKIN")),]


################################################################################
#
# Reorganize results for plotting - fixed effect
#
################################################################################

# model 3: EA
model3.EA <- data.frame(trait = c(FGR11.3.EA$trait,UKB.3.EA$trait,GS.3.EA$trait,
                               FEMA.3.EA$Phenotype),
                     HR = c(FGR11.3.EA$PRS_HR,UKB.3.EA$PRS_HR,GS.3.EA$PRS_HR,
                            FEMA.3.EA$HR),
                     lb = c(FGR11.3.EA$PRS_HR_lower95,UKB.3.EA$PRS_HR_lower95,
                            GS.3.EA$PRS_HR_lower95,FEMA.3.EA$Cineg),
                     ub = c(FGR11.3.EA$PRS_HR_upper95,UKB.3.EA$PRS_HR_upper95,
                            GS.3.EA$PRS_HR_upper95,FEMA.3.EA$Cipos),
                     beta = c(FGR11.3.EA$PRS_beta,UKB.3.EA$PRS_beta,GS.3.EA$PRS_beta,
                              FEMA.3.EA$Beta),
                     se = c(FGR11.3.EA$PRS_se,UKB.3.EA$PRS_se,GS.3.EA$PRS_se,
                            FEMA.3.EA$SE),
                     Test = c(rep("PRS in low EA",0.5*nrow(FGR11.3.EA)), rep("PRS in high EA",0.5*nrow(FGR11.3.EA)),
                              rep("PRS in low EA",0.5*nrow(UKB.3.EA)), rep("PRS in high EA",0.5*nrow(UKB.3.EA)),
                              rep("PRS in low EA",0.5*nrow(GS.3.EA)), rep("PRS in high EA",0.5*nrow(GS.3.EA)),
                              rep("PRS in low EA",0.5*nrow(FEMA.3.EA)), rep("PRS in high EA",0.5*nrow(FEMA.3.EA))),
                     Biobanks = c(FGR11.3.EA$Biobank,UKB.3.EA$Biobank,
                                  GS.3.EA$Biobank,FEMA.3.EA$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model3.EA$trait <- factor(model3.EA$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model3.EA$trait <- fct_rev(model3.EA$trait)
# EA levels as factor to plot them in order of magnitude - adjust labels to
# simple for further data wrangling down the line
model3.EA$Test <- factor(model3.EA$Test, levels = c("PRS in low EA","PRS in high EA"), 
                      labels = c("LowEA","HighEA"))
model3.EA$Biobanks <- factor(model3.EA$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                             labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB & GS estimates
model3.EA <- model3.EA[-which(model3.EA$Biobanks=="UK Biobank" | model3.EA$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
model3.EA <- model3.EA[-which(model3.EA$trait[which(model3.EA$Biobanks=="FinnGen")] %in% 
                                c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                  "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                  "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                  "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                  "Knee Osteoarthritis")),]

# model 3: OCC
model3.OCC <- data.frame(trait = c(FGR11.3.OCC$trait,UKB.3.OCC$trait,GS.3.OCC$trait,
                               FEMA.3.OCC$Phenotype),
                     HR = c(FGR11.3.OCC$PRS_HR,UKB.3.OCC$PRS_HR,GS.3.OCC$PRS_HR,
                            FEMA.3.OCC$HR),
                     lb = c(FGR11.3.OCC$PRS_HR_lower95,UKB.3.OCC$PRS_HR_lower95,
                            GS.3.OCC$PRS_HR_lower95,FEMA.3.OCC$Cineg),
                     ub = c(FGR11.3.OCC$PRS_HR_upper95,UKB.3.OCC$PRS_HR_upper95,
                            GS.3.OCC$PRS_HR_upper95,FEMA.3.OCC$Cipos),
                     beta = c(FGR11.3.OCC$PRS_beta,UKB.3.OCC$PRS_beta,
                              GS.3.OCC$PRS_beta,FEMA.3.OCC$Beta),
                     se = c(FGR11.3.OCC$PRS_se,UKB.3.OCC$PRS_se,
                            GS.3.OCC$PRS_se,FEMA.3.OCC$SE),
                     Test = c(rep("PRS in lower level",0.5*nrow(FGR11.3.OCC)), rep("PRS in upper level",0.5*nrow(FGR11.3.OCC)),
                              rep("PRS in lower level",0.5*nrow(UKB.3.OCC)), rep("PRS in upper level",0.5*nrow(UKB.3.OCC)),
                              rep("PRS in lower level",0.5*nrow(GS.3.OCC)), rep("PRS in upper level",0.5*nrow(GS.3.OCC)),
                              rep("PRS in lower level",0.5*nrow(FEMA.3.OCC)), rep("PRS in upper level",0.5*nrow(FEMA.3.OCC))),
                     Biobanks = c(FGR11.3.OCC$Biobank,UKB.3.OCC$Biobank,
                                  GS.3.OCC$Biobank,FEMA.3.OCC$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model3.OCC$trait <- factor(model3.OCC$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model3.OCC$trait <- fct_rev(model3.OCC$trait)
# EA levels as factor to plot them in order of magnitude - adjust labels to
# simple for further data wrangling down the line
model3.OCC$Test <- factor(model3.OCC$Test, levels = c("PRS in lower level","PRS in upper level"), 
                      labels = c("Lower","Upper"))
model3.OCC$Biobanks <- factor(model3.OCC$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                             labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB & GS estimates
model3.OCC <- model3.OCC[-which(model3.OCC$Biobanks=="UK Biobank" | model3.OCC$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
model3.OCC <- model3.OCC[-which(model3.OCC$trait[which(model3.OCC$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]

# combine data frames model 3 EA & OCC for plotting 
df34 <- rbind(model3.EA,model3.OCC) #row bind
df34$Model <- c(rep(c("model1", "model2"),each=nrow(model3.EA)))


################################################################################
#
# Calculate significance differences between EA and OCC model 3 - fixed effects
#
################################################################################

# split by Test for calculation
df34.low <- df34[which(df34$Test=="LowEA" | df34$Test=="Lower"),]
df34.high <- df34[which(df34$Test=="HighEA"| df34$Test=="Upper"),]

# calculate difference in the PGS between education and occupation for each
# trait to inform ordering of the traits in the plot arrange dataframe by trait
# and Test group (low vs high education/occupation)
plot34.df.low <- df34.low %>%
  arrange(trait, Model) 
plot34.df.high <- df34.high %>%
  arrange(trait, Model) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot34_grouped_low <- plot34.df.low %>%
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
plot34_grouped_high <- plot34.df.high %>%
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
plot34_diff_low <- plot34_grouped_low %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot34_diff_high <- plot34_grouped_high %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot34_diff_low <- plot34_diff_low %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot34_diff_high <- plot34_diff_high %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot34_diff_low <- plot34_diff_low %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot34_diff_high <- plot34_diff_high %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))

# combine OCC and high results
plot34_diff_low$Test <- "low"
plot34_diff_high$Test <- "high"
plot34_diff <- rbind(plot34_diff_low,plot34_diff_high)

# write to file
fwrite(plot34_diff, paste("output/EmploymentStatus/MetaAnalysis/FGR11_UKB_GS/model3/", as.character(Sys.Date()),
                          "_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_EAvsOccupationDifferences.csv",sep=""))


################################################################################
#
# Reorganize results for plotting - random effect
#
################################################################################

# model 3: EA
model3r.EA <- data.frame(trait = c(FGR11.3.EA$trait,UKB.3.EA$trait,GS.3.EA$trait,
                                   REMA.3.EA$Phenotype),
                         HR = c(FGR11.3.EA$PRS_HR,UKB.3.EA$PRS_HR,GS.3.EA$PRS_HR,
                                REMA.3.EA$HR),
                         lb = c(FGR11.3.EA$PRS_HR_lower95,UKB.3.EA$PRS_HR_lower95,
                                GS.3.EA$PRS_HR_lower95,REMA.3.EA$Cineg),
                         ub = c(FGR11.3.EA$PRS_HR_upper95,UKB.3.EA$PRS_HR_upper95,
                                GS.3.EA$PRS_HR_upper95,REMA.3.EA$Cipos),
                         beta = c(FGR11.3.EA$PRS_beta,UKB.3.EA$PRS_beta,GS.3.EA$PRS_beta,
                                  REMA.3.EA$Beta),
                         se = c(FGR11.3.EA$PRS_se,UKB.3.EA$PRS_se,GS.3.EA$PRS_se,
                                REMA.3.EA$SE),
                         Test = c(rep("PRS in low EA",0.5*nrow(FGR11.3.EA)), rep("PRS in high EA",0.5*nrow(FGR11.3.EA)),
                                  rep("PRS in low EA",0.5*nrow(UKB.3.EA)), rep("PRS in high EA",0.5*nrow(UKB.3.EA)),
                                  rep("PRS in low EA",0.5*nrow(GS.3.EA)), rep("PRS in high EA",0.5*nrow(GS.3.EA)),
                                  rep("PRS in low EA",0.5*nrow(REMA.3.EA)), rep("PRS in high EA",0.5*nrow(REMA.3.EA))),
                         Biobanks = c(FGR11.3.EA$Biobank,UKB.3.EA$Biobank,
                                      GS.3.EA$Biobank,REMA.3.EA$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model3r.EA$trait <- factor(model3r.EA$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model3r.EA$trait <- fct_rev(model3r.EA$trait)
# EA levels as factor to plot them in order of magnitude - adjust labels to
# simple for further data wrangling down the line
model3r.EA$Test <- factor(model3r.EA$Test, levels = c("PRS in low EA","PRS in high EA"), 
                          labels = c("LowEA","HighEA"))
model3r.EA$Biobanks <- factor(model3r.EA$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                              labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB & GS estimates
model3r.EA <- model3r.EA[-which(model3r.EA$Biobanks=="UK Biobank" | model3r.EA$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
model3r.EA <- model3r.EA[-which(model3r.EA$trait[which(model3r.EA$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]

# model 3: OCC
model3r.OCC <- data.frame(trait = c(FGR11.3.OCC$trait,UKB.3.OCC$trait,GS.3.OCC$trait,
                                    REMA.3.OCC$Phenotype),
                          HR = c(FGR11.3.OCC$PRS_HR,UKB.3.OCC$PRS_HR,GS.3.OCC$PRS_HR,
                                 REMA.3.OCC$HR),
                          lb = c(FGR11.3.OCC$PRS_HR_lower95,UKB.3.OCC$PRS_HR_lower95,
                                 GS.3.OCC$PRS_HR_lower95,REMA.3.OCC$Cineg),
                          ub = c(FGR11.3.OCC$PRS_HR_upper95,UKB.3.OCC$PRS_HR_upper95,
                                 GS.3.OCC$PRS_HR_upper95,REMA.3.OCC$Cipos),
                          beta = c(FGR11.3.OCC$PRS_beta,UKB.3.OCC$PRS_beta,
                                   GS.3.OCC$PRS_beta,REMA.3.OCC$Beta),
                          se = c(FGR11.3.OCC$PRS_se,UKB.3.OCC$PRS_se,
                                 GS.3.OCC$PRS_se,REMA.3.OCC$SE),
                          Test = c(rep("PRS in lower level",0.5*nrow(FGR11.3.OCC)), rep("PRS in upper level",0.5*nrow(FGR11.3.OCC)),
                                   rep("PRS in lower level",0.5*nrow(UKB.3.OCC)), rep("PRS in upper level",0.5*nrow(UKB.3.OCC)),
                                   rep("PRS in lower level",0.5*nrow(GS.3.OCC)), rep("PRS in upper level",0.5*nrow(GS.3.OCC)),
                                   rep("PRS in lower level",0.5*nrow(REMA.3.OCC)), rep("PRS in upper level",0.5*nrow(REMA.3.OCC))),
                          Biobanks = c(FGR11.3.OCC$Biobank,UKB.3.OCC$Biobank,
                                       GS.3.OCC$Biobank,REMA.3.OCC$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model3r.OCC$trait <- factor(model3r.OCC$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
model3r.OCC$trait <- fct_rev(model3r.OCC$trait)
# EA levels as factor to plot them in order of magnitude - adjust labels to
# simple for further data wrangling down the line
model3r.OCC$Test <- factor(model3r.OCC$Test, levels = c("PRS in lower level","PRS in upper level"), 
                           labels = c("Lower","Upper"))
model3r.OCC$Biobanks <- factor(model3r.OCC$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                               labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB & GS estimates
model3r.OCC <- model3r.OCC[-which(model3r.OCC$Biobanks=="UK Biobank" | model3r.OCC$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
model3r.OCC <- model3r.OCC[-which(model3r.OCC$trait[which(model3r.OCC$Biobanks=="FinnGen")] %in% 
                                    c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease",
                                      "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                      "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                      "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                      "Knee Osteoarthritis")),]

# combine data frames model 3 EA & OCC for plotting 
df34r <- rbind(model3r.EA,model3r.OCC) #row bind
df34r$Model <- c(rep(c("model1", "model2"),each=nrow(model3r.EA)))


################################################################################
#
# Calculate significance differences between EA and OCC model 3 - random effects
#
################################################################################

# split by Test for calculation
df34r.low <- df34r[which(df34r$Test=="LowEA" | df34r$Test=="Lower"),]
df34r.high <- df34r[which(df34r$Test=="HighEA"| df34r$Test=="Upper"),]

# calculate difference in the PGS between education and occupation for each
# trait to inform ordering of the traits in the plot arrange dataframe by trait
# and Test group (low vs high education/occupation)
plot34r.df.low <- df34r.low %>%
  arrange(trait, Model) 
plot34r.df.high <- df34r.high %>%
  arrange(trait, Model) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot34r_grouped_low <- plot34r.df.low %>%
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
plot34r_grouped_high <- plot34r.df.high %>%
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
plot34r_diff_low <- plot34r_grouped_low %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot34r_diff_high <- plot34r_grouped_high %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot34r_diff_low <- plot34r_diff_low %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot34r_diff_high <- plot34r_diff_high %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot34r_diff_low <- plot34r_diff_low %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot34r_diff_high <- plot34r_diff_high %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))

# combine OCC and high results
plot34r_diff_low$Test <- "low"
plot34r_diff_high$Test <- "high"
plot34r_diff <- rbind(plot34r_diff_low,plot34r_diff_high)

# write to file
fwrite(plot34r_diff, paste("output/EmploymentStatus/MetaAnalysis/FGR11_UKB_GS/model3/", as.character(Sys.Date()),
                           "_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model3_EAvsOccupationDifferences.csv",sep=""))
