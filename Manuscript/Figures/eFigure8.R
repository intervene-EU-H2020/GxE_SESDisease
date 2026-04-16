#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# occupation) in risk of 18 common diseases (as previously selected in the
# INTERVENE flagship manuscript: https://doi.org/10.1101/2023.06.12.23291186)
# and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Create supplementary figures (per cohort + meta-analysis)
#
# Figure this script creates: eFigure 3: model 1 (occupation or PRS): per cohort + meta-analysis
#
# Data: FGR11 + UKB + GS + meta-analysis model 1a+b
#
# Last edits: 16/04/2026 (edits, FAH: replace code original eFigure8 with new)
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
# efficiently reading in large data sets; ggplot2 = versatile visualizations;
# viridis = color-blind friendly colors; dplyr, forcats & stringr = data
# wrangling; cowplot + grid + gridExtra = combining plots; readxl = read excel
# files (upload to googledrive converts csv to xlsx).
packages("data.table","ggplot2","viridis","dplyr","forcats","stringr","cowplot",
         "grid","gridExtra","readxl")


################################################################################
#
# Read in full sample hazard ratios per standard deviation
#
################################################################################

# read in model 1a - EA only
FGR11.1a <- fread("output/GoogleDrive/FGR11/2025-01-30_INTERVENE_Occupation_Coeffs_CoxPH_model1a_FinnGenR11.txt", data.table=FALSE)
FGR11.1a$Biobank <- "FinnGen"
#
UKB.1a <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.1a$Biobank <- "UK Biobank"
#
GS.1a <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
GS.1a$Biobank <- "Generation Scotland"
#
FEMA.1a <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv", data.table = FALSE)
FEMA.1a$Biobank <- "FE meta-analysis"
#
REMA.1a <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv",data.table = FALSE)
REMA.1a$Biobank <- "RE meta-analysis"

# read in model 1b - PRS only
FGR11.1b <- fread("output/GoogleDrive/FGR11/2025-01-30_INTERVENE_Occupation_Coeffs_CoxPH_model1b_FinnGenR11.txt", data.table=FALSE)
FGR11.1b$Biobank <- "FinnGen"
#
UKB.1b <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.1b$Biobank <- "UK Biobank"
#
GS.1b <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
GS.1b$Biobank <- "Generation Scotland"
#
FEMA.1b <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv", data.table = FALSE)
FEMA.1b$Biobank <- "FE meta-analysis"
#
REMA.1b <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv", data.table = FALSE)
REMA.1b$Biobank <- "RE meta-analysis"


################################################################################
#
# Generation Scotland included "T1D, Rheumatoid arthritis, skin melanoma, and
# epilepsy" which have too small sample sizes to perform the analyses in. Remove
# these traits prior to the meta-analysis
#
################################################################################

GS.1a <- GS.1a[-which(GS.1a$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                         "C3_MELANOMA_SKIN","G6_EPLEPSY")),]
GS.1b <- GS.1b[-which(GS.1b$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                         "C3_MELANOMA_SKIN","G6_EPLEPSY")),]


################################################################################
#
# As current version of the meta-analysis also includes traits only available in
# FinnGen, subset those data frames to only include the traits where >=2 cohorts
# were analysed.
#
################################################################################

## remove traits not meta-analyzed ##
# fixed effect
FEMA.1a <- FEMA.1a[-which(FEMA.1a$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                   "C3_MELANOMA_SKIN")),]
FEMA.1b <- FEMA.1b[-which(FEMA.1b$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                   "C3_MELANOMA_SKIN")),]

# random effect
REMA.1a <- REMA.1a[-which(REMA.1a$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                   "C3_MELANOMA_SKIN")),]
REMA.1b <- REMA.1b[-which(REMA.1b$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                   "C3_MELANOMA_SKIN")),]


################################################################################
#
# Combine FGR11 & UKB & GenScot & meta-analysis results
#
################################################################################

# model 1 
model1 <- data.frame(trait = c(FGR11.1a$trait,FGR11.1b$trait,UKB.1a$trait,
                               UKB.1b$trait,GS.1a$trait,GS.1b$trait,
                               FEMA.1a$Phenotype,FEMA.1b$Phenotype,REMA.1a$Phenotype,REMA.1b$Phenotype),
                     HR = c(FGR11.1a$`OccupationUpper-level_HR`,FGR11.1b$PRS_HR,
                            UKB.1a$`OccupationUpper-level_HR`,UKB.1b$PRS_HR,
                            GS.1a$`OccupationUpper-level_HR`,GS.1b$PRS_HR,
                            FEMA.1a$HR,FEMA.1b$HR,REMA.1a$HR,REMA.1b$HR),
                     lb = c(FGR11.1a$`OccupationUpper-level_HR_lower95`,FGR11.1b$PRS_HR_lower95,
                            UKB.1a$`OccupationUpper-level_HR_lower95`,UKB.1b$PRS_HR_lower95,
                            GS.1a$`OccupationUpper-level_HR_lower95`,GS.1b$PRS_HR_lower95,
                            FEMA.1a$Cineg,FEMA.1b$Cineg,REMA.1a$Cineg,REMA.1b$Cineg),
                     ub = c(FGR11.1a$`OccupationUpper-level_HR_upper95`,FGR11.1b$PRS_HR_upper95,
                            UKB.1a$`OccupationUpper-level_HR_upper95`,UKB.1b$PRS_HR_upper95,
                            GS.1a$`OccupationUpper-level_HR_upper95`,GS.1b$PRS_HR_upper95,
                            FEMA.1a$Cipos,FEMA.1b$Cipos,REMA.1a$Cipos,REMA.1b$Cipos),
                     Test = c(rep("Upper-level Occupation",nrow(FGR11.1a)),rep("PRS",nrow(FGR11.1b)), 
                              rep("Upper-level Occupation", nrow(UKB.1a)),rep("PRS",nrow(UKB.1b)), 
                              rep("Upper-level Occupation",nrow(GS.1a)),rep("PRS",nrow(GS.1b)),
                              rep("Upper-level Occupation",nrow(FEMA.1a)), rep("PRS",nrow(FEMA.1b)),
                              rep("Upper-level Occupation",nrow(REMA.1a)), rep("PRS",nrow(REMA.1b))),
                     Biobanks = c(FGR11.1a$Biobank,FGR11.1b$Biobank,UKB.1a$Biobank,
                                  UKB.1b$Biobank,GS.1a$Biobank,GS.1b$Biobank,
                                  FEMA.1a$Biobank,FEMA.1b$Biobank,
                                  REMA.1a$Biobank,REMA.1b$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model1$trait <- factor(model1$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                "C3_COLORECTAL","I9_AF","I9_CHD",
                                                "COX_ARTHROSIS","C3_MELANOMA_SKIN",
                                                "J10_ASTHMA","KNEE_ARTHROSIS",
                                                "F5_DEPRESSIO","C3_BRONCHUS_LUNG",
                                                "C3_CANCER","K11_APPENDACUT","G6_EPLEPSY",
                                                "AUD_SWEDISH"),
                       labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                                  "* Colorectal Cancer","* Atrial Fibrillation",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "* Skin Melanoma","Asthma","Knee Osteoarthritis",
                                  "Major Depression","Lung Cancer","Any Cancer",
                                  "Appendicitis","Epilepsy","Alcohol Use Disorder"))
# EA levels as factor to plot them in order of magnitude
model1$Test <- factor(model1$Test, levels = c("Upper-level Occupation","PRS"), labels = c("Upper-level Occupation","PRS"))
# Biobank as factor
model1$Biobanks <- factor(model1$Biobanks, levels = c("Generation Scotland","UK Biobank","FinnGen",
                                                      "RE meta-analysis","FE meta-analysis"), 
                          labels = c("Generation Scotland","UK Biobank","FinnGen",
                                     "RE meta-analysis","FE meta-analysis"))
#reverse factor order 
model1$Biobanks <- fct_rev(model1$Biobanks)


################################################################################
#
# Create Figure
#
################################################################################

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
FigS8 <- ggplot(model1, aes(x = HR, y = Biobanks, 
                            xmin = lb, xmax = ub,
                            group = Test, color = Test)) + 
  geom_point(position = position_dodge(1), size=9.5) + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5) +
  theme_minimal() + scale_colour_manual(values= c("#BC65DB","black")) + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,5.5,1),color="black",size=.5) +
  scale_x_continuous(transform = scales::transform_log(), 
                     breaks = c(0.2,0.3,0.4,0.6,1.0, 1.5, 2, 2.6,3.3)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio per Standard Deviation (95% CI)") + facet_wrap(~trait)

# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_Occupation_eFigure8.png"),
     width=700,height=600,units='mm',res=600,pointsize=6)
FigS8
dev.off()

