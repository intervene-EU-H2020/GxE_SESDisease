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
# Figure this script creates: Figure S12: model 3: per cohort + meta-analysis
#
# Data: FGR11 + UKB + GS + meta-analysis model 3
#
# Last edits: 16/04/2026 (edits, FAH: update to new efigure 12)
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

# read in model 3
FGR11.3 <- fread("output/GoogleDrive/FGR11/2025-02-12_INTERVENE_Occupation_Coeffs_CoxPH_model3_FinnGenR11.txt", data.table=FALSE)
FGR11.3$Biobank <- "FinnGen"
#
UKB.3 <- fread("output/GoogleDrive/UKB/2026-02-17_UKBiobank_INTERVENE_Occupation_CoxPH_model3_Coeffs.txt",data.table=FALSE)
UKB.3$Biobank <- "UK Biobank"
#
GS.3 <- fread("output/GoogleDrive/GS/2026-02-27_GS_INTERVENE_Occupation_CoxPH_model3_Coeffs.txt",data.table=FALSE)
GS.3$Biobank <- "Generation Scotland"
#
FEMA.3 <- fread("output/GoogleDrive/MetaAnalysis/2026-03-13_INTERVENE_Occupation_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3.csv", data.table = FALSE)
FEMA.3$Biobank <- "FE meta-analysis"
#
REMA.3 <- fread("output/GoogleDrive/MetaAnalysis/2026-03-13_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model3.csv", data.table = FALSE)
REMA.3$Biobank <- "RE meta-analysis"


################################################################################
#
# Generation Scotland included "T1D, Rheumatoid arthritis, skin melanoma, and
# epilepsy" which have too small sample sizes to perform the analyses in. Remove
# these traits prior to the meta-analysis
#
################################################################################

GS.3 <- GS.3[-which(GS.3$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
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
FEMA.3 <- FEMA.3[-which(FEMA.3$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                "C3_MELANOMA_SKIN")),]

# random effect
REMA.3 <- REMA.3[-which(REMA.3$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                "C3_MELANOMA_SKIN")),]


################################################################################
#
# Combine FGR11 & UKB & GenScot & meta-analysis results
#
################################################################################

# create dataset combining results for model 3 
model3 <- data.frame(Biobank = c(FEMA.3$Biobank,REMA.3$Biobank,FGR11.3$Biobank,
                              UKB.3$Biobank,GS.3$Biobank),
                  OccupationGroup = c(FEMA.3$EA,REMA.3$EA,FGR11.3$Test,UKB.3$Test,
                                     GS.3$Test),
                  Phenotype = c(FEMA.3$Phenotype,REMA.3$Phenotype,FGR11.3$trait,
                                UKB.3$trait,GS.3$trait),
                  Beta = c(FEMA.3$Beta,REMA.3$Beta,FGR11.3$PRS_beta,UKB.3$PRS_beta,
                           GS.3$PRS_beta),
                  SE = c(FEMA.3$SE,REMA.3$SE,FGR11.3$PRS_se,UKB.3$PRS_se,GS.3$PRS_se),
                  Pval = c(FEMA.3$Pval,REMA.3$Pval,FGR11.3$PRS_p,UKB.3$PRS_p,GS.3$PRS_p),
                  HR = c(FEMA.3$HR,REMA.3$HR,FGR11.3$PRS_HR,UKB.3$PRS_HR,GS.3$PRS_HR),
                  Cineg = c(FEMA.3$Cineg,REMA.3$Cineg,FGR11.3$PRS_HR_lower95,
                            UKB.3$PRS_HR_lower95,GS.3$PRS_HR_lower95),
                  Cipos = c(FEMA.3$Cipos,REMA.3$Cipos,FGR11.3$PRS_HR_upper95,
                            UKB.3$PRS_HR_upper95,GS.3$PRS_HR_upper95))
# adjust labels
model3$OccupationGroup[which(model3$OccupationGroup=="Lower-level")] <- "Lower-level Occupation"
model3$OccupationGroup[which(model3$OccupationGroup=="Upper-level")] <- "Upper-level Occupation"
#
model3$Phenotype <- factor(model3$Phenotype, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
                                   "Knee Osteoarthritis","* Skin Melanoma","Lung Cancer",
                                   "Major Depression","Any Cancer","Epilepsy",
                                   "Appendicitis","Alcohol Use Disorder"))
# Biobank as factor
model3$Biobank <- factor(model3$Biobank, levels = c("FE meta-analysis","RE meta-analysis",
                                              "FinnGen","UK Biobank", "Generation Scotland"), 
                      labels = c("FE meta-analysis","RE meta-analysis","FinnGen",
                                 "UK Biobank","Generation Scotland"))


################################################################################
#
# Create Figure
#
################################################################################

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
FigS12 <- ggplot(model3, aes(x = HR, y = Biobank, 
                         xmin = Cineg, xmax = Cipos,
                         group = OccupationGroup, color = OccupationGroup)) + 
  geom_point(position = position_dodge(1), size=9.5) + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5) +
  theme_minimal() + scale_colour_manual(values= c(  "#BC65DB","#3869AF")) + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,5.5,1),color="black",size=.5) +
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(0.5,0.7,1.0,1.5,2.0,2.5,3.0,4.0),
                     limits = c(0.5, 4.1),
                     expand = c(0,0)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio per Standard Deviation (95% CI)") + facet_wrap(~Phenotype)


# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_Occupation_eFigure12.png"),
     width=700,height=600,units='mm',res=600,pointsize=6)
FigS12
dev.off()

