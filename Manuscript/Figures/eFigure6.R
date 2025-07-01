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
# Figure this script creates: Figure S6: model 3: per cohort + meta-analysis
#
# Data: FGR11 + UKB + GS + meta-analysis model 3
#
# Last edits: 01/07//2025 (edits, FAH: final checks and minor tweaks prior to
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
FGR11.3 <- fread("output/GoogleDrive/FGR11/2025-02-07_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt", data.table=FALSE)
FGR11.3$Biobank <- "FinnGen"
#
UKB.3 <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model3b_Coeffs.txt",data.table=FALSE)
UKB.3$Biobank <- "UK Biobank"
#
GS.3 <- fread("output/GoogleDrive/GS/2025-02-24_GS_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
GS.3$Test <- c(rep("LowEA",length(unique(GS.3$trait))),rep("HighEA",length(unique(GS.3$trait)))) # in initial script shared with GS forgot a line of code to add this, so now add it manually. 
GS.3$Biobank <- "Generation Scotland"
#
FEMA.3 <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3.xlsx"))
FEMA.3$Biobank <- "FE meta-analysis"


################################################################################
#
# Updated results for model 3 in Generation Scotland included "Rheumatoid
# arthritis" which we previously determined the sample size was too small to
# perform the analyses in. Remove this trait prior to the meta-analysis
#
################################################################################

GS.3 <- GS.3[-which(GS.3$trait %in% c("RHEUMA_SEROPOS_OTH")),]


################################################################################
#
# As current version of the meta-analysis also includes traits only available in
# FinnGen, subset those data frames to only include the traits where >=2 cohorts
# were analysed.
#
################################################################################

## remove traits not meta-analyzed ##
FEMA.3 <- FEMA.3[-which(FEMA.3$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]


################################################################################
#
# Combine FGR11 & UKB & GenScot & meta-analysis results
#
################################################################################

# create dataset combining results for model 3 
TS7 <- data.frame(Biobank = c(FEMA.3$Biobank,FGR11.3$Biobank,UKB.3$Biobank,
                              GS.3$Biobank),
                  EducationGroup = c(FEMA.3$EA,FGR11.3$Test,UKB.3$Test,GS.3$Test),
                  Phenotype = c(FEMA.3$Phenotype,FGR11.3$trait,UKB.3$trait,
                                GS.3$trait),
                  Beta = c(FEMA.3$Beta,FGR11.3$PRS_beta,UKB.3$PRS_beta,
                           GS.3$PRS_beta),
                  SE = c(FEMA.3$SE,FGR11.3$PRS_se,UKB.3$PRS_se,GS.3$PRS_se),
                  Pval = c(FEMA.3$Pval,FGR11.3$PRS_p,UKB.3$PRS_p,GS.3$PRS_p),
                  HR = c(FEMA.3$HR,FGR11.3$PRS_HR,UKB.3$PRS_HR,GS.3$PRS_HR),
                  Cineg = c(FEMA.3$Cineg,FGR11.3$PRS_HR_lower95,
                            UKB.3$PRS_HR_lower95,GS.3$PRS_HR_lower95),
                  Cipos = c(FEMA.3$Cipos,FGR11.3$PRS_HR_upper95,
                            UKB.3$PRS_HR_upper95,GS.3$PRS_HR_upper95),
                  QHet = c(FEMA.3$QHet,
                           rep(NA,nrow(FGR11.3)),
                           rep(NA,nrow(UKB.3)),
                           rep(NA,nrow(GS.3))),
                  HetPval = c(FEMA.3$HetPval,
                              rep(NA,nrow(FGR11.3)),
                              rep(NA,nrow(UKB.3)),
                              rep(NA,nrow(GS.3))))
# adjust labels
TS7$EducationGroup[which(TS7$EducationGroup=="LowEA")] <- "low educational attainment"
TS7$EducationGroup[which(TS7$EducationGroup=="HighEA")] <- "high educational attainment"
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
# Biobank as factor
TS7$Biobank <- factor(TS7$Biobank, levels = c("FE meta-analysis","FinnGen","UK Biobank", "Generation Scotland"), 
                      labels = c("FE meta-analysis","FinnGen","UK Biobank","Generation Scotland"))


################################################################################
#
# Create Figure
#
################################################################################

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
FigS6 <- ggplot(TS7, aes(x = HR, y = Biobank, 
                         xmin = Cineg, xmax = Cipos,
                         group = EducationGroup, color = EducationGroup)) + 
  geom_point(position = position_dodge(1), size=9.5) + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5) +
  theme_minimal() + scale_colour_manual(values= c(  "#BC65DB","#3869AF")) + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,18.5,1),color="black",size=.5) +
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(0.5,0.7,1.0,1.5,2.0,2.5,3.0,4.0),
                     limits = c(0.5, 4.5),
                     expand = c(0,0)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio High vs. Low Education (95% CI)") + facet_wrap(~Phenotype)


# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_EducationalAttainment_eFigure6.png"),
     width=700,height=600,units='mm',res=600,pointsize=6)
FigS6
dev.off()

