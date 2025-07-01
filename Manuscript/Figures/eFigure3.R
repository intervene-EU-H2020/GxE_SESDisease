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
# Figure this script creates: eFigure 3: model 1 (EA or PRS): per cohort + meta-analysis
#
# Data: FGR11 + UKB + GS + meta-analysis model 1a+b
#
# Last edits: 01/07/2025 (edits, FAH: final checks and minor tweaks prior to
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

# read in model 1a - EA only
FGR11.1a <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt", data.table=FALSE)
FGR11.1a$Biobank <- "FinnGen"
#
UKB.1a <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.1a$Biobank <- "UK Biobank"
#
GS.1a <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
GS.1a$Biobank <- "Generation Scotland"
#
FEMA.1a <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.xlsx"))
FEMA.1a$Biobank <- "FE meta-analysis"

# read in model 1b - PRS only
FGR11.1b <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt", data.table=FALSE)
FGR11.1b$Biobank <- "FinnGen"
#
UKB.1b <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.1b$Biobank <- "UK Biobank"
#
GS.1b <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
GS.1b$Biobank <- "Generation Scotland"
#
FEMA.1b <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.xlsx"))
FEMA.1b$Biobank <- "FE meta-analysis"


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
                  Test = c(rep("high educational attainment",nrow(FEMA.1a)), rep("PGS",nrow(FEMA.1b)),
                           rep("high educational attainment",nrow(FGR11.1a)), rep("PGS",nrow(FGR11.1b)), 
                           rep("high educational attainment", nrow(UKB.1a)), rep("PGS",nrow(UKB.1b)), 
                           rep("high educational attainment",nrow(GS.1a)), rep("PGS",nrow(GS.1b))),
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

# Biobank as factor
TS5$Biobank <- factor(TS5$Biobank, levels = c("FE meta-analysis","FinnGen","UK Biobank", "Generation Scotland"), 
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
FigS3 <- ggplot(TS5, aes(x = HR, y = Biobank, 
                            xmin = Cineg, xmax = Cipos,
                            group = Test, color = Test)) + 
  geom_point(position = position_dodge(1), size=9.5) + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5) +
  theme_minimal() + scale_colour_manual(values= c("#BC65DB","black")) + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,18.5,1),color="black",size=.5) +
  scale_x_continuous(transform = scales::transform_log(), 
                     breaks = c(0.2,0.3,0.4,0.5,0.7,1.1, 1.5, 2, 2.5, 3)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio per Standard Deviation (95% CI)") + facet_wrap(~Phenotype)

# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_EducationalAttainment_eFigure3.png"),
     width=700,height=600,units='mm',res=600,pointsize=6)
FigS3
dev.off()

