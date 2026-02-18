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
# Figure this script creates: Figure S5: model 2 (EA+PRS): per cohort + meta-analysis
#
# Data: FGR11 + UKB + GS + meta-analysis model 2
#
# Last edits: 18/02/2026 (edits, FAH: add random-effect meta-analytical results)
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

# read in model 2
FGR11.2 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt", data.table=FALSE)
FGR11.2$Biobank <- "FinnGen"
#
UKB.2.EUR <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
UKB.2.EUR$Biobank <- "UK Biobank"
#
GS.2 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
GS.2$Biobank <- "Generation Scotland"
#
FEMA.2 <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2.xlsx"))
FEMA.2$Biobank <- "FE meta-analysis"
#
REMA.2 <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model2.csv", data.table = FALSE)
REMA.2$Biobank <- "RE meta-analysis"


################################################################################
#
# As current version of the meta-analysis also includes traits only available in
# FinnGen, subset those data frames to only include the traits where >=2 cohorts
# were analysed.
#
################################################################################

## remove traits not meta-analyzed ##
# fixed effect
FEMA.2 <- FEMA.2[-which(FEMA.2$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]

# random effect
REMA.2 <- REMA.2[-which(REMA.2$Phenotype %in% c("I9_AF","C3_COLORECTAL")),]


################################################################################
#
# Combine FGR11 & UKB & GenScot & meta-analysis results
#
################################################################################

# create dataset combining results for model 2
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
TS6$Test[which(TS6$Test=="PRS")] <- "PGS"
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
# Biobank as factor
TS6$Biobank <- factor(TS6$Biobank, levels = c("FE meta-analysis","RE meta-analysis",
                                              "FinnGen","UK Biobank", "Generation Scotland"), 
                      labels = c("FE meta-analysis","RE meta-analysis",
                                              "FinnGen","UK Biobank", "Generation Scotland"))


################################################################################
#
# Create Figure
#
################################################################################

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
FigS5 <- ggplot(TS6, aes(x = HR, y = Biobank, 
                               xmin = Cineg, xmax = Cipos,
                               group = Test, color = Test)) + 
  geom_point(position = position_dodge(1), size=9.5) + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5) +
  theme_minimal() + scale_colour_manual(values= c("#BC65DB","black")) + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,5.5,1),color="black",size=.5) +
  scale_x_continuous(transform = scales::transform_log(), 
                     breaks = c(0.2,0.3,0.4,0.5,0.7,1.1, 1.5, 2, 2.5, 3)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio per Standard Deviation (95% CI)") + facet_wrap(~Phenotype)


# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_EducationalAttainment_eFigure5.png"),
     width=700,height=600,units='mm',res=600,pointsize=6)
FigS5
dev.off()

