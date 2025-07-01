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
# Script: Create multi-ancestry supplementary figures (per ancestry)
#
# Figures this script creates: model 1 (EA or PRS): per ancestry
#
# Data: UKB (EUR, AFR, EAS, and SAS) model 1a+b
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
# wrangling; cowplot + grid + gridExtra = combining plots.
packages("data.table","ggplot2","viridis","dplyr","forcats","stringr","cowplot",
         "grid","gridExtra")


################################################################################
#
# Read in full sample hazard ratios per standard deviation
#
################################################################################

# read in model 1a - EA only
UKB.EUR.1a <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.EUR.1a$Ancestry <- "EUR"
#
UKB.AFR.1a <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_AFR_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.AFR.1a$Ancestry <- "AFR"
#
UKB.EAS.1a <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EAS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.EAS.1a$Ancestry <- "EAS"
#
UKB.SAS.1a <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_SAS_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt",data.table=FALSE)
UKB.SAS.1a$Ancestry <- "SAS"

# read in model 1b - PRS only
UKB.EUR.1b <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.EUR.1b$Ancestry <- "EUR"
#
UKB.AFR.1b <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_AFR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.AFR.1b$Ancestry <- "AFR"
#
UKB.EAS.1b <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EAS_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.EAS.1b$Ancestry <- "EAS"
#
UKB.SAS.1b <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_SAS_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.SAS.1b$Ancestry <- "SAS"


################################################################################
#
# From EUR dataframes remove T1D + AUD as those cannot be replicated
#
################################################################################

UKB.EUR.1a <- UKB.EUR.1a[-which(UKB.EUR.1a$trait=="T1D" | UKB.EUR.1a$trait=="AUD_SWEDISH"),]
UKB.EUR.1b <- UKB.EUR.1b[-which(UKB.EUR.1b$trait=="T1D" | UKB.EUR.1b$trait=="AUD_SWEDISH"),]

################################################################################
#
# Combine multi-ancestry results
#
################################################################################

# create dataset combining results for model 1a
Model1a <- data.frame(Ancestry = c(UKB.EUR.1a$Ancestry,UKB.AFR.1a$Ancestry,
                                  UKB.EAS.1a$Ancestry,UKB.SAS.1a$Ancestry),
                  Phenotype = c(UKB.EUR.1a$trait,UKB.AFR.1a$trait,
                                UKB.EAS.1a$trait,UKB.SAS.1a$trait),
                  HR = c(UKB.EUR.1a$EAhigh_HR,UKB.AFR.1a$EAhigh_HR,
                         UKB.EAS.1a$EAhigh_HR,UKB.SAS.1a$EAhigh_HR),
                  Cineg = c(UKB.EUR.1a$EAhigh_HR_lower95,UKB.AFR.1a$EAhigh_HR_lower95,
                            UKB.EAS.1a$EAhigh_HR_lower95,UKB.SAS.1a$EAhigh_HR_lower95),
                  Cipos = c(UKB.EUR.1a$EAhigh_HR_upper95,UKB.AFR.1a$EAhigh_HR_upper95,
                            UKB.EAS.1a$EAhigh_HR_upper95,UKB.SAS.1a$EAhigh_HR_upper95))
# adjust labels
Model1a$Phenotype <- factor(Model1a$Phenotype, levels = c("C3_PROSTATE","GOUT",
                                                        "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                        "G6_EPLEPSY"),
                           labels = c("Prostate Cancer","Gout","Rheumatoid Arthritis",
                                      "Breast Cancer","Epilepsy"))
Model1a$Ancestry <- factor(Model1a$Ancestry, levels = c("EUR","AFR","EAS","SAS"), 
                           labels = c("EUR","AFR","EAS","SAS"))

# create dataset combining results for model 1b 
Model1b <- data.frame(Ancestry = c(UKB.EUR.1b$Ancestry,UKB.AFR.1b$Ancestry,
                                   UKB.EAS.1b$Ancestry,UKB.SAS.1b$Ancestry),
                      Phenotype = c(UKB.EUR.1b$trait,UKB.AFR.1b$trait,
                                    UKB.EAS.1b$trait,UKB.SAS.1b$trait),
                      HR = c(UKB.EUR.1b$PRS_HR,UKB.AFR.1b$PRS_HR,
                             UKB.EAS.1b$PRS_HR,UKB.SAS.1b$PRS_HR),
                      Cineg = c(UKB.EUR.1b$PRS_HR_lower95,UKB.AFR.1b$PRS_HR_lower95,
                                UKB.EAS.1b$PRS_HR_lower95,UKB.SAS.1b$PRS_HR_lower95),
                      Cipos = c(UKB.EUR.1b$PRS_HR_upper95,UKB.AFR.1b$PRS_HR_upper95,
                                UKB.EAS.1b$PRS_HR_upper95,UKB.SAS.1b$PRS_HR_upper95))
# adjust labels
Model1b$Phenotype <- factor(Model1b$Phenotype, levels = c("C3_PROSTATE","GOUT",
                                                          "RHEUMA_SEROPOS_OTH","C3_BREAST",
                                                          "G6_EPLEPSY"),
                            labels = c("Prostate Cancer","Gout","Rheumatoid Arthritis",
                                       "Breast Cancer","Epilepsy"))
Model1b$Ancestry <- factor(Model1b$Ancestry, levels = c("EUR","AFR","EAS","SAS"), 
                           labels = c("EUR","AFR","EAS","SAS"))


################################################################################
#
# Create Figures 
#
################################################################################

## PGS ##

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
Fig1a <- ggplot(Model1b, aes(x = HR, y = Ancestry, 
                             xmin = Cineg, xmax = Cipos)) + 
  geom_point(position = position_dodge(1), size=9.5) + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5) +
  theme_minimal() + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,18.5,1),color="black",size=.5) +
  scale_x_continuous(transform = scales::transform_log(), 
                     breaks = c(0.8,0.9,1,1.1,1.2, 1.4,1.6,1.8,2,2.2)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio per Standard Deviation (95% CI)") + facet_wrap(~Phenotype)


## Education ##

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
Fig1b <- ggplot(Model1a, aes(x = HR, y = Ancestry, 
                            xmin = Cineg, xmax = Cipos)) + 
  geom_point(position = position_dodge(1), size=9.5, colour = "#BC65DB") + 
  scale_shape_manual(values=c(15,16))+
  geom_linerange(position = position_dodge(1),size=5, colour = "#BC65DB") +
  theme_minimal() + geom_vline(xintercept = 1, lty = 2,size = 2) +
  geom_hline(yintercept=seq(0.5,18.5,1),color="black",size=.5) +
  scale_x_continuous(transform = scales::transform_log(), 
                     breaks = c(0.4,0.5,0.6,0.8,1,1.2,1.5, 2, 2.5, 3)) +
  theme(axis.title.y = element_blank(),legend.title = element_blank(), 
        legend.position = "bottom", axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=20),axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),strip.text.x = element_text(size = 20)) + 
  xlab("Hazard Ratio High vs. Low Education (95% CI)") + facet_wrap(~Phenotype)


################################################################################
#
# Create Figure as panel figure comprising 1A-B
#
################################################################################

# combine plots with empty space
top_plt <- plot_grid(Fig1a,Fig1b,nrow=1, align="h")
# add additional empty space for legends
Fig9 <- plot_grid(NULL,top_plt, nrow=2, rel_heights=c(0.05,0.95))

# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_EducationalAttainment_eFigure9.png"),
     width=700,height=600,units='mm',res=600,pointsize=6)
Fig9
dev.off()

