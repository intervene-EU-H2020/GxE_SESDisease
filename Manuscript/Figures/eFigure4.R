#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# educational attainment [EA]) in risk of 18 common diseases (as previously
# selected in the INTERVENE flagship manuscript:
# https://doi.org/10.1101/2023.06.12.23291186) and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Create manuscript figures - supplementary figure 4: A: scatter
# comparison model 1a and 2 fixed effect (EA); B: scatter comparison model 1b
# and 2 fixed effect(PGS); C: scatter comparison model 1a and 2 random effect
# (EA); D: scatter comparison model 1b and 2 random effect(PGS)
#
# Data: 
#       1) FGR11 + UKB + GS + meta-analysis model 1a (EA )
#       2) FGR11 + UKB + GS + meta-analysis model 1b (PGS)
#       3) FGR11 + UKB + GS + meta-analysis model 2
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
# viridis = color-blind friendly colors; dplyr, forcats, stringr & tidyr = data
# wrangling; cowplot + grid + gridExtra = combining plots; readxl = read excel
# files (upload to googledrive converts csv to xlsx)
packages("data.table","ggplot2","viridis","dplyr","forcats","stringr","tidyr",
         "cowplot","grid","gridExtra", "RColorBrewer","readxl")

# plot set-up
line_size=1.9
point_size=7
axis_line_size <- 1
base_size <- 28
size_small <- 16
size_medium <- 18
font <- "Sans Serif"

# theme functions from Kira Detrois [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
theme_hrs <- function(base_size = 18,
                      legend_pos = "bottom",
                      plot_top_margin = -30,
                      axis_x_bottom_margin=0,
                      axis_y_left_margin=0,
                      legend_box_spacing=1,
                      line_size=1.5) {
  ggplot2::theme_minimal(base_size = base_size) %+replace%
    ggplot2::theme(
      text=ggplot2::element_text(colour="black"),
      # Titles
      plot.title=ggplot2::element_text(hjust=0, margin=margin(t=plot_top_margin, b=5), size=base_size),
      plot.subtitle=ggplot2::element_text(hjust=0, size=base_size*0.9,  margin=margin(t=plot_top_margin, b=5), face="bold"),
      plot.caption=ggplot2::element_text(size=base_size*0.6, hjust=0, margin=margin(t=10)),
      # Facet grid / wrap titles
      strip.text = ggplot2::element_text(hjust=0, face="bold", size=base_size*0.8, margin=margin(b=5)),
      # Legend
      legend.title=ggplot2::element_blank(),
      legend.position = legend_pos,
      legend.text=ggplot2::element_text(size=base_size*0.8),
      legend.key.spacing.y=grid::unit(0.5, "lines"),
      legend.margin = ggplot2::margin(legend_box_spacing, legend_box_spacing, legend_box_spacing, legend_box_spacing),
      # Axes
      axis.title=ggplot2::element_text(size=base_size*0.8),
      axis.text = ggplot2::element_text(size=base_size*0.75),
      axis.title.x = ggplot2::element_text(margin=margin(t=5, b=axis_x_bottom_margin)),
      axis.title.y = ggplot2::element_text(margin=margin(r=5), l=axis_y_left_margin, angle=90),
      # Other settings
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(colour=NA, fill=NA),
      # Grid settings
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour="grey", linewidth=line_size-0.5*line_size, linetype=2),
      panel.grid.minor.x = ggplot2::element_line(colour = "grey", linewidth=line_size-0.75*line_size, linetype=2),
    )
}

theme_comp <- function(base_size = 18,
                       legend_pos = "bottom",
                       plot_top_margin = -30,
                       axis_x_bottom_margin=0,
                       axis_y_left_margin=0,
                       legend_box_spacing=1) {
  ggplot2::theme_minimal(base_size = base_size) %+replace%
    ggplot2::theme(
      text=ggplot2::element_text(colour="black"),
      # Titles
      plot.title=ggplot2::element_text(hjust=0, margin=margin(t=plot_top_margin, b=5), size=base_size),
      plot.subtitle=ggplot2::element_text(hjust=0, size=base_size*0.9,  margin=margin(t=plot_top_margin, b=5), face="bold"),
      plot.caption=ggplot2::element_text(size=base_size*0.5, hjust=0),
      # Facet grid / wrap titles
      strip.text = ggplot2::element_text(hjust=0, face="bold", size=base_size*0.8, margin=margin(b=5)),
      # Legend
      legend.title=ggplot2::element_blank(),
      legend.position = legend_pos,
      legend.text=ggplot2::element_text(size=base_size*0.75),
      legend.key.spacing.y=grid::unit(0.5, "lines"),
      legend.margin = ggplot2::margin(legend_box_spacing, legend_box_spacing, legend_box_spacing, legend_box_spacing),
      # Axes
      axis.title=ggplot2::element_text(size=base_size*0.8),
      axis.text = ggplot2::element_text(size=base_size*0.75),
      axis.title.x = ggplot2::element_text(margin=margin(t=5, b=axis_x_bottom_margin)),
      axis.title.y = ggplot2::element_text(margin=margin(r=5), l=axis_y_left_margin, angle=90),
      # Other settings
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(colour=NA, fill=NA),
      aspect.ratio=1
    )
}

# Color list by trait, adapted from Kira Detrois
# (https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R)
color_list <- c("#841C26","#B53389","#C6878F","#A81C07","#D5694F","#FDBF6F",
                "#FBCF9D","#CCB6AF","#7D7C7F","#91A3B0","#3C4E2D","#7BA05B",
                "#9BB59B","#588986","#29557B","#748AAA","#A4D3EE","#ADD8E6",
                "#D6ECFF")


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
#
REMA.1a <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1a.csv",data.table = FALSE)
REMA.1a$Biobank <- "RE meta-analysis"

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
#
REMA.1b <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv", data.table = FALSE)
REMA.1b$Biobank <- "RE meta-analysis"

# read in model 2
FGR11.2 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt", data.table=FALSE)
FGR11.2$Biobank <- "FinnGen"
#
UKB.2 <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
UKB.2$Biobank <- "UK Biobank"
#
GS.2 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt",data.table=FALSE)
GS.2$Biobank <- "Generation Scotland"
#
FEMA.2 <-as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2.xlsx"))
FEMA.2$Biobank <- "FE meta-analysis"
#
REMA.2 <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model2.csv", data.table = FALSE)
REMA.2$Biobank <- "RE meta-analysis"


################################################################################
#
# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits: "Atrial Fibrillation", "Colorectal Cancer",
#
################################################################################

# fixed effect
FEMA.1a <- FEMA.1a[-which(FEMA.1a$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
FEMA.1b <- FEMA.1b[-which(FEMA.1b$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
FEMA.2 <- FEMA.2[-which(FEMA.2$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]

# random effect
REMA.1a <- REMA.1a[-which(REMA.1a$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
REMA.1b <- REMA.1b[-which(REMA.1b$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
REMA.2 <- REMA.2[-which(REMA.2$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results for plotting - fixed effects
#
################################################################################

# model 1 
model1 <- data.frame(trait = c(FGR11.1a$trait,FGR11.1b$trait,UKB.1a$trait,
                               UKB.1b$trait,GS.1a$trait,GS.1b$trait,
                               FEMA.1a$Phenotype,FEMA.1b$Phenotype),
                     HR = c(FGR11.1a$EAhigh_HR,FGR11.1b$PRS_HR,UKB.1a$EAhigh_HR,
                            UKB.1b$PRS_HR,GS.1a$EAhigh_HR,GS.1b$PRS_HR,FEMA.1a$HR,FEMA.1b$HR),
                     lb = c(FGR11.1a$EAhigh_HR_lower95,FGR11.1b$PRS_HR_lower95,
                            UKB.1a$EAhigh_HR_lower95,UKB.1b$PRS_HR_lower95,
                            GS.1a$EAhigh_HR_lower95,GS.1b$PRS_HR_lower95,
                            FEMA.1a$Cineg,FEMA.1b$Cineg),
                     ub = c(FGR11.1a$EAhigh_HR_upper95,FGR11.1b$PRS_HR_upper95,
                            UKB.1a$EAhigh_HR_upper95,UKB.1b$PRS_HR_upper95,
                            GS.1a$EAhigh_HR_upper95,GS.1b$PRS_HR_upper95,
                            FEMA.1a$Cipos,FEMA.1b$Cipos),
                     Test = c(rep("high EA",nrow(FGR11.1a)),rep("PRS",nrow(FGR11.1b)), 
                              rep("high EA", nrow(UKB.1a)),rep("PRS",nrow(UKB.1b)), 
                              rep("high EA",nrow(GS.1a)),rep("PRS",nrow(GS.1b)),
                              rep("high EA",nrow(FEMA.1a)), rep("PRS",nrow(FEMA.1b))),
                     Biobanks = c(FGR11.1a$Biobank,FGR11.1b$Biobank,UKB.1a$Biobank,
                                  UKB.1b$Biobank,GS.1a$Biobank,GS.1b$Biobank,
                                  FEMA.1a$Biobank,FEMA.1b$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model1$trait <- factor(model1$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model1$trait <- fct_rev(model1$trait)
# EA levels as factor to plot them in order of magnitude
model1$Test <- factor(model1$Test, levels = c("high EA","PRS"), labels = c("High Education","PRS"))
# Biobank as factor
model1$Biobanks <- factor(model1$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

# model 2
model2 <- data.frame(trait = c(rep(FGR11.2$trait,2),rep(UKB.2$trait,2),
                               rep(GS.2$trait,2),FEMA.2$Phenotype),
                     HR = c(FGR11.2$EAhigh_HR,FGR11.2$PRS_HR,UKB.2$EAhigh_HR,
                            UKB.2$PRS_HR,GS.2$EAhigh_HR,GS.2$PRS_HR,FEMA.2$HR),
                     lb = c(FGR11.2$EAhigh_HR_lower95,FGR11.2$PRS_HR_lower95,
                            UKB.2$EAhigh_HR_lower95,UKB.2$PRS_HR_lower95,
                            GS.2$EAhigh_HR_lower95,GS.2$PRS_HR_lower95,FEMA.2$Cineg),
                     ub = c(FGR11.2$EAhigh_HR_upper95,FGR11.2$PRS_HR_upper95,
                            UKB.2$EAhigh_HR_upper95,UKB.2$PRS_HR_upper95,
                            GS.2$EAhigh_HR_upper95,GS.2$PRS_HR_upper95,FEMA.2$Cipos),
                     Test = c(rep("high EA",nrow(FGR11.2)), rep("PRS",nrow(FGR11.2)), 
                              rep("high EA", nrow(UKB.2)), rep("PRS",nrow(UKB.2)), 
                              rep("high EA", nrow(GS.2)), rep("PRS",nrow(GS.2)),
                              rep("high EA",0.5*nrow(FEMA.2)), rep("PRS",0.5*nrow(FEMA.2))),
                     Biobanks = c(rep(FGR11.2$Biobank,2),rep(UKB.2$Biobank,2),
                                  rep(GS.2$Biobank,2),FEMA.2$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model2$trait <- factor(model2$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model2$trait <- fct_rev(model2$trait)
# EA levels as factor to plot them in order of magnitude
model2$Test <- factor(model2$Test, levels = c("high EA","PRS"), labels = c("High Education","PRS"))
# Biobank as factor
model2$Biobanks <- factor(model2$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results for plotting - random effect
#
################################################################################

# model 1 
model1r <- data.frame(trait = c(FGR11.1a$trait,FGR11.1b$trait,UKB.1a$trait,
                               UKB.1b$trait,GS.1a$trait,GS.1b$trait,
                               REMA.1a$Phenotype,REMA.1b$Phenotype),
                     HR = c(FGR11.1a$EAhigh_HR,FGR11.1b$PRS_HR,UKB.1a$EAhigh_HR,
                            UKB.1b$PRS_HR,GS.1a$EAhigh_HR,GS.1b$PRS_HR,
                            REMA.1a$HR,REMA.1b$HR),
                     lb = c(FGR11.1a$EAhigh_HR_lower95,FGR11.1b$PRS_HR_lower95,
                            UKB.1a$EAhigh_HR_lower95,UKB.1b$PRS_HR_lower95,
                            GS.1a$EAhigh_HR_lower95,GS.1b$PRS_HR_lower95,
                            REMA.1a$Cineg,REMA.1b$Cineg),
                     ub = c(FGR11.1a$EAhigh_HR_upper95,FGR11.1b$PRS_HR_upper95,
                            UKB.1a$EAhigh_HR_upper95,UKB.1b$PRS_HR_upper95,
                            GS.1a$EAhigh_HR_upper95,GS.1b$PRS_HR_upper95,
                            REMA.1a$Cipos,REMA.1b$Cipos),
                     Test = c(rep("high EA",nrow(FGR11.1a)),rep("PRS",nrow(FGR11.1b)), 
                              rep("high EA", nrow(UKB.1a)),rep("PRS",nrow(UKB.1b)), 
                              rep("high EA",nrow(GS.1a)),rep("PRS",nrow(GS.1b)),
                              rep("high EA",nrow(REMA.1a)), rep("PRS",nrow(REMA.1b))),
                     Biobanks = c(FGR11.1a$Biobank,FGR11.1b$Biobank,UKB.1a$Biobank,
                                  UKB.1b$Biobank,GS.1a$Biobank,GS.1b$Biobank,
                                  REMA.1a$Biobank,REMA.1b$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model1r$trait <- factor(model1r$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model1r$trait <- fct_rev(model1r$trait)
# EA levels as factor to plot them in order of magnitude
model1r$Test <- factor(model1r$Test, levels = c("high EA","PRS"), labels = c("High Education","PRS"))
# Biobank as factor
model1r$Biobanks <- factor(model1r$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland",
                                                      "RE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland",
                                     "RE meta-analysis"))

# model 2
model2r <- data.frame(trait = c(rep(FGR11.2$trait,2),rep(UKB.2$trait,2),
                               rep(GS.2$trait,2),REMA.2$Phenotype),
                     HR = c(FGR11.2$EAhigh_HR,FGR11.2$PRS_HR,UKB.2$EAhigh_HR,
                            UKB.2$PRS_HR,GS.2$EAhigh_HR,GS.2$PRS_HR,REMA.2$HR),
                     lb = c(FGR11.2$EAhigh_HR_lower95,FGR11.2$PRS_HR_lower95,
                            UKB.2$EAhigh_HR_lower95,UKB.2$PRS_HR_lower95,
                            GS.2$EAhigh_HR_lower95,GS.2$PRS_HR_lower95,REMA.2$Cineg),
                     ub = c(FGR11.2$EAhigh_HR_upper95,FGR11.2$PRS_HR_upper95,
                            UKB.2$EAhigh_HR_upper95,UKB.2$PRS_HR_upper95,
                            GS.2$EAhigh_HR_upper95,GS.2$PRS_HR_upper95,REMA.2$Cipos),
                     Test = c(rep("high EA",nrow(FGR11.2)), rep("PRS",nrow(FGR11.2)), 
                              rep("high EA", nrow(UKB.2)), rep("PRS",nrow(UKB.2)), 
                              rep("high EA", nrow(GS.2)), rep("PRS",nrow(GS.2)),
                              rep("high EA",0.5*nrow(REMA.2)), rep("PRS",0.5*nrow(REMA.2))),
                     Biobanks = c(rep(FGR11.2$Biobank,2),rep(UKB.2$Biobank,2),
                                  rep(GS.2$Biobank,2),REMA.2$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model2r$trait <- factor(model2r$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
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
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model2r$trait <- fct_rev(model2r$trait)
# EA levels as factor to plot them in order of magnitude
model2r$Test <- factor(model2r$Test, levels = c("high EA","PRS"), labels = c("High Education","PRS"))
# Biobank as factor
model2r$Biobanks <- factor(model2r$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland",
                                                      "RE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland",
                                     "RE meta-analysis"))


################################################################################
#
# Adjust dataframe to create Main Figure 1A + B - fixed effect
#
################################################################################

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB estimates
plot1a.dat <- model1[-which(model1$Biobanks=="UK Biobank" | model1$Biobanks=="Generation Scotland"),]
plot2.dat <- model2[-which(model2$Biobanks=="UK Biobank" | model2$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot1a.dat <- plot1a.dat[-which(plot1a.dat$trait[which(plot1a.dat$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]
plot2.dat <- plot2.dat[-which(plot2.dat$trait[which(plot2.dat$Biobanks=="FinnGen")] %in% 
                                c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                  "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                  "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                  "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                  "Knee Osteoarthritis")),]

# combine data frames model 1 and 2 for plotting 
df12 <- rbind(plot1a.dat,plot2.dat) #row bind
df12$Model <- c(rep(c("model 1", "model2"),each=nrow(plot1a.dat)))

# create dataframe to plot the background rectangles for Fig1A, adapted from Kira Detrois
# [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
endpt_order <- filter(df12, Test=="PRS" & Model=="model 1") %>%
  arrange(HR) %>% 
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))

# combine data frames model 1 and 2 for plotting 
df12alt <- cbind(plot1a.dat,plot2.dat[,c("HR","lb","ub")]) #column bind
names(df12alt) <- c("trait","HR.x","lb.x","ub.x","Test","Biobanks",
                    "HR.y","lb.y","ub.y") # rename columns

# reorder trait by endpoint order
df12alt$trait <- factor(df12alt$trait, levels = endpt_order$trait)


################################################################################
#
# Adjust dataframe to create Main Figure 1A + B - random effect
#
################################################################################

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB estimates
plot1a.datr <- model1r[-which(model1r$Biobanks=="UK Biobank" | model1r$Biobanks=="Generation Scotland"),]
plot2.datr <- model2r[-which(model2r$Biobanks=="UK Biobank" | model2r$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot1a.datr <- plot1a.datr[-which(plot1a.datr$trait[which(plot1a.datr$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]
plot2.datr <- plot2.datr[-which(plot2.datr$trait[which(plot2.datr$Biobanks=="FinnGen")] %in% 
                                c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                  "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                  "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                  "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                  "Knee Osteoarthritis")),]

# combine data frames model 1 and 2 for plotting
df12r <- rbind(plot1a.datr,plot2.datr) #row bind
df12r$Model <- c(rep(c("model 1", "model2"),each=nrow(plot1a.datr)))

# create dataframe to plot the background rectangles for Fig1A, adapted from Kira Detrois
# [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
endpt_orderr <- filter(df12r, Test=="PRS" & Model=="model 1") %>%
  arrange(HR) %>% 
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))

# combine data frames model 1 and 2 for plotting - fixed effect
df12altr <- cbind(plot1a.datr,plot2.datr[,c("HR","lb","ub")]) #column bind
names(df12altr) <- c("trait","HR.x","lb.x","ub.x","Test","Biobanks",
                    "HR.y","lb.y","ub.y") # rename columns

# reorder trait by endpoint order
df12altr$trait <- factor(df12altr$trait, levels = endpt_orderr$trait)


################################################################################
#
# Create SupplementalFigure 1A+B (fixed effect)
#
################################################################################

## Figure A ## 

# scatterplot of the unadjusted HRs for EA vs. the HRs for EA adjusted for the PGS
FigA <- ggplot(df12alt[which(df12alt$Test=="PRS"),], 
               aes(x = HR.x, y = HR.y, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=1, linetype=1, size=line_size) +
  geom_vline(xintercept=1, linetype=1, size=line_size) +
  geom_abline(intercept=0, slope=1, linetype=6, size=line_size) +
  # Confidence intervals
  geom_linerange(aes(xmin = lb.x, xmax = ub.x), size=line_size) +
  geom_linerange(aes(ymin = lb.y, ymax = ub.y), size=line_size) +
  # Data
  geom_point(size=point_size) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.2,1.4,1.6,1.8,2.1), 
                     limits = c(1, 2.3),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2.0,2.2), 
                     limits = c(1, 2.3),
                     expand = c(0,0)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-20, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))


## Figure B ## 

# scatterplot of the unadjusted HRs for EA vs. the HRs for EA adjusted for the PGS
FigB <- ggplot(df12alt[which(df12alt$Test=="High Education"),], 
                aes(x = HR.x, y = HR.y, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=0.5, linetype=1, size=line_size) +
  geom_vline(xintercept=0.5, linetype=1, size=line_size) +
  geom_abline(intercept=0, slope=1, linetype=6, size=line_size) +
  # Confidence intervals
  geom_linerange(aes(xmin = lb.x, xmax = ub.x), size=line_size) +
  geom_linerange(aes(ymin = lb.y, ymax = ub.y), size=line_size) +
  # Data
  geom_point(size=point_size) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Limits
  scale_x_continuous(transform = scales::transform_log(), 
                     breaks = c(0.5,0.6,0.8,1.0,1.2,1.5)) +
  scale_y_continuous(transform = scales::transform_log(), 
                     breaks = c(0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.3,1.5)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-20, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))


################################################################################
#
# Create SupplementalFigure 1C+D (random effect)
#
################################################################################

## Figure C ## 

# scatterplot of the unadjusted HRs for EA vs. the HRs for EA adjusted for the PGS
FigC <- ggplot(df12altr[which(df12altr$Test=="PRS"),], 
               aes(x = HR.x, y = HR.y, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=1, linetype=1, size=line_size) +
  geom_vline(xintercept=1, linetype=1, size=line_size) +
  geom_abline(intercept=0, slope=1, linetype=6, size=line_size) +
  # Confidence intervals
  geom_linerange(aes(xmin = lb.x, xmax = ub.x), size=line_size) +
  geom_linerange(aes(ymin = lb.y, ymax = ub.y), size=line_size) +
  # Data
  geom_point(size=point_size) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.2,1.4,1.6,1.8,2.1), 
                     limits = c(1, 2.3),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2.0,2.2), 
                     limits = c(1, 2.3),
                     expand = c(0,0)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-20, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))


## Figure D ## 

# scatterplot of the unadjusted HRs for EA vs. the HRs for EA adjusted for the PGS
FigD <- ggplot(df12altr[which(df12altr$Test=="High Education"),], 
               aes(x = HR.x, y = HR.y, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=0.5, linetype=1, size=line_size) +
  geom_vline(xintercept=0.5, linetype=1, size=line_size) +
  geom_abline(intercept=0, slope=1, linetype=6, size=line_size) +
  # Confidence intervals
  geom_linerange(aes(xmin = lb.x, xmax = ub.x), size=line_size) +
  geom_linerange(aes(ymin = lb.y, ymax = ub.y), size=line_size) +
  # Data
  geom_point(size=point_size) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Limits
  scale_x_continuous(transform = scales::transform_log(), 
                     breaks = c(0.5,0.6,0.8,1.0,1.2,1.5)) +
  scale_y_continuous(transform = scales::transform_log(), 
                     breaks = c(0.5,0.6,0.7,0.8,1.0,1.2,1.5)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-20, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))


################################################################################
#
# Create Main Figure 1 as panel figure comprising 1A-1D
#
################################################################################

# combine plots with empty space
top_plt <- plot_grid(FigA,NULL,FigB, 
                     NULL,NULL,NULL,
                     FigC,NULL,FigD,
                     nrow=3, align="h", 
                     rel_widths=c(0.5, 0.02, 0.5),
                     rel_heights = c(0.49,0.01,0.49))
# add additional empty space for legends
Fig4 <- plot_grid(NULL,top_plt,NULL, nrow=3, rel_heights=c(0.01,0.89, 0.10))

# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                      "_INTERVENE_EducationalAttainment_eFigure4.png"),
    width = 11, height = 8,units='in',res=600)
Fig4
dev.off()
# figure is finalized in inkscape

