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
# Script: Suplemental figure 11 - compare EA and OCC 
#
# Data: 
#       1) FGR11 + UKB + GS + random-effect meta-analysis model 1a (EA + occupation)
#       2) FGR11 + UKB + GS + random-effect meta-analysis model 1b (PGS)
#       3) FGR11 + UKB + GS + random-effect meta-analysis model 3b (EA + occupation)
#
# Last edits: 16/04/2026 (edits, FAH: update to new EFigure 11)
# 
################################################################################

################################################################################
#
# Set up script
#
################################################################################

# set working directory (the working directory is the project folder on my VM on
# the FinnGen Sosioeconomic Data Sandbox)
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
# viridis = color-blind friendly colors; dplyr, forcats, stringr & tidyr = data
# wrangling; cowplot + grid + gridExtra = combining plots; readxl = read excel
# files (upload to googledrive converts csv to xlsx.
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

# Color list by trait, adapted from Kira Detrois (in contrast to previous script
# adjust order so I'm sure the color order will match previous figures)
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
REMA.1b.EA <- fread("output/GoogleDrive/MetaAnalysis/2026-02-18_INTERVENE_EducationalAttainment_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv",data.table=FALSE)
REMA.1b.EA$Biobank <- "RE meta-analysis"
REMA.1b.OCC <- fread("output/GoogleDrive/MetaAnalysis/2026-03-12_INTERVENE_Occupation_REMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.csv",data.table=FALSE)
REMA.1b.OCC$Biobank <- "RE meta-analysis"

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
GS.1a.OCC <- GS.1a.OCC[-which(GS.1a.OCC$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                                  "C3_MELANOMA_SKIN","G6_EPLEPSY")),]
GS.1b.OCC <- GS.1b.OCC[-which(GS.1b.OCC$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                                  "C3_MELANOMA_SKIN","G6_EPLEPSY")),]
GS.3.OCC <- GS.3.OCC[-which(GS.3.OCC$trait %in% c("T1D","RHEUMA_SEROPOS_OTH",
                                      "C3_MELANOMA_SKIN","G6_EPLEPSY")),]


################################################################################
#
# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits
#
################################################################################

# Educational attainment
REMA.1a.EA <- REMA.1a.EA[-which(REMA.1a.EA$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
REMA.1b.EA <- REMA.1b.EA[-which(REMA.1b.EA$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
REMA.3.EA <- REMA.3.EA[-which(REMA.3.EA$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]

# Occupation
REMA.1a.OCC <- REMA.1a.OCC[-which(REMA.1a.OCC$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                               "C3_MELANOMA_SKIN")),]
REMA.1b.OCC <- REMA.1b.OCC[-which(REMA.1b.OCC$Phenotype %in% c("I9_AF","C3_COLORECTAL",
                                                               "C3_MELANOMA_SKIN")),]
REMA.3.OCC <- REMA.3.OCC[-which(REMA.3.OCC$Phenotype %in% c("I9_AF","C3_COLORECTAL",
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



################################################################################
#
# Adjust dataframes to create figures - random effects
#
################################################################################

# reintroduce plotting order traits
plot12r_diff$trait <- factor(plot12r_diff$trait, levels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                                          "Gout","Rheumatoid Arthritis","Breast Cancer",
                                                          "Colorectal Cancer","Atrial Fibrillation",
                                                          "Coronary Heart Disease","Hip Osteoarthritis",
                                                          "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                                          "Major Depression","Lung Cancer","Any Cancer",
                                                          "Appendicitis","Epilepsy","Alcohol Use Disorder"),
                            labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                       "Gout","Rheumatoid Arthritis","Breast Cancer",
                                       "Colorectal Cancer","Atrial Fibrillation",
                                       "Coronary Heart Disease","Hip Osteoarthritis",
                                       "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                       "Major Depression","Lung Cancer","Any Cancer",
                                       "Appendicitis","Epilepsy","Alcohol Use Disorder"))
plot34r_diff$trait <- factor(plot34r_diff$trait, levels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                                          "Gout","Rheumatoid Arthritis","Breast Cancer",
                                                          "Colorectal Cancer","Atrial Fibrillation",
                                                          "Coronary Heart Disease","Hip Osteoarthritis",
                                                          "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                                          "Major Depression","Lung Cancer","Any Cancer",
                                                          "Appendicitis","Epilepsy","Alcohol Use Disorder"),
                            labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                       "Gout","Rheumatoid Arthritis","Breast Cancer",
                                       "Colorectal Cancer","Atrial Fibrillation",
                                       "Coronary Heart Disease","Hip Osteoarthritis",
                                       "Skin Melanoma","Asthma","Knee Osteoarthritis",
                                       "Major Depression","Lung Cancer","Any Cancer",
                                       "Appendicitis","Epilepsy","Alcohol Use Disorder"))

# create dataframe to plot the background rectangles for Fig2, adapted from Kira Detrois
# [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
endpt_order_12r <- filter(plot12r_diff) %>%
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))
#
endpt_order_34r <- filter(plot34r_diff) %>%
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))

# merge to HRs
plot12r_diff <- merge(plot12r_diff,endpt_order_12r,by = "trait")
plot34r_diff <- merge(plot34r_diff,endpt_order_34r,by = "trait")

#reverse factor order for plotting
plot12r_diff$trait <- fct_rev(plot12r_diff$trait)
plot34r_diff$trait <- fct_rev(plot34r_diff$trait)


################################################################################
#
# Create Figure A+B (compare model a/b) - random effects
#
################################################################################

## Figure A ## 

# scatterplot of the unadjusted HRs for EA vs. the HRs for EA adjusted for the PGS
FigA <- ggplot(plot12r_diff[which(plot12r_diff$Test=="SES"),], 
               aes(x = Model1_HR, y = Model2_HR, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=1, linetype=1, size=line_size, colour = "grey") +
  geom_vline(xintercept=1, linetype=1, size=line_size, colour = "grey") +
  geom_abline(intercept=0, slope=1, linetype=6, size=line_size) +
  geom_hline(yintercept=0.3, linetype=1, size=line_size) +
  geom_vline(xintercept=0.3, linetype=1, size=line_size) +
  # Confidence intervals
  geom_linerange(aes(xmin = Model1_HR_lb, xmax = Model1_HR_ub), size=line_size) +
  geom_linerange(aes(ymin = Model2_HR_lb, ymax = Model2_HR_ub), size=line_size) +
  # Data
  geom_point(size=point_size) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.2,1.4,1.6,1.80),
                     limits = c(0.3, 1.92),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.9),
                     limits = c(0.3, 1.92),
                     expand = c(0,0)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-10, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))

## Figure B ## 

# scatterplot of the unadjusted HRs for EA vs. the HRs for EA adjusted for the PGS
FigB <- ggplot(plot12r_diff[which(plot12r_diff$Test=="PRS"),], 
               aes(x = Model1_HR, y = Model2_HR, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=0.9, linetype=1, size=line_size) +
  geom_vline(xintercept=0.9, linetype=1, size=line_size) +
  geom_abline(intercept=0, slope=1, linetype=6, size=line_size) +
  # Confidence intervals
  geom_linerange(aes(xmin = Model1_HR_lb, xmax = Model1_HR_ub), size=line_size) +
  geom_linerange(aes(ymin = Model2_HR_lb, ymax = Model2_HR_ub), size=line_size) +
  # Data
  geom_point(size=point_size) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.2,2.4,2.6),
                     limits = c(0.9, 2.6),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1,2.2,2.3,2.4,2.5),
                     limits = c(0.9, 2.6),
                     expand = c(0,0)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-10, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))


################################################################################
#
# Create Figure C+D (compare model 3) - random effects
#
################################################################################

## Figure C ## 

# low EA vs lower-level OCC
FigC <- ggplot(plot34r_diff[which(plot34r_diff$Test=="low"),], 
               aes(x = Model1_HR, y = Model2_HR, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=0.9, linetype=1, size=line_size) +
  geom_vline(xintercept=0.9, linetype=1, size=line_size) +
  geom_abline(intercept=0, slope=1, linetype=6, size=line_size) +
  # Confidence intervals
  geom_linerange(aes(xmin = Model1_HR_lb, xmax = Model1_HR_ub), size=line_size) +
  geom_linerange(aes(ymin = Model2_HR_lb, ymax = Model2_HR_ub), size=line_size) +
  # Data
  geom_point(size=point_size) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1,2.2,2.3,2.5),
                     limits = c(0.9, 2.5),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1,2.2,2.3,2.4,2.5),
                     limits = c(0.9, 2.5),
                     expand = c(0,0)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-10, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))

## Figure D ## 

#high EA vs higher-level occupation
FigD <- ggplot(plot34r_diff[which(plot34r_diff$Test=="high"),], 
               aes(x = Model1_HR, y = Model2_HR, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=0.8, linetype=1, size=line_size) +
  geom_vline(xintercept=0.8, linetype=1, size=line_size) +
  geom_abline(intercept=0, slope=1, linetype=6, size=line_size) +
  # Confidence intervals
  geom_linerange(aes(xmin = Model1_HR_lb, xmax = Model1_HR_ub), size=line_size) +
  geom_linerange(aes(ymin = Model2_HR_lb, ymax = Model2_HR_ub), size=line_size) +
  # Data
  geom_point(size=point_size) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,2.0,2.2,2.4,2.6,2.8),
                     limits = c(0.8, 2.8),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1,2.3,2.5,2.7,2.9,3.1,3.3,3.5),
                     limits = c(0.8, 3.71),
                     expand = c(0,0)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-10, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))


################################################################################
#
# Create Figure as panel figure comprising A-D
#
################################################################################

# combine plots REMA with empty space
top_plt <- plot_grid(FigA,FigB, 
                     NULL,NULL,
                     FigC,FigD,
                     nrow=3, align="h", 
                     rel_widths=c(0.5, 0.5),
                     rel_heights = c(0.49,0.01,0.49))
# add additional empty space for legends
FigS11 <- plot_grid(NULL,top_plt,NULL, nrow=3, rel_heights=c(0.01,0.89, 0.10))

# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                      "_INTERVENE_Occupation_eFigure11.png"),
    width = 20, height = 22,units='in',res=600)
FigS11
dev.off()
