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
# Script: Suplemental figure 12 - compare EA and OCC 
#
# Data: 
#       1) FGR11 model 1a (EA + occupation)
#       2) FGR11 model 1b (PGS)
#       3) FGR11 model 3b (EA + occupation)
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
FGR11.1a.OCC <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model1/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model1a_FinnGenR11.txt", data.table=FALSE)

# read in model 1b - PRS only
FGR11.1b.EA <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt", data.table=FALSE)
FGR11.1b.EA$Biobank <- "FinnGen"
FGR11.1b.OCC <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model1/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model1b_FinnGenR11.txt", data.table=FALSE)
#
UKB.1b.EA <- fread("output/GoogleDrive/UKB/2025-03-07_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.1b.EA$Biobank <- "UK Biobank"
#
GS.1b.EA <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
GS.1b.EA$Biobank <- "Generation Scotland"
#
FEMA.1b.EA <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-03-07_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.xlsx"))
FEMA.1b.EA$Biobank <- "FE meta-analysis"

# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits: "Atrial Fibrillation", "Colorectal Cancer",
FEMA.1b.EA <- FEMA.1b.EA[-which(FEMA.1b.EA$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]

# read in model 3 - PGS stratified by EA/OCC
FGR11.3.EA <- fread("output/2classEA/FG11/CoxPropHaz_model3/2025-02-07_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model3b_FinnGenR11.txt", data.table=FALSE)
FGR11.3.OCC <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model3/2025-02-12_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model3b_FinnGenR11.txt", data.table=FALSE)


################################################################################
#
# Rename "OccupationUpper-level" because this crashes R
#
################################################################################

names(FGR11.1a.OCC) <- gsub("-", "", names(FGR11.1a.OCC))
names(FGR11.1b.OCC) <- gsub("-", "", names(FGR11.1b.OCC))


################################################################################
#
# Reorganize results for plotting
#
################################################################################

# model 1: EA 
model1 <- data.frame(trait = c(FGR11.1a.EA$trait,FGR11.1b.EA$trait),
                     HR = c(FGR11.1a.EA$EAhigh_HR,FGR11.1b.EA$PRS_HR),
                     lb = c(FGR11.1a.EA$EAhigh_HR_lower95,FGR11.1b.EA$PRS_HR_lower95),
                     ub = c(FGR11.1a.EA$EAhigh_HR_upper95,FGR11.1b.EA$PRS_HR_upper95),
                     beta = c(FGR11.1a.EA$EAhigh_beta,FGR11.1b.EA$PRS_beta),
                     se = c(FGR11.1a.EA$EAhigh_se,FGR11.1b.EA$PRS_se),
                     Test = c(rep("High Education",nrow(FGR11.1a.EA)),rep("PRS",nrow(FGR11.1b.EA))))
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
                                  "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                  "Major Depression","Any Cancer","Epilepsy",
                                  "Appendicitis","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model1$trait <- fct_rev(model1$trait)
# EA levels as factor to plot them in order of magnitude
model1$Test <- factor(model1$Test, levels = c("High Education","PRS"), 
                      labels = c("High Education","PRS"))

# model 1: OCC 
model2 <- data.frame(trait = c(FGR11.1a.OCC$trait,FGR11.1b.OCC$trait),
                     HR = c(FGR11.1a.OCC$OccupationUpperlevel_HR,FGR11.1b.OCC$PRS_HR),
                     lb = c(FGR11.1a.OCC$OccupationUpperlevel_HR_lower95,FGR11.1b.OCC$PRS_HR_lower95),
                     ub = c(FGR11.1a.OCC$OccupationUpperlevel_HR_upper95,FGR11.1b.OCC$PRS_HR_upper95),
                     beta = c(FGR11.1a.OCC$OccupationUpperlevel_beta,FGR11.1b.OCC$PRS_beta),
                     se = c(FGR11.1a.OCC$OccupationUpperlevel_se,FGR11.1b.OCC$PRS_se),
                     Test = c(rep("Upper level occupation",nrow(FGR11.1a.OCC)),
                              rep("PRS",nrow(FGR11.1b.OCC))))
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
                                  "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                  "Major Depression","Any Cancer","Epilepsy",
                                  "Appendicitis","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model2$trait <- fct_rev(model2$trait)
# OCC levels as factor to plot them in order of magnitude
model2$Test <- factor(model2$Test, levels = c("Upper level occupation","PRS"), 
                      labels = c("Upper-level occupation","PRS"))

# combine data frames model 1 and 2 for plotting 
df12 <- rbind(model1,model2) #row bind
df12$Model <- c(rep(c("model1", "model2"),each=nrow(model1)))

# model 3: EA
model3 <- data.frame(trait = c(FGR11.3.EA$trait),
                     HR = c(FGR11.3.EA$PRS_HR),
                     lb = c(FGR11.3.EA$PRS_HR_lower95),
                     ub = c(FGR11.3.EA$PRS_HR_upper95),
                     beta = c(FGR11.3.EA$PRS_beta),
                     se = c(FGR11.3.EA$PRS_se),
                     Test = c(rep("PRS in low EA",0.5*nrow(FGR11.3.EA)), rep("PRS in high EA",0.5*nrow(FGR11.3.EA))))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model3$trait <- factor(model3$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                "K11_APPENDACUT","AUD_SWEDISH"),
                       labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                                  "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                  "Major Depression","Any Cancer","Epilepsy",
                                  "Appendicitis","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model3$trait <- fct_rev(model3$trait)
# EA levels as factor to plot them in order of magnitude - adjust labels to
# simple for further data wrangling down the line
model3$Test <- factor(model3$Test, levels = c("PRS in low EA","PRS in high EA"), 
                      labels = c("LowEA","HighEA"))

# model 3: OCC
model4 <- data.frame(trait = c(FGR11.3.OCC$trait),
                     HR = c(FGR11.3.OCC$PRS_HR),
                     lb = c(FGR11.3.OCC$PRS_HR_lower95),
                     ub = c(FGR11.3.OCC$PRS_HR_upper95),
                     beta = c(FGR11.3.OCC$PRS_beta),
                     se = c(FGR11.3.OCC$PRS_se),
                     Test = c(rep("PRS in lower level",0.5*nrow(FGR11.3.OCC)), rep("PRS in upper level",0.5*nrow(FGR11.3.OCC))))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4$trait <- factor(model4$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                "K11_APPENDACUT","AUD_SWEDISH"),
                       labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                                  "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                  "Major Depression","Any Cancer","Epilepsy",
                                  "Appendicitis","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model4$trait <- fct_rev(model4$trait)
# EA levels as factor to plot them in order of magnitude - adjust labels to
# simple for further data wrangling down the line
model4$Test <- factor(model4$Test, levels = c("PRS in lower level","PRS in upper level"), 
                      labels = c("Lower","Upper"))

# combine data frames model 3 and 4 for plotting 
df34 <- rbind(model3,model4) #row bind
df34$Model <- c(rep(c("model1", "model2"),each=nrow(model3)))


# model 1 EA all cohorts to determine plotting order
model1.EA <- data.frame(trait = c(FGR11.1b.EA$trait,UKB.1b.EA$trait,GS.1b.EA$trait,
                                  FEMA.1b.EA$Phenotype),
                        HR = c(FGR11.1b.EA$PRS_HR,UKB.1b.EA$PRS_HR,GS.1b.EA$PRS_HR,
                               FEMA.1b.EA$HR),
                        lb = c(FGR11.1b.EA$PRS_HR_lower95,UKB.1b.EA$PRS_HR_lower95,
                               GS.1b.EA$PRS_HR_lower95,FEMA.1b.EA$Cineg),
                        ub = c(FGR11.1b.EA$PRS_HR_upper95,UKB.1b.EA$PRS_HR_upper95,
                               GS.1b.EA$PRS_HR_upper95,FEMA.1b.EA$Cipos),
                        Biobanks = c(FGR11.1b.EA$Biobank,UKB.1b.EA$Biobank,GS.1b.EA$Biobank,
                                     FEMA.1b.EA$Biobank))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model1.EA$trait <- factor(model1.EA$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                      "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                      "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                      "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                      "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                      "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                      "K11_APPENDACUT","AUD_SWEDISH"),
                          labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                     "Gout","Rheumatoid Arthritis","Breast Cancer",
                                     "Atrial Fibrillation","Colorectal Cancer","Asthma",
                                     "Coronary Heart Disease","Hip Osteoarthritis",
                                     "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                     "Major Depression","Any Cancer","Epilepsy",
                                     "Appendicitis","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model1.EA$trait <- fct_rev(model1.EA$trait)
# Biobank as factor
model1.EA$Biobanks <- factor(model1.EA$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                             labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

################################################################################
#
# Calculate significance differences between EA and OCC model 1a/b
#
################################################################################

# split by Test for calculation
df12.SES <- df12[which(df12$Test=="High Education" | df12$Test=="Upper-level occupation"),]
df12.PRS <- df12[which(df12$Test=="PRS"),]

# calculate difference in the education and PGS effect between model 1 and 2 for each
# trait to inform ordering of the traits in the plot
# arrange dataframe by trait and Test group (education vs PGS)
plot12.df.SES <- df12.SES %>%
  arrange(trait, Model) 
plot12.df.PRS <- df12.PRS %>%
  arrange(trait, Model) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot12_grouped_SES <- plot12.df.SES %>%
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
plot12_grouped_PRS <- plot12.df.PRS %>%
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
plot12_diff_SES <- plot12_grouped_SES %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot12_diff_PRS <- plot12_grouped_PRS %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot12_diff_SES <- plot12_diff_SES %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot12_diff_PRS <- plot12_diff_PRS %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot12_diff_SES <- plot12_diff_SES %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot12_diff_PRS <- plot12_diff_PRS %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))

# combine OCC and PRS results
plot12_diff_SES$Test <- "SES"
plot12_diff_PRS$Test <- "PRS"
plot12_diff <- rbind(plot12_diff_SES,plot12_diff_PRS)

# # write to file
# fwrite(plot12_diff, paste("output/", as.character(Sys.Date()), 
#                           "_INTERVENE_FinnGenR11_EAvsOccupation_Differences.csv",sep=""))


################################################################################
#
# Calculate significance differences between EA and OCC model 3
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
# fwrite(plot34_diff, paste("output/", as.character(Sys.Date()),
#                           "_INTERVENE_FinnGenR11_EAvsOccupation_Differences_model3.csv",sep=""))


################################################################################
#
# Adjust dataframes to create figures
#
################################################################################

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB estimates
plot1a.dat <- model1.EA[-which(model1.EA$Biobanks=="UK Biobank" | model1.EA$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot1a.dat <- plot1a.dat[-which(plot1a.dat$trait[which(plot1a.dat$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]

# create dataframe to plot the background rectangles for Fig1, adapted from Kira Detrois
# [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
endpt_order <- filter(plot1a.dat) %>%
  arrange(HR) %>% 
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))

# reorder trait by endpoint order
plot12_diff$trait <- factor(plot12_diff$trait, levels = endpt_order$trait)
plot34_diff$trait <- factor(plot34_diff$trait, levels = endpt_order$trait)

# create dataframe to plot the background rectangles for Fig2, adapted from Kira Detrois
# [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
endpt_order_12 <- filter(plot12_diff) %>%
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))
#
endpt_order_34 <- filter(plot34_diff) %>%
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))

# merge to HRs
plot12_diff <- merge(plot12_diff,endpt_order_12,by = "trait")
plot34_diff <- merge(plot34_diff,endpt_order_34,by = "trait")


################################################################################
#
# Create "Main" Figure 1A+B (compare model 1a/b)
#
################################################################################

## Figure 1 A ## 

# scatterplot of the unadjusted HRs for EA vs. the HRs for EA adjusted for the PGS
Fig1A <- ggplot(plot12_diff[which(plot12_diff$Test=="SES"),], 
                  aes(x = Model1_HR, y = Model2_HR, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=1, linetype=1, size=line_size, colour = "grey") +
  geom_vline(xintercept=1, linetype=1, size=line_size, colour = "grey") +
  geom_abline(intercept=0, slope=1, linetype=6, size=line_size) +
  geom_hline(yintercept=0.5, linetype=1, size=line_size) +
  geom_vline(xintercept=0.5, linetype=1, size=line_size) +
  # Confidence intervals
  geom_linerange(aes(xmin = Model1_HR_lb, xmax = Model1_HR_ub), size=line_size) +
  geom_linerange(aes(ymin = Model2_HR_lb, ymax = Model2_HR_ub), size=line_size) +
  # Data
  geom_point(size=point_size) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2.0),
                     limits = c(0.5, 2.0),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2.0),
                     limits = c(0.5, 2.0),
                     expand = c(0,0)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-10, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))

## Figure 1 B ## 

# scatterplot of the unadjusted HRs for EA vs. the HRs for EA adjusted for the PGS
Fig1B <- ggplot(plot12_diff[which(plot12_diff$Test=="PRS"),], 
                aes(x = Model1_HR, y = Model2_HR, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=1, linetype=1, size=line_size) +
  geom_vline(xintercept=1, linetype=1, size=line_size) +
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
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1),
                     limits = c(1.0, 2.15),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1),
                     limits = c(1.0, 2.15),
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
# Create "Main" Figure 1C+D (compare model 3)
#
################################################################################

## Figure 1 C ## 

# low EA vs lower-level OCC
Fig1C <- ggplot(plot34_diff[which(plot34_diff$Test=="low"),], 
                aes(x = Model1_HR, y = Model2_HR, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=1, linetype=1, size=line_size) +
  geom_vline(xintercept=1, linetype=1, size=line_size) +
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
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1),
                     limits = c(1.0, 2.1),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1),
                     limits = c(1.0, 2.1),
                     expand = c(0,0)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-10, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))

## Figure 1 D ## 

#high EA vs higher-level occupation
Fig1D <- ggplot(plot34_diff[which(plot34_diff$Test=="high"),], 
                aes(x = Model1_HR, y = Model2_HR, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=0.95, linetype=1, size=line_size) +
  geom_vline(xintercept=0.95, linetype=1, size=line_size) +
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
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2.0,2.2,2.4,2.6,2.8),
                     limits = c(0.95, 2.8),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2.0,2.2,2.4,2.6,2.8),
                     limits = c(0.95, 2.8),
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
# Create "Main" Figure as panel figure comprising 1A-1D
#
################################################################################

# combine plots with empty space
top_plt <- plot_grid(Fig1A,NULL,Fig1B, 
                     NULL,NULL,NULL,
                     Fig1C,NULL,Fig1D,
                     nrow=3, align="h", 
                     rel_widths=c(0.5, 0.02, 0.5),
                     rel_heights = c(0.49,0.01,0.49))
# add additional empty space for legends
Fig12 <- plot_grid(NULL,top_plt,NULL, nrow=3, rel_heights=c(0.01,0.89, 0.10))

# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                      "_INTERVENE_Occupation_eFigure12.png"),
    width = 20, height = 22,units='in',res=600)
Fig12
dev.off()

