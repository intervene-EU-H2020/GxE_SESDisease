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
# Script: Create manuscript figures - Main Figure 4: prediction (AUC) results
#
# Data:  Data: FGR11 (20%) + UKB predictive results (AUC)
#
# Last edits: 01/09/2025 (FAH, edits: final checks and minor tweaks prior to
# upload to GitHub)
# 
################################################################################

################################################################################
#
# Set up script
#
################################################################################

# set working directory 
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
# packages function as specified above): data.table = package for efficiently
# reading in large data sets; ggplot2 = versatile visualizations; viridis =
# color-blind friendly colors; dplyr, forcats, stringr & tidyr = data wrangling;
# cowplot + grid + gridExtra + patchwork = combining plots; googledrive +
# googlesheets4 = read/write to/from GoogleDrive; readxl = read excel files
# (upload to googledrive converts csv to xlsx).
packages("data.table","ggplot2","viridis","dplyr","forcats","stringr","tidyr",
         "cowplot","grid","gridExtra","patchwork","RColorBrewer","googledrive",
         "googlesheets4","readxl")

# run GoogleDrive - set to FALSE if latest results have already been downloaded!
run_googledrive<-FALSE

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

################################################################################
#
# Read in AUCs for plotting + HRs from model 1 to ensure plotting of traits is
# the same across all plots
#
################################################################################

# read in AUCs FinnGen (20%) - glm models: birth year
FGR11.glm.1a2 <- fread("output/Prediction/FinnGen/2025-06-16_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparison_Model1a-2_by.txt",data.table=FALSE)
FGR11.glm.1a2$Biobank <- "FinnGen"
#
FGR11.glm.1b2 <- fread("output/Prediction/FinnGen/2025-06-13_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparison_Model1b-2_by.txt",data.table=FALSE)
FGR11.glm.1b2$Biobank <- "FinnGen"
#
FGR11.glm.24 <- fread("output/Prediction/FinnGen/2025-06-13_FinnGen_20percent_INTERVENE_EducationalAttainment_AUCcomparison_Model2-4_by.txt",data.table=FALSE)
FGR11.glm.24$Biobank <- "FinnGen"

# read in AUCs UKB - glm models: birth year
UKB.glm.1a2 <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model1a-2_by.txt",data.table=FALSE)
UKB.glm.1a2$Biobank <- "UKBiobank"
#
UKB.glm.1b2 <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model1b-2_by.txt",data.table=FALSE)
UKB.glm.1b2$Biobank <- "UKBiobank"
#
UKB.glm.24 <- fread("output/Prediction/UKB/2025-06-13_UKBiobank_EUR_INTERVENE_EducationalAttainment_AUCcomparison_Model2-4_by.txt",data.table=FALSE)
UKB.glm.24$Biobank <- "UKBiobank"

# download latest version of files to local machine
if (run_googledrive==TRUE) {
  #identify folder
  folder_id = drive_get(as_id("18iI9QxxJ7WXrXO6hcNal_amvGmR1F_jv")) # this ID links to the "INTERVENE flagship/GxE_SESDisease/Output_CoxPHmodels_perBB" folder. ID obtained with drive_find(pattern = "Output_Cox")
  
  #find files in folder
  files = drive_ls(folder_id)
  
  #loop dirs and download files inside them
  for (i in seq_along(files$name)) {
    #list files
    i_dir = drive_ls(files[i, ])
    
    #mkdir
    try({dir.create(paste0("output/GoogleDrive/",files$name[i]))})
    
    #download files
    for (file_i in seq_along(i_dir$name)) {
      #fails if already exists
      try({
        drive_download(
          as_id(i_dir$id[file_i]),
          path = paste0("output/GoogleDrive/",files$name[i], "/",i_dir$name[file_i])
        )
      })
    }
  }
}

# read in model 1b - PRS only
FGR11.1b <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt", data.table=FALSE)
FGR11.1b$Biobank <- "FinnGen"
#
UKB.1b <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
UKB.1b$Biobank <- "UK Biobank"
#
GS.1b <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt",data.table=FALSE)
GS.1b$Biobank <- "Generation Scotland"

# Extract meta-analysis results from Google Drive
if (run_googledrive == TRUE) {
  
  # Identify folder
  folder_id <- drive_get(as_id("1Wi0KDwGtnZoUclUgZwu7F_Dwj-6uvJYH")) # this ID links to the "INTERVENE flagship/GxE_SESDisease/Output_CoxPHmodels_meta-analysis" folder. ID obtained with drive_find(pattern = "Output_Cox")
  
  # List files in the folder (no subfolders expected)
  files <- drive_ls(folder_id)
  
  # Create local directory for downloads if it doesn't exist
  dir.create("output/GoogleDrive/MetaAnalysis", showWarnings = FALSE, recursive = TRUE)
  
  # Download each file in the folder
  for (file_i in seq_along(files$name)) {
    try({
      drive_download(
        as_id(files$id[file_i]),
        path = paste0("output/GoogleDrive/MetaAnalysis/", files$name[file_i]),
        overwrite = FALSE
      )
    }, silent = TRUE)
  }
}

# read in model 1b - meta-analysis
FEMA.1b <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.xlsx"))
FEMA.1b$Biobank <- "FE meta-analysis"


################################################################################
#
# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits: "Atrial Fibrillation",
# "Colorectal Cancer"
#
################################################################################

FEMA.1b <- FEMA.1b[-which(FEMA.1b$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results HR model to guide plotting
#
################################################################################

# model 1 
model1 <- data.frame(trait = c(FGR11.1b$trait,UKB.1b$trait,GS.1b$trait,
                               FEMA.1b$Phenotype),
                     HR = c(FGR11.1b$PRS_HR,UKB.1b$PRS_HR,GS.1b$PRS_HR,FEMA.1b$HR),
                     lb = c(FGR11.1b$PRS_HR_lower95,UKB.1b$PRS_HR_lower95,
                            GS.1b$PRS_HR_lower95,FEMA.1b$Cineg),
                     ub = c(FGR11.1b$PRS_HR_upper95,UKB.1b$PRS_HR_upper95,
                            GS.1b$PRS_HR_upper95,FEMA.1b$Cipos),
                     Biobanks = c(FGR11.1b$Biobank,UKB.1b$Biobank,GS.1b$Biobank,
                                  FEMA.1b$Biobank))
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
# Biobank as factor
model1$Biobanks <- factor(model1$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))

## meta-analysis when available and FGR11 if meta-analysis not available ## 
# remove UK estimates
plot1a.dat <- model1[-which(model1$Biobanks=="UK Biobank" | model1$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot1a.dat <- plot1a.dat[-which(plot1a.dat$trait[which(plot1a.dat$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]


################################################################################
#
# In prediction files: adjust disease names for table
#
################################################################################

# function to set labels for traits
set_trait_labels <- function(df, trait_col = "trait") {
  levels_vec <- c("T1D","C3_PROSTATE","T2D","GOUT",
                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                  "K11_APPENDACUT","AUD_SWEDISH")
  
  labels_vec <- c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                  "Atrial Fibrillation","Colorectal Cancer","Asthma",
                  "Coronary Heart Disease","Hip Osteoarthritis",
                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                  "Major Depression","Any Cancer","Epilepsy",
                  "Appendicitis","Alcohol Use Disorder")
  
  df[[trait_col]] <- factor(df[[trait_col]], levels = levels_vec, labels = labels_vec)
  return(df)
}

# adjust labels diseases - FinnGen
FGR11.glm.1a2 <- set_trait_labels(FGR11.glm.1a2)
FGR11.glm.1b2 <- set_trait_labels(FGR11.glm.1b2)
FGR11.glm.24 <- set_trait_labels(FGR11.glm.24)

# adjust labels diseases - UK Biobank
UKB.glm.1a2 <- set_trait_labels(UKB.glm.1a2)
UKB.glm.1b2 <- set_trait_labels(UKB.glm.1b2)
UKB.glm.24 <- set_trait_labels(UKB.glm.24)


################################################################################
#
# Adjust column names model 2 vs 4 comparison for combined table
#
################################################################################

# change column names so function to reorder and combine will work for the model
# 2 vs 4 comparison also (and not just for the model 1a/b vs 2)
names(FGR11.glm.24) <- names(FGR11.glm.1a2)
names(UKB.glm.24) <- names(UKB.glm.1a2)


################################################################################
#
# Combine results across prediction model for each biobank for plotting
#
################################################################################

# FinnGen 
FinnGen <- data.frame(trait = c(FGR11.glm.1a2$trait,FGR11.glm.1b2$trait,
                                rep(FGR11.glm.24$trait,2)),
                     AUC = c(FGR11.glm.1a2$AUC_model1,FGR11.glm.1b2$AUC_model1,
                             FGR11.glm.24$AUC_model1,FGR11.glm.24$AUC_model2),
                     lb = c(FGR11.glm.1a2$CI_model1_lower,FGR11.glm.1b2$CI_model1_lower,
                            FGR11.glm.24$CI_model1_lower,FGR11.glm.24$CI_model2_lower),
                     ub = c(FGR11.glm.1a2$CI_model1_upper,FGR11.glm.1b2$CI_model1_upper,
                            FGR11.glm.24$CI_model1_upper,FGR11.glm.24$CI_model2_upper),
                     Model = c(rep("Education",nrow(FGR11.glm.1a2)),
                               rep("PGS",nrow(FGR11.glm.1b2)),
                               rep("PGS+Education",nrow(FGR11.glm.24)),
                               rep("PGS+Education+Interaction",nrow(FGR11.glm.24))))
# Model as factor
FinnGen$Model <- factor(FinnGen$Model, levels = c("PGS","Education","PGS+Education","PGS+Education+Interaction"), 
                          labels = c("PGS","Education","PGS+Education","PGS+Education+Interaction"))

# UKB 
UKB <- data.frame(trait = c(UKB.glm.1a2$trait,UKB.glm.1b2$trait,
                                rep(UKB.glm.24$trait,2)),
                      AUC = c(UKB.glm.1a2$AUC_model1,UKB.glm.1b2$AUC_model1,
                              UKB.glm.24$AUC_model1,UKB.glm.24$AUC_model2),
                      lb = c(UKB.glm.1a2$CI_model1_lower,UKB.glm.1b2$CI_model1_lower,
                             UKB.glm.24$CI_model1_lower,UKB.glm.24$CI_model2_lower),
                      ub = c(UKB.glm.1a2$CI_model1_upper,UKB.glm.1b2$CI_model1_upper,
                             UKB.glm.24$CI_model1_upper,UKB.glm.24$CI_model2_upper),
                      Model = c(rep("Education",nrow(UKB.glm.1a2)),
                                rep("PGS",nrow(UKB.glm.1b2)),
                                rep("PGS+Education",nrow(UKB.glm.24)),
                                rep("PGS+Education+Interaction",nrow(UKB.glm.24))))
# Model as factor
UKB$Model <- factor(UKB$Model, levels = c("PGS","Education","PGS+Education","PGS+Education+Interaction"), 
                        labels = c("PGS","Education","PGS+Education","PGS+Education+Interaction"))

# create dataframe to plot the background rectangles, adapted from Kira Detrois
# [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
endpt_order <- filter(plot1a.dat) %>%
  arrange(HR) %>% 
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))

# reorder trait by endpoint order
FinnGen$trait <- factor(FinnGen$trait, levels = endpt_order$trait)
UKB$trait <- factor(UKB$trait, levels = endpt_order$trait)

# create dataframe to plot the background rectangles for Fig4, adapted from Kira Detrois
# [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
# FinnGen
endpt_order <- filter(FinnGen) %>%
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))
# merge to AUCs
FinnGen <- merge(FinnGen,endpt_order,by = "trait")

# UKB
endpt_order <- filter(UKB) %>%
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))
# merge to AUCs
UKB <- merge(UKB,endpt_order,by = "trait")


################################################################################
#
# Create Main Figure 4 A + B (i.e., FinnGen and UKB)
#
################################################################################

## Figure 4 A (FinnGen) ## 

# plot AUCs in ggplot2 with geom_point + geom_linerange.
Fig4A <- ggplot(FinnGen, aes(x = AUC, y = trait, color = Model)) + 
  # Extra Lines
  geom_vline(xintercept=0.5, linetype=1, size=axis_line_size, color="grey") +
  # Confidence intervals
  geom_linerange(aes(xmin = lb, xmax = ub), position=position_dodge(1), size=line_size) +
  # Scale manual
  scale_fill_manual(values = c("white", "grey75"), name="", guide="none") +
  scale_size_continuous(range = c(point_size/2, point_size), guide="none") +
  scale_colour_manual(values= c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF"), 
                      labels = c("PGS","Education","PGS+Education","PGS+Education+Interaction")) +
  # Axis limits
  scale_x_continuous(breaks = c(0.5,0.6,0.7,0.8,0.9,1.0),
                     limits = c(0.45,1),
                     expand = c(0,0)) +
  # Stripes
  geom_rect(aes(ymax = CRNT_ENDPT_ORDER+0.5,
                ymin = CRNT_ENDPT_ORDER-0.5,
                xmin = 0.45, xmax = 1, fill=stripe), color=NA, alpha = 0.1) +
  # Data points
  geom_point(aes(size=18.5), shape=18, position=position_dodge(1)) +
  # Theme
  labs(y="", x="Area Under the Curve (95% CI)", title="", fill=NULL, subtitle="") +
  guides(colour=guide_legend(override.aes = list(shape=18, size=7))) +
  theme_hrs(base_size=base_size, plot_top_margin=-20) +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_medium, family=font),
        axis.title=element_text(size=size_medium, family=font))

## Figure 4 B (UKB) ## 

# plot AUCs in ggplot2 with geom_point + geom_linerange.
Fig4B <- ggplot(UKB, aes(x = AUC, y = trait, color = Model)) + 
  # Extra Lines
  geom_vline(xintercept=0.5, linetype=1, size=axis_line_size, color="grey") +
  # Confidence intervals
  geom_linerange(aes(xmin = lb, xmax = ub), position=position_dodge(1), size=line_size) +
  # Scale manual
  scale_fill_manual(values = c("white", "grey75"), name="", guide="none") +
  scale_size_continuous(range = c(point_size/2, point_size), guide="none") +
  scale_colour_manual(values= c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF"), 
                      labels = c("PGS","Education","PGS+Education","PGS+Education+Interaction")) +
  # Axis limits
  scale_x_continuous(breaks = c(0.5,0.6,0.7,0.8,0.9,1.0),
                     limits = c(0.45,1),
                     expand = c(0,0)) +
  # Stripes
  geom_rect(aes(ymax = CRNT_ENDPT_ORDER+0.5,
                ymin = CRNT_ENDPT_ORDER-0.5,
                xmin = 0.45, xmax = 1, fill=stripe), color=NA, alpha = 0.1) +
  # Data points
  geom_point(aes(size=18.5), shape=18, position=position_dodge(1)) +
  # Theme
  labs(y="", x="Area Under the Curve (95% CI)", title="", fill=NULL, subtitle="") +
  guides(colour=guide_legend(override.aes = list(shape=18, size=7))) +
  theme_hrs(base_size=base_size, plot_top_margin=-20) +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_medium, family=font),
        axis.title=element_text(size=size_medium, family=font))
  

################################################################################
#
# Create Main Figure 4 as panel figure comprising 4A-B
#
################################################################################

# combine plot (with patchwork)
Fig4 <- Fig4A + Fig4B + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                       "_INTERVENE_SESDiffDiseases_MainFigure4.png"),
     width = 22, height = 12,units='in',res=600)
Fig4
dev.off()

## Please note that the figure was finalized in InkScape! ##