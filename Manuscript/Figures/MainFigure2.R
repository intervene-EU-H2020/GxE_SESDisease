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
# Script: Create manuscript figures - Main Figure 2: model 3 (EA)
#
# required input data: FinnGen data release 11 (FGR11) + UK Biobank (UKB) + 
# Generation Scotland (GS) + meta-analysis model 3 (EA models)
#
# Last edits: 07/11/2024 (edits, FAH: final checks and minor tweaks prior to
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
# packages function as specified in the source file): data.table = package for
# efficiently reading in large data sets; ggplot2 = versatile visualizations;
# viridis = color-blind friendly colors; dplyr, forcats, stringr & tidyr = data
# wrangling; cowplot + grid + gridExtra = combining plots; googledrive +
# googlesheets4 = read/write to/from GoogleDrive; readxl = read excel files
# (upload to googledrive converts csv to xlsx).
packages("data.table","ggplot2","viridis","dplyr","forcats","stringr","tidyr",
         "cowplot","grid","gridExtra", "RColorBrewer","googledrive",
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
      panel.grid.major.x = ggplot2::element_line(colour="black", linewidth=line_size-0.5*line_size, linetype=2),
      panel.grid.minor.x = ggplot2::element_line(colour = "black", linewidth=line_size-0.75*line_size, linetype=2),
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

# read in model 3 - per biobank
FGR11.3 <- fread("output/GoogleDrive/FGR11/2025-02-07_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt", data.table=FALSE)
FGR11.3$Biobank <- "FinnGen"
#
UKB.3 <- fread("output/GoogleDrive/UKB/2025-05-22_UKBiobank_EUR_INTERVENE_EducationalAttainment_CoxPH_model3b_Coeffs.txt",data.table=FALSE)
UKB.3$Biobank <- "UK Biobank"
#
GS.3 <- fread("output/GoogleDrive/GS/2025-02-24_GS_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
GS.3$Test <- c(rep("LowEA",length(unique(GS.3$trait))),rep("HighEA",length(unique(GS.3$trait)))) # in initial script shared with GS forgot a line of code to add this, so now add it manually. 
GS.3$Biobank <- "Generation Scotland"

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

# read in model 3 - meta-analysis
FEMA.3 <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3.xlsx"))
FEMA.3$Biobank <- "FE meta-analysis"

# read in model 1b - meta-analysis
FEMA.1b <- as.data.frame(read_excel("output/GoogleDrive/MetaAnalysis/2025-05-22_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b.xlsx"))
FEMA.1b$Biobank <- "FE meta-analysis"


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
# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits: "Atrial Fibrillation", "Colorectal Cancer",
#
################################################################################

FEMA.3 <- FEMA.3[-which(FEMA.3$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]
FEMA.1b <- FEMA.1b[-which(FEMA.1b$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results for plotting
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
                                  "* Atrial Fibrillation","* Colorectal Cancer","Asthma",
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

# model 3 - also add beta and se (needed to determine significanc of difference
# PGS effect between low/high education groups)
model3 <- data.frame(trait = c(FGR11.3$trait,UKB.3$trait,GS.3$trait,
                               FEMA.3$Phenotype),
                     HR = c(FGR11.3$PRS_HR,UKB.3$PRS_HR,GS.3$PRS_HR,FEMA.3$HR),
                     lb = c(FGR11.3$PRS_HR_lower95,UKB.3$PRS_HR_lower95,
                            GS.3$PRS_HR_lower95,FEMA.3$Cineg),
                     ub = c(FGR11.3$PRS_HR_upper95,UKB.3$PRS_HR_upper95,
                            GS.3$PRS_HR_upper95,FEMA.3$Cipos),
                     beta = c(FGR11.3$PRS_beta,UKB.3$PRS_beta,GS.3$PRS_beta,
                              FEMA.3$Beta),
                     se = c(FGR11.3$PRS_se,UKB.3$PRS_se,GS.3$PRS_se,FEMA.3$SE),
                     Test = c(FGR11.3$Test,UKB.3$Test,GS.3$Test,FEMA.3$EA),
                     Biobanks = c(FGR11.3$Biobank,UKB.3$Biobank,
                                  GS.3$Biobank,FEMA.3$Biobank))
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
                                  "* Atrial Fibrillation","* Colorectal Cancer","Asthma",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                  "Major Depression","Any Cancer","Epilepsy",
                                  "Appendicitis","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model3$trait <- fct_rev(model3$trait)
# EA levels as factor to plot them in order of magnitude - adjust labels to
# simple for further data wrangling down the line
model3$Test <- factor(model3$Test, levels = c("LowEA","HighEA"), 
                      labels = c("LowEA","HighEA"))
# Biobank as factor
model3$Biobanks <- factor(model3$Biobanks, levels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"), 
                          labels = c("FinnGen","UK Biobank","Generation Scotland","FE meta-analysis"))


################################################################################
#
# Adjust dataframe to create Main Figure 2
#
################################################################################

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB estimates
plot1a.dat <- model1[-which(model1$Biobanks=="UK Biobank" | model1$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot1a.dat <- plot1a.dat[-which(plot1a.dat$trait[which(plot1a.dat$Biobanks=="FinnGen")] %in% 
                                  c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                    "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                    "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                    "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                    "Knee Osteoarthritis")),]

## plot meta-analysis when available and plot FGR11 if meta-analysis not available ## 
# remove UKB and GS estimates
plot3.dat <- model3[-which(model3$Biobanks=="UK Biobank" | model3$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot3.dat <- plot3.dat[-which(plot3.dat$trait[which(plot3.dat$Biobanks=="FinnGen")] %in% 
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
plot3.dat$trait <- factor(plot3.dat$trait, levels = endpt_order$trait)

# create dataframe to plot the background rectangles for Fig2, adapted from Kira Detrois
# [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
endpt_order <- filter(plot3.dat) %>%
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))

# merge to HRs
plot3.dat <- merge(plot3.dat,endpt_order,by = "trait")


################################################################################
#
# Create Main Figure 2
#
################################################################################

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
Fig2 <- ggplot(plot3.dat, aes(x = HR, y = trait, color = Test)) + 
  # Extra Lines
  geom_vline(xintercept=1, linetype=1, size=axis_line_size, color="grey") +  
  # Confidence intervals
  geom_linerange(aes(xmin = lb, xmax = ub), position=position_dodge(1), size=line_size) +
  # Scale manual
  scale_fill_manual(values = c("white", "grey75"), name="", guide="none") +
  scale_size_continuous(range = c(point_size/2, point_size), guide="none") +
  scale_colour_manual(values= c("#3869AF",  "#BC65DB"), labels = c("PGS in low education group", "PGS in high education group")) +
  # Axis limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.2,2.4,2.5),
                     limits = c(1, 2.5),
                     expand = c(0,0)) +
  # Stripes
  geom_rect(aes(ymax = CRNT_ENDPT_ORDER+0.5,
                ymin = CRNT_ENDPT_ORDER-0.5,
                xmin = 1, xmax = 2.5, fill=stripe), color=NA, alpha = 0.1) +
  # Data points
  geom_point(aes(size=18.5), shape=18, position=position_dodge(1)) +
  # Theme
  labs(y="", x="Meta-analyzed Hazard Ratio per Standard Deviation (95% CI)", title="", fill=NULL, subtitle="") +
  guides(colour=guide_legend(override.aes = list(shape=18, size=7))) +
  theme_hrs(base_size=base_size, plot_top_margin=-20) +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_medium, family=font),
        axis.title=element_text(size=size_medium, family=font))
  
# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                          "_INTERVENE_SESDiffDiseases_MainFigure2.png"),
        width = 11, height = 10,units='in',res=600)
  Fig2
  dev.off()

## Please note that the figure was finalized in InkScape! ##
