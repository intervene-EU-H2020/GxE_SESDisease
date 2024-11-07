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
# required input data: FinnGen data release 11 (FGR11) + UK Biobank (UKB) + Generation Scotland (GS) + meta-analysis model 3 (EA models)
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
# wrangling; cowplot + grid + gridExtra = combining plots.
packages("data.table","ggplot2","viridis","dplyr","forcats","stringr","tidyr",
         "cowplot","grid","gridExtra", "RColorBrewer")

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

# read in model 3
FGR11.3 <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt", data.table=FALSE)
FGR11.3$Biobank <- "FinnGen"
#
UKB.3 <- fread("output/GoogleDrive/UKB/2024-04-04_UKB_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
UKB.3$Biobank <- "UK Biobank"
#
GS.3 <- fread("output/GoogleDrive/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt",data.table=FALSE)
GS.3$Biobank <- "Generation Scotland"
#
FEMA.3 <- fread("output/2classEA/MetaAnalysis/FGR11_UKB_GS/model3/2024-07-08_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_anyN.csv",data.table=FALSE)
FEMA.3$Biobank <- "FE meta-analysis"


################################################################################
#
# As current version of UKB results also includes the traits
# where UKB was in the discovery GWAS, subset those data frames to only include
# the traits where UKB was not in the discovery GWAS: "Type 1
# Diabetes","Prostate Cancer","Gout","Rheumatoid Arthritis","Breast
# Cancer","Epilepsy","Alcohol Use Disorder"
#
################################################################################

UKB.3 <- UKB.3[which(UKB.3$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]


################################################################################
#
# As current version of meta-analysis results also includes the traits where FG
# was the only included cohort, remove those traits: "Atrial Fibrillation", "Colorectal Cancer",
#
################################################################################

FEMA.3 <- FEMA.3[-which(FEMA.3$Phenotype %in% c("C3_COLORECTAL","I9_AF")),]


################################################################################
#
# Combine FGR11 & UKB & meta-analysis results for plotting
#
################################################################################

# model 3 - also add beta and se (needed to determine significanc of difference
# PGS effect between low/high education groups)
model3 <- data.frame(trait = c(rep(FGR11.3$trait,2),rep(UKB.3$trait,2),
                               rep(GS.3$trait,2),FEMA.3$Phenotype),
                     HR = c(FGR11.3$GxlowEA_HR,FGR11.3$GxhighEA_HR,
                            UKB.3$GxlowEA_HR,UKB.3$GxhighEA_HR,
                            GS.3$GxlowEA_HR,GS.3$GxhighEA_HR,FEMA.3$HR),
                     lb = c(FGR11.3$GxlowEA_HR_lower95,FGR11.3$GxhighEA_HR_lower95,
                            UKB.3$GxlowEA_HR_lower95,UKB.3$GxhighEA_HR_lower95,
                            GS.3$GxlowEA_HR_lower95,GS.3$GxhighEA_HR_lower95,
                            FEMA.3$Cineg),
                     ub = c(FGR11.3$GxlowEA_HR_upper95,FGR11.3$GxhighEA_HR_upper95,
                            UKB.3$GxlowEA_HR_upper95,UKB.3$GxhighEA_HR_upper95,
                            GS.3$GxlowEA_HR_upper95,GS.3$GxhighEA_HR_upper95,
                            FEMA.3$Cipos),
                     beta = c(FGR11.3$GxlowEA_beta,FGR11.3$GxhighEA_beta,
                            UKB.3$GxlowEA_beta,UKB.3$GxhighEA_beta,
                            GS.3$GxlowEA_beta,GS.3$GxhighEA_beta,FEMA.3$Beta),
                     se = c(FGR11.3$GxlowEA_se,FGR11.3$GxhighEA_se,
                            UKB.3$GxlowEA_se,UKB.3$GxhighEA_se,
                            GS.3$GxlowEA_se,GS.3$GxhighEA_se,
                            FEMA.3$SE),
                     Test = c(rep("PRS in low EA",nrow(FGR11.3)), rep("PRS in high EA",nrow(FGR11.3)), 
                              rep("PRS in low EA", nrow(UKB.3)), rep("PRS in high EA",nrow(UKB.3)), 
                              rep("PRS in low EA",nrow(GS.3)), rep("PRS in high EA",nrow(GS.3)),
                              rep("PRS in low EA",0.5*nrow(FEMA.3)), rep("PRS in high EA",0.5*nrow(FEMA.3))),
                     Biobanks = c(rep(FGR11.3$Biobank,2),rep(UKB.3$Biobank,2),
                                  rep(GS.3$Biobank,2),FEMA.3$Biobank))
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
model3$Test <- factor(model3$Test, levels = c("PRS in low EA","PRS in high EA"), 
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
plot3.dat <- model3[-which(model3$Biobanks=="UK Biobank" | model3$Biobanks=="Generation Scotland"),]

# remove FGR11 if trait was meta-analyzed 
plot3.dat <- plot3.dat[-which(plot3.dat$trait[which(plot3.dat$Biobanks=="FinnGen")] %in% 
                                c("Type 1 Diabetes","Gout","Asthma","Coronary Heart Disease","Skin Melanoma",
                                  "Lung Cancer","Major Depression","Any Cancer","Epilepsy","Appendicitis",
                                  "Type 2 Diabetes","Rheumatoid Arthritis","Alcohol Use Disorder",
                                  "Prostate Cancer","Breast Cancer","Hip Osteoarthritis",
                                  "Knee Osteoarthritis")),]

# calculate difference in the PGS effect between high and low EA group for each
# trait to inform ordering of the traits in the plot (biggest differences with
# higher PRS effects in low edu group on top, and diseases with biggest
# differences + higher PRS effects in the high edu group at the bottom)
# Load necessary libraries
# arrange dataframe by trait and EA group
plot3.df <- plot3.dat %>%
  arrange(trait, Test)

# Ensure each trait has only two rows (one for each group)
plot3_grouped <- plot3.df %>%
  group_by(trait) %>%
  summarise(
    LowEA = HR[Test == "LowEA"],
    HighEA = HR[Test == "HighEA"],
    lowEAb = beta[Test == "LowEA"],
    highEAb = beta[Test == "HighEA"],
    lowEAse = se[Test == "LowEA"],
    highEAse = se [Test == "HighEA"]
  )

# Calculate the difference in effect size for each trait
plot3_diff <- plot3_grouped %>%
  mutate(difference = LowEA - HighEA,
         Betadiff = lowEAb - highEAb,
         SEDiff = sqrt(lowEAse**2 + highEAse**2))

# Determine whether difference between PGS effect (beta) low and high education
# groups is significant. Code adapted from:
# https://github.com/intervene-EU-H2020/flagship/blob/main/MetaAnalysis/AgeSpecific_MetaAnalysisandHetero.R
# calculate z-value
plot3_diff <- plot3_diff %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot3_diff <- plot3_diff %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# Create a column to guide the order of plotting
plot3_diff <- plot3_diff %>%
  arrange(desc(difference)) %>%
  mutate(order = rank(-difference, ties.method = "first"))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot3_diff <- plot3_diff %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
# manually adjust the order 
plot3_diff$order <- c(1:2,6,3:5,7:14,17,15:16,18:19)

# Merge the order column back with the original dataframe
plot3_ordered <- plot3.dat %>%
  left_join(plot3_diff %>% select(trait, difference, order,PvalDiff), by = "trait")

# add stripe information fo the background rectangles for Fig2 (which is ordered
# by difference between PGS effect in high vs low EA groups), adapted from Kira
# Detrois [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
plot3_ordered <- plot3_ordered %>%
  mutate(stripe = ifelse(order %% 2 == 0, "even", "odd"))



################################################################################
#
# Create Main Figure 2
#
################################################################################

# plot hazard ratios in ggplot2 with geom_point + geom_errorbar. Use color-blind
# friendly colors for the different HR's. Get classic layout and add vertical
# line on x=1 to represent HR=1 (i.e., if HR's cross 1 they're not significant).
# Remove y axis and legend label, and rename x axis label.
Fig2 <- ggplot(plot3_ordered, aes(x = HR, y = reorder(trait,-order), color = Test)) + 
  # Confidence intervals
  geom_linerange(aes(xmin = lb, xmax = ub), position=position_dodge(1), size=line_size) +
  # Scale manual
  scale_fill_manual(values = c("white", "grey75"), name="", guide="none") +
  scale_size_continuous(range = c(point_size/2, point_size), guide="none") +
  scale_colour_manual(values= c("#F0E442",  "#CC79A7"), labels = c("PGS in low education group", "PGS in high education group")) +
  # Axis limits
  scale_x_continuous(breaks = round(seq(min(plot3.dat$lb)-0.05, max(plot3.dat$ub)+0.05, by = 0.1),1)) +
  # Stripes
  geom_rect(aes(ymax = order+0.5,
                ymin = order-0.5,
                xmin = -Inf, xmax = Inf, fill=stripe), color=NA, alpha = 0.1) +
  # Data points
  geom_point(aes(size=18.5), shape=18, position=position_dodge(1)) +
  # Extra Lines
  geom_vline(xintercept=1, linetype=1, size=axis_line_size) +
  # Theme
  labs(y="", x="Meta-analyzed Hazard Ratio (95% CI)", title="", fill=NULL, subtitle="") +
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