#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# education [EA]) in risk of 18 common diseases (as previously
# selected in the INTERVENE flagship manuscript:
# https://doi.org/10.1101/2023.06.12.23291186) and alcohol use disorder
#
# Authors: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Create Supplementary Figure for lifetime risk of PGS (Model 6 (PGS as
# factor: <20%, 20-40%, 40-60% (=reference group), 60-95%, >95%) stratified by
# EA for Finngen (FGR11)))
#
# Data: Bootstrapped lifetime risk estimates for education stratified PGSs for
# all diseases except AF + T2D (in Main Figure 3)
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

# theme functions from Kira Detrois [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]. 
# Added dashed y grid.
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
      axis.title=ggplot2::element_text(size=base_size*0.5),
      axis.text = ggplot2::element_text(size=base_size*0.75,margin = margin(b=0)),
      axis.title.x = ggplot2::element_text(margin=margin(t=5, b=axis_x_bottom_margin)),
      axis.title.y = ggplot2::element_text(margin=margin(r=10), l=axis_y_left_margin, angle=90),
      # Other settings
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(colour=NA, fill=NA),
      # Grid settings
      panel.grid.minor.y = ggplot2::element_line(colour="grey", linewidth=line_size-0.75*line_size, linetype=2),
      panel.grid.major.y = ggplot2::element_line(colour="grey", linewidth=line_size-0.5*line_size, linetype=2),
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
      strip.text = ggplot2::element_text(hjust=0, face="bold", size=base_size*0.6, margin=margin(b=5)),
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
# Read in bootstrapped lifetime risk estimates 
#
################################################################################

# bootstrapped lifetime risk estimates - low education
ASTHMAlow <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-26_J10_ASTHMA_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
CHDlow <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-26_I9_CHD_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
Hiplow <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_COX_ARTHROSIS_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
Kneelow <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_KNEE_ARTHROSIS_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
Cancerlow <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_CANCER_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
Prostatelow <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_PROSTATE_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
T1Dlow <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_T1D_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)

# bootstrapped lifetime risk estimates - high education
Cancerhigh <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_CANCER_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
T1Dhigh <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_T1D_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
Asthmahigh <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_J10_ASTHMA_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
CHDhigh <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_I9_CHD_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
Hiphigh <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_COX_ARTHROSIS_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
Kneehigh <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_KNEE_ARTHROSIS_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
Prostatehigh <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_C3_PROSTATE_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)

################################################################################
#
# Adjust data prior to modelling  
#
################################################################################

# add variable to indicate whether results are for low vs high education
ASTHMAlow$Education <- "low"
CHDlow$Education <- "low"
Hiplow$Education <- "low"
Kneelow$Education <- "low"
Cancerlow$Education <- "low"
Prostatelow$Education <- "low"
T1Dlow$Education <- "low"
#
Asthmahigh$Education <- "high"
CHDhigh$Education <- "high"
Hiphigh$Education <- "high"
Kneehigh$Education <- "high"
Cancerhigh$Education <- "high"
Prostatehigh$Education <- "high"
T1Dhigh$Education <- "high"

# combine the low and high results 
ASTHMA <- rbind(ASTHMAlow,Asthmahigh)
CHD <- rbind(CHDlow,CHDhigh)
Hip <- rbind(Hiplow,Hiphigh)
Knee <- rbind(Kneelow,Kneehigh)
Cancer <- rbind(Cancerlow,Cancerhigh)
Prostate <- rbind(Prostatelow,Prostatehigh)
T1D <- rbind(T1Dlow,T1Dhigh)

# Education as factor
ASTHMA$Education <- factor(ASTHMA$Education, levels = c("low","high"),
                       labels = c("Low Education","High Education"))
CHD$Education <- factor(CHD$Education, levels = c("low","high"),
                       labels = c("Low Education","High Education"))
Hip$Education <- factor(Hip$Education, levels = c("low","high"),
                        labels = c("Low Education","High Education"))
Knee$Education <- factor(Knee$Education, levels = c("low","high"),
                        labels = c("Low Education","High Education"))
Cancer$Education <- factor(Cancer$Education, levels = c("low","high"),
                        labels = c("Low Education","High Education"))
Prostate$Education <- factor(Prostate$Education, levels = c("low","high"),
                        labels = c("Low Education","High Education"))
T1D$Education <- factor(T1D$Education, levels = c("low","high"),
                        labels = c("Low Education","High Education"))


# remove PGS groups we do not want to plot the lifetime risk estimates for
ASTHMA <- ASTHMA[-which(ASTHMA$Group=="20-40%" | ASTHMA$Group=="60-95%"),]
CHD <- CHD[-which(CHD$Group=="20-40%" | CHD$Group=="60-95%"),]
Hip <- Hip[-which(Hip$Group=="20-40%" | Hip$Group=="60-95%"),]
Knee <- Knee[-which(Knee$Group=="20-40%" | Knee$Group=="60-95%"),]
Cancer <- Cancer[-which(Cancer$Group=="20-40%" | Cancer$Group=="60-95%"),]
Prostate <- Prostate[-which(Prostate$Group=="20-40%" | Prostate$Group=="60-95%"),]
T1D <- T1D[-which(T1D$Group=="20-40%" | T1D$Group=="60-95%"),]

# PGS group as factor
ASTHMA$Group <- factor(ASTHMA$Group, levels = c(">95%","40-60%","<20%"),
                   labels = c(">95%","40-60%","<20%"))
CHD$Group <- factor(CHD$Group, levels = c(">95%","40-60%","<20%"),
                   labels = c(">95%","40-60%","<20%"))
Hip$Group <- factor(Hip$Group, levels = c(">95%","40-60%","<20%"),
                    labels = c(">95%","40-60%","<20%"))
Knee$Group <- factor(Knee$Group, levels = c(">95%","40-60%","<20%"),
                    labels = c(">95%","40-60%","<20%"))
Cancer$Group <- factor(Cancer$Group, levels = c(">95%","40-60%","<20%"),
                    labels = c(">95%","40-60%","<20%"))
Prostate$Group <- factor(Prostate$Group, levels = c(">95%","40-60%","<20%"),
                    labels = c(">95%","40-60%","<20%"))
T1D$Group <- factor(T1D$Group, levels = c(">95%","40-60%","<20%"),
                    labels = c(">95%","40-60%","<20%"))

################################################################################
#
# Create lifetime risk plots where low vs high education are panels  
#
################################################################################

# Asthma
Fig.Asthma <- ggplot(ASTHMA, aes(age, LifetimeRisk, color=Group, group=Group)) +
  # Theme
  labs(x="Age Range", y="Cummulative Risk (%)", title="", fill=NULL, subtitle="") +
  theme_hrs(base_size=base_size, plot_top_margin=-20,legend_pos = "none") +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        strip.text =element_text(size = size_medium,family = font)) +
  # Extra lines
  geom_vline(xintercept=0, linetype=1, size=axis_line_size, color="black") +
  geom_hline(yintercept=0, linetype=1, size=axis_line_size, color="black") +
  # Data points
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin=CIneg, ymax=CIpos, fill=Group), alpha=0.3, colour = NA) +
  facet_wrap(~Education) +
  # Scales
  scale_color_hue(labels = c(">95%", "40-60%","<20%")) +
  scale_fill_hue(labels = c(">95%", "40-60%%", "<20%")) + 
  # Axis limits
  scale_x_continuous(limits = c(0, 80),
                     expand = c(0,0)) +
  # Axis limits
  scale_y_continuous(limits = c(0, 55),
                     expand = c(0,0))


# CHD
Fig.CHD <- ggplot(CHD, aes(age, LifetimeRisk, color=Group, group=Group)) +
  # Theme
  labs(x="Age Range", y="Cummulative Risk (%)", title="", fill=NULL, subtitle="") +
  theme_hrs(base_size=base_size, plot_top_margin=-20,legend_pos = "none") +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        strip.text =element_text(size = size_medium,family = font)) +
  # Extra lines
  geom_vline(xintercept=0, linetype=1, size=axis_line_size, color="black") +
  geom_hline(yintercept=0, linetype=1, size=axis_line_size, color="black") +
  # Data points
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin=CIneg, ymax=CIpos, fill=Group), alpha=0.3, colour = NA) +
  facet_wrap(~Education) +
  # Scales
  scale_color_hue(labels = c(">95%", "40-60%","<20%")) +
  scale_fill_hue(labels = c(">95%", "40-60%%", "<20%")) + 
  # Axis limits
  scale_x_continuous(limits = c(0, 80),
                     expand = c(0,0)) +
  # Axis limits
  scale_y_continuous(limits = c(0, 50),
                     expand = c(0,0))

# Hip OA
Fig.Hip <- ggplot(Hip, aes(age, LifetimeRisk, color=Group, group=Group)) +
  # Theme
  labs(x="Age Range", y="Cummulative Risk (%)", title="", fill=NULL, subtitle="") +
  theme_hrs(base_size=base_size, plot_top_margin=-20,legend_pos = "none") +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        strip.text =element_text(size = size_medium,family = font)) +
  # Extra lines
  geom_vline(xintercept=0, linetype=1, size=axis_line_size, color="black") +
  geom_hline(yintercept=0, linetype=1, size=axis_line_size, color="black") +
  # Data points
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin=CIneg, ymax=CIpos, fill=Group), alpha=0.3, colour = NA) +
  facet_wrap(~Education) +
  # Scales
  scale_color_hue(labels = c(">95%", "40-60%","<20%")) +
  scale_fill_hue(labels = c(">95%", "40-60%%", "<20%")) + 
  # Axis limits
  scale_x_continuous(limits = c(0, 80),
                     expand = c(0,0)) +
  # Axis limits
  scale_y_continuous(limits = c(0, 10),
                     expand = c(0,0))

# Knee OA
Fig.Knee <- ggplot(Knee, aes(age, LifetimeRisk, color=Group, group=Group)) +
  # Theme
  labs(x="Age Range", y="Cummulative Risk (%)", title="", fill=NULL, subtitle="") +
  theme_hrs(base_size=base_size, plot_top_margin=-20,legend_pos = "none") +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        strip.text =element_text(size = size_medium,family = font)) +
  # Extra lines
  geom_vline(xintercept=0, linetype=1, size=axis_line_size, color="black") +
  geom_hline(yintercept=0, linetype=1, size=axis_line_size, color="black") +
  # Data points
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin=CIneg, ymax=CIpos, fill=Group), alpha=0.3, colour = NA) +
  facet_wrap(~Education) +
  # Scales
  scale_color_hue(labels = c(">95%", "40-60%","<20%")) +
  scale_fill_hue(labels = c(">95%", "40-60%%", "<20%")) + 
  # Axis limits
  scale_x_continuous(limits = c(0, 80),
                     expand = c(0,0)) +
  # Axis limits
  scale_y_continuous(limits = c(0, 60),
                     expand = c(0,0))

# Any Cancer
Fig.Cancer <- ggplot(Cancer, aes(age, LifetimeRisk, color=Group, group=Group)) +
  # Theme
  labs(x="Age Range", y="Cummulative Risk (%)", title="", fill=NULL, subtitle="") +
  theme_hrs(base_size=base_size, plot_top_margin=-20,legend_pos = "none") +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        strip.text =element_text(size = size_medium,family = font)) +
  # Extra lines
  geom_vline(xintercept=0, linetype=1, size=axis_line_size, color="black") +
  geom_hline(yintercept=0, linetype=1, size=axis_line_size, color="black") +
  # Data points
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin=CIneg, ymax=CIpos, fill=Group), alpha=0.3, colour = NA) +
  facet_wrap(~Education) +
  # Scales
  scale_color_hue(labels = c(">95%", "40-60%","<20%")) +
  scale_fill_hue(labels = c(">95%", "40-60%%", "<20%")) + 
  # Axis limits
  scale_x_continuous(limits = c(0, 80),
                     expand = c(0,0)) +
  # Axis limits
  scale_y_continuous(limits = c(0, 50),
                     expand = c(0,0))

# Prostate Cancer
Fig.Prostate <- ggplot(Prostate, aes(age, LifetimeRisk, color=Group, group=Group)) +
  # Theme
  labs(x="Age Range", y="Cummulative Risk (%)", title="", fill=NULL, subtitle="") +
  theme_hrs(base_size=base_size, plot_top_margin=-20,legend_pos = "none") +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        strip.text =element_text(size = size_medium,family = font)) +
  # Extra lines
  geom_vline(xintercept=0, linetype=1, size=axis_line_size, color="black") +
  geom_hline(yintercept=0, linetype=1, size=axis_line_size, color="black") +
  # Data points
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin=CIneg, ymax=CIpos, fill=Group), alpha=0.3, colour = NA) +
  facet_wrap(~Education) +
  # Scales
  scale_color_hue(labels = c(">95%", "40-60%","<20%")) +
  scale_fill_hue(labels = c(">95%", "40-60%%", "<20%")) + 
  # Axis limits
  scale_x_continuous(limits = c(0, 80),
                     expand = c(0,0)) +
  # Axis limits
  scale_y_continuous(limits = c(0, 45),
                     expand = c(0,0))

# T1D
Fig.T1D <- ggplot(T1D, aes(age, LifetimeRisk, color=Group, group=Group)) +
  # Theme
  labs(x="Age Range", y="Cummulative Risk (%)", title="", fill=NULL, subtitle="") +
  theme_hrs(base_size=base_size, plot_top_margin=-20,legend_pos = "none") +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        strip.text =element_text(size = size_medium,family = font)) +
  # Extra lines
  geom_vline(xintercept=0, linetype=1, size=axis_line_size, color="black") +
  geom_hline(yintercept=0, linetype=1, size=axis_line_size, color="black") +
  # Data points
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin=CIneg, ymax=CIpos, fill=Group), alpha=0.3, colour = NA) +
  facet_wrap(~Education) +
  # Scales
  scale_color_hue(labels = c(">95%", "40-60%","<20%")) +
  scale_fill_hue(labels = c(">95%", "40-60%%", "<20%")) + 
  # Axis limits
  scale_x_continuous(limits = c(0, 80),
                     expand = c(0,0)) +
  # Axis limits
  scale_y_continuous(limits = c(0, 11),
                     expand = c(0,0))


################################################################################
#
# Create figure comprising all traits (2 traits per row)
#
################################################################################

# combine plots with empty space
top_plt <- plot_grid(Fig.Asthma,Fig.Cancer,
                     Fig.CHD,Fig.Hip,
                     Fig.Knee,Fig.Prostate,
                     Fig.T1D,nrow=4)
# add additional empty space for legends
Fig7 <- plot_grid(top_plt,NULL, nrow=2, rel_heights=c(0.95, 0.05))

# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                      "_INTERVENE_EducationalAttainment_eFigure7.png"),
    width = 22, height = 22,units='in',res=600)
Fig7
dev.off()
#script is finalized in inkscape