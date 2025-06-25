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
# Script: Create Main Figure 3 (Model 6 (PGS as factor: <20%, 20-40%, 40-60%
# (=reference group), 60-95%, >95%) stratified by EA for Finngen (FGR11)));
# lifetime risk estimates for AF (3a) + TD2 (3b)
#
# Data: Bootstrapped lifetime risk estimates for education stratified PGSs for
# T2D and AF
#
# Last edits: 25/06/2025 (FAH, edits: final checks and minor tweaks prior to
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

# bootstrapped lifetime risk estimates 
AFlow <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-26_I9_AF_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
AFhigh <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_I9_AF_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
T2Dlow <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-26_T2D_LifetimeRisk_LowEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)
T2Dhigh <- fread("output/LifetimeRisk/model6/FinnGen/2025-03-27_T2D_LifetimeRisk_HighEducation_Bootstrapped_FinnGen.csv",data.table=FALSE)


################################################################################
#
# Adjust data prior to modelling  
#
################################################################################

# add variable to indicate whether results are for low vs high education
AFlow$Education <- "low"
AFhigh$Education <- "high"
T2Dlow$Education <- "low"
T2Dhigh$Education <- "high"

# combine the low and high results 
AF <- rbind(AFlow,AFhigh)
T2D <- rbind(T2Dlow,T2Dhigh)

# Education as factor
AF$Education <- factor(AF$Education, levels = c("low","high"),
                       labels = c("Low Education","High Education"))
T2D$Education <- factor(T2D$Education, levels = c("low","high"),
                       labels = c("Low Education","High Education"))

# remove PGS groups we do not want to plot the lifetime risk estimates for
AF <- AF[-which(AF$Group=="20-40%" | AF$Group=="60-95%"),]
T2D <- T2D[-which(T2D$Group=="20-40%" | T2D$Group=="60-95%"),]

# PGS group as factor
AF$Group <- factor(AF$Group, levels = c(">95%","40-60%","<20%"),
                   labels = c(">95%","40-60%","<20%"))
T2D$Group <- factor(T2D$Group, levels = c(">95%","40-60%","<20%"),
                   labels = c(">95%","40-60%","<20%"))

################################################################################
#
# Create lifetime risk plots where low vs high education are panels  
#
################################################################################

# Figure 3a: AF
AF.3a <- ggplot(AF, aes(age, LifetimeRisk, color=Group, group=Group)) +
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
  scale_y_continuous(limits = c(0, 22),
                     expand = c(0,0))


# Figure 3b: T2D
T2D.3b <- ggplot(T2D, aes(age, LifetimeRisk, color=Group, group=Group)) +
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
  scale_y_continuous(limits = c(0, 65),
                     expand = c(0,0))

################################################################################
#
# Create Main Figure 3 as panel figure comprising 3A-B
#
################################################################################

# combine plots with empty space
top_plt <- plot_grid(AF.3a,T2D.3b,nrow=2)
# add additional empty space for legends
Fig3 <- plot_grid(top_plt,NULL, nrow=2, rel_heights=c(0.95, 0.05))

# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                      "_INTERVENE_SESDiffDiseases_MainFigure3.png"),
    width = 8, height = 8,units='in',res=600)
Fig3
dev.off()

## Please note that the figure was finalized in InkScape! ##