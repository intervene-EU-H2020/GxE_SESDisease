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
# Script: Compare results for model 4 (Education + PGS + interaction +
# covariates) using Cox Proportional Hazard Model and Fine-Gray model (competing
# risk for non-cause specific death) for those disorders with a significant
# PGS*Education interaction
#
# Data: FGR11 Cox + FG results model 4 (EA + PGS + interaction)
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
# only retain the 7 colors corresponding to the 7 traits included in both the
# Cox and Fine-Gray models
color_list <- c("#841C26","#FBCF9D","#91A3B0","#3C4E2D","#CCB6AF","#7BA05B","#A4D3EE")


################################################################################
#
# Read in full sample hazard ratios per standard deviation
#
################################################################################

# read in model 4 - Cox
FGR11.Cox <- fread("output/2classEA/FG11/CoxPropHaz_model4/2024-03-13_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model4c_FinnGenR11.txt", data.table=FALSE)

# read in model 4 - FG
FGR11.FG <- fread("output/2classEA/FG11/CoxPropHaz_model4/2025-03-14_INTERVENE_SESDiffDiseases_Coeffs_FineGray_model4_FinnGenR11.txt", data.table=FALSE)


################################################################################
#
# Combine Cox and FG results for plotting
#
################################################################################

# from Cox results only retain the disorders also tested in the Fine-Gray model
FGR11.Cox <- FGR11.Cox[which(FGR11.Cox$trait %in% FGR11.FG$trait),]

# Combine results model 4
model4 <- data.frame(trait = c(rep(FGR11.Cox$trait,3),rep(FGR11.FG$trait,3)),
                     HR = c(FGR11.Cox$EAhigh_HR,FGR11.Cox$PRS_HR,FGR11.Cox$`PRS:EAhigh_HR`,
                            FGR11.FG$EAhigh_HR,FGR11.FG$PRS_HR,FGR11.FG$interaction_HR),
                     lb = c(FGR11.Cox$EAhigh_HR_lower95,FGR11.Cox$PRS_HR_lower95,
                            FGR11.Cox$`PRS:EAhigh_HR_lower95`,FGR11.FG$EAhigh_HR_lower,
                            FGR11.FG$PRS_HR_lower,FGR11.FG$interaction_HR_lower),
                     ub = c(FGR11.Cox$EAhigh_HR_upper95,FGR11.Cox$PRS_HR_upper95,
                            FGR11.Cox$`PRS:EAhigh_HR_upper95`,FGR11.FG$EAhigh_HR_upper,
                            FGR11.FG$PRS_HR_upper,FGR11.FG$interaction_HR_upper),
                     beta = c(FGR11.Cox$EAhigh_beta,FGR11.Cox$PRS_beta,FGR11.Cox$`PRS:EAhigh_beta`,
                              FGR11.FG$EAhigh_beta,FGR11.FG$PRS_beta,FGR11.FG$interaction_beta),
                     se = c(FGR11.Cox$EAhigh_se,FGR11.Cox$PRS_se,FGR11.Cox$`PRS:EAhigh_se`,
                            FGR11.FG$EAhigh_se,FGR11.FG$PRS_se,FGR11.FG$interaction_se),
                     Test = c(rep("Education",nrow(FGR11.Cox)), rep("PRS",nrow(FGR11.Cox)),
                              rep("interaction",nrow(FGR11.Cox)), rep("Education",nrow(FGR11.FG)), 
                              rep("PRS",nrow(FGR11.FG)),rep("interaction",nrow(FGR11.FG))),
                     Model = c(rep("Cox",3*nrow(FGR11.Cox)),rep("FineGray",3*nrow(FGR11.FG))))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4$trait <- factor(model4$trait, levels = c("T2D","I9_AF","J10_ASTHMA","I9_CHD",
                                                "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                "C3_CANCER"),
                       labels = c("Type 2 Diabetes","Atrial Fibrillation","Asthma",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Knee Osteoarthritis","Any Cancer"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model4$trait <- fct_rev(model4$trait)
# Test as factor
model4$Test <- factor(model4$Test, levels = c("Education","PRS","interaction"), 
                      labels = c("Education","PGS","Interaction"))
# Model as factor
model4$Model <- factor(model4$Model, levels = c("Cox","FineGray"),
                       labels = c("Cox Proportional Hazard Model",
                                  "Fine-Gray Competing Risk Model"))

# split by Test 
df12.EA <- model4[which(model4$Test=="Education"),]
df12.PRS <- model4[which(model4$Test=="PGS"),]
df12.inter <- model4[which(model4$Test=="Interaction"),]

# arrange dataframes by trait and Test group 
plot12.df.EA <- df12.EA %>%
  arrange(trait, Model) 
plot12.df.PRS <- df12.PRS %>%
  arrange(trait, Model) 
plot12.df.inter <- df12.inter %>%
  arrange(trait, Model) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot12_grouped_EA <- plot12.df.EA %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Model == "Cox Proportional Hazard Model"],
    Model2_HR = HR[Model == "Fine-Gray Competing Risk Model"],
    Model1_HR_lb = lb[Model == "Cox Proportional Hazard Model"],
    Model2_HR_lb = lb[Model == "Fine-Gray Competing Risk Model"],
    Model1_HR_ub = ub[Model == "Cox Proportional Hazard Model"],
    Model2_HR_ub = ub[Model == "Fine-Gray Competing Risk Model"],
    Model1b = beta[Model == "Cox Proportional Hazard Model"],
    Model2b = beta[Model == "Fine-Gray Competing Risk Model"],
    Model1se = se[Model == "Cox Proportional Hazard Model"],
    Model2se = se[Model == "Fine-Gray Competing Risk Model"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )
plot12_grouped_PRS <- plot12.df.PRS %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Model == "Cox Proportional Hazard Model"],
    Model2_HR = HR[Model == "Fine-Gray Competing Risk Model"],
    Model1_HR_lb = lb[Model == "Cox Proportional Hazard Model"],
    Model2_HR_lb = lb[Model == "Fine-Gray Competing Risk Model"],
    Model1_HR_ub = ub[Model == "Cox Proportional Hazard Model"],
    Model2_HR_ub = ub[Model == "Fine-Gray Competing Risk Model"],
    Model1b = beta[Model == "Cox Proportional Hazard Model"],
    Model2b = beta[Model == "Fine-Gray Competing Risk Model"],
    Model1se = se[Model == "Cox Proportional Hazard Model"],
    Model2se = se[Model == "Fine-Gray Competing Risk Model"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )
plot12_grouped_inter <- plot12.df.inter %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Model == "Cox Proportional Hazard Model"],
    Model2_HR = HR[Model == "Fine-Gray Competing Risk Model"],
    Model1_HR_lb = lb[Model == "Cox Proportional Hazard Model"],
    Model2_HR_lb = lb[Model == "Fine-Gray Competing Risk Model"],
    Model1_HR_ub = ub[Model == "Cox Proportional Hazard Model"],
    Model2_HR_ub = ub[Model == "Fine-Gray Competing Risk Model"],
    Model1b = beta[Model == "Cox Proportional Hazard Model"],
    Model2b = beta[Model == "Fine-Gray Competing Risk Model"],
    Model1se = se[Model == "Cox Proportional Hazard Model"],
    Model2se = se[Model == "Fine-Gray Competing Risk Model"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )

# Determine whether difference between PGS effect (beta) low and Upper-level occupation
# groups is significant. Code adapted from:
# https://github.com/intervene-EU-H2020/flagship/blob/main/MetaAnalysis/AgeSpecific_MetaAnalysisandHetero.R
# calculate z-value
plot12_diff_EA <- plot12_grouped_EA %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot12_diff_PRS <- plot12_grouped_PRS %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot12_diff_inter <- plot12_grouped_inter %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot12_diff_EA <- plot12_diff_EA %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot12_diff_PRS <- plot12_diff_PRS %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot12_diff_inter <- plot12_diff_inter %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot12_diff_EA <- plot12_diff_EA %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/7, 1, 0))
plot12_diff_PRS <- plot12_diff_PRS %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/7, 1, 0))
plot12_diff_inter <- plot12_diff_inter %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/7, 1, 0))

# combine results
plot12_diff_EA$Test <- "Education"
plot12_diff_PRS$Test <- "PGS"
plot12_diff_inter$Test <- "Interaction"
plot12_diff <- rbind(plot12_diff_EA,plot12_diff_PRS,plot12_diff_inter)

# write to file
fwrite(plot12_diff, paste("output/2classEA/FG11/CoxPropHaz_model4/", as.character(Sys.Date()),
                          "_INTERVENE_SESDiffDiseases_Differences_CoxvsFG_model4_FinnGenR11.csv",sep=""))


################################################################################
#
# Create Comparison on Cox vs FG 
#
################################################################################

## Figure 1A: Education ## 

# scatterplot of the effect of high education in Cox vs FG
Fig1A <- ggplot(plot12_diff[which(plot12_diff$Test=="Education"),], 
                aes(x = Model1_HR, y = Model2_HR, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=1, linetype=1, size=line_size, colour = "grey") +
  geom_vline(xintercept=1, linetype=1, size=line_size, colour = "grey") +
  geom_abline(intercept=0, slope=1, linetype=6, size=line_size) +
  geom_hline(yintercept=0.6, linetype=1, size=line_size) +
  geom_vline(xintercept=0.6, linetype=1, size=line_size) +
  # Confidence intervals
  geom_linerange(aes(xmin = Model1_HR_lb, xmax = Model1_HR_ub), size=line_size) +
  geom_linerange(aes(ymin = Model2_HR_lb, ymax = Model2_HR_ub), size=line_size) +
  # Data
  geom_point(size=point_size) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(0.6,0.7,0.8,0.9,1.0,1.1,1.2),
                     limits = c(0.6, 1.2),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(0.6,0.7,0.8,0.9,1.0,1.1,1.2),
                     limits = c(0.6, 1.2),
                     expand = c(0,0)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-10, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))

## Figure 1B: PGS ## 

# scatterplot of the effect of the PGSs in Cox vs FG
Fig1B <- ggplot(plot12_diff[which(plot12_diff$Test=="PGS"),], 
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
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7),
                     limits = c(1.0, 1.7),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7),
                     limits = c(1.0, 1.7),
                     expand = c(0,0)) +
  # Theme
  labs(x="", y="", title="", subtitle="")  +
  theme_comp(base_size=base_size, legend_pos="none", plot_top_margin=-10, legend_box_spacing=0) +
  # Font setting
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text=element_text(size=size_small, family=font),
        axis.title=element_text(size=size_medium, family=font),
        plot.subtitle=element_text(size=size_medium, family=font))

## Figure 1C: interaction ## 

# low EA vs lower-level OCC
Fig1C <- ggplot(plot12_diff[which(plot12_diff$Test=="Interaction"),], 
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
                     breaks = c(1.0,1.02,1.04,1.06,1.08,1.1,1.12),
                     limits = c(1.0, 1.13),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.02,1.04,1.06,1.08,1.1,1.12),
                     limits = c(1.0, 1.13),
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
# Create combined Figure as panel figure comprising 1A-1C
#
################################################################################

# combine plots with empty space
top_plt <- plot_grid(Fig1A,NULL,Fig1B, 
                     NULL,NULL,NULL,
                     Fig1C,NULL,NULL,
                     nrow=3, align="h", 
                     rel_widths=c(0.5, 0.02, 0.5),
                     rel_heights = c(0.49,0.01,0.49))
# add additional empty space for legends
Fig8 <- plot_grid(NULL,top_plt,NULL, nrow=3, rel_heights=c(0.01,0.89, 0.10))

  
# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                      "_INTERVENE_EducationalAttainment_eFigure8.png"),
        width = 22, height = 20,units='in',res=600)
  Fig8
  dev.off()
# Figure is finalized in inkscape
