#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# occuption) in risk of 18 common diseases (as previously
# selected in the INTERVENE flagship manuscript:
# https://doi.org/10.1101/2023.06.12.23291186) and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: create figure S10: A: model 1a (occupation), B: model 1b (PGS)
#
# Data: 
#       1) FGR11 + sensitivity model 1a (occupation + EA)
#       2) FGR11 + sensitivity model 1b (PGS + EA)
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
# wrangling; cowplot + grid + gridExtra = combining plots; readxl = read excel
# files (upload to googledrive converts csv to xlsx).
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

# read in model 1a - occupation only
FGR11.1a <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model1/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model1a_FinnGenR11.txt", data.table=FALSE)
FGR11.1a$Biobank <- "FinnGen"

# read in model 1b - PRS only
FGR11.1b <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model1/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model1b_FinnGenR11.txt", data.table=FALSE)
FGR11.1b$Biobank <- "FinnGen"

# read in model 2
FGR11.2 <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model2/2025-01-30_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model2_FinnGenR11.txt", data.table=FALSE)
FGR11.2$Biobank <- "FinnGen"

# read in model 1 for education to ensure the order of plotting is the same
FGR11.1b.EA <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt", data.table=FALSE)
FGR11.1b.EA$Biobank <- "FinnGen"
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


################################################################################
#
# Rename "OccupationUpper-level" because this crashes R
#
################################################################################

names(FGR11.1a) <- gsub("-", "", names(FGR11.1a))
names(FGR11.1b) <- gsub("-", "", names(FGR11.1b))
names(FGR11.2) <- gsub("-", "", names(FGR11.2))


################################################################################
#
# Reorganize results for plotting
#
################################################################################

## Reference = Manual worker + Lower-level admin ##
# model 1 
model1 <- data.frame(trait = c(FGR11.1a$trait,FGR11.1b$trait),
                     HR = c(FGR11.1a$OccupationUpperlevel_HR,FGR11.1b$PRS_HR),
                     lb = c(FGR11.1a$OccupationUpperlevel_HR_lower95,FGR11.1b$PRS_HR_lower95),
                     ub = c(FGR11.1a$OccupationUpperlevel_HR_upper95,FGR11.1b$PRS_HR_upper95),
                     Test = c(rep("Upper level occupation",nrow(FGR11.1a)),rep("PRS",nrow(FGR11.1b))))
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
model1$Test <- factor(model1$Test, levels = c("Upper level occupation","PRS"), labels = c("Upper-level occupation","PRS"))

# model 2
model2 <- data.frame(trait = c(rep(FGR11.2$trait,2)),
                     HR = c(FGR11.2$OccupationUpperlevel_HR,FGR11.2$PRS_HR),
                     lb = c(FGR11.2$OccupationUpperlevel_HR_lower95,FGR11.2$PRS_HR_lower95),
                     ub = c(FGR11.2$OccupationUpperlevel_HR_upper95,FGR11.2$PRS_HR_upper95),
                     Test = c(rep("Upper level occupation",nrow(FGR11.2)), rep("PRS",nrow(FGR11.2))))
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
# EA levels as factor to plot them in order of magnitude
model2$Test <- factor(model2$Test, levels = c("Upper level occupation","PRS"), labels = c("Upper-level occupation","PRS"))

# EUDCATION
# model 1 
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
# Adjust dataframe to create figure A + B
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

# combine data frames model 1 and 2 for plotting 
df12 <- rbind(model1,model2) #row bind
df12$Model <- c(rep(c("model 1", "model2"),each=nrow(model1)))

# reorder trait by endpoint order
df12$trait <- factor(df12$trait, levels = endpt_order$trait)

# create dataframe to plot the background rectangles for Fig2, adapted from Kira Detrois
# [https://github.com/intervene-EU-H2020/onset_prediction/blob/main/analysis/scripts/utils.R]
endpt_order_OCC <- filter(df12) %>%
  select(trait) %>%
  distinct() %>% 
  mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
  mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))

# merge to HRs
df12 <- merge(df12,endpt_order_OCC,by = "trait")


################################################################################
#
# Create Figure A + C
#
################################################################################

## Figure A ## 

# plot hazard ratios in ggplot2 with geom_point + geom_linerange.
Fig1A <- ggplot(df12[which(df12$Test=="PRS" & df12$Model=="model 1"),], aes(x = HR, y = trait, color = trait)) + 
  # Extra Lines
  geom_vline(xintercept=1, linetype=1, size=axis_line_size, color="grey") +
  # Confidence intervals
  geom_linerange(aes(xmin = lb, xmax = ub), position=position_dodge(0.5), size=line_size) +
  # Scale manual
  scale_fill_manual(values = c("white", "grey75"), name="", guide="none") +
  scale_size_continuous(range = c(point_size/2, point_size), guide="none") +
  # Axis limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1),
                     limits = c(1, 2.12),
                     expand = c(0,0)) +
 # Stripes
  geom_rect(aes(ymax = CRNT_ENDPT_ORDER+0.5,
                ymin = CRNT_ENDPT_ORDER-0.5,
                xmin = 1, 
                xmax = 2.12, 
                fill=stripe), color=NA, alpha = 0.1) +
  # Data points
  geom_point(aes(size=20.5), shape=18, position=position_dodge(0.5)) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Theme
  labs(y="", x="", title="", fill=NULL, subtitle="") +
  theme_hrs(base_size=base_size, plot_top_margin=-20,legend_pos = "none") +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_medium, family=font),
        axis.title=element_text(size=size_medium, family=font))

## Figure C ## 


# plot hazard ratios in ggplot2 with geom_point + geom_linerange. 
Fig1C <- ggplot(df12[which(df12$Test=="Upper-level occupation" & df12$Model=="model 1"),], aes(x = HR, y = trait, color = trait)) + 
  # Extra Lines
  geom_vline(xintercept=1, linetype=1, size=axis_line_size, color="grey") +
  # Confidence intervals
  geom_linerange(aes(xmin = lb, xmax = ub), position=position_dodge(0.5), size=line_size) +
  # Scale manual
  scale_fill_manual(values = c("white", "grey75"), name="", guide="none") +
  scale_size_continuous(range = c(point_size/2, point_size), guide="none") +
  # Axis limits
  scale_x_continuous(transform = scales::transform_log(),
                     breaks = c(0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0),
                     limits = c(0.65, 2),
                     expand = c(0,0)) +
  # Stripes
  geom_rect(aes(ymax = CRNT_ENDPT_ORDER+0.5,
                ymin = CRNT_ENDPT_ORDER-0.5,
                xmin = 0.65,xmax = 2, fill=stripe), 
            color=NA, alpha = 0.1) +
  # Data points
  geom_point(aes(size=20.5), shape=18, position=position_dodge(0.5)) +
  # Scales
  scale_color_manual(values=color_list, name="") +
  # Theme
  labs(y="", x="", title="", fill=NULL, subtitle="") +
  theme_hrs(base_size=base_size, plot_top_margin=-20,legend_pos = "none") +
  ## Fonts
  theme(legend.text=element_text(size=size_medium, family=font), 
        axis.text.x=element_text(size=size_small, family=font),
        axis.text.y=element_text(size=size_medium, family=font),
        axis.title=element_text(size=size_medium, family=font))


################################################################################
#
# Adjust dataframe to create Figure  B + D
#
################################################################################

# combine data frames model 1 and 2 for plotting 
df12alt <- cbind(model1,model2[,c("HR","lb","ub")]) #column bind
names(df12alt) <- c("trait","HR.x","lb.x","ub.x","Test",
                    "HR.y","lb.y","ub.y") # rename 

# reorder trait by endpoint order
df12alt$trait <- factor(df12alt$trait, levels = endpt_order$trait)


################################################################################
#
# Create "Main" Figure 1B+D
#
################################################################################

## Figure 1 B ## 

# scatterplot of the unadjusted HRs for EA vs. the HRs for EA adjusted for the PGS
Fig1B <- ggplot(df12alt[which(df12alt$Test=="PRS"),], 
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
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1),
                     limits = c(1, 2.12),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1),
                     limits = c(1, 2.12),
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

# scatterplot of the unadjusted HRs for EA vs. the HRs for EA adjusted for the PGS
Fig1D <- ggplot(df12alt[which(df12alt$Test=="Upper-level occupation"),], 
                aes(x = HR.x, y = HR.y, color=trait)) +
  # Guide Lines
  geom_hline(yintercept=0.6, linetype=1, size=line_size) +
  geom_vline(xintercept=0.6, linetype=1, size=line_size) +
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
                     breaks = c(0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.7,1.9),
                     limits = c(0.6, 2),
                     expand = c(0,0)) +
  scale_y_continuous(transform = scales::transform_log(),
                     breaks = c(0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.7,1.9),
                     limits = c(0.6, 2),
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
# Create combined figure
#
################################################################################

# combine plots with empty space
top_plt <- plot_grid(Fig1A,NULL,Fig1B,
                     Fig1C,NULL,Fig1D, 
                     nrow=2, align="h", 
                     rel_widths=c(0.65, 0.02, 0.35))
# add additional empty space for legends
Fig10 <- plot_grid(top_plt,NULL, nrow=2, rel_heights=c(0.90, 0.10))

# save figure as png
png(filename = paste0("output/Figures/Manuscript/",as.character(Sys.Date()),
                      "_INTERVENE_Occupation_eFigure10.png"),
    width = 22, height = 20,units='in',res=600)
Fig10
dev.off()

# figure is finalized in inkscape