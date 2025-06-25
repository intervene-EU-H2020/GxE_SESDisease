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
# Script: Create Main Table 2
#
# Data: FGR11 (20%) + UKB predictive results (AUC)
#
# Last edits: 25/06/2025 (edits, FAH: final checks and minor tweaks prior to
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
# efficiently reading in large data sets; dplyr, forcats, stringr,purrr & tidyr
# = data wrangling; metafor = meta-analysis
packages("data.table","dplyr","forcats","stringr","tidyr","purrr","metafor")


################################################################################
#
# Read in AUCs
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


################################################################################
#
# Adjust disease names for table
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
# Create main table 2 and write to file
#
################################################################################

# function to format AUC + CI
format_auc <- function(auc, lower, upper) {
  sprintf("%.3f (%.3f–%.3f)", auc, lower, upper)
}

# Function to clean a single result df
# tag = which model comparison this result represents (e.g. "1a_vs_2")
clean_results <- function(df, comparison) {
  df %>%
    mutate(
      AUC_model1_fmt = format_auc(AUC_model1, CI_model1_lower, CI_model1_upper),
      AUC_model2_fmt = format_auc(AUC_model2, CI_model2_lower, CI_model2_upper),
      comparison = comparison
    ) %>%
    select(trait, Biobank, comparison, AUC_model1_fmt, AUC_model2_fmt, AUC_p_value)
}

# Clean each dataset with the appropriate model comparison tag
FGR11.glm.1a2.clean <- clean_results(FGR11.glm.1a2, "1a_vs_2")
FGR11.glm.1b2.clean <- clean_results(FGR11.glm.1b2, "1b_vs_2")
FGR11.glm.24.clean <- clean_results(FGR11.glm.24, "2_vs_4")
#
UKB.glm.1a2.clean <- clean_results(UKB.glm.1a2, "1a_vs_2")
UKB.glm.1b2.clean <- clean_results(UKB.glm.1b2, "1b_vs_2")
UKB.glm.24.clean <- clean_results(UKB.glm.24, "2_vs_4")


# combine all data in long format
combined_long <- bind_rows(FGR11.glm.1a2.clean,FGR11.glm.1b2.clean,FGR11.glm.24.clean,
                           UKB.glm.1a2.clean,UKB.glm.1b2.clean,UKB.glm.24.clean)

# Combine all cleaned results into one wide format
combined_wide <- combined_long %>%
  pivot_wider(
    id_cols = trait,
    names_from = c(Biobank,comparison),
    values_from = c(AUC_model1_fmt, AUC_model2_fmt, AUC_p_value),
    names_glue = "{Biobank}_{comparison}_{.value}"
  )

# Define custom column order:
biobanks <- c("FinnGen", "UKBiobank")  # adjust as needed
comparisons <- c("1b_vs_2","1a_vs_2", "2_vs_4")
metrics <- c("AUC_model1_fmt", "AUC_model2_fmt", "AUC_p_value")

# Build desired column names
desired_order <- c("trait")  # always start with trait
for (b in biobanks) {
  for (c in comparisons) {
    for (m in metrics) {
      col_name <- paste(b, c, m, sep = "_")
      if (col_name %in% colnames(combined_wide)) {
        desired_order <- c(desired_order, col_name)
      }
    }
  }
}

# Apply column order
T2 <- combined_wide %>%
  select(all_of(desired_order))

# as dataframe
T2 <- as.data.frame(T2)

# write file 
write.table(T2, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_MainTable2.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
