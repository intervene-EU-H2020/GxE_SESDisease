#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# educational attainment [EA]) in incidences of 19 common diseases (as
# previously selected in the INTERVENE flagship manuscript + Alcohol Use
# Disorder: https://doi.org/10.1101/2023.06.12.23291186)
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Compare prediction accuracy of model 0a (covariates only) with model
# 1a (covariates + main effect of education) by comparing the models Receiver
# Operating Characteristic (ROC) Area Under the Curve(AUC), continuous Net
# Reclassification Index (NRI), and Integrated Discrimination Index (IDI)
#
# Required input data: 
#   1) biobank-specific INTERVENE combined phenotype file as created in the
#   educational attainment DataPrep script
#   (https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/DataPrep_EducationalAttainment.R)
#   2) When predicting in 20% of the FinnGen study or the UK Biobank: the output
#   of the logistic regression models 0a and 1a based 80% of the FinnGen study
#   (download output files with suffix
#   "*_INTERVENE_SESDiffDiseases_Coeffs_glm_by_model*_FinnGenR11_80percent"
#   from:
#   https://drive.google.com/drive/folders/1pxDlg6Mt610pdBChhRt8Tx2A7DgUtIdw?usp=sharing)
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

# function to load and if required install R packages
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
# reading in large data sets; foreach = run multiple analyses in a loop, also
# install "doParallel" which will allow you to run foreach in parallel across
# the number of cores you desire/have available; plyr + dplyr + Hmisc = data
# wrangling; pROC = caluclating ROC/AUC; nricens = calculate continouis
# reclassification tables.
packages("data.table","foreach","doParallel","plyr","dplyr","Hmisc","pROC","nricens")

# set-up multicore-like functionality (with doParallel when running foreach). I
# like to leave one core "open", thus, I select the number of available cores - 1
registerDoParallel(cores = (detectCores()-1))

# add vector with Biobank name (no spaces!)
Biobank <- c("Biobank")

################################################################################
#
# Load Data
#
################################################################################

# load data full sample
load("[PathToPhenotypeFile/PhenotypeFile]")

## Read model results ##

# read in model 0a
FGR11.0a.by <- fread("[PathToCoxOutputModel1a/CoxOutputModel0aFile]", data.table=FALSE)

# read in model 1a
FGR11.1a.by <- fread("[PathToCoxOutputModel1a/CoxOutputModel1aFile]", data.table=FALSE)


################################################################################
#
# Prep Data for analyses
#
################################################################################

# reorganize into long format - birth year
FGR11.0a.bylong <- data.frame(trait = c(rep(FGR11.0a.by$trait,2)),
                             beta = c(FGR11.0a.by$SEX_beta,FGR11.0a.by$birth_year_beta),
                             se = c(FGR11.0a.by$SEX_se,FGR11.0a.by$birth_year_se),
                             Test = c(rep("SEX",nrow(FGR11.0a.by)),
                                      rep("birth_year",nrow(FGR11.0a.by))))
FGR11.1a.bylong <- data.frame(trait = c(rep(FGR11.1a.by$trait,3)),
                             beta = c(FGR11.1a.by$EAhigh_beta,FGR11.1a.by$SEX_beta,FGR11.1a.by$birth_year_beta),
                             se = c(FGR11.1a.by$EAhigh_se,FGR11.1a.by$SEX_se,FGR11.1a.by$birth_year_se),
                             Test = c(rep("Education",nrow(FGR11.1a.by)),rep("SEX",nrow(FGR11.1a.by)),
                                      rep("birth_year",nrow(FGR11.1a.by))))

# split long format model by trait into lists
FGR11.0a.by.list <- split(FGR11.0a.bylong,FGR11.0a.bylong$trait)
FGR11.1a.by.list <- split(FGR11.1a.bylong,FGR11.1a.bylong$trait)

# add trait names to list items
names(FGR11.0a.by.list) <- c(unlist(lapply(FGR11.0a.by.list, function(x) { x$trait[1] })))
names(FGR11.1a.by.list) <- c(unlist(lapply(FGR11.1a.by.list, function(x) { x$trait[1] })))

# reorder individual level data list to match trait order in results
INTERVENE.list <- INTERVENE.list[c("AUD_SWEDISH","C3_BREAST","C3_BRONCHUS_LUNG","C3_CANCER","C3_COLORECTAL",
                                   "C3_MELANOMA_SKIN","C3_PROSTATE","COX_ARTHROSIS","F5_DEPRESSIO","G6_EPLEPSY",
                                   "GOUT","I9_AF","I9_CHD","J10_ASTHMA","K11_APPENDACUT","KNEE_ARTHROSIS",
                                   "RHEUMA_SEROPOS_OTH","T1D","T2D")]

# function to extract birth year
extract_birth_year <- function(filelist, date_col = "DATE_OF_BIRTH") {
  lapply(filelist, function(df) {
    df$birth_year <- as.numeric(format(as.Date(df[[date_col]]), "%Y"))
    return(df)
  })
}

#run function to extract birth year
INTERVENE.list <- extract_birth_year(INTERVENE.list)

# add trait names to list items 
names(INTERVENE.list) <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))

# vector with traits in Biobanks
trait <- unique(c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] }))))

# Compute risk scores from betas (of Cox model from FinnGen) - birth year
INTERVENE.list_score_0a_by <- foreach(i=trait) %dopar% {
  exp(if (!is.na(FGR11.0a.by.list[[i]]$beta[which(FGR11.0a.by.list[[i]]$Test=="SEX")])) FGR11.0a.by.list[[i]]$beta[which(FGR11.0a.by.list[[i]]$Test=="SEX")] * (as.numeric(INTERVENE.list[[i]]$SEX)) else 0 + 
        FGR11.0a.by.list[[i]]$beta[which(FGR11.0a.by.list[[i]]$Test=="birth_year")] * INTERVENE.list[[i]]$birth_year)
}
INTERVENE.list_score_1a_by <- foreach(i=trait) %dopar% {
  exp(FGR11.1a.by.list[[i]]$beta[which(FGR11.1a.by.list[[i]]$Test=="Education")] * (as.numeric(INTERVENE.list[[i]]$EA)-1) + 
        if (!is.na(FGR11.1a.by.list[[i]]$beta[which(FGR11.1a.by.list[[i]]$Test=="SEX")])) FGR11.1a.by.list[[i]]$beta[which(FGR11.1a.by.list[[i]]$Test=="SEX")] * (as.numeric(INTERVENE.list[[i]]$SEX)) else 0 + 
        FGR11.1a.by.list[[i]]$beta[which(FGR11.1a.by.list[[i]]$Test=="birth_year")] * INTERVENE.list[[i]]$birth_year)
}

# add trait names to list items 
names(INTERVENE.list_score_0a_by) <- trait
names(INTERVENE.list_score_1a_by) <- trait

# Normalize risk scores to range [0,1] (needed for NRI/IDI)
INTERVENE.list_score_0an_by <- foreach(i=trait) %dopar% {
  (INTERVENE.list_score_0a_by[[i]] - min(INTERVENE.list_score_0a_by[[i]])) / (max(INTERVENE.list_score_0a_by[[i]]) - min(INTERVENE.list_score_0a_by[[i]]))
}
INTERVENE.list_score_1an_by <- foreach(i=trait) %dopar% {
  (INTERVENE.list_score_1a_by[[i]] - min(INTERVENE.list_score_1a_by[[i]])) / (max(INTERVENE.list_score_1a_by[[i]]) - min(INTERVENE.list_score_1a_by[[i]]))
}

# the column is now named after the PRS change that to reflect the score 
for (i in 1:length(INTERVENE.list_score_0an_by)) {
  names(INTERVENE.list_score_0an_by[[i]]) <- "score_0a_by"
}
for (i in 1:length(INTERVENE.list_score_1an_by)) {
  names(INTERVENE.list_score_1an_by[[i]]) <- "score_1a_by"
}

# add scores to biobank data
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cbind(INTERVENE.list[[i]],score_0a_by=INTERVENE.list_score_0an_by[[i]])
}
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cbind(INTERVENE.list[[i]],score_1a_by=INTERVENE.list_score_1an_by[[i]])
}

# add trait names to list items 
names(INTERVENE.list) <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))


################################################################################
#
# AUC analyses and comparison of AUC between models
#
################################################################################

# Compute AUC for all models - birth year 
auc_0a_by <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  if(names(INTERVENE.list[i]) %in% c("C3_CANCER","C3_MELANOMA_SKIN","KNEE_ARTHROSIS")) {
    roc(INTERVENE.list[[i]][,15], INTERVENE.list[[i]]$score_0a_by, ci = TRUE, direction=">")
  } else {
    roc(INTERVENE.list[[i]][,15], INTERVENE.list[[i]]$score_0a_by, ci = TRUE)
  }
}
auc_1a_by <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  if(names(INTERVENE.list[i]) %in% c("C3_CANCER")) {
    roc(INTERVENE.list[[i]][,15], INTERVENE.list[[i]]$score_1a_by, ci = TRUE, direction=">")
  } else {
    roc(INTERVENE.list[[i]][,15], INTERVENE.list[[i]]$score_1a_by, ci = TRUE)
  }
}

# Compare AUCs using DeLong test
auc_test0a1a_by <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  roc.test(auc_0a_by[[i]], auc_1a_by[[i]])
}

# function to extract AUCs
extract_auc_comparison <- function(roc_list, trait_vector) {
  results <- data.frame(
    trait = character(),
    AUC_model1 = numeric(),
    CI_model1_lower = numeric(),
    CI_model1_upper = numeric(),
    AUC_model2 = numeric(),
    CI_model2_lower = numeric(),
    CI_model2_upper = numeric(),
    AUC_p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(roc_list)) {
    roc_test <- roc_list[[i]]
    
    if (!is.null(roc_test)) {
      auc1 <- if (!is.null(roc_test$roc1$auc)) roc_test$roc1$auc else 0.5
      ci1 <- if (!is.null(roc_test$roc1$ci) && length(roc_test$roc1$ci) == 3) roc_test$roc1$ci else rep(0.5, 3)
      
      auc2 <- if (!is.null(roc_test$roc2$auc)) roc_test$roc2$auc else 0.5
      ci2 <- if (!is.null(roc_test$roc2$ci) && length(roc_test$roc2$ci) == 3) roc_test$roc2$ci else rep(0.5, 3)
      
      p_value <- if (!is.null(roc_test$p.value)) roc_test$p.value else NA
      
      results <- rbind(results, data.frame(
        trait = trait_vector[i],
        AUC_model1 = auc1,
        CI_model1_lower = ci1[1],
        CI_model1_upper = ci1[3],
        AUC_model2 = auc2,
        CI_model2_lower = ci2[1],
        CI_model2_upper = ci2[3],
        AUC_p_value = p_value
      ))
    }
  }
  
  return(results)
}

# extract AUCs
auc0a1a_by_comparison_table <- extract_auc_comparison(roc_list = auc_test0a1a_by, trait_vector = trait)

# write table with model AUC to output as tab-delimited text files
write.table(auc0a1a_by_comparison_table, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                                  "_",Biobank,"_INTERVENE_EducationalAttainment_AUCcomparison_Model0a-1a.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


################################################################################
#
# Compute continuous Net Reclassification Index (NRI) and Integrated Discrimination Index (IDI)
#
################################################################################

# Compute percentile ranks for risk scores (required for continuous NRI)
INTERVENE.list <- lapply(INTERVENE.list, function(df) {
  df %>%
  mutate(score_0a_by_rank = percent_rank(score_0a_by),
         score_1a_by_rank = percent_rank(score_1a_by))
})
  

# Function to compute NRI & IDI for a list of datasets
compute_nri_idi <- function(data_list, trait_names) {
  
  results_list <- lapply(seq_along(data_list), function(i) {
    data <- data_list[[i]]
    trait_name <- trait_names[i]  # Extract trait name
    
    # Convert ranked probabilities to numeric
    x <- as.numeric(data$score_0a_by_rank)  # Reference model
    y <- as.numeric(data$score_1a_by_rank)  # New model
    
    # Compute NRI and IDI
    nri_results <- improveProb(x, y, data[,15])
    
    # Store results in a dataframe
    data.frame(
      Trait = trait_name,
      Events_NRI = signif(nri_results$nri.ev, 2),
      Events_NRI_Lower = signif(nri_results$nri.ev - 1.96 * nri_results$se.nri.ev, 2),
      Events_NRI_Upper = signif(nri_results$nri.ev + 1.96 * nri_results$se.nri.ev, 2),
      NonEvents_NRI = signif(nri_results$nri.ne, 2),
      NonEvents_NRI_Lower = signif(nri_results$nri.ne - 1.96 * nri_results$se.nri.ne, 2),
      NonEvents_NRI_Upper = signif(nri_results$nri.ne + 1.96 * nri_results$se.nri.ne, 2),
      Overall_NRI = signif(nri_results$nri, 2),
      Overall_NRI_Lower = signif(nri_results$nri - 1.96 * nri_results$se.nri, 2),
      Overall_NRI_Upper = signif(nri_results$nri + 1.96 * nri_results$se.nri, 2),
      IDI = signif(nri_results$idi, 2),
      IDI_Lower = signif(nri_results$idi - 1.96 * nri_results$se.idi, 2),
      IDI_Upper = signif(nri_results$idi + 1.96 * nri_results$se.idi, 2)
    )
  })
  
  # Combine all results into a single dataframe
  final_results <- bind_rows(results_list)
  
  return(final_results)
}

# Extract NRI & IDI for all traits
nri_idi_results <- compute_nri_idi(data_list = INTERVENE.list, trait_names = trait)

# write table with NRI & IDI to output as tab-delimited text files
write.table(nri_idi_results, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                             "_",Biobank,"_EUR_INTERVENE_EducationalAttainment_NRI_IDI_Model0a-1a_by.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
