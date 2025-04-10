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
# Script: Compare prediction accuracy of model 1a (main effect of education)
# with model 2 (main effects of education and disease-specific PGS) by comparing
# the models Receiver Operating Characteristic (ROC) Area Under the Curve(AUC),
# continuous Net Reclassification Index (NRI), and Integrated Discrimination
# Index (IDI)
#
# Required input data: 
#   1) biobank-specific INTERVENE combined phenotype file as created in the
#   educational attainment DataPrep script
#   (https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/DataPrep_EducationalAttainment.R)
#   2) When predicting in 20% of the FinnGen study, the UK Biobank or Generation 
#   Scotland: the output of the Cox Proportional Hazard models 2 and 4 based 80%
#   of the FinnGen study (download output files with suffix 
#   "*_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model*_FinnGenR11_80percent.txt"
#   from: https://drive.google.com/drive/folders/1nmUJUz0vupaEiWCNk4gBM5U6G3VyNmbB?usp=sharing)
#
# Last edits: 10/04/2025 (edits, FAH: final checks and minor tweaks prior to
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
# wrangling; pROC = calculating ROC/AUC; nricens = calculate continuous
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

# read in model 1
FGR11.1a <- fread("[PathToCoxOutputModel1a/CoxOutputModel1aFile]", data.table=FALSE)

# read in model 2
FGR11.2 <- fread("[PathToCoxOutputModel2/CoxOutputModel2File]", data.table=FALSE)


################################################################################
#
# Prep Data for analyses
#
################################################################################

# vector with traits in Biobank
trait <- unique(c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] }))))

# from model results only retain overlapping traits
FGR11.1a <- FGR11.1a[which(FGR11.1a$trait %in% trait),]
FGR11.2 <- FGR11.2[which(FGR11.2$trait %in% trait),]

# reorganize into long format
FGR11.1along <- data.frame(trait = c(FGR11.1a$trait),
                           beta = c(FGR11.1a$EAhigh_beta),
                           se = c(FGR11.1a$EAhigh_se))
FGR11.2long <- data.frame(trait = c(rep(FGR11.2$trait,2)),
                          beta = c(FGR11.2$EAhigh_beta,FGR11.2$PRS_beta),
                          se = c(FGR11.2$EAhigh_se,FGR11.2$PRS_se),
                          Test = c(rep(c("EA","PRS"),each=nrow(FGR11.2))))

# split long format model by trait into lists
FGR11.1a.list <- split(FGR11.1along,FGR11.1along$trait)
FGR11.2.list <- split(FGR11.2long,FGR11.2long$trait)

# Compute risk scores from betas of Cox model from 80% FinnGen
INTERVENE.list_score_1a <- foreach(i=trait) %dopar% {
  exp(FGR11.1a.list[[i]]$beta * (as.numeric(INTERVENE.list[[i]]$EA)-1))
}
INTERVENE.list_score_2 <- foreach(i=trait) %dopar% {
  exp(FGR11.2.list[[i]]$beta[1] * (as.numeric(INTERVENE.list[[i]]$EA)-1) + 
        FGR11.2.list[[i]]$beta[2] * INTERVENE.list[[i]][20])
}

# add trait names to list items 
names(FGR11.1a.list) <- trait
names(INTERVENE.list_score_2) <- trait

# Normalize risk scores to range [0,1] (needed for NRI/IDI)
INTERVENE.list_score_1an <- foreach(i=trait) %dopar% {
  (INTERVENE.list_score_1a[[i]] - min(INTERVENE.list_score_1a[[i]])) / (max(INTERVENE.list_score_1a[[i]]) - min(INTERVENE.list_score_1a[[i]]))
}
INTERVENE.list_score_2n <- foreach(i=trait) %dopar% {
  (INTERVENE.list_score_2[[i]] - min(INTERVENE.list_score_2[[i]])) / (max(INTERVENE.list_score_2[[i]]) - min(INTERVENE.list_score_2[[i]]))
}

# the column is now named after the PRS change that to reflect the score
for (i in 1:length(INTERVENE.list_score_1an)) {
  names(INTERVENE.list_score_1an[[i]]) <- "score_1a"
}
for (i in 1:length(INTERVENE.list_score_2n)) {
  names(INTERVENE.list_score_2n[[i]]) <- "score_2"
}

# add scores to biobank data
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cbind(INTERVENE.list[[i]],score_1a=INTERVENE.list_score_1an[[i]])
}
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cbind(INTERVENE.list[[i]],score_2=INTERVENE.list_score_2n[[i]])
}


################################################################################
#
# AUC analyses and comparison of AUC between models
#
################################################################################

# Compute AUC for both models
auc_1a <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  roc(INTERVENE.list[[i]][,15], INTERVENE.list[[i]]$score_1a, ci = TRUE)
}
auc_2 <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  roc(INTERVENE.list[[i]][,15], INTERVENE.list[[i]]$score_2, ci = TRUE)
}

# Compare AUCs using DeLong test
auc_test <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  roc.test(auc_1a[[i]], auc_2[[i]])
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
      auc1 <- roc_test$roc1$auc
      ci1 <- roc_test$roc1$ci
      auc2 <- roc_test$roc2$auc
      ci2 <- roc_test$roc2$ci
      p_value <- roc_test$p.value
      
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
auc_comparison_table <- extract_auc_comparison(roc_list = auc_test, trait_vector = trait)

# write table with model AUC to output as tab-delimited text files
write.table(auc_comparison_table, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                                  "_",Biobank,"_INTERVENE_EducationalAttainment_AUCcomparison_Model1a-2.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

################################################################################
#
# Compute continuous Net Reclassification Index (NRI) and Integrated Discrimination Index (IDI)
#
################################################################################

# Compute percentile ranks for risk scores (required for continuous NRI)
INTERVENE.list <- lapply(INTERVENE.list, function(df) {
  df %>%
  mutate(score_1a_rank = percent_rank(score_1a),
         score_2_rank = percent_rank(score_2))
})
  

# Function to compute NRI & IDI for a list of datasets
compute_nri_idi <- function(data_list, trait_names) {
  
  results_list <- lapply(seq_along(data_list), function(i) {
    data <- data_list[[i]]
    trait_name <- trait_names[i]  # Extract trait name
    
    # Convert ranked probabilities to numeric
    x <- as.numeric(data$score_1a_rank)  # Reference model
    y <- as.numeric(data$score_2_rank)  # New model
    
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
write.table(nri_idi_results, file=paste0("[PathToOutputFolder/]/",as.character(Sys.Date()),
                                              "_",Biobank,"_INTERVENE_EducationalAttainment_NRI_IDI_Model1a-2.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

