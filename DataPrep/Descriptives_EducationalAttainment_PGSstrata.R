#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# educational attainment [EA]) in risk of 19 common diseases (as
# previously selected in the INTERVENE flagship manuscript + Alcohol Use
# Disorder: https://doi.org/10.1101/2023.06.12.23291186)
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: calculate the summary statistics on the phenotype files, including
# Educational Attainment and PGS strata
#
# Required input data: biobank-specific INTERVENE combined phenotype and PGS
# files
#
# Last edits: 27/02/2026 (FAH, edits: generalize the indication of how to treat 
# prostate cancer and add breast cancer)
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
# reading in/working with large data sets; foreach = run multiple analyses in a
# loop, also install "doParallel" which will allow you to run foreach in
# parallel across the number of cores you desire/have available; lubridate = to
# calculate etc. with dates.
packages("data.table","foreach","doParallel","lubridate")

# set-up multicore-like functionality (with doParallel when running foreach). I
# like to leave one core "open", thus, I select the number of available cores - 1
registerDoParallel(cores = (detectCores()-1))

# add vector with Biobank name (no spaces!)
Biobank <- c("Biobank")


################################################################################
#
# Load Data RData files
#
################################################################################

# full sample
load("[PathToPhenotypeFile/PhenotypeFile.RData]")


################################################################################
#
# Adjust data prior to estimating descriptives
#
################################################################################

# split by Education
lowEA <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  subset(INTERVENE.list[[i]], EA == "low")
}
highEA <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  subset(INTERVENE.list[[i]], EA == "high")
}

# this might result <5 individuals with a given birth decade and in empty levels for
# birth decade factor; remove those
# Function to drop unused levels of birthdecade in each dataframe
clean_birthdecade <- function(df) {
  df <- df %>%
    group_by(birthdecade) %>%
    filter(n() >= 5) %>%
    ungroup()
  
  df$birthdecade <- droplevels(df$birthdecade)  # Drop empty levels
  
  return(df)
}

# Apply to both lists of dataframes
lowEA <- lapply(lowEA, clean_birthdecade)
highEA <- lapply(highEA, clean_birthdecade)


# to get proper descriptives only retain individuals in the stratified groups
# (as some had been removed to due <5 individuals in a birth cohort)
INTERVENE.list2 <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  rbind(lowEA[[i]],highEA[[i]])
}

################################################################################
#
# Create descriptive tables for each of the traits
#
################################################################################

# Function to create descriptive tables for each of the traits when EA is
# dichotomizd into low vs high EA and the PGS is categorized
desc.table.mod6 <- function(filelist) {
  # extract trait name
  trait <- names(filelist)[15]
  
  # extract N controls, N cases, and calculate prevalence
  Ncontrols <- sum(filelist[,15]==0)
  Ncases <- sum(filelist[,15]==1)
  prevalence <- round(Ncases/(Ncases+Ncontrols)*100,2)
  
  # extract N and % of females for the controls and cases
  Ncontrols_females <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0)]==1), 
                                " (", 
                                round(prop.table(table(filelist$SEX[which(filelist[,15]==0)]))[2]*100,1), 
                                "%)"))
  Ncases_females <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1)]==1), 
                             " (", 
                             round(prop.table(table(filelist$SEX[which(filelist[,15]==1)]))[2]*100,1), 
                             "%)"))
  
  # calculate quantiles and interquartile range of age at onset in cases only
  AgeOnset_q25 <- round(quantile(filelist$AGE[which(filelist[,15]==1)], probs = .25),2)
  AgeOnstet_q50 <- round(quantile(filelist$AGE[which(filelist[,15]==1)], probs = .5),2)
  AgeOnset_q75 <- round(quantile(filelist$AGE[which(filelist[,15]==1)], probs = .75),2)
  AgeOnset_IQR <- round(IQR(filelist$AGE[which(filelist[,15]==1)]),2)
  
  # calculate interquartile range and median time of follow-up
  Followup_IQR <- round(IQR((filelist$DATE_OF_BIRTH %--% filelist$END_OF_FOLLOWUP) / years(1)),2)
  Followup_Median <- round(median((filelist$DATE_OF_BIRTH %--% filelist$END_OF_FOLLOWUP) / years(1)),2)
  
  # extract N per EA level for the controls and cases
  Ncontrols_lowEA <- sum(filelist$EA[which(filelist[,15]==0)]=="low")
  Ncases_lowEA <- sum(filelist$EA[which(filelist[,15]==1)]=="low")
  Ncontrols_highEA <- sum(filelist$EA[which(filelist[,15]==0)]=="high")
  Ncases_highEA <- sum(filelist$EA[which(filelist[,15]==1)]=="high")
  
  # calculate prevalences per EA level
  prevalence_lowEA <- round(Ncases_lowEA/(Ncases_lowEA+Ncontrols_lowEA)*100,2)
  prevalence_highEA <- round(Ncases_highEA/(Ncases_highEA+Ncontrols_highEA)*100,2)
  
  # extract N and % females for the controls and cases per EA level
  Ncontrols_females_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$EA=="low")]==1), 
                                      " (", 
                                      round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$EA=="low")]))[2]*100,1), 
                                      "%)"))
  Ncases_females_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$EA=="low")]==1), 
                                   " (", 
                                   round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$EA=="low")]))[2]*100,1), 
                                   "%)"))
  Ncontrols_females_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$EA=="high")]==1), 
                                       " (", 
                                       round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$EA=="high")]))[2]*100,1), 
                                       "%)"))
  Ncases_females_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$EA=="high")]==1), 
                                    " (", 
                                    round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$EA=="high")]))[2]*100,1), 
                                    "%)"))
  
  # extract N per PGS category for the cases and controls
  Ncontrols_PGS_Group1 <- sum(filelist$PGS_group[which(filelist[,15]==0)]=="Group 1")
  Ncases_PGS_Group1 <- sum(filelist$PGS_group[which(filelist[,15]==1)]=="Group 1")
  Ncontrols_PGS_Group2 <- sum(filelist$PGS_group[which(filelist[,15]==0)]=="Group 2")
  Ncases_PGS_Group2 <- sum(filelist$PGS_group[which(filelist[,15]==1)]=="Group 2")
  Ncontrols_PGS_Group3 <- sum(filelist$PGS_group[which(filelist[,15]==0)]=="Group 3")
  Ncases_PGS_Group3 <- sum(filelist$PGS_group[which(filelist[,15]==1)]=="Group 3")
  Ncontrols_PGS_Group4 <- sum(filelist$PGS_group[which(filelist[,15]==0)]=="Group 4")
  Ncases_PGS_Group4 <- sum(filelist$PGS_group[which(filelist[,15]==1)]=="Group 4")
  Ncontrols_PGS_Group5 <- sum(filelist$PGS_group[which(filelist[,15]==0)]=="Group 5")
  Ncases_PGS_Group5 <- sum(filelist$PGS_group[which(filelist[,15]==1)]=="Group 5")
  
  # extract N and % females for the controls and cases per PGS category level
  Ncontrols_females_PGS_Group1 <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 1")]==1), 
                                           " (", 
                                           round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 1")]))[2]*100,1), 
                                           "%)"))
  Ncases_females_PGS_Group1 <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 1")]==1), 
                                        " (", 
                                        round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 1")]))[2]*100,1), 
                                        "%)"))
  Ncontrols_females_PGS_Group2 <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 2")]==1), 
                                           " (", 
                                           round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 2")]))[2]*100,1), 
                                           "%)"))
  Ncases_females_PGS_Group2 <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 2")]==1), 
                                        " (", 
                                        round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 2")]))[2]*100,1), 
                                        "%)"))
  Ncontrols_females_PGS_Group3 <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 3")]==1), 
                                           " (", 
                                           round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 3")]))[2]*100,1), 
                                           "%)"))
  Ncases_females_PGS_Group3 <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 3")]==1), 
                                        " (", 
                                        round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 3")]))[2]*100,1), 
                                        "%)"))
  Ncontrols_females_PGS_Group4 <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 4")]==1), 
                                           " (", 
                                           round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 4")]))[2]*100,1), 
                                           "%)"))
  Ncases_females_PGS_Group4 <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 4")]==1), 
                                        " (", 
                                        round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 4")]))[2]*100,1), 
                                        "%)"))
  Ncontrols_females_PGS_Group5 <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 5")]==1), 
                                           " (", 
                                           round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 5")]))[2]*100,1), 
                                           "%)"))
  Ncases_females_PGS_Group5 <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 5")]==1), 
                                        " (", 
                                        round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 5")]))[2]*100,1), 
                                        "%)"))
  
  # extract N per PGS category and EA for the cases and controls
  Ncontrols_PGS_Group1_lowEA <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$EA=="low")]=="Group 1")
  Ncontrols_PGS_Group1_highEA <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$EA=="high")]=="Group 1")
  Ncases_PGS_Group1_lowEA <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$EA=="low")]=="Group 1")
  Ncases_PGS_Group1_highEA <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$EA=="high")]=="Group 1")
  Ncontrols_PGS_Group2_lowEA <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$EA=="low")]=="Group 2")
  Ncontrols_PGS_Group2_highEA <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$EA=="high")]=="Group 2")
  Ncases_PGS_Group2_lowEA <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$EA=="low")]=="Group 2")
  Ncases_PGS_Group2_highEA <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$EA=="high")]=="Group 2")
  Ncontrols_PGS_Group3_lowEA <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$EA=="low")]=="Group 3")
  Ncontrols_PGS_Group3_highEA <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$EA=="high")]=="Group 3")
  Ncases_PGS_Group3_lowEA <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$EA=="low")]=="Group 3")
  Ncases_PGS_Group3_highEA <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$EA=="high")]=="Group 3")
  Ncontrols_PGS_Group4_lowEA <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$EA=="low")]=="Group 4")
  Ncontrols_PGS_Group4_highEA <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$EA=="high")]=="Group 4")
  Ncases_PGS_Group4_lowEA <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$EA=="low")]=="Group 4")
  Ncases_PGS_Group4_highEA <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$EA=="high")]=="Group 4")
  Ncontrols_PGS_Group5_lowEA <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$EA=="low")]=="Group 5")
  Ncontrols_PGS_Group5_highEA <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$EA=="high")]=="Group 5")
  Ncases_PGS_Group5_lowEA <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$EA=="low")]=="Group 5")
  Ncases_PGS_Group5_highEA <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$EA=="high")]=="Group 5")
  
  # extract N and % females for the controls and cases per PGS category level and EA
  Ncontrols_females_PGS_Group1_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 1" & filelist$EA=="low")]==1), 
                                                 " (", 
                                                 round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 1" & filelist$EA=="low")]))[2]*100,1), 
                                                 "%)"))
  Ncontrols_females_PGS_Group1_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 1" & filelist$EA=="high")]==1), 
                                                  " (", 
                                                  round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 1" & filelist$EA=="high")]))[2]*100,1), 
                                                  "%)"))
  Ncases_females_PGS_Group1_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 1" & filelist$EA=="low")]==1), 
                                              " (", 
                                              round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 1" & filelist$EA=="low")]))[2]*100,1), 
                                              "%)"))
  Ncases_females_PGS_Group1_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 1" & filelist$EA=="high")]==1), 
                                               " (", 
                                               round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 1" & filelist$EA=="high")]))[2]*100,1), 
                                               "%)"))
  Ncontrols_females_PGS_Group2_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 2" & filelist$EA=="low")]==1), 
                                                 " (", 
                                                 round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 2" & filelist$EA=="low")]))[2]*100,1), 
                                                 "%)"))
  Ncontrols_females_PGS_Group2_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 2" & filelist$EA=="high")]==1), 
                                                  " (", 
                                                  round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 2" & filelist$EA=="high")]))[2]*100,1), 
                                                  "%)"))
  Ncases_females_PGS_Group2_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 2" & filelist$EA=="low")]==1), 
                                              " (", 
                                              round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 2" & filelist$EA=="low")]))[2]*100,1), 
                                              "%)"))
  Ncases_females_PGS_Group2_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 2" & filelist$EA=="high")]==1), 
                                               " (", 
                                               round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 2" & filelist$EA=="high")]))[2]*100,1), 
                                               "%)"))
  Ncontrols_females_PGS_Group3_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 3" & filelist$EA=="low")]==1), 
                                                 " (", 
                                                 round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 3" & filelist$EA=="low")]))[2]*100,1), 
                                                 "%)"))
  Ncontrols_females_PGS_Group3_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 3" & filelist$EA=="high")]==1), 
                                                  " (", 
                                                  round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 3" & filelist$EA=="high")]))[2]*100,1), 
                                                  "%)"))
  Ncases_females_PGS_Group3_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 3" & filelist$EA=="low")]==1), 
                                              " (", 
                                              round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 3" & filelist$EA=="low")]))[2]*100,1), 
                                              "%)"))
  Ncases_females_PGS_Group3_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 3" & filelist$EA=="high")]==1), 
                                               " (", 
                                               round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 3" & filelist$EA=="high")]))[2]*100,1), 
                                               "%)"))
  Ncontrols_females_PGS_Group4_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 4" & filelist$EA=="low")]==1), 
                                                 " (", 
                                                 round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 4" & filelist$EA=="low")]))[2]*100,1), 
                                                 "%)"))
  Ncontrols_females_PGS_Group4_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 4" & filelist$EA=="high")]==1), 
                                                  " (", 
                                                  round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 4" & filelist$EA=="high")]))[2]*100,1), 
                                                  "%)"))
  Ncases_females_PGS_Group4_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 4" & filelist$EA=="low")]==1), 
                                              " (", 
                                              round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 4" & filelist$EA=="low")]))[2]*100,1), 
                                              "%)"))
  Ncases_females_PGS_Group4_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 4" & filelist$EA=="high")]==1), 
                                               " (", 
                                               round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 4" & filelist$EA=="high")]))[2]*100,1), 
                                               "%)"))
  Ncontrols_females_PGS_Group5_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 5" & filelist$EA=="low")]==1), 
                                                 " (", 
                                                 round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 5" & filelist$EA=="low")]))[2]*100,1), 
                                                 "%)"))
  Ncontrols_females_PGS_Group5_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 5" & filelist$EA=="high")]==1), 
                                                  " (", 
                                                  round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 5" & filelist$EA=="high")]))[2]*100,1), 
                                                  "%)"))
  Ncases_females_PGS_Group5_lowEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 5" & filelist$EA=="low")]==1), 
                                              " (", 
                                              round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 5" & filelist$EA=="low")]))[2]*100,1), 
                                              "%)"))
  Ncases_females_PGS_Group5_highEA <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 5" & filelist$EA=="high")]==1), 
                                               " (", 
                                               round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 5" & filelist$EA=="high")]))[2]*100,1), 
                                               "%)"))
  
  # combine all descriptives into a single data.frame
  dat <- as.data.frame(cbind(trait,Ncontrols,Ncases,Ncontrols_females,
                             Ncases_females,prevalence,AgeOnset_q25,
                             AgeOnstet_q50,AgeOnset_q75,AgeOnset_IQR,
                             Followup_IQR,Followup_Median,Ncontrols_lowEA,
                             Ncases_lowEA,Ncontrols_highEA,Ncases_highEA,
                             prevalence_lowEA,prevalence_highEA,
                             Ncontrols_females_lowEA,Ncases_females_lowEA,
                             Ncontrols_females_highEA,Ncases_females_highEA,
                             Ncontrols_PGS_Group1,Ncases_PGS_Group1,Ncontrols_PGS_Group2,
                             Ncases_PGS_Group2,Ncontrols_PGS_Group3,Ncases_PGS_Group3,
                             Ncontrols_PGS_Group4,Ncases_PGS_Group4,Ncontrols_PGS_Group5,
                             Ncases_PGS_Group5,Ncontrols_females_PGS_Group1,Ncases_females_PGS_Group1,
                             Ncontrols_females_PGS_Group2,Ncases_females_PGS_Group2,
                             Ncontrols_females_PGS_Group3,Ncases_females_PGS_Group3,
                             Ncontrols_females_PGS_Group4,Ncases_females_PGS_Group4,
                             Ncontrols_females_PGS_Group5,Ncases_females_PGS_Group5,
                             Ncontrols_PGS_Group1_lowEA,Ncontrols_PGS_Group1_highEA,
                             Ncases_PGS_Group1_lowEA,Ncases_PGS_Group1_highEA,
                             Ncontrols_PGS_Group2_lowEA,Ncontrols_PGS_Group2_highEA,
                             Ncases_PGS_Group2_lowEA,Ncases_PGS_Group2_highEA,
                             Ncontrols_PGS_Group3_lowEA,Ncontrols_PGS_Group3_highEA,
                             Ncases_PGS_Group3_lowEA,Ncases_PGS_Group3_highEA,
                             Ncontrols_PGS_Group4_lowEA,Ncontrols_PGS_Group4_highEA,
                             Ncases_PGS_Group4_lowEA,Ncases_PGS_Group4_highEA,
                             Ncontrols_PGS_Group5_lowEA,Ncontrols_PGS_Group5_highEA,
                             Ncases_PGS_Group5_lowEA,Ncases_PGS_Group5_highEA,
                             Ncontrols_females_PGS_Group1_lowEA,Ncontrols_females_PGS_Group1_highEA,
                             Ncases_females_PGS_Group1_lowEA,Ncases_females_PGS_Group1_highEA,
                             Ncontrols_females_PGS_Group2_lowEA,Ncontrols_females_PGS_Group2_highEA,
                             Ncases_females_PGS_Group2_lowEA,Ncases_females_PGS_Group2_highEA,
                             Ncontrols_females_PGS_Group3_lowEA,Ncontrols_females_PGS_Group3_highEA,
                             Ncases_females_PGS_Group3_lowEA,Ncases_females_PGS_Group3_highEA,
                             Ncontrols_females_PGS_Group4_lowEA,Ncontrols_females_PGS_Group4_highEA,
                             Ncases_females_PGS_Group4_lowEA,Ncases_females_PGS_Group4_highEA,
                             Ncontrols_females_PGS_Group5_lowEA,Ncontrols_females_PGS_Group5_highEA,
                             Ncases_females_PGS_Group5_lowEA,Ncases_females_PGS_Group5_highEA))
  return(dat)
}

#create descriptive table in a foreach loop foreach of the traits
desc.table.dat <- foreach(i=1:length(INTERVENE.list2)) %dopar% {
  desc.table.mod6(filelist = INTERVENE.list2[[i]])
}

# combine separate descriptives tables into 1 big table
desc.table.comb <- rbindlist(desc.table.dat)

# as prostate cancer is only assessed in males and breast cancer only in
# females, fix the % to reflect this
desc.table.comb$Ncontrols_females[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group1[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group1[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group2[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group2[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group3[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group3[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group4[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group4[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group5[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group5[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group1_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group1_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group1_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group1_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group2_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group2_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group2_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group2_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group3_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group3_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group3_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group3_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group4_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group4_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group4_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group4_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group5_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group5_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group5_lowEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group5_highEA[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
#
desc.table.comb$Ncontrols_females[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0)]==1), " (100%)")
desc.table.comb$Ncases_females[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1)]==1), " (100%)")
desc.table.comb$Ncontrols_females_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1)," (100%)")
desc.table.comb$Ncases_females_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncontrols_females_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1)," (100%)")
desc.table.comb$Ncases_females_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group1[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 1")]==1)," (100%)")
desc.table.comb$Ncases_females_PGS_Group1[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 1")]==1)," (100%)")
desc.table.comb$Ncontrols_females_PGS_Group2[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 2")]==1)," (100%)")
desc.table.comb$Ncases_females_PGS_Group2[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 2")]==1)," (100%)")
desc.table.comb$Ncontrols_females_PGS_Group3[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 3")]==1)," (100%)")
desc.table.comb$Ncases_females_PGS_Group3[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 3")]==1)," (100%)")
desc.table.comb$Ncontrols_females_PGS_Group4[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 4")]==1)," (100%)")
desc.table.comb$Ncases_females_PGS_Group4[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 4")]==1)," (100%)")
desc.table.comb$Ncontrols_females_PGS_Group5[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 5")]==1)," (100%)")
desc.table.comb$Ncases_females_PGS_Group5[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 5")]==1)," (100%)")
desc.table.comb$Ncontrols_females_PGS_Group1_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 1" & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group1_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 1" & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group1_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 1" & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group1_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 1" & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group2_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 2" & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group2_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 2" & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group2_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 2" & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group2_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 2" & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group3_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 3" & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group3_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 3" & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group3_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 3" & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group3_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 3" & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group4_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 4" & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group4_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 4" & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group4_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 4" & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group4_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 4" & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group5_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 5" & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group5_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 5" & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group5_lowEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 5" & INTERVENE.list2[["C3_BREAST"]]$EA=="low")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group5_highEA[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 5" & INTERVENE.list2[["C3_BREAST"]]$EA=="high")]==1), " (100%)")

# write table with descriptives to output as tab-delimited text file
write.table(desc.table.comb, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                         "_",Biobank,"_INTERVENE_EducationalAttainment_PGSstrata_SampleDescriptives.txt"),

            row.names=F, col.names = T, sep="\t",quote = F)
