#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# occupation) in risk of 19 common diseases (as
# previously selected in the INTERVENE flagship manuscript + Alcohol Use
# Disorder: https://doi.org/10.1101/2023.06.12.23291186)
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: calculate the summary statistics on the phenotype files including
# occupation and PGS strata
#
# Required input data: biobank-specific INTERVENE combined phenotype and PGS
# files
#
# Last edits: 27/02/2026 (FAH, edits: generalize script for GitHub upload)
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
lowOCC <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  subset(INTERVENE.list[[i]], Occupation == "Lower-level")
}
highOCC <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  subset(INTERVENE.list[[i]], Occupation == "Upper-level")
}

# to get proper descriptives only retain individuals in the stratified groups
# (as some had been removed to due <5 individuals in a birth cohort)
INTERVENE.list2 <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  rbind(lowOCC[[i]],highOCC[[i]])
}

################################################################################
#
# Create descriptive tables for each of the traits
#
################################################################################

# Function to create descriptive tables for each of the traits when Occupation is
# dichotomizd into lower vs higher-level occupation and the PGS is categorized
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
  
  # extract N per Occupation level for the controls and cases
  Ncontrols_lowOCC <- sum(filelist$Occupation[which(filelist[,15]==0)]=="Lower-level")
  Ncases_lowOCC <- sum(filelist$Occupation[which(filelist[,15]==1)]=="Lower-level")
  Ncontrols_highOCC <- sum(filelist$Occupation[which(filelist[,15]==0)]=="Upper-level")
  Ncases_highOCC <- sum(filelist$Occupation[which(filelist[,15]==1)]=="Upper-level")
  
  # calculate prevalences per Occupation level
  prevalence_lowOCC <- round(Ncases_lowOCC/(Ncases_lowOCC+Ncontrols_lowOCC)*100,2)
  prevalence_highOCC <- round(Ncases_highOCC/(Ncases_highOCC+Ncontrols_highOCC)*100,2)
  
  # extract N and % females for the controls and cases per Occupation level
  Ncontrols_females_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Lower-level")]==1), 
                                      " (", 
                                      round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                      "%)"))
  Ncases_females_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Lower-level")]==1), 
                                   " (", 
                                   round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                   "%)"))
  Ncontrols_females_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Upper-level")]==1), 
                                       " (", 
                                       round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                       "%)"))
  Ncases_females_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Upper-level")]==1), 
                                    " (", 
                                    round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Upper-level")]))[2]*100,1), 
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
  
  # extract N per PGS category and Occupation for the cases and controls
  Ncontrols_PGS_Group1_lowOCC <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$Occupation=="Lower-level")]=="Group 1")
  Ncontrols_PGS_Group1_highOCC <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$Occupation=="Upper-level")]=="Group 1")
  Ncases_PGS_Group1_lowOCC <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$Occupation=="Lower-level")]=="Group 1")
  Ncases_PGS_Group1_highOCC <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$Occupation=="Upper-level")]=="Group 1")
  Ncontrols_PGS_Group2_lowOCC <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$Occupation=="Lower-level")]=="Group 2")
  Ncontrols_PGS_Group2_highOCC <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$Occupation=="Upper-level")]=="Group 2")
  Ncases_PGS_Group2_lowOCC <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$Occupation=="Lower-level")]=="Group 2")
  Ncases_PGS_Group2_highOCC <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$Occupation=="Upper-level")]=="Group 2")
  Ncontrols_PGS_Group3_lowOCC <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$Occupation=="Lower-level")]=="Group 3")
  Ncontrols_PGS_Group3_highOCC <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$Occupation=="Upper-level")]=="Group 3")
  Ncases_PGS_Group3_lowOCC <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$Occupation=="Lower-level")]=="Group 3")
  Ncases_PGS_Group3_highOCC <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$Occupation=="Upper-level")]=="Group 3")
  Ncontrols_PGS_Group4_lowOCC <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$Occupation=="Lower-level")]=="Group 4")
  Ncontrols_PGS_Group4_highOCC <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$Occupation=="Upper-level")]=="Group 4")
  Ncases_PGS_Group4_lowOCC <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$Occupation=="Lower-level")]=="Group 4")
  Ncases_PGS_Group4_highOCC <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$Occupation=="Upper-level")]=="Group 4")
  Ncontrols_PGS_Group5_lowOCC <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$Occupation=="Lower-level")]=="Group 5")
  Ncontrols_PGS_Group5_highOCC <- sum(filelist$PGS_group[which(filelist[,15]==0 & filelist$Occupation=="Upper-level")]=="Group 5")
  Ncases_PGS_Group5_lowOCC <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$Occupation=="Lower-level")]=="Group 5")
  Ncases_PGS_Group5_highOCC <- sum(filelist$PGS_group[which(filelist[,15]==1 & filelist$Occupation=="Upper-level")]=="Group 5")
  
  # extract N and % females for the controls and cases per PGS category level and Occupation
  Ncontrols_females_PGS_Group1_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 1" & filelist$Occupation=="Lower-level")]==1), 
                                                 " (", 
                                                 round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 1" & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                                 "%)"))
  Ncontrols_females_PGS_Group1_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 1" & filelist$Occupation=="Upper-level")]==1), 
                                                  " (", 
                                                  round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 1" & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                                  "%)"))
  Ncases_females_PGS_Group1_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 1" & filelist$Occupation=="Lower-level")]==1), 
                                              " (", 
                                              round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 1" & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                              "%)"))
  Ncases_females_PGS_Group1_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 1" & filelist$Occupation=="Upper-level")]==1), 
                                               " (", 
                                               round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 1" & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                               "%)"))
  Ncontrols_females_PGS_Group2_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 2" & filelist$Occupation=="Lower-level")]==1), 
                                                 " (", 
                                                 round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 2" & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                                 "%)"))
  Ncontrols_females_PGS_Group2_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 2" & filelist$Occupation=="Upper-level")]==1), 
                                                  " (", 
                                                  round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 2" & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                                  "%)"))
  Ncases_females_PGS_Group2_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 2" & filelist$Occupation=="Lower-level")]==1), 
                                              " (", 
                                              round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 2" & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                              "%)"))
  Ncases_females_PGS_Group2_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 2" & filelist$Occupation=="Upper-level")]==1), 
                                               " (", 
                                               round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 2" & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                               "%)"))
  Ncontrols_females_PGS_Group3_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 3" & filelist$Occupation=="Lower-level")]==1), 
                                                 " (", 
                                                 round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 3" & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                                 "%)"))
  Ncontrols_females_PGS_Group3_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 3" & filelist$Occupation=="Upper-level")]==1), 
                                                  " (", 
                                                  round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 3" & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                                  "%)"))
  Ncases_females_PGS_Group3_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 3" & filelist$Occupation=="Lower-level")]==1), 
                                              " (", 
                                              round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 3" & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                              "%)"))
  Ncases_females_PGS_Group3_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 3" & filelist$Occupation=="Upper-level")]==1), 
                                               " (", 
                                               round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 3" & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                               "%)"))
  Ncontrols_females_PGS_Group4_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 4" & filelist$Occupation=="Lower-level")]==1), 
                                                 " (", 
                                                 round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 4" & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                                 "%)"))
  Ncontrols_females_PGS_Group4_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 4" & filelist$Occupation=="Upper-level")]==1), 
                                                  " (", 
                                                  round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 4" & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                                  "%)"))
  Ncases_females_PGS_Group4_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 4" & filelist$Occupation=="Lower-level")]==1), 
                                              " (", 
                                              round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 4" & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                              "%)"))
  Ncases_females_PGS_Group4_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 4" & filelist$Occupation=="Upper-level")]==1), 
                                               " (", 
                                               round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 4" & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                               "%)"))
  Ncontrols_females_PGS_Group5_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 5" & filelist$Occupation=="Lower-level")]==1), 
                                                 " (", 
                                                 round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 5" & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                                 "%)"))
  Ncontrols_females_PGS_Group5_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 5" & filelist$Occupation=="Upper-level")]==1), 
                                                  " (", 
                                                  round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$PGS_group=="Group 5" & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                                  "%)"))
  Ncases_females_PGS_Group5_lowOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 5" & filelist$Occupation=="Lower-level")]==1), 
                                              " (", 
                                              round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 5" & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                              "%)"))
  Ncases_females_PGS_Group5_highOCC <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 5" & filelist$Occupation=="Upper-level")]==1), 
                                               " (", 
                                               round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$PGS_group=="Group 5" & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                               "%)"))
  
  # combine all descriptives into a single data.frame
  dat <- as.data.frame(cbind(trait,Ncontrols,Ncases,Ncontrols_females,
                             Ncases_females,prevalence,AgeOnset_q25,
                             AgeOnstet_q50,AgeOnset_q75,AgeOnset_IQR,
                             Followup_IQR,Followup_Median,Ncontrols_lowOCC,
                             Ncases_lowOCC,Ncontrols_highOCC,Ncases_highOCC,
                             prevalence_lowOCC,prevalence_highOCC,
                             Ncontrols_females_lowOCC,Ncases_females_lowOCC,
                             Ncontrols_females_highOCC,Ncases_females_highOCC,
                             Ncontrols_PGS_Group1,Ncases_PGS_Group1,Ncontrols_PGS_Group2,
                             Ncases_PGS_Group2,Ncontrols_PGS_Group3,Ncases_PGS_Group3,
                             Ncontrols_PGS_Group4,Ncases_PGS_Group4,Ncontrols_PGS_Group5,
                             Ncases_PGS_Group5,Ncontrols_females_PGS_Group1,Ncases_females_PGS_Group1,
                             Ncontrols_females_PGS_Group2,Ncases_females_PGS_Group2,
                             Ncontrols_females_PGS_Group3,Ncases_females_PGS_Group3,
                             Ncontrols_females_PGS_Group4,Ncases_females_PGS_Group4,
                             Ncontrols_females_PGS_Group5,Ncases_females_PGS_Group5,
                             Ncontrols_PGS_Group1_lowOCC,Ncontrols_PGS_Group1_highOCC,
                             Ncases_PGS_Group1_lowOCC,Ncases_PGS_Group1_highOCC,
                             Ncontrols_PGS_Group2_lowOCC,Ncontrols_PGS_Group2_highOCC,
                             Ncases_PGS_Group2_lowOCC,Ncases_PGS_Group2_highOCC,
                             Ncontrols_PGS_Group3_lowOCC,Ncontrols_PGS_Group3_highOCC,
                             Ncases_PGS_Group3_lowOCC,Ncases_PGS_Group3_highOCC,
                             Ncontrols_PGS_Group4_lowOCC,Ncontrols_PGS_Group4_highOCC,
                             Ncases_PGS_Group4_lowOCC,Ncases_PGS_Group4_highOCC,
                             Ncontrols_PGS_Group5_lowOCC,Ncontrols_PGS_Group5_highOCC,
                             Ncases_PGS_Group5_lowOCC,Ncases_PGS_Group5_highOCC,
                             Ncontrols_females_PGS_Group1_lowOCC,Ncontrols_females_PGS_Group1_highOCC,
                             Ncases_females_PGS_Group1_lowOCC,Ncases_females_PGS_Group1_highOCC,
                             Ncontrols_females_PGS_Group2_lowOCC,Ncontrols_females_PGS_Group2_highOCC,
                             Ncases_females_PGS_Group2_lowOCC,Ncases_females_PGS_Group2_highOCC,
                             Ncontrols_females_PGS_Group3_lowOCC,Ncontrols_females_PGS_Group3_highOCC,
                             Ncases_females_PGS_Group3_lowOCC,Ncases_females_PGS_Group3_highOCC,
                             Ncontrols_females_PGS_Group4_lowOCC,Ncontrols_females_PGS_Group4_highOCC,
                             Ncases_females_PGS_Group4_lowOCC,Ncases_females_PGS_Group4_highOCC,
                             Ncontrols_females_PGS_Group5_lowOCC,Ncontrols_females_PGS_Group5_highOCC,
                             Ncases_females_PGS_Group5_lowOCC,Ncases_females_PGS_Group5_highOCC))
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
desc.table.comb$Ncontrols_females_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
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
desc.table.comb$Ncontrols_females_PGS_Group1_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group1_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group1_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group1_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group2_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group2_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group2_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group2_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group3_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group3_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group3_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group3_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group4_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group4_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group4_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group4_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group5_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_PGS_Group5_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group5_lowOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_PGS_Group5_highOCC[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
#
desc.table.comb$Ncontrols_females[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0)]==1), " (100%)")
desc.table.comb$Ncases_females[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1)]==1), " (100%)")
desc.table.comb$Ncontrols_females_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1)," (100%)")
desc.table.comb$Ncases_females_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1)," (100%)")
desc.table.comb$Ncases_females_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")
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
desc.table.comb$Ncontrols_females_PGS_Group1_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 1" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group1_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 1" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group1_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 1" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group1_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 1" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group2_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 2" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group2_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 2" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group2_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 2" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group2_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 2" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group3_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 3" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group3_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 3" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group3_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 3" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group3_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 3" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group4_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 4" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group4_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 4" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group4_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 4" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group4_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 4" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group5_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 5" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_PGS_Group5_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==0 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 5" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group5_lowOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 5" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncases_females_PGS_Group5_highOCC[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list2[["C3_BREAST"]]$SEX[which(INTERVENE.list2[["C3_BREAST"]][,15]==1 & INTERVENE.list2[["C3_BREAST"]]$PGS_group=="Group 5" & INTERVENE.list2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")

# write table with descriptives to output as tab-delimited text file
write.table(desc.table.comb, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                         "_",Biobank,"_INTERVENE_Occupation_PGSstrata_SampleDescriptives.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)