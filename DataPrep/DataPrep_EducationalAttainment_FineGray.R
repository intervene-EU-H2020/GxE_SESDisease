#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# educational attainment [EA]) in incidences of 18 common diseases (as
# previously selected in the INTERVENE flagship manuscript:
# https://doi.org/10.1101/2023.06.12.23291186) and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# educational attainment [EA]) in risk of 19 common diseases (as
# previously selected in the INTERVENE flagship manuscript + Alcohol Use
# Disorder: https://doi.org/10.1101/2023.06.12.23291186)
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Create combined phenotype and PRS RData files for running analyses
# where EA is the socioeconomic index and the competing risk is all-cause
# mortality
#
# Required input data: biobank-specific data file as made with
# DataPrep_EducationalAttainment.R
# script(https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/DataPrep_EducationalAttainment.R)
# and biobank-specific INTERVENE phenotype file with information on all-cause
# mortality
#
# Last edits: 30/06/2025 (FAH, edits: last edits and tweaks for GitHub upload)
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
# the number of cores you desire/have available; lubridate = to calculate etc.
# with dates; forcats, stringr, plyr & dplyr = data wrangling.
packages("data.table","foreach","doParallel","lubridate","plyr","dplyr",
         "forcats","stringr")

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

# load full sample
load("[PathToPhenotypeFile/PhenotypeFile.RData]")

# Read in the phenotype file with the "fread" command from the "data.table"
# package, and select only required columns (ID, DEATH, DEATH_AGE).
pheno <- fread("[PathToPhenotypeFile/PhenotypeFile]",data.table = F,
               select = c("ID","DEATH","DEATH_AGE","DEATH_YEAR"))


################################################################################
#
# Data preparation for analyses
#
################################################################################

# Append death information
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  merge(INTERVENE.list[[i]],pheno, by = "ID")
}


## create new event variable: 0 = control; 1 = disease-onset; 3 = death other causes ##

# function to calculate a new 'event' variable.
calc.event <- function(filelist) {
  event <- ifelse(filelist[,15]==1, 1, ifelse(filelist$DEATH==1, 2, 0))
  event <- factor(event, levels = c(0,1,2))
  return(event)
}

# apply new function and append to dataframes
INTERVENE.event <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  calc.event(filelist = INTERVENE.list[[i]])
}
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cbind(INTERVENE.list[[i]],event=INTERVENE.event[[i]])
}

# add premade interaction term as riskRegression package does not handle PRS:EA.
# Column 20 is the PGS column
INTERVENE.interaction <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  INTERVENE.list[[i]][,20] * as.numeric(INTERVENE.list[[i]]$EA)
}
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cbind(INTERVENE.list[[i]],interaction=INTERVENE.interaction[[i]])
}

# add trait names to list items 
names(INTERVENE.list) <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))

# write datasets to file 
save(INTERVENE.list, file = paste("[PathToOutputFolder/]", as.character(Sys.Date()),
                                  "_",Biobank,"_INTERVENE_EducationalAttainment_dat_FineGray.RData",sep = ""))
