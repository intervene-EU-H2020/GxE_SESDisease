#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# occupation) in risk of 19 common diseases (as previously selected in the
# INTERVENE flagship manuscript + Alcohol Use Disorder:
# https://doi.org/10.1101/2023.06.12.23291186)
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: calculate the summary statistics on the phenotype files including
# Occupation
#
# Required input data: biobank-specific INTERVENE combined phenotype and PGS
# files
#
# Last edits: 19/06/2025 (FAH, edits: remove PGS groups and update code to 
# reflect that occupation is now 2 groups: "lower-level" vs "upper-level")
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
# Load Data
#
################################################################################

# full sample
load("[PathToPhenotypeFile/PhenotypeFile.RData]")


################################################################################
#
# Create descriptive tables for each of the 19 traits 
#
################################################################################

# Function to create descriptive tables for each of the traits for Occupation Status
desc.table.Occ <- function(filelist) {
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
  Ncontrols_Lowerlevel <- sum(filelist$Occupation[which(filelist[,15]==0)]=="Lower-level")
  Ncases_Lowerlevel <- sum(filelist$Occupation[which(filelist[,15]==1)]=="Lower-level")
  Ncontrols_Upperlevel <- sum(filelist$Occupation[which(filelist[,15]==0)]=="Upper-level")
  Ncases_Upperlevel <- sum(filelist$Occupation[which(filelist[,15]==1)]=="Upper-level")
  
  # calculate prevalences per EA level
  prevalence_Lowerlevel <- round(Ncases_Lowerlevel/(Ncases_Lowerlevel+Ncontrols_Lowerlevel)*100,2)
  prevalence_Upperlevel <- round(Ncases_Upperlevel/(Ncases_Upperlevel+Ncontrols_Upperlevel)*100,2)
  
  # extract N and % females for the controls and cases per EA level
  Ncontrols_females_Lowerlevel <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Lower-level")]==1), 
                                           " (", 
                                           round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                           "%)"))
  Ncases_females_Lowerlevel <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Lower-level")]==1), 
                                        " (", 
                                        round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Lower-level")]))[2]*100,1), 
                                        "%)"))
  Ncontrols_females_Upperlevel <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Upper-level")]==1), 
                                           " (", 
                                           round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                           "%)"))
  Ncases_females_Upperlevel <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Upper-level")]==1), 
                                        " (", 
                                        round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Upper-level")]))[2]*100,1), 
                                        "%)"))
  
  # combine all descriptives into a single data.frame
  dat <- as.data.frame(cbind(trait,Ncontrols,Ncases,Ncontrols_females,
                             Ncases_females,prevalence,AgeOnset_q25,
                             AgeOnstet_q50,AgeOnset_q75,AgeOnset_IQR,
                             Followup_IQR,Followup_Median,Ncontrols_Lowerlevel,
                             Ncases_Lowerlevel,Ncontrols_Upperlevel,
                             Ncases_Upperlevel,prevalence_Lowerlevel,prevalence_Upperlevel,
                             Ncontrols_females_Lowerlevel,Ncases_females_Lowerlevel,
                             Ncontrols_females_Upperlevel,Ncases_females_Upperlevel))
  return(dat)
}

## all PGS strata ##
#create descriptive table in a foreach loop foreach of the 19 traits (function
#from source code).
desc.table.dat <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  desc.table.Occ(filelist = INTERVENE.list[[i]])
}

# combine separate descriptives tables into 1 big table
desc.table.comb <- rbindlist(desc.table.dat)

# as prostate cancer is only assessed in males and breast cancer only in
# females, fix the % to reflect this
desc.table.comb$Ncontrols_females[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_Lowerlevel[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_Lowerlevel[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_Upperlevel[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_Upperlevel[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
#
desc.table.comb$Ncontrols_females[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==0)]==1), " (100%)")
desc.table.comb$Ncases_females[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==1)]==1), " (100%)")
desc.table.comb$Ncontrols_females_Lowerlevel[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==0 & INTERVENE.list[["C3_BREAST"]]$Occupation=="Lower-level")]==1)," (100%)")
desc.table.comb$Ncases_females_Lowerlevel[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==1 & INTERVENE.list[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_Upperlevel[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==0 & INTERVENE.list[["C3_BREAST"]]$Occupation=="Upper-level")]==1)," (100%)")
desc.table.comb$Ncases_females_Upperlevel[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==1 & INTERVENE.list[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")

# write table with descriptives to output as tab-delimited text file
write.table(desc.table.comb, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                         "_",Biobank,"_INTERVENE_Occupation_SampleDescriptives.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
