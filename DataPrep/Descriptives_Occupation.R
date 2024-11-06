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
# Last edits: 19/04/2024 (FAH, edits: globalize script for use in other
# INTERVENE biobanks and upload to GitHub)
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

# each of the 3 PRS groups
load("[PathToPhenotypeFile/PhenotypeFile.RData]")
load("[PathToPhenotypeFile/PhenotypeFile.RData]")
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
  Ncontrols_Manualworker <- sum(filelist$Occupation[which(filelist[,15]==0)]=="Manual worker")
  Ncases_Manualworker <- sum(filelist$Occupation[which(filelist[,15]==1)]=="Manual worker")
  Ncontrols_Lowerlevel <- sum(filelist$Occupation[which(filelist[,15]==0)]=="Lower-level")
  Ncases_Lowerlevel <- sum(filelist$Occupation[which(filelist[,15]==1)]=="Lower-level")
  Ncontrols_Upperlevel <- sum(filelist$Occupation[which(filelist[,15]==0)]=="Upper-level")
  Ncases_Upperlevel <- sum(filelist$Occupation[which(filelist[,15]==1)]=="Upper-level")
  
  # calculate prevalences per EA level
  prevalence_Manualworker <- round(Ncases_Manualworker/(Ncases_Manualworker+Ncontrols_Manualworker)*100,2)
  prevalence_Lowerlevel <- round(Ncases_Lowerlevel/(Ncases_Lowerlevel+Ncontrols_Lowerlevel)*100,2)
  prevalence_Upperlevel <- round(Ncases_Upperlevel/(Ncases_Upperlevel+Ncontrols_Upperlevel)*100,2)
  
  # extract N and % females for the controls and cases per EA level
  Ncontrols_females_Manualworker <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Manual worker")]==1), 
                                             " (", 
                                             round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Manual worker")]))[2]*100,1), 
                                             "%)"))
  Ncases_females_Manualworker <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Manual worker")]==1), 
                                          " (", 
                                          round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Manual worker")]))[2]*100,1), 
                                          "%)"))
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
                             Followup_IQR,Followup_Median,Ncontrols_Manualworker,
                             Ncases_Manualworker,Ncontrols_Lowerlevel,
                             Ncases_Lowerlevel,Ncontrols_Upperlevel,
                             Ncases_Upperlevel,prevalence_Manualworker,
                             prevalence_Lowerlevel,prevalence_Upperlevel,
                             Ncontrols_females_Manualworker,Ncases_females_Manualworker,
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
desc.table.comb$Ncontrols_females_Manualworker[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_Manualworker[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_Lowerlevel[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_Lowerlevel[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncontrols_females_Upperlevel[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb$Ncases_females_Upperlevel[which(desc.table.comb$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
#
desc.table.comb$Ncontrols_females[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==0)]==1), " (100%)")
desc.table.comb$Ncases_females[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==1)]==1), " (100%)")
desc.table.comb$Ncontrols_females_Manualworker[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==0 & INTERVENE.list[["C3_BREAST"]]$Occupation=="Manual worker")]==1)," (100%)")
desc.table.comb$Ncases_females_Manualworker[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==1 & INTERVENE.list[["C3_BREAST"]]$Occupation=="Manual worker")]==1), " (100%)")
desc.table.comb$Ncontrols_females_Lowerlevel[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==0 & INTERVENE.list[["C3_BREAST"]]$Occupation=="Lower-level")]==1)," (100%)")
desc.table.comb$Ncases_females_Lowerlevel[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==1 & INTERVENE.list[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb$Ncontrols_females_Upperlevel[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==0 & INTERVENE.list[["C3_BREAST"]]$Occupation=="Upper-level")]==1)," (100%)")
desc.table.comb$Ncases_females_Upperlevel[which(desc.table.comb$trait=="C3_BREAST")] <- paste0(sum(INTERVENE.list[["C3_BREAST"]]$SEX[which(INTERVENE.list[["C3_BREAST"]][,15]==1 & INTERVENE.list[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")

# write table with descriptives to output as tab-delimited text file
write.table(desc.table.comb, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                         "_",Biobank,"_INTERVENE_Occupation_SampleDescriptives.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)

## Group 1 ## 
#create descriptive table in a foreach loop foreach of the 19 traits (function
#from source code).
desc.table.Group1 <- foreach(i=1:length(Group1)) %dopar% {
  desc.table.Occ(filelist = Group1[[i]])
}

# combine separate descriptives tables into 1 big table
desc.table.comb.Group1 <- rbindlist(desc.table.Group1)

# as prostate cancer is only assessed in males and breast cancer only in
# females, fix the % to reflect this
desc.table.comb.Group1$Ncontrols_females[which(desc.table.comb.Group1$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group1$Ncases_females[which(desc.table.comb.Group1$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group1$Ncontrols_females_Manualworker[which(desc.table.comb.Group1$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group1$Ncases_females_Manualworker[which(desc.table.comb.Group1$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group1$Ncontrols_females_Lowerlevel[which(desc.table.comb.Group1$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group1$Ncases_females_Lowerlevel[which(desc.table.comb.Group1$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group1$Ncontrols_females_Upperlevel[which(desc.table.comb.Group1$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group1$Ncases_females_Upperlevel[which(desc.table.comb.Group1$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
#
desc.table.comb.Group1$Ncontrols_females[which(desc.table.comb.Group1$trait=="C3_BREAST")] <- paste0(sum(Group1[["C3_BREAST"]]$SEX[which(Group1[["C3_BREAST"]][,15]==0)]==1), " (100%)")
desc.table.comb.Group1$Ncases_females[which(desc.table.comb.Group1$trait=="C3_BREAST")] <- paste0(sum(Group1[["C3_BREAST"]]$SEX[which(Group1[["C3_BREAST"]][,15]==1)]==1), " (100%)")
desc.table.comb.Group1$Ncontrols_females_Manualworker[which(desc.table.comb.Group1$trait=="C3_BREAST")] <- paste0(sum(Group1[["C3_BREAST"]]$SEX[which(Group1[["C3_BREAST"]][,15]==0 & Group1[["C3_BREAST"]]$Occupation=="Manual worker")]==1)," (100%)")
desc.table.comb.Group1$Ncases_females_Manualworker[which(desc.table.comb.Group1$trait=="C3_BREAST")] <- paste0(sum(Group1[["C3_BREAST"]]$SEX[which(Group1[["C3_BREAST"]][,15]==1 & Group1[["C3_BREAST"]]$Occupation=="Manual worker")]==1), " (100%)")
desc.table.comb.Group1$Ncontrols_females_Lowerlevel[which(desc.table.comb.Group1$trait=="C3_BREAST")] <- paste0(sum(Group1[["C3_BREAST"]]$SEX[which(Group1[["C3_BREAST"]][,15]==0 & Group1[["C3_BREAST"]]$Occupation=="Lower-level")]==1)," (100%)")
desc.table.comb.Group1$Ncases_females_Lowerlevel[which(desc.table.comb.Group1$trait=="C3_BREAST")] <- paste0(sum(Group1[["C3_BREAST"]]$SEX[which(Group1[["C3_BREAST"]][,15]==1 & Group1[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb.Group1$Ncontrols_females_Upperlevel[which(desc.table.comb.Group1$trait=="C3_BREAST")] <- paste0(sum(Group1[["C3_BREAST"]]$SEX[which(Group1[["C3_BREAST"]][,15]==0 & Group1[["C3_BREAST"]]$Occupation=="Upper-level")]==1)," (100%)")
desc.table.comb.Group1$Ncases_females_Upperlevel[which(desc.table.comb.Group1$trait=="C3_BREAST")] <- paste0(sum(Group1[["C3_BREAST"]]$SEX[which(Group1[["C3_BREAST"]][,15]==1 & Group1[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")

# adjust names of descriptive table
names(desc.table.comb.Group1)[2:length(desc.table.comb.Group1)] <- gsub(".*^","Group1_",names(desc.table.comb.Group1)[2:length(desc.table.comb.Group1)])

## Group 2 ## 
#create descriptive table in a foreach loop foreach of the 19 traits (function
#from source code).
desc.table.Group2 <- foreach(i=1:length(Group2)) %dopar% {
  desc.table.Occ(filelist = Group2[[i]])
}

# combine separate descriptives tables into 1 big table
desc.table.comb.Group2 <- rbindlist(desc.table.Group2)

# as prostate cancer is only assessed in males and breast cancer only in
# females, fix the % to reflect this
desc.table.comb.Group2$Ncontrols_females[which(desc.table.comb.Group2$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group2$Ncases_females[which(desc.table.comb.Group2$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group2$Ncontrols_females_Manualworker[which(desc.table.comb.Group2$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group2$Ncases_females_Manualworker[which(desc.table.comb.Group2$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group2$Ncontrols_females_Lowerlevel[which(desc.table.comb.Group2$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group2$Ncases_females_Lowerlevel[which(desc.table.comb.Group2$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group2$Ncontrols_females_Upperlevel[which(desc.table.comb.Group2$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group2$Ncases_females_Upperlevel[which(desc.table.comb.Group2$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
#
desc.table.comb.Group2$Ncontrols_females[which(desc.table.comb.Group2$trait=="C3_BREAST")] <- paste0(sum(Group2[["C3_BREAST"]]$SEX[which(Group2[["C3_BREAST"]][,15]==0)]==1), " (100%)")
desc.table.comb.Group2$Ncases_females[which(desc.table.comb.Group2$trait=="C3_BREAST")] <- paste0(sum(Group2[["C3_BREAST"]]$SEX[which(Group2[["C3_BREAST"]][,15]==1)]==1), " (100%)")
desc.table.comb.Group2$Ncontrols_females_Manualworker[which(desc.table.comb.Group2$trait=="C3_BREAST")] <- paste0(sum(Group2[["C3_BREAST"]]$SEX[which(Group2[["C3_BREAST"]][,15]==0 & Group2[["C3_BREAST"]]$Occupation=="Manual worker")]==1)," (100%)")
desc.table.comb.Group2$Ncases_females_Manualworker[which(desc.table.comb.Group2$trait=="C3_BREAST")] <- paste0(sum(Group2[["C3_BREAST"]]$SEX[which(Group2[["C3_BREAST"]][,15]==1 & Group2[["C3_BREAST"]]$Occupation=="Manual worker")]==1), " (100%)")
desc.table.comb.Group2$Ncontrols_females_Lowerlevel[which(desc.table.comb.Group2$trait=="C3_BREAST")] <- paste0(sum(Group2[["C3_BREAST"]]$SEX[which(Group2[["C3_BREAST"]][,15]==0 & Group2[["C3_BREAST"]]$Occupation=="Lower-level")]==1)," (100%)")
desc.table.comb.Group2$Ncases_females_Lowerlevel[which(desc.table.comb.Group2$trait=="C3_BREAST")] <- paste0(sum(Group2[["C3_BREAST"]]$SEX[which(Group2[["C3_BREAST"]][,15]==1 & Group2[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb.Group2$Ncontrols_females_Upperlevel[which(desc.table.comb.Group2$trait=="C3_BREAST")] <- paste0(sum(Group2[["C3_BREAST"]]$SEX[which(Group2[["C3_BREAST"]][,15]==0 & Group2[["C3_BREAST"]]$Occupation=="Upper-level")]==1)," (100%)")
desc.table.comb.Group2$Ncases_females_Upperlevel[which(desc.table.comb.Group2$trait=="C3_BREAST")] <- paste0(sum(Group2[["C3_BREAST"]]$SEX[which(Group2[["C3_BREAST"]][,15]==1 & Group2[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")

# adjust names of descriptive table
names(desc.table.comb.Group2)[2:length(desc.table.comb.Group2)] <- gsub(".*^","Group2_",names(desc.table.comb.Group2)[2:length(desc.table.comb.Group2)])

## Group 3 ## 
#create descriptive table in a foreach loop foreach of the 19 traits (function
#from source code).
desc.table.Group3 <- foreach(i=1:length(Group3)) %dopar% {
  desc.table.Occ(filelist = Group3[[i]])
}

# combine separate descriptives tables into 1 big table
desc.table.comb.Group3 <- rbindlist(desc.table.Group3)

# as prostate cancer is only assessed in males and breast cancer only in
# females, fix the % to reflect this
desc.table.comb.Group3$Ncontrols_females[which(desc.table.comb.Group3$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group3$Ncases_females[which(desc.table.comb.Group3$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group3$Ncontrols_females_Manualworker[which(desc.table.comb.Group3$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group3$Ncases_females_Manualworker[which(desc.table.comb.Group3$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group3$Ncontrols_females_Lowerlevel[which(desc.table.comb.Group3$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group3$Ncases_females_Lowerlevel[which(desc.table.comb.Group3$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group3$Ncontrols_females_Upperlevel[which(desc.table.comb.Group3$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
desc.table.comb.Group3$Ncases_females_Upperlevel[which(desc.table.comb.Group3$trait=="C3_PROSTATE")] <- paste0("0 (0%)")
#
desc.table.comb.Group3$Ncontrols_females[which(desc.table.comb.Group3$trait=="C3_BREAST")] <- paste0(sum(Group3[["C3_BREAST"]]$SEX[which(Group3[["C3_BREAST"]][,15]==0)]==1), " (100%)")
desc.table.comb.Group3$Ncases_females[which(desc.table.comb.Group3$trait=="C3_BREAST")] <- paste0(sum(Group3[["C3_BREAST"]]$SEX[which(Group3[["C3_BREAST"]][,15]==1)]==1), " (100%)")
desc.table.comb.Group3$Ncontrols_females_Manualworker[which(desc.table.comb.Group3$trait=="C3_BREAST")] <- paste0(sum(Group3[["C3_BREAST"]]$SEX[which(Group3[["C3_BREAST"]][,15]==0 & Group3[["C3_BREAST"]]$Occupation=="Manual worker")]==1)," (100%)")
desc.table.comb.Group3$Ncases_females_Manualworker[which(desc.table.comb.Group3$trait=="C3_BREAST")] <- paste0(sum(Group3[["C3_BREAST"]]$SEX[which(Group3[["C3_BREAST"]][,15]==1 & Group3[["C3_BREAST"]]$Occupation=="Manual worker")]==1), " (100%)")
desc.table.comb.Group3$Ncontrols_females_Lowerlevel[which(desc.table.comb.Group3$trait=="C3_BREAST")] <- paste0(sum(Group3[["C3_BREAST"]]$SEX[which(Group3[["C3_BREAST"]][,15]==0 & Group3[["C3_BREAST"]]$Occupation=="Lower-level")]==1)," (100%)")
desc.table.comb.Group3$Ncases_females_Lowerlevel[which(desc.table.comb.Group3$trait=="C3_BREAST")] <- paste0(sum(Group3[["C3_BREAST"]]$SEX[which(Group3[["C3_BREAST"]][,15]==1 & Group3[["C3_BREAST"]]$Occupation=="Lower-level")]==1), " (100%)")
desc.table.comb.Group3$Ncontrols_females_Upperlevel[which(desc.table.comb.Group3$trait=="C3_BREAST")] <- paste0(sum(Group3[["C3_BREAST"]]$SEX[which(Group3[["C3_BREAST"]][,15]==0 & Group3[["C3_BREAST"]]$Occupation=="Upper-level")]==1)," (100%)")
desc.table.comb.Group3$Ncases_females_Upperlevel[which(desc.table.comb.Group3$trait=="C3_BREAST")] <- paste0(sum(Group3[["C3_BREAST"]]$SEX[which(Group3[["C3_BREAST"]][,15]==1 & Group3[["C3_BREAST"]]$Occupation=="Upper-level")]==1), " (100%)")

# adjust names of descriptive table
names(desc.table.comb.Group3)[2:length(desc.table.comb.Group3)] <- gsub(".*^","Group3_",names(desc.table.comb.Group3)[2:length(desc.table.comb.Group3)])

# combine into 1 dataset 
desc.table.comb.all <- cbind(desc.table.comb.Group1,desc.table.comb.Group2,desc.table.comb.Group3)

# write table with descriptives to output as tab-delimited text file
write.table(desc.table.comb.all, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                            "_",Biobank,"_INTERVENE_Occupation_SampleDescriptives_byPGS3group.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
