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
# Script: calculate the summary statistics on the phenotype files including
# Educational Attainment and PGS strata
#
# Required input data: biobank-specific INTERVENE combined phenotype and PGS
# files for Fine-Gray analyses
#
# Last edits: 27/06/2025 (FAH, edits: last edits & tweaks prior to GitHub upload)
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
# Create descriptive tables for each of the 19 traits
#
################################################################################

# Function to create descriptive tables for each of the traits when EA is
# dichotomizd into low vs high EA and instead of using case/control we are using
# 'event' which is case (1), dead control (2), living control (0)
#
################################################################################

desc.table.FG.EA2 <- function(filelist) {
  # extract trait name
  trait <- names(filelist)[15]
  
  # extract N controls, N cases, and calculate prevalence
  Ncontrols <- sum(filelist$event==0)
  Ncases <- sum(filelist$event==1)
  Ndeathother <- sum(filelist$event==2)
  
  # extract N and % of females for the controls and cases
  Ncontrols_females <- c(paste0(sum(filelist$SEX[which(filelist$event==0)]==1), 
                                " (", 
                                round(prop.table(table(filelist$SEX[which(filelist$event==0)]))[2]*100,1), 
                                "%)"))
  Ncases_females <- c(paste0(sum(filelist$SEX[which(filelist$event==1)]==1), 
                             " (", 
                             round(prop.table(table(filelist$SEX[which(filelist$event==1)]))[2]*100,1), 
                             "%)"))
  Ndeathother_females <- c(paste0(sum(filelist$SEX[which(filelist$event==2)]==1), 
                                  " (", 
                                  round(prop.table(table(filelist$SEX[which(filelist$event==2)]))[2]*100,1), 
                                  "%)"))
  
  # extract N per EA level for the controls and cases
  Ncontrols_lowEA <- sum(filelist$EA[which(filelist$event==0)]=="low")
  Ncases_lowEA <- sum(filelist$EA[which(filelist$event==1)]=="low")
  Ndeathother_lowEA <- sum(filelist$EA[which(filelist$event==2)]=="low")
  Ncontrols_highEA <- sum(filelist$EA[which(filelist$event==0)]=="high")
  Ncases_highEA <- sum(filelist$EA[which(filelist$event==1)]=="high")
  Ndeathother_highEA <- sum(filelist$EA[which(filelist$event==2)]=="high")
  
  # extract N and % females for the controls and cases per EA level
  Ncontrols_females_lowEA <- c(paste0(sum(filelist$SEX[which(filelist$event==0 & filelist$EA=="low")]==1), 
                                      " (", 
                                      round(prop.table(table(filelist$SEX[which(filelist$event==0 & filelist$EA=="low")]))[2]*100,1), 
                                      "%)"))
  Ncases_females_lowEA <- c(paste0(sum(filelist$SEX[which(filelist$event==1 & filelist$EA=="low")]==1), 
                                   " (", 
                                   round(prop.table(table(filelist$SEX[which(filelist$event==1 & filelist$EA=="low")]))[2]*100,1), 
                                   "%)"))
  Ndeathother_females_lowEA <- c(paste0(sum(filelist$SEX[which(filelist$event==2 & filelist$EA=="low")]==1), 
                                        " (", 
                                        round(prop.table(table(filelist$SEX[which(filelist$event==2 & filelist$EA=="low")]))[2]*100,1), 
                                        "%)"))
  Ncontrols_females_highEA <- c(paste0(sum(filelist$SEX[which(filelist$event==0 & filelist$EA=="high")]==1), 
                                       " (", 
                                       round(prop.table(table(filelist$SEX[which(filelist$event==0 & filelist$EA=="high")]))[2]*100,1), 
                                       "%)"))
  Ncases_females_highEA <- c(paste0(sum(filelist$SEX[which(filelist$event==1 & filelist$EA=="high")]==1), 
                                    " (", 
                                    round(prop.table(table(filelist$SEX[which(filelist$event==1 & filelist$EA=="high")]))[2]*100,1), 
                                    "%)"))
  Ndeathother_females_highEA <- c(paste0(sum(filelist$SEX[which(filelist$event==2 & filelist$EA=="high")]==1), 
                                         " (", 
                                         round(prop.table(table(filelist$SEX[which(filelist$event==2 & filelist$EA=="high")]))[2]*100,1), 
                                         "%)"))
  
  # combine all descriptives into a single data.frame
  dat <- as.data.frame(cbind(trait,Ncontrols,Ncases,Ndeathother,
                             Ncontrols_females,Ncases_females,Ndeathother_females,
                             Ncontrols_lowEA,Ncases_lowEA,Ndeathother_lowEA,
                             Ncontrols_highEA,Ncases_highEA,Ndeathother_highEA,
                             Ncontrols_females_lowEA,Ncases_females_lowEA,Ndeathother_females_lowEA,
                             Ncontrols_females_highEA,Ncases_females_highEA,Ndeathother_females_highEA))
  return(dat)
}

#create descriptive table in a foreach loop for each of the traits 
desc.table.dat <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  desc.table.FG.EA2(filelist = INTERVENE.list[[i]])
}

# combine separate descriptives tables into 1 big table
desc.table.comb <- rbindlist(desc.table.dat)

# write table with descriptives to output as tab-delimited text file
write.table(desc.table.comb, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                         "_",Biobank,"_INTERVENE_EducationalAttainment_FineGray_SampleDescriptives.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)