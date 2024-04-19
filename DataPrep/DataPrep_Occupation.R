#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# occupation) in incidences of 19 common diseases (as previously selected in the
# INTERVENE flagship manuscript + Alcohol Use Disorder:
# https://doi.org/10.1101/2023.06.12.23291186)
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Create combined phenotype and PRS RData files for running analyses
# where occupation is the socioeconomic index
#
# Required input data: biobank-specific INTERVENE phenotype file and
# biobank-specific INTERVENE PGS files
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

# Read in the phenotype file with the "fread" command from the "data.table"
# package.
pheno <- fread("[PathToPhenotypeFile/PhenotypeFile]",data.table = F)

# Read in file with extra phenotype information, incl. Occupation, with the
# "fread" command from the "data.table" package
pheno2 <- fread("[PathToPhenotypeFile/PhenotypeFile]",data.table = F)

# create list of PGS files
filelist <- c(list.files(path=as.character("[PathToPRSFiles/]"), pattern="*.sscore",recursive = T,full.names = T))

# read in PGS files in a list    
files = lapply(filelist, function(x)fread(x, data.table = F)) 

# rename the PGS scores in each file to reflect the phenoype it was generated
# for and replace the .sscore with _prs to reflect naming convention in INTERVENE
# flagship scripts
for(i in 1:length(files))  {
  names(files[[i]])[2] <- gsub("[PathToPRSFiles//]","", #Note, you need to add the double // here to ensure gsub actually removes the / 
                               gsub(".sscore","_prs",filelist[i]))
}

# merge list of PGS files into 1 df
PGS <- Reduce(function(x,y) merge(x = x, y = y, by = c("UKBID"),all=T), files)

################################################################################
#
# Data preparation for analyses
#
################################################################################

## recode variables from phenotype file ##
# sex as dummy variables where 0 = males and 1 = females
pheno$SEX <- as.numeric(factor(pheno$SEX, 
                               levels = c("male","female")))-1

# function to calculate Occupation Status from the 20277-0.0 field in UKB.
calc.Occupation <- function(dat) {
  # create empty variable
  Occupation <- rep(NA,nrow(dat))
  
  # if else in a loop to assign Occupation status 
  for (i in 1:nrow(dat)) {
    # upper level = [1XXX] managers and senior officials; [2XXX] professional occupations; [3XXX] associate professional and technical occupations
    if(!is.na(dat$`20277-0.0`[i]) & (grepl("^1", dat$`20277-0.0`[i]) | grepl("^2", dat$`20277-0.0`[i]) | grepl("^3", dat$`20277-0.0`[i]))) {
      Occupation[i] <- "Upper-level"
    } 
    # lower level = [4XXX] administrative and secretarial occupations; [6XXX] personal service occupations; [7XXX] sales and customer service occupations; [92XX] Elementary occupations (elementary administration and service occupations)
    else if(!is.na(dat$`20277-0.0`[i]) & (grepl("^4", dat$`20277-0.0`[i]) | grepl("^6", dat$`20277-0.0`[i]) | grepl("^7", dat$`20277-0.0`[i]) | grepl("^92", dat$`20277-0.0`[i]))) {
      Occupation[i] <- "Lower-level"
    } 
    # manual work = [5XXX] skilled trades occupations; [8XXX] process, plant and machine operatives; [91XX] Elementary occupations (elementary trades, plant and storage related occupations)
    else if(!is.na(dat$`20277-0.0`[i]) & (grepl("^5", dat$`20277-0.0`[i]) | grepl("^8", dat$`20277-0.0`[i]) | grepl("^91", dat$`20277-0.0`[i]))) {
      Occupation[i] <- "Manual worker"
    }  
  }
  
  # convert Occupation Status to factor
  Occupation <- factor(Occupation, levels = c("Manual worker","Lower-level",
                                              "Upper-level"))
  
  # output Occupation
  return(Occupation)
}

# create Occupation variable
pheno2$Occupation <- calc.Occupation(dat = pheno2)

# merge phenotypic files
phenos <- merge(pheno,pheno2[,c("eid","Occupation")], by.x = "ID", by.y = "eid")

# merge phenotypic and PGS files
INTERVENE <- merge(phenos,PGS,by.x = "ID",by.y = "UKBID")

# check whether all individuals are of EU ancestry, if not remove non-EU
# ancestry individuals
sum(!INTERVENE$ANCESTRY=="EUR",na.rm = T) #0 individuals no need for further filtering

# separate vectors for each of the 18 diseases included in the INTERVENE
# flagship manuscript and Alcohol use disorder for UKB Besides the case-control
# indicator for each disease + the event date (naming convention from FinnGen)
# and the PRSs (names as downloaded), also include the following variables
# (rename if required):
# "ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8",
# "PC9","PC10","ANCESTRY",and "END_OF_FOLLOWUP" (should mostly have been
# included in the phenotype file) + newly created occupation index + (if
# relevant) biobank specific additional (technical covariates).
# Note, please keep variables in this order and add additionally required
# variables at the end (otherwise the scripts will run on the wrong variable or
# not at all)
T1D = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","T1D","T1D_DATE",
                         "END_OF_FOLLOWUP","Occupation","T1D_prs")]) 
PC = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5"
                        ,"PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_PROSTATE","C3_PROSTATE_DATE",
                        "END_OF_FOLLOWUP","Occupation","Prostate_Cancer_prs")]) 
T2D = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","T2D","T2D_DATE",
                         "END_OF_FOLLOWUP","Occupation","T2D_prs")]) 
GOUT = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                          "PC6","PC7","PC8","PC9","PC10","ANCESTRY","GOUT","GOUT_DATE",
                          "END_OF_FOLLOWUP","Occupation","Gout_prs")]) 
RA = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                        "PC6","PC7","PC8","PC9","PC10","ANCESTRY","RHEUMA_SEROPOS_OTH","RHEUMA_SEROPOS_OTH_DATE",
                        "END_OF_FOLLOWUP","Occupation","Rheumatoid_Arthritis_prs")]) 
BC = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                        "PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_BREAST","C3_BREAST_DATE",
                        "END_OF_FOLLOWUP","Occupation","Breast_Cancer_prs")]) 
AF = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                        "PC6","PC7","PC8","PC9","PC10","ANCESTRY","I9_AF","I9_AF_DATE",
                        "END_OF_FOLLOWUP","Occupation","Atrial_Fibrillation_prs")]) 
CC = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                        "PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_COLORECTAL","C3_COLORECTAL_DATE",
                        "END_OF_FOLLOWUP","Occupation","Colorectal_Cancer_prs")]) 
ASTHMA = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                            "PC6","PC7","PC8","PC9","PC10","ANCESTRY","J10_ASTHMA","J10_ASTHMA_DATE",
                            "END_OF_FOLLOWUP","Occupation","Asthma_prs")]) 
CHD = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","I9_CHD","I9_CHD_DATE",
                         "END_OF_FOLLOWUP","Occupation","CHD_prs")]) 
HIP = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","COX_ARTHROSIS","COX_ARTHROSIS_DATE",
                         "END_OF_FOLLOWUP","Occupation","Hip_Osteoarthritis_prs")]) 
KNEE = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                          "PC6","PC7","PC8","PC9","PC10","ANCESTRY","KNEE_ARTHROSIS","KNEE_ARTHROSIS_DATE",
                          "END_OF_FOLLOWUP","Occupation","Knee_Osteoarthritis_prs")]) 
SKIN = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                          "PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_MELANOMA_SKIN","C3_MELANOMA_SKIN_DATE",
                          "END_OF_FOLLOWUP","Occupation","Melanoma_prs")]) 
LC = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                        "PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_BRONCHUS_LUNG","C3_BRONCHUS_LUNG_DATE",
                        "END_OF_FOLLOWUP","Occupation","Lung_Cancer_prs")]) 
MDD = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","F5_DEPRESSIO","F5_DEPRESSIO_DATE",
                         "END_OF_FOLLOWUP","Occupation","MDD_prs")]) 
CANCER = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                            "PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_CANCER","C3_CANCER_DATE",
                            "END_OF_FOLLOWUP","Occupation","AllCancers_prs")]) 
EPILEP = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                            "PC6","PC7","PC8","PC9","PC10","ANCESTRY","G6_EPLEPSY","G6_EPLEPSY_DATE",
                            "END_OF_FOLLOWUP","Occupation","Epilepsy_prs")])
APPC = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                          "PC6","PC7","PC8","PC9","PC10","ANCESTRY","K11_APPENDACUT","K11_APPENDACUT_DATE",
                          "END_OF_FOLLOWUP","Occupation","Appendicitis_prs")]) 
AUD = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","AUD_SWEDISH","AUD_SWEDISH_DATE",
                         "END_OF_FOLLOWUP","Occupation","Alcohol_Use_Disorder_prs")]) 

# based on the list of column name vectors produced in the previous step, create
# a list with data.frames for each of the 5 disorders.
INTERVENE.list <- list(T1D = INTERVENE[,which(names(INTERVENE) %in% T1D)],
                       PC = INTERVENE[,which(names(INTERVENE) %in% PC)],
                       T2D = INTERVENE[,which(names(INTERVENE) %in% T2D)],
                       GOUT = INTERVENE[,which(names(INTERVENE) %in% GOUT)],
                       RA = INTERVENE[,which(names(INTERVENE) %in% RA)],
                       BC = INTERVENE[,which(names(INTERVENE) %in% BC)],
                       AF = INTERVENE[,which(names(INTERVENE) %in% AF)],
                       CC = INTERVENE[,which(names(INTERVENE) %in% CC)],
                       ASTHMA = INTERVENE[,which(names(INTERVENE) %in% ASTHMA)],
                       CHD = INTERVENE[,which(names(INTERVENE) %in% CHD)],
                       HIP = INTERVENE[,which(names(INTERVENE) %in% HIP)],
                       KNEE = INTERVENE[,which(names(INTERVENE) %in% KNEE)],
                       SKIN = INTERVENE[,which(names(INTERVENE) %in% SKIN)],
                       LC = INTERVENE[,which(names(INTERVENE) %in% LC)],
                       MDD = INTERVENE[,which(names(INTERVENE) %in% MDD)],
                       CANCER = INTERVENE[,which(names(INTERVENE) %in% CANCER)],
                       EPILEP = INTERVENE[,which(names(INTERVENE) %in% EPILEP)],
                       APPC = INTERVENE[,which(names(INTERVENE) %in% APPC)],
                       AUD = INTERVENE[which(names(INTERVENE) %in% AUD)])

# function to calculate age at event (i.e., age at onset) for cases (date event
# minus birthday. %..% calculates and interval between two dates and by dividing
# the interval in years age is expressed in years (lubridate package). trunc
# expresses age to full year (truncation towards zero). For controls, replace it
# with the age at the end of follow-up (date end of follow-up minus birthday)
calc.age <- function(filelist) {
  age <- ifelse(filelist[[15]]==1, #15th column is always the phenotype
                (filelist$DATE_OF_BIRTH %--% filelist[, names(filelist[grep("_DATE", names(filelist))])]) / years(1),
                (filelist$DATE_OF_BIRTH %--% filelist$END_OF_FOLLOWUP) / years(1))
  return(age)
}
# run function to calculate age in a parallel foreach loop for each of the
# traits.
INTERVENE.AGE <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  calc.age(filelist = INTERVENE.list[[i]])
}

# append the new age vector to the data frames of each trait
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cbind(INTERVENE.list[[i]],AGE=INTERVENE.AGE[[i]])
}

# according to the INTERVENE flagship follow-up of participants started at birth
# and ended with (1) age of first record of disease diagnosis, (2) age at death
# for non-disease cause, (3) age at last available EHR, or (4) age 80. Under the
# assumption that the first 3 criteria have been incorporated in the
# specification of date of event or end of follow-up, we now need to remove
# everyone of 80 years or older from the data frames. In addition, remove
# everyone younger than 34.8 as that's the median age at which Finnish
# individuals obtain their PhD.
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  INTERVENE.list[[i]][which(INTERVENE.list[[i]]$AGE>34.8 & INTERVENE.list[[i]]$AGE<80),] 
}

# in each data set remove rows without case/control information, occupation
# information, age, and PGS. The 15th column always contains the
# trait information, the 18th column the occupation information, the 19th column
# the PGS, and the 20th age
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  INTERVENE.list[[i]][complete.cases(INTERVENE.list[[i]][,c(15,18:20)]),] 
}

# Check whether the PGS for each of the traits behave as expected. Use the
# glm function (stats package, included in the basic/auto-loaded R packages)
# with binomial family to account for the case/control binary trait outcomes.
# 15th column is always the trait column and the 19th the PRS column. Include
# sex (except for breast and prostate cancer), age, first 10 genetic PCs.
glm.PGS.res <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  if(names(INTERVENE.list[[i]][15])=="C3_PROSTATE" | names(INTERVENE.list[[i]][15])=="C3_BREAST") {
    summary(glm(as.formula(paste0(names(INTERVENE.list[[i]][15]), " ~ ",
                                  names(INTERVENE.list[[i]][19]),
                                  " + AGE + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")),
                family = binomial, data = INTERVENE.list[[i]]))$coefficients[2,]
  } else {
    summary(glm(as.formula(paste0(names(INTERVENE.list[[i]][15]), " ~ ",
                                  names(INTERVENE.list[[i]][19]),
                                  " + SEX + AGE + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")),
                family = binomial, data = INTERVENE.list[[i]]))$coefficients[2,]
  }
}

# combine results in 1 data frame
glm.PGS.res.tab <- as.data.frame(matrix(NA, ncol = 0, nrow = length(INTERVENE.list))) # create empty dataframe with rows equal to phenotypes
glm.PGS.res.tab$Trait <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))
glm.PGS.res.tab$PRS <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[19] })))
glm.PGS.res.tab$Estimate <- c(unlist(lapply(glm.PGS.res, function(x) { x[1] })))
glm.PGS.res.tab$SE <- c(unlist(lapply(glm.PGS.res, function(x) { x[2] })))
glm.PGS.res.tab$P <- c(unlist(lapply(glm.PGS.res, function(x) { x[4] })))

# every association should be positive (>PRS associated with case status), when
# this is not the case a strand flippage likely occurred and the PGS needs to be
# recoded.
neg <- as.numeric(rownames(glm.PGS.res.tab[which(glm.PGS.res.tab$Estimate<0),])) # vector with the row numbers of the PGSs with negative associations with their phenotype

# recode PGS
for (i in 1:length(INTERVENE.list)) {
  if(i %in% neg) {
    INTERVENE.list[[i]][19] <- INTERVENE.list[[i]][19]*-1
  }
}

# remove unneeded files
rm(glm.PGS.res.tab,glm.PGS.res)

# Calculate PGS tertiles and create groups for each
INTERVENE.tert <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cut(x = INTERVENE.list[[i]][,19], breaks = quantile(INTERVENE.list[[i]][,19], probs = c(c(0,0.25),rev(1-c(0,0.25)))), include.lowest = T,
      labels = paste("Group",1:(2*length(c(0,0.25))-1)))
}

# append the new tertile vector to the data frames for each trait
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cbind(INTERVENE.list[[i]],PGS_group=INTERVENE.tert[[i]])
}

# scale PGS
for (i in 1:length(INTERVENE.list)) {
  INTERVENE.list[[i]][,19] <- scale(INTERVENE.list[[i]][,19])
}

# split data by PGS strata
Group1 <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  subset(INTERVENE.list[[i]],PGS_group=="Group 1")
}
Group2 <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  subset(INTERVENE.list[[i]],PGS_group=="Group 2")
}
Group3 <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  subset(INTERVENE.list[[i]],PGS_group=="Group 3")
}

# add trait names to list items 
names(INTERVENE.list) <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))
names(Group1) <- c(unlist(lapply(Group1, function(x) { names(x)[15] })))
names(Group2) <- c(unlist(lapply(Group2, function(x) { names(x)[15] })))
names(Group3) <- c(unlist(lapply(Group3, function(x) { names(x)[15] })))

# write datasets to file 
save(INTERVENE.list, file = paste("[PathToOutputFolder/]", as.character(Sys.Date()),
                                  "_",Biobank,"_INTERVENE_Occupation_dat.RData",sep = ""))
save(Group1, file = paste("[PathToOutputFolder/]", as.character(Sys.Date()),
                          "_",Biobank,"_INTERVENE_PGSgroup1_Occupation_dat.RData",sep = ""))
save(Group2, file = paste("[PathToOutputFolder/]", as.character(Sys.Date()),
                          "_",Biobank,"_INTERVENE_PGSgroup2_Occupation_dat.RData",sep = ""))
save(Group3, file = paste("[PathToOutputFolder/]", as.character(Sys.Date()),
                          "_",Biobank,"_INTERVENE_PGSgroup3_Occupation_dat.RData",sep = ""))
