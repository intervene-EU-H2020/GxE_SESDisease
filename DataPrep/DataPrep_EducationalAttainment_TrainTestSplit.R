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
# Script: Create combined phenotype and PRS RData files for running analyses
# where EA is the socioeconomic index
#
# Required input data: biobank-specific INTERVENE phenotype file and
# biobank-specific INTERVENE PGS files
#
# Last edits: 19/06/2025 (FAH, edits: globalize script for upload to GitHub)
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
# education convert to factor
pheno$EDUCATION_11 <- factor(pheno$EDUCATION_11, levels = c(2,3,4,7),
                             labels = c("ISCED 2","ISCED 3","ISCED 4","ISCED 7"))

# function to convert ISCED 11 to ISCED 1997
calc.ISCED <- function(dat) {
  # create emply variable
  ISCED97 <- rep(NA, nrow(dat))
  
  # if else in a loop to assign low vs high EA
  for (i in 1:nrow(dat)) {
    if(!is.na(dat$EDUCATION_11[i]) & (dat$EDUCATION_11[i]=="ISCED 2")) { 
      ISCED97[i] <- "ISCED 2"
    } else if(!is.na(dat$EDUCATION_11[i]) & (dat$EDUCATION_11[i]=="ISCED 3")) {
      ISCED97[i] <- "ISCED 3"
    } else if(!is.na(dat$EDUCATION_11[i]) & (dat$EDUCATION_11[i]=="ISCED 4")) {
      ISCED97[i] <- "ISCED 4"
    } else if(!is.na(dat$EDUCATION_11[i]) & (dat$EDUCATION_11[i]=="ISCED 7")) {
      ISCED97[i] <- "ISCED 5"
    }
  }
  
  # convert EA to factor
  ISCED97 <- factor(ISCED97, levels = c("ISCED 2","ISCED 3","ISCED 4","ISCED 5"))
  
  # output EA
  return(ISCED97)
}

# create ISCED 1997 variable
pheno$ISCED97 <- calc.ISCED(dat = pheno)

# function to create dichtomized EA variable
calc.EA <- function(dat) {
  # create empty variable
  EA <- rep(NA,nrow(dat))
  
  # if else in a loop to assign low vs high EA
  for (i in 1:nrow(dat)) {
    if(!is.na(dat$ISCED97[i]) & (dat$ISCED97[i]=="ISCED 1" | dat$ISCED97[i]=="ISCED 2" | dat$ISCED97[i]=="ISCED 3" | dat$ISCED97[i]=="ISCED 4")) { 
      EA[i] <- "low"
    } else if(!is.na(dat$ISCED97[i]) & (dat$ISCED97[i]=="ISCED 5" | dat$ISCED97[i]=="ISCED 6")) {
      EA[i] <- "high"
    }
  }
  
  # convert EA to factor
  EA <- factor(EA, levels = c("low","high"))
  
  # output EA
  return(EA)
}

# create dichtomized EA variable
pheno$EA <- calc.EA(dat = pheno)

# merge phenotypic and PGS files
INTERVENE <- merge(pheno,PGS,by.x = "ID",by.y = "UKBID")

# check whether all individuals are of EU ancestry, if not remove non-EU
# ancestry individuals
sum(!INTERVENE$ANCESTRY=="EUR",na.rm = T) #0 individuals no need for further filtering

## split data randomly into 80-20 (model development, model prediction)
set.seed(123)  # set seed for reproducibility

# randomly sample 80% of individuals for training and put those in an index for extraction
train_index <- sample(seq_len(nrow(INTERVENE)), size = 0.8 * nrow(INTERVENE))  

# split data in to 80% training and 20% testing
INTERVENE_80 <- INTERVENE[train_index, ]  # 80% training set
INTERVENE_20 <- INTERVENE[-train_index, ]  # 20% testing set

# separate vectors for each of the 18 diseases included in the INTERVENE
# flagship manuscript and Alcohol use disorder for UKB Besides the case-control
# indicator for each disease + the event date (naming convention from FinnGen)
# and the PRSs (names as downloaded), also include the following variables
# (rename if required):
# "ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8",
# "PC9","PC10","ANCESTRY","END_OF_FOLLOWUP","ISCED97", and "EA" (should mostly
# have been included in the phenotype file) + (if relevant) biobank specific
# additional (technical covariates).
# Note, please keep variables in this order and add additionally required
# variables at the end (otherwise the scripts will run on the wrong variable or
# not at all)
T1D = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","T1D","T1D_DATE",
                         "END_OF_FOLLOWUP","ISCED97","EA","T1D_prs")]) 
PC = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5"
                        ,"PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_PROSTATE","C3_PROSTATE_DATE",
                        "END_OF_FOLLOWUP","ISCED97","EA","Prostate_Cancer_prs")]) 
T2D = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","T2D","T2D_DATE",
                         "END_OF_FOLLOWUP","ISCED97","EA","T2D_prs")]) 
GOUT = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                          "PC6","PC7","PC8","PC9","PC10","ANCESTRY","GOUT","GOUT_DATE",
                          "END_OF_FOLLOWUP","ISCED97","EA","Gout_prs")]) 
RA = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                        "PC6","PC7","PC8","PC9","PC10","ANCESTRY","RHEUMA_SEROPOS_OTH","RHEUMA_SEROPOS_OTH_DATE",
                        "END_OF_FOLLOWUP","ISCED97","EA","Rheumatoid_Arthritis_prs")]) 
BC = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                        "PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_BREAST","C3_BREAST_DATE",
                        "END_OF_FOLLOWUP","ISCED97","EA","Breast_Cancer_prs")]) 
AF = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                        "PC6","PC7","PC8","PC9","PC10","ANCESTRY","I9_AF","I9_AF_DATE",
                        "END_OF_FOLLOWUP","ISCED97","EA","Atrial_Fibrillation_prs")]) 
CC = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                        "PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_COLORECTAL","C3_COLORECTAL_DATE",
                        "END_OF_FOLLOWUP","ISCED97","EA","Colorectal_Cancer_prs")]) 
ASTHMA = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                            "PC6","PC7","PC8","PC9","PC10","ANCESTRY","J10_ASTHMA","J10_ASTHMA_DATE",
                            "END_OF_FOLLOWUP","ISCED97","EA","Asthma_prs")]) 
CHD = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","I9_CHD","I9_CHD_DATE",
                         "END_OF_FOLLOWUP","ISCED97","EA","CHD_prs")]) 
HIP = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","COX_ARTHROSIS","COX_ARTHROSIS_DATE",
                         "END_OF_FOLLOWUP","ISCED97","EA","Hip_Osteoarthritis_prs")]) 
KNEE = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                          "PC6","PC7","PC8","PC9","PC10","ANCESTRY","KNEE_ARTHROSIS","KNEE_ARTHROSIS_DATE",
                          "END_OF_FOLLOWUP","ISCED97","EA","Knee_Osteoarthritis_prs")]) 
SKIN = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                          "PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_MELANOMA_SKIN","C3_MELANOMA_SKIN_DATE",
                          "END_OF_FOLLOWUP","ISCED97","EA","Melanoma_prs")]) 
LC = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                        "PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_BRONCHUS_LUNG","C3_BRONCHUS_LUNG_DATE",
                        "END_OF_FOLLOWUP","ISCED97","EA","Lung_Cancer_prs")]) 
MDD = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","F5_DEPRESSIO","F5_DEPRESSIO_DATE",
                         "END_OF_FOLLOWUP","ISCED97","EA","MDD_prs")]) 
CANCER = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                            "PC6","PC7","PC8","PC9","PC10","ANCESTRY","C3_CANCER","C3_CANCER_DATE",
                            "END_OF_FOLLOWUP","ISCED97","EA","AllCancers_prs")]) 
EPILEP = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                            "PC6","PC7","PC8","PC9","PC10","ANCESTRY","G6_EPLEPSY","G6_EPLEPSY_DATE",
                            "END_OF_FOLLOWUP","ISCED97","EA","Epilepsy_prs")])
APPC = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                          "PC6","PC7","PC8","PC9","PC10","ANCESTRY","K11_APPENDACUT","K11_APPENDACUT_DATE",
                          "END_OF_FOLLOWUP","ISCED97","EA","Appendicitis_prs")]) 
AUD = names(INTERVENE[,c("ID","SEX","DATE_OF_BIRTH","PC1","PC2","PC3","PC4","PC5",
                         "PC6","PC7","PC8","PC9","PC10","ANCESTRY","AUD_SWEDISH","AUD_SWEDISH_DATE",
                         "END_OF_FOLLOWUP","ISCED97","EA","Alcohol_Use_Disorder_prs")]) 


################################################################################
#
# Create final analysis set for 80% of data
#
################################################################################

# based on the list of column name vectors produced in the previous step, create
# a list with data.frames for each of the 5 disorders.
INTERVENE.list.80 <- list(T1D = INTERVENE_80[,which(names(INTERVENE_80) %in% T1D)],
                       PC = INTERVENE_80[,which(names(INTERVENE_80) %in% PC)],
                       T2D = INTERVENE_80[,which(names(INTERVENE_80) %in% T2D)],
                       GOUT = INTERVENE_80[,which(names(INTERVENE_80) %in% GOUT)],
                       RA = INTERVENE_80[,which(names(INTERVENE_80) %in% RA)],
                       BC = INTERVENE_80[,which(names(INTERVENE_80) %in% BC)],
                       AF = INTERVENE_80[,which(names(INTERVENE_80) %in% AF)],
                       CC = INTERVENE_80[,which(names(INTERVENE_80) %in% CC)],
                       ASTHMA = INTERVENE_80[,which(names(INTERVENE_80) %in% ASTHMA)],
                       CHD = INTERVENE_80[,which(names(INTERVENE_80) %in% CHD)],
                       HIP = INTERVENE_80[,which(names(INTERVENE_80) %in% HIP)],
                       KNEE = INTERVENE_80[,which(names(INTERVENE_80) %in% KNEE)],
                       SKIN = INTERVENE_80[,which(names(INTERVENE_80) %in% SKIN)],
                       LC = INTERVENE_80[,which(names(INTERVENE_80) %in% LC)],
                       MDD = INTERVENE_80[,which(names(INTERVENE_80) %in% MDD)],
                       CANCER = INTERVENE_80[,which(names(INTERVENE_80) %in% CANCER)],
                       EPILEP = INTERVENE_80[,which(names(INTERVENE_80) %in% EPILEP)],
                       APPC = INTERVENE_80[,which(names(INTERVENE_80) %in% APPC)],
                       AUD = INTERVENE_80[which(names(INTERVENE_80) %in% AUD)])

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
INTERVENE.AGE <- foreach(i=1:length(INTERVENE.list.80)) %dopar% {
  calc.age(filelist = INTERVENE.list.80[[i]])
}

# append the new age vector to the data frames of each trait
INTERVENE.list.80 <- foreach(i=1:length(INTERVENE.list.80)) %dopar% {
  cbind(INTERVENE.list.80[[i]],AGE=INTERVENE.AGE[[i]])
}

# according to the INTERVENE flagship follow-up of participants started at birth
# and ended with (1) age of first record of disease diagnosis, (2) age at death
# for non-disease cause, (3) age at last available EHR, or (4) age 80. Under the
# assumption that the first 3 criteria have been incorporated in the
# specification of date of event or end of follow-up, we now need to remove
# everyone of 80 years or older from the data frames. In addition, remove
# everyone younger than 34.8 as that's the median age at which Finnish
# individuals obtain their PhD.
INTERVENE.list.80 <- foreach(i=1:length(INTERVENE.list.80)) %dopar% {
  INTERVENE.list.80[[i]][which(INTERVENE.list.80[[i]]$AGE>34.8 & INTERVENE.list.80[[i]]$AGE<80),] 
}

# in each data set remove rows without case/control information, education
# information, age, and PGS. The 15th column always contains the
# trait information, the 19th column the education information, the 20th column
# the PGS, and the 21st age
INTERVENE.list.80 <- foreach(i=1:length(INTERVENE.list.80)) %dopar% {
  INTERVENE.list.80[[i]][complete.cases(INTERVENE.list.80[[i]][,c(15,19:21)]),] 
}

# Check whether the PGS for each of the traits behave as expected. Use the glm
# function (stats package, included in the basic/auto-loaded R packages) with
# binomial family to account for the case/control binary trait outcomes. 15th
# column is always the trait column and the 20th the PRS column. Include sex
# (except for breast and prostate cancer), age, first 10 genetic PCs (and if
# relevant any additional technical covariates).
glm.PGS.res <- foreach(i=1:length(INTERVENE.list.80)) %dopar% {
  if(names(INTERVENE.list.80[[i]][15])=="C3_PROSTATE" | names(INTERVENE.list.80[[i]][15])=="C3_BREAST") {
    summary(glm(as.formula(paste0(names(INTERVENE.list.80[[i]][15]), " ~ ",
                                  names(INTERVENE.list.80[[i]][20]),
                                  " + AGE + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")),
                family = binomial, data = INTERVENE.list.80[[i]]))$coefficients[2,]
  } else {
    summary(glm(as.formula(paste0(names(INTERVENE.list.80[[i]][15]), " ~ ",
                                  names(INTERVENE.list.80[[i]][20]),
                                  " + SEX + AGE + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")),
                family = binomial, data = INTERVENE.list.80[[i]]))$coefficients[2,]
  }
}

# combine results in 1 data frame
glm.PGS.res.tab <- as.data.frame(matrix(NA, ncol = 0, nrow = length(INTERVENE.list.80))) # create empty dataframe with rows equal to phenotypes
glm.PGS.res.tab$Trait <- c(unlist(lapply(INTERVENE.list.80, function(x) { names(x)[15] })))
glm.PGS.res.tab$PRS <- c(unlist(lapply(INTERVENE.list.80, function(x) { names(x)[20] })))
glm.PGS.res.tab$Estimate <- c(unlist(lapply(glm.PGS.res, function(x) { x[1] })))
glm.PGS.res.tab$SE <- c(unlist(lapply(glm.PGS.res, function(x) { x[2] })))
glm.PGS.res.tab$P <- c(unlist(lapply(glm.PGS.res, function(x) { x[4] })))

# every association should be positive (>PRS associated with case status), when
# this is not the case a strand flippage likely occurred and the PGS needs to be
# recoded.
neg <- as.numeric(rownames(glm.PGS.res.tab[which(glm.PGS.res.tab$Estimate<0),])) # vector with the row numbers of the PGSs with negative associations with their phenotype

# recode PGS
for (i in 1:length(INTERVENE.list.80)) {
  if(i %in% neg) {
    INTERVENE.list.80[[i]][20] <- INTERVENE.list.80[[i]][20]*-1
  }
}

# scale PGS
for (i in 1:length(INTERVENE.list.80)) {
  INTERVENE.list.80[[i]][,20] <- scale(INTERVENE.list.80[[i]][,20])
}

# function to create birth decade variable, input is data frame per trait, use
# date of birth vector set observations to NA if birth decade <5 individuals
calc.birthdecade <- function(dat) {
  #extract birth year from birth date
  byear <- as.numeric(format(as.Date(dat, format = "%d/%m/%Y"),"%Y"))
  
  # create decades in variable in if else loop
  decade <- rep(NA,length(byear)) # empty vector
  for (i in 1:length(byear)) {
    if(byear[i]<1940) {
      decade[i] <- "1930s"
    } else if (byear[i]>=1940 & byear[i]<1950) {
      decade[i] <- "1940s"
    } else if (byear[i]>=1950 & byear[i]<1960) {
      decade[i] <- "1950s"
    } else if (byear[i]>=1960 & byear[i]<1980) {
      decade[i] <- "1960sand1970s" # combined for UK Biobank because the number of individuals born in the 1970s was too small to warrant their own birth decade (comparative few individuals born in the early 1970s)
    } else {
      decade[i] <- NA
    }
  }
  # convert decade to factor
  decade <- as.factor(decade)
  
  # list of birth decade to hold whether (1) or not (0) to remove it
  decade.list <- vector(mode = "list",length = length(unique(decade)))
  # if <5 individuals in birth decade remove it
  for (i in 1:length(unique(decade))) { 
    if(table(decade)[i]<5) {
      decade.list[[i]] <- 1
    } else {
      decade.list[[i]] <- 0
    }
  }
  
  # if total birth decade <5 remove observations
  for (i in 1:length(decade)) {
    for (j in length(decade.list)) {
      if(!is.na(decade[i]) & decade[i]==levels(decade)[j] & decade.list[[j]]==1) {
        decade[i] <- NA
      }
    }
  }
  
  # if relevant remove unused levels
  decade <- droplevels(decade)
  
  # output decade
  return(decade)
}

# run function to create irth decade variable in a parallel foreach loop
INTERVENE.BIRTH <- foreach(i=1:length(INTERVENE.list.80)) %dopar% {
  calc.birthdecade(dat = INTERVENE.list.80[[i]]$DATE_OF_BIRTH)
}

# append the new birth decade vector to the data frames of each trait
INTERVENE.list.80 <- foreach(i=1:length(INTERVENE.list.80)) %dopar% {
  cbind(INTERVENE.list.80[[i]],birthdecade=INTERVENE.BIRTH[[i]])
}

# add trait names to list items 
names(INTERVENE.list.80) <- c(unlist(lapply(INTERVENE.list.80, function(x) { names(x)[15] })))

# write datasets to file 
save(INTERVENE.list.80, file = paste("[PathToOutputFolder/]", as.character(Sys.Date()),
                                  "_",Biobank,"_INTERVENE_EducationalAttainment_dat_80percent.RData",sep = ""))


################################################################################
#
# Create final analysis set for 20% of data
#
################################################################################

# based on the list of column name vectors produced in the previous step, create
# a list with data.frames for each of the 5 disorders.
INTERVENE.list.20 <- list(T1D = INTERVENE_20[,which(names(INTERVENE_20) %in% T1D)],
                       PC = INTERVENE_20[,which(names(INTERVENE_20) %in% PC)],
                       T2D = INTERVENE_20[,which(names(INTERVENE_20) %in% T2D)],
                       GOUT = INTERVENE_20[,which(names(INTERVENE_20) %in% GOUT)],
                       RA = INTERVENE_20[,which(names(INTERVENE_20) %in% RA)],
                       BC = INTERVENE_20[,which(names(INTERVENE_20) %in% BC)],
                       AF = INTERVENE_20[,which(names(INTERVENE_20) %in% AF)],
                       CC = INTERVENE_20[,which(names(INTERVENE_20) %in% CC)],
                       ASTHMA = INTERVENE_20[,which(names(INTERVENE_20) %in% ASTHMA)],
                       CHD = INTERVENE_20[,which(names(INTERVENE_20) %in% CHD)],
                       HIP = INTERVENE_20[,which(names(INTERVENE_20) %in% HIP)],
                       KNEE = INTERVENE_20[,which(names(INTERVENE_20) %in% KNEE)],
                       SKIN = INTERVENE_20[,which(names(INTERVENE_20) %in% SKIN)],
                       LC = INTERVENE_20[,which(names(INTERVENE_20) %in% LC)],
                       MDD = INTERVENE_20[,which(names(INTERVENE_20) %in% MDD)],
                       CANCER = INTERVENE_20[,which(names(INTERVENE_20) %in% CANCER)],
                       EPILEP = INTERVENE_20[,which(names(INTERVENE_20) %in% EPILEP)],
                       APPC = INTERVENE_20[,which(names(INTERVENE_20) %in% APPC)],
                       AUD = INTERVENE_20[which(names(INTERVENE_20) %in% AUD)])

# run function to calculate age in a parallel foreach loop for each of the
# traits.
INTERVENE.AGE <- foreach(i=1:length(INTERVENE.list.20)) %dopar% {
  calc.age(filelist = INTERVENE.list.20[[i]])
}

# append the new age vector to the data frames of each trait
INTERVENE.list.20 <- foreach(i=1:length(INTERVENE.list.20)) %dopar% {
  cbind(INTERVENE.list.20[[i]],AGE=INTERVENE.AGE[[i]])
}

# according to the INTERVENE flagship follow-up of participants started at birth
# and ended with (1) age of first record of disease diagnosis, (2) age at death
# for non-disease cause, (3) age at last available EHR, or (4) age 80. Under the
# assumption that the first 3 criteria have been incorporated in the
# specification of date of event or end of follow-up, we now need to remove
# everyone of 80 years or older from the data frames. In addition, remove
# everyone younger than 34.8 as that's the median age at which Finnish
# individuals obtain their PhD.
INTERVENE.list.20 <- foreach(i=1:length(INTERVENE.list.20)) %dopar% {
  INTERVENE.list.20[[i]][which(INTERVENE.list.20[[i]]$AGE>34.8 & INTERVENE.list.20[[i]]$AGE<80),] 
}

# in each data set remove rows without case/control information, education
# information, age, and PGS. The 15th column always contains the
# trait information, the 19th column the education information, the 20th column
# the PGS, and the 21st age
INTERVENE.list.20 <- foreach(i=1:length(INTERVENE.list.20)) %dopar% {
  INTERVENE.list.20[[i]][complete.cases(INTERVENE.list.20[[i]][,c(15,19:21)]),] 
}

# Check whether the PGS for each of the traits behave as expected. Use the glm
# function (stats package, included in the basic/auto-loaded R packages) with
# binomial family to account for the case/control binary trait outcomes. 15th
# column is always the trait column and the 20th the PRS column. Include sex
# (except for breast and prostate cancer), age, first 10 genetic PCs (and if
# relevant any additional technical covariates).
glm.PGS.res <- foreach(i=1:length(INTERVENE.list.20)) %dopar% {
  if(names(INTERVENE.list.20[[i]][15])=="C3_PROSTATE" | names(INTERVENE.list.20[[i]][15])=="C3_BREAST") {
    summary(glm(as.formula(paste0(names(INTERVENE.list.20[[i]][15]), " ~ ",
                                  names(INTERVENE.list.20[[i]][20]),
                                  " + AGE + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")),
                family = binomial, data = INTERVENE.list.20[[i]]))$coefficients[2,]
  } else {
    summary(glm(as.formula(paste0(names(INTERVENE.list.20[[i]][15]), " ~ ",
                                  names(INTERVENE.list.20[[i]][20]),
                                  " + SEX + AGE + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")),
                family = binomial, data = INTERVENE.list.20[[i]]))$coefficients[2,]
  }
}

# combine results in 1 data frame
glm.PGS.res.tab <- as.data.frame(matrix(NA, ncol = 0, nrow = length(INTERVENE.list.20))) # create empty dataframe with rows equal to phenotypes
glm.PGS.res.tab$Trait <- c(unlist(lapply(INTERVENE.list.20, function(x) { names(x)[15] })))
glm.PGS.res.tab$PRS <- c(unlist(lapply(INTERVENE.list.20, function(x) { names(x)[20] })))
glm.PGS.res.tab$Estimate <- c(unlist(lapply(glm.PGS.res, function(x) { x[1] })))
glm.PGS.res.tab$SE <- c(unlist(lapply(glm.PGS.res, function(x) { x[2] })))
glm.PGS.res.tab$P <- c(unlist(lapply(glm.PGS.res, function(x) { x[4] })))

# every association should be positive (>PRS associated with case status), when
# this is not the case a strand flippage likely occurred and the PGS needs to be
# recoded.
neg <- as.numeric(rownames(glm.PGS.res.tab[which(glm.PGS.res.tab$Estimate<0),])) # vector with the row numbers of the PGSs with negative associations with their phenotype

# recode PGS
for (i in 1:length(INTERVENE.list.20)) {
  if(i %in% neg) {
    INTERVENE.list.20[[i]][20] <- INTERVENE.list.20[[i]][20]*-1
  }
}

# scale PGS
for (i in 1:length(INTERVENE.list.20)) {
  INTERVENE.list.20[[i]][,20] <- scale(INTERVENE.list.20[[i]][,20])
}

# run function to create irth decade variable in a parallel foreach loop
INTERVENE.BIRTH <- foreach(i=1:length(INTERVENE.list.20)) %dopar% {
  calc.birthdecade(dat = INTERVENE.list.20[[i]]$DATE_OF_BIRTH)
}

# append the new birth decade vector to the data frames of each trait
INTERVENE.list.20 <- foreach(i=1:length(INTERVENE.list.20)) %dopar% {
  cbind(INTERVENE.list.20[[i]],birthdecade=INTERVENE.BIRTH[[i]])
}

# add trait names to list items 
names(INTERVENE.list.20) <- c(unlist(lapply(INTERVENE.list.20, function(x) { names(x)[15] })))

# write datasets to file 
save(INTERVENE.list.20, file = paste("[PathToOutputFolder/]", as.character(Sys.Date()),
                                  "_",Biobank,"_INTERVENE_EducationalAttainment_dat_20percent.RData",sep = ""))

