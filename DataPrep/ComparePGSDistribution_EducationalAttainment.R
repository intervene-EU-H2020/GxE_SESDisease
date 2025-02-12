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
# Script: Compare PGS distributions between EA groups in UK Biobank where EA is
# dichotomized to low vs high EA
#
# Required input data: biobank-specific INTERVENE phenotype file and
# biobank-specific INTERVENE PGS files
#
# Last edits: 12/2/2025 (FAH, edits: globalize script for use in other
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
# with dates; forcats, stringr, tidyr, plyr & dplyr = data wrangling; ggplot2 =
# visualisation.
packages("data.table","foreach","doParallel","lubridate","plyr","dplyr",
         "forcats","stringr","tidyr","ggplot2")

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

################################################################################
#
# Create scaled PGS distribution plots by EA
#
################################################################################

# vector with PGSs
vec <- names(INTERVENE)[grep("_prs",names(INTERVENE))]

# scale PGSs
for (i in vec) {
  INTERVENE[,vec] <- scale(INTERVENE[,vec])
}

# data wrangling for plotting
df2 <- INTERVENE[,c("EA",vec)]
# convert to long format
df_long2 <- df2 %>%
  pivot_longer(cols = ends_with("_prs"),
               names_to = "Trait",
               values_to = "PGS") %>%
  mutate(Trait = gsub("_prs$","",Trait)) # remove _prs suffix
# convert to dataframe
df_long2 <- as.data.frame(df_long2)

# create plot
plot <- ggplot(df_long2, aes(x = PGS, color = EA)) + 
  geom_density(size = 1) + 
  facet_wrap(~Trait, scales = "free") +
  scale_color_manual(values = c("#BC65DB","#3869AF")) +
  labs(x = "Polygenic Score", y = "Density", fill = "Educational Attainment") +
  theme_minimal() + 
  theme(legend.position = "bottom")

# save figure as pdf.
ggsave(filename=paste0("[PathToOutputFolder/]",as.character(Sys.Date()), "_", 
                       Biobank,"_INTERVENE_FIG_CompareScaledPGSDistribution_ByEducationalAttainment.pdf"),
       plot = plot, device = pdf, dpi = 300, width = 350, height = 300, units = "mm")