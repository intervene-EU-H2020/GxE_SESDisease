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
# Script model 1b: Run logistic regression models with trait-specifict polygenic
# score (PGS), sex (except for prostate and breast cancer), first 10 genetic
# PCs, and birth year as covariates.
#
# Required input data: biobank-specific INTERVENE combined phenotype and PGS
# file (80% training)
#
# Last edits: 25/06/2024 (FAH, edits: globalize script for use in other
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
# packages function as specified above): foreach = run multiple analyses in a
# loop, also install "doParallel" which will allow you to run foreach in
# parallel across the number of cores you desire/have available; forcats,
# stringr, tidyr, plyr & dplyr = data wrangling.
packages("foreach","doParallel","tidyr","plyr","dplyr","forcats","stringr")

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


###############################################################################
#
# Run logistic regression models with trait-specific PGS, sex (except for breast
# and prostate cancer), birth year, and the first 10 genetic PCS as covariates
#
################################################################################

# run logistic regrssion in loop with foreach  in parallel for each of the 19 diseases.
res.glm.model1b.by <- foreach(i=1:length(INTERVENE.list.80)) %dopar% {
  if(i==2 | i==6) {
    summary(glm(as.formula(paste0(names(INTERVENE.list.80[[i]][15]), 
                                  " ~ ", names(INTERVENE.list.80[[i]][19]),
                                  " + birth_year + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")),
                family = binomial, data = INTERVENE.list.80[[i]]))
  } else {
    summary(glm(as.formula(paste0(names(INTERVENE.list.80[[i]][15]), 
                                  " ~ ",names(INTERVENE.list.80[[i]][19]), 
                                  " + SEX + birth_year + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")),
                family = binomial, data = INTERVENE.list.80[[i]]))
  }
}

# add trait names to list items 
names(res.glm.model1b.by) <- c(unlist(lapply(INTERVENE.list.80, function(x) { names(x)[15] })))

# function to extract coefficients from model output stored in a list and output
# the coefficients as a data frame
extract.coeffs <- function(model.output,filelist) {
  # obtain trait names
  trait <- c(unlist(lapply(filelist, function(x) { names(x)[15] })))
  
  # create a list, were each list contains a vector of betas for each of the
  # tested covariates/covariate levels
  Beta <- foreach(i=1:nrow(model.output[[1]]$coefficients)) %dopar% {
    c(unlist(lapply(model.output, function(x) { x$coefficients[i,1] })))
  }
  # convert list into a data frame
  Beta <- data.frame(Beta)
  # rename the columns to reflect the covariate it was obtained for
  names(Beta) <- c(paste0(row.names(model.output[[1]]$coefficients),"_beta"))
  
  # create a list, were each list contains a vector of se's for each of the
  # tested covariates/covariate levels
  SE <- foreach(i=1:nrow(model.output[[1]]$coefficients)) %dopar% {
    c(unlist(lapply(model.output, function(x) { x$coefficients[i,2] })))
  }
  # convert list into a data frame
  SE <- data.frame(SE)
  # rename the columns to reflect the covariate it was obtained for
  names(SE) <- c(paste0(row.names(model.output[[1]]$coefficients),"_se"))
  
  # create a list, were each list contains a vector of p-values for each of the
  # tested covariates/covariate levels
  P <- foreach(i=1:nrow(model.output[[1]]$coefficients)) %dopar% {
    c(unlist(lapply(model.output, function(x) { x$coefficients[i,4] })))
  }
  # convert list into a data frame
  P <- data.frame(P)
  # rename the columns to reflect the covariate it was obtained for
  names(P) <- c(paste0(row.names(model.output[[1]]$coefficients),"_p"))
  
  # combine the data frames with all coefficients into a single data frame
  out = cbind(trait, Beta, SE, P)
  
  # function outputs the data frame with coefficients for each model in the model output list
  return(out)
}

# extract model coefficients for each trait with sex as covariate
modcoeffs.glm.model1b.by.sex <- extract.coeffs(model.output = res.glm.model1b.by[!names(res.glm.model1b.by) %in% c("C3_PROSTATE","C3_BREAST")],
                                               filelist = INTERVENE.list.80[!names(INTERVENE.list.80) %in% c("C3_PROSTATE","C3_BREAST")])
# extract model coefficients for each trait without sex as covariate
modcoeffs.glm.model1b.by.nosex <- extract.coeffs(model.output = res.glm.model1b.by[c("C3_PROSTATE","C3_BREAST")],
                                                 filelist = INTERVENE.list.80[c("C3_PROSTATE","C3_BREAST")])
#combine into single result data frame
names(modcoeffs.glm.model1b.by.sex) <- gsub(".*prs","PRS", names(modcoeffs.glm.model1b.by.sex)) #rename prs variables columns
names(modcoeffs.glm.model1b.by.nosex) <- gsub(".*prs","PRS", names(modcoeffs.glm.model1b.by.nosex)) #rename prs variables columns
modcoeffs.glm.model1b.by <- rbind.fill(modcoeffs.glm.model1b.by.sex,modcoeffs.glm.model1b.by.nosex)

# write table with model coefficients to output as tab-delimited text files
write.table(modcoeffs.glm.model1b.by, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                                  "_",Biobank,"_INTERVENE_EducationalAttainment_LogReg_model1b_Coeffs.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)