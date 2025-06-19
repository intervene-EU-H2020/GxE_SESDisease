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
# Script model 3: Run Cox Proportional-Hazards models with age of onset as
# timescale, with trait-specific PGS by occupation level, sex (except for breast
# and prostate cancer), bith decade, and the first 10 genetic PCS as covariates.
#
# Required input data: biobank-specific INTERVENE combined phenotype and PGS
# file
#
# Last edits: 19/06/2025 (FAH, edits: replace with traditional stratification)
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
# packages function as specified above): survival = computing survival
# analyses; foreach = run multiple analyses in a loop, also install "doParallel"
# which will allow you to run foreach in parallel across the number of cores you
# desire/have available; plyr = data wrangling.
packages("survival","foreach","doParallel","plyr")

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
# split data by occupation group to run stratified analyses
#
################################################################################

# create list of dataframes containing the lower-level occupation data
lowOCC <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  subset(INTERVENE.list[[i]], Occupation == "Lower-level")
}

# create list of dataframes containing the high educational attainment data
highOCC <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  subset(INTERVENE.list[[i]], Occupation == "Upper-level")
}

# resulting dataframe may have empty birth decade levels. Remove these. 
# function to drop empty levels
clean_birthdecade <- function(df) {
  df$birthdecade <- droplevels(df$birthdecade)
  return(df)
}
# apply to both lists of dataframes
lowOCC <- lapply(lowOCC, clean_birthdecade)
highOCC <- lapply(highOCC, clean_birthdecade)

# add trait names to list items
names(lowOCC) <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))
names(highOCC) <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))


###############################################################################
#
# Run Cox Proportional-Hazards models with trait-specific PGS by Occupation
# level, sex (except for breast and prostate cancer), and the first 10 genetic
# PCS as covariates
#
################################################################################

# function to run Cox Proportional Hazards Models with multiple covariates
cox.model.PGS <- function(filelist,covformula) {
  fit <- coxph(as.formula(paste0("Surv(AGE,",names(filelist[15]),") ~ ",
                                 names(filelist[20])," + ",covformula)), 
               data =  filelist, na.action = na.exclude)
  return(fit)
}

# create the formula with the first 10 genetic PCs, birth decade, and sex as
# covariates, and one with birth decade, and the first 10 genetic PCs as
# covariates (for running the analyses for prostate and breast cancer)
mod3sex.formula <- paste0("SEX + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + birthdecade")
mod3nosex.formula <- paste0("PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10  + birthdecade")

# run Cox-PH model 3 in loop with foreach  in parallel for each of the 19
# diseases and both education groups.
res.cox.model3.low <- foreach(i=1:length(lowOCC)) %do% {
  if(names(lowOCC[[i]][15])=="C3_PROSTATE" | names(lowOCC[[i]][15])=="C3_BREAST") {
    cox.model.PGS(filelist = lowOCC[[i]],covformula = mod3nosex.formula) 
  } else {
    cox.model.PGS(filelist = lowOCC[[i]],covformula = mod3sex.formula)
  }
}
res.cox.model3.high <- foreach(i=1:length(highOCC)) %do% {
  if(names(highOCC[[i]][15])=="C3_PROSTATE" | names(highOCC[[i]][15])=="C3_BREAST") {
    cox.model.PGS(filelist = highOCC[[i]],covformula = mod3nosex.formula) 
  } else {
    cox.model.PGS(filelist = highOCC[[i]],covformula = mod3sex.formula)
  }
}

# add trait names to list items for both lists
names(res.cox.model3.low) <- c(unlist(lapply(lowOCC, function(x) { names(x)[15] })))
names(res.cox.model3.high) <- c(unlist(lapply(highOCC, function(x) { names(x)[15] })))

# function to extract coefficients from model output stored in a list and output
# the coefficients as a data frame
extract.coeffs <- function(model.output,filelist) {
  # obtain trait names
  trait <- c(unlist(lapply(filelist, function(x) { names(x)[15] })))
  
  # store the N and N of events for each model output included in the list in a
  # vector
  N <- c(unlist(lapply(model.output, function(x) { x$n })))
  N_cases <- c(unlist(lapply(model.output, function(x) { x$nevent })))
  # combine them into a dataframe
  NN <- data.frame(N,N_cases)
  
  # create a list, were each list contains a vector of betas for each of the
  # tested covariates/covariate levels
  Beta <- foreach(i=1:nrow(summary(model.output[[1]])$coefficients)) %dopar% {
    c(unlist(lapply(model.output, function(x) { summary(x)$coefficients[i,1] })))
  }
  # convert list into a data frame
  Beta <- data.frame(Beta)
  # rename the columns to reflect the covariate it was obtained for
  names(Beta) <- c(paste0(row.names(summary(model.output[[1]])$coefficients),"_beta"))
  
  # create a list, were each list contains a vector of se's for each of the
  # tested covariates/covariate levels
  SE <- foreach(i=1:nrow(summary(model.output[[1]])$coefficients)) %dopar% {
    c(unlist(lapply(model.output, function(x) { summary(x)$coefficients[i,3] })))
  }
  # convert list into a data frame
  SE <- data.frame(SE)
  # rename the columns to reflect the covariate it was obtained for
  names(SE) <- c(paste0(row.names(summary(model.output[[1]])$coefficients),"_se"))
  
  # create a list, were each list contains a vector of p-values for each of the
  # tested covariates/covariate levels
  P <- foreach(i=1:nrow(summary(model.output[[1]])$coefficients)) %dopar% {
    c(unlist(lapply(model.output, function(x) { summary(x)$coefficients[i,5] })))
  }
  # convert list into a data frame
  P <- data.frame(P)
  # rename the columns to reflect the covariate it was obtained for
  names(P) <- c(paste0(row.names(summary(model.output[[1]])$coefficients),"_p"))
  
  # create a list, were each list contains a vector of hazard ratios for each of
  # the tested covariates/covariate levels
  HR <- foreach(i=1:nrow(summary(model.output[[1]])$coefficients)) %dopar% {
    c(unlist(lapply(model.output, function(x) { summary(x)$conf.int[i,1] })))
  }
  # convert list into a data frame
  HR <- data.frame(HR)
  # rename the columns to reflect the covariate it was obtained for
  names(HR) <- c(paste0(row.names(summary(model.output[[1]])$coefficients),"_HR"))
  
  # create a list, were each list contains a vector of the lower 95% CI bound of
  # the hazard ratios for each of the tested covariates/covariate levels
  HR_lower95 <- foreach(i=1:nrow(summary(model.output[[1]])$coefficients)) %dopar% {
    c(unlist(lapply(model.output, function(x) { summary(x)$conf.int[i,3] })))
  }
  # convert list into a data frame
  HR_lower95 <- data.frame(HR_lower95)
  # rename the columns to reflect the covariate it was obtained for
  names(HR_lower95) <- c(paste0(row.names(summary(model.output[[1]])$coefficients),"_HR_lower95"))
  
  # create a list, were each list contains a vector of the upper 95% CI bound
  # hazard ratios for each of the tested covariates/covariate levels
  HR_upper95 <- foreach(i=1:nrow(summary(model.output[[1]])$coefficients)) %dopar% {
    c(unlist(lapply(model.output, function(x) { summary(x)$conf.int[i,4] })))
  }
  # convert list into a data frame
  HR_upper95 <- data.frame(HR_upper95)
  # rename the columns to reflect the covariate it was obtained for
  names(HR_upper95) <- c(paste0(row.names(summary(model.output[[1]])$coefficients),"_HR_upper95"))
  
  # combine the data frames with all coefficients into a single data frame
  out = cbind(trait, NN, Beta, SE, P, HR, HR_lower95, HR_upper95)
  
  # function outputs the data frame with coefficients for each model in the model output list
  return(out)
}

# extract model coefficients for each trait with sex as covariate for both lists
modcoeffs.cox.model3.sex.low <- extract.coeffs(model.output = res.cox.model3.low[!names(res.cox.model3.low) %in% c("C3_PROSTATE","C3_BREAST")],
                                           filelist = lowOCC[!names(lowOCC) %in% c("C3_PROSTATE","C3_BREAST")])
modcoeffs.cox.model3.sex.high <- extract.coeffs(model.output = res.cox.model3.high[!names(res.cox.model3.high) %in% c("C3_PROSTATE","C3_BREAST")],
                                               filelist = highOCC[!names(highOCC) %in% c("C3_PROSTATE","C3_BREAST")])
# extract model coefficients for each trait without sex as covariate for both lists
modcoeffs.cox.model3.nosex.low <- extract.coeffs(model.output = res.cox.model3.low[c("C3_PROSTATE","C3_BREAST")],
                                             filelist = lowOCC[c("C3_PROSTATE","C3_BREAST")])
modcoeffs.cox.model3.nosex.high <- extract.coeffs(model.output = res.cox.model3.high[c("C3_PROSTATE","C3_BREAST")],
                                                 filelist = highOCC[c("C3_PROSTATE","C3_BROCCST")])

#combine into single result data frame
names(modcoeffs.cox.model3.sex.low) <- gsub("^.*\\prs","PRS", names(modcoeffs.cox.model3.sex.low)) #rename prs variables columns
names(modcoeffs.cox.model3.sex.high) <- gsub("^.*\\prs","PRS", names(modcoeffs.cox.model3.sex.high)) #rename prs variables columns
names(modcoeffs.cox.model3.nosex.low) <- gsub("^.*\\prs","PRS", names(modcoeffs.cox.model3.nosex.low)) #rename prs variables columns
names(modcoeffs.cox.model3.nosex.high) <- gsub("^.*\\prs","PRS", names(modcoeffs.cox.model3.nosex.high)) #rename prs variables columns
modcoeffs.cox.model3 <- rbind.fill(modcoeffs.cox.model3.sex.low,modcoeffs.cox.model3.nosex.low,modcoeffs.cox.model3.sex.high,modcoeffs.cox.model3.nosex.high)

# add column indicating from which model the results hail
modcoeffs.cox.model3$Test <- c(rep("Lower-level",.5*nrow(modcoeffs.cox.model3)),rep("Upper-level",.5*nrow(modcoeffs.cox.model3)))

# write table with model coefficients to output as tab-delimited text files
write.table(modcoeffs.cox.model3, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                              "_",Biobank,"_INTERVENE_Occupation_CoxPH_model3_Coeffs.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
