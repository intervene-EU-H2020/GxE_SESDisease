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
# Script model 2: Run Cox Proportional-Hazards models with age of onset as
# timescale, where occupation is classified into "Manual worker", "Lower-level",
# "Upper-level" and (optional) "Self-employed" (reference = Manual worker),
# and include the trait-specific PGS, sex (except for prostate and breast
# cancer), birth decade and the first 10 genetic PCs as covariates
#
# Required input data: biobank-specific INTERVENE combined phenotype and PGS
# file
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
# Run Cox Proportional-Hazards models with trait-specific PGS, sex (except for
# breast and prostate cancer), the first 10 genetic PCS, and Employment as
# covariates use EA as factor where the Manual will become the offset (Model 2).
# Create plot of Hazard Ratios for both models for all 19 traits and cummulative
# event survival curves for both complete models and as shown by EA strata for
# all 19 traits.
#
################################################################################

# function to run Cox Proportional Hazards Models with multiple covariates and
# PGSs
cox.model.PGS <- function(filelist,covformula) {
  fit <- coxph(as.formula(paste0("Surv(AGE,",names(filelist[15]),") ~ ",
                                 names(filelist[19])," + ",covformula)), 
               data =  filelist, na.action = na.exclude)
  return(fit)
}

# create the formula with Employment, the first 10 genetic PCs, and sex as
# covariates, and one with only Employment, the first 10 genetic PCs, as
# covariates (for running the analyses for prostate and breast cancer)
mod2sex.formula <- paste0("Occupation + SEX + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")
mod2nosex.formula <- paste0("Occupation + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")

# run Cox-PH model 2 which includes the scaled trait-specific PGS in loop with
# foreach in parallel for each of the 19 diseases.
res.cox.model2 <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  if(names(INTERVENE.list[[i]][15])=="C3_PROSTATE" | names(INTERVENE.list[[i]][15])=="C3_BREAST") {
    cox.model.PGS(filelist = INTERVENE.list[[i]],covformula = mod2nosex.formula) 
  } else {
    cox.model.PGS(filelist = INTERVENE.list[[i]],covformula = mod2sex.formula)
  }
}

# add trait names to list items 
names(res.cox.model2) <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))

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

# extract model coefficients for each trait with sex as covariate
modcoeffs.cox.model2.sex <- extract.coeffs(model.output = res.cox.model2[!names(res.cox.model2) %in% c("C3_PROSTATE","C3_BREAST")],
                                           filelist = INTERVENE.list[!names(INTERVENE.list) %in% c("C3_PROSTATE","C3_BREAST")])
# extract model coefficients for each trait without sex as covariate
modcoeffs.cox.model2.nosex <- extract.coeffs(model.output = res.cox.model2[c("C3_PROSTATE","C3_BREAST")],
                                             filelist = INTERVENE.list[c("C3_PROSTATE","C3_BREAST")])
#combine into single result data frame
names(modcoeffs.cox.model2.sex) <- gsub("^.*\\prs","PRS", names(modcoeffs.cox.model2.sex)) #rename prs variables columns
names(modcoeffs.cox.model2.nosex) <- gsub("^.*\\prs","PRS", names(modcoeffs.cox.model2.nosex)) #rename prs variables columns
modcoeffs.cox.model2 <- rbind.fill(modcoeffs.cox.model2.sex,modcoeffs.cox.model2.nosex) #combine data

# write table with model coefficients to output as tab-delimited text files
write.table(modcoeffs.cox.model2, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                              "_",Biobank,"_INTERVENE_Occupation_CoxPH_model2_Coeffs.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
