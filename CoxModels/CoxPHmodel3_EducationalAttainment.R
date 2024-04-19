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
# Script model 3: Run Cox Proportional-Hazards models with age of onset as
# timescale, with trait-specific PGS by EA level, sex (except for breast and
# prostate cancer), bith decade, and the first 10 genetic PCS as covariates.
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
# Create new variables to assess the PGS per EA level (dummy variable per EA
# level * PGS)
#
###############################################################################

# function to code to create new variables to assess the PGS per EA level 
# (dummy variable per EA level * PGS) for 2 group EA
calc.GxE.EA2 <- function(filelist) {
  # create dummy variables of the EA levels
  lowEA <- ifelse(filelist$EA=="low", 1, 0)
  highEA <- ifelse(filelist$EA=="high", 1, 0)
  
  # create GxE variables, where the PGS is always the 20th column in FinnGen
  GxlowEA <- filelist[,20] * lowEA
  GxhighEA <- filelist[,20] * highEA
  
  # combine the new variables into a single data frame
  out = cbind(GxlowEA,GxhighEA)
  
  # function outputs the newly created variables in a data frame
  return(out)
}

# run function to create the GxE variables (from source file) in parallel foreach
# loop for each of the traits. 
INTERVENE.GxE <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  calc.GxE.EA2(filelist = INTERVENE.list[[i]])
}

# append the new GxE data frame to the data frame of each trait
INTERVENE.list <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cbind(INTERVENE.list[[i]],INTERVENE.GxE[[i]])
}

# for some reason it does not automatically name the new variables, so
# add new names manually. 
for (i in 1:length(INTERVENE.list)) {
  names(INTERVENE.list[[i]]) <- c(names(INTERVENE.list[[i]][1:23]),"GxlowEA","GxhighEA")
}


###############################################################################
#
# Run Cox Proportional-Hazards models with trait-specific PGS by EA level, sex
# (except for breast and prostate cancer), bith decade, and the first 10 genetic
# PCS as covariates.
#
################################################################################

# function to run Cox Proportional Hazards Models with multiple covariates
cox.model <- function(filelist,covformula) {
  fit <- coxph(as.formula(paste0("Surv(AGE,",names(filelist[15]),") ~ ",covformula)), 
               data =  filelist, na.action = na.exclude)
  return(fit)
}

# create the formula with the scaled GxE variables, the first 10 genetic PCs,
# birth decde, and sex as covariates, and one with the scaled
# GxE variable, birth decade, and the first 10 genetic PCs as
# covariates (for running the analyses for prostate and breast cancer)
mod3sex.formula <- paste0("GxlowEA + GxhighEA + SEX + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + birthdecade")
mod3nosex.formula <- paste0("GxlowEA + GxhighEA + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10  + birthdecade")

# run Cox-PH model 3 in loop with foreach  in parallel for each of the 18 diseases.
res.cox.model3 <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  if(names(INTERVENE.list[[i]][15])=="C3_PROSTATE" | names(INTERVENE.list[[i]][15])=="C3_BREAST") {
    cox.model(filelist = INTERVENE.list[[i]],covformula = mod3nosex.formula) 
  } else {
    cox.model(filelist = INTERVENE.list[[i]],covformula = mod3sex.formula)
  }
}

# add trait names to list items 
names(res.cox.model3) <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))

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
modcoeffs.cox.model3.sex <- extract.coeffs(model.output = res.cox.model3[!names(res.cox.model3) %in% c("C3_PROSTATE","C3_BREAST")],
                                           filelist = INTERVENE.list[!names(INTERVENE.list) %in% c("C3_PROSTATE","C3_BREAST")])
# extract model coefficients for each trait without sex as covariate
modcoeffs.cox.model3.nosex <- extract.coeffs(model.output = res.cox.model3[c("C3_PROSTATE","C3_BREAST")],
                                             filelist = INTERVENE.list[c("C3_PROSTATE","C3_BREAST")])
#combine into single result data frame
modcoeffs.cox.model3 <- rbind.fill(modcoeffs.cox.model3.sex,modcoeffs.cox.model3.nosex)

# write table with model coefficients to output as tab-delimited text files
write.table(modcoeffs.cox.model3, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                              "_",Biobank,"_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
