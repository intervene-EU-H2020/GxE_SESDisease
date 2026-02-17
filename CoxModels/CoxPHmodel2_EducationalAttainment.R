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
# Script model 2: Run Cox Proportional-Hazards models with age of onset as
# timescale, where EA is dichotomized into low vs high EA (reference = low EA),
# and include the trait-specific PGS, sex (except for prostate and breast
# cancer), birth decade and the first 10 genetic PCs as covariates
#
# Required input data: biobank-specific INTERVENE combined phenotype and PGS
# file
#
# Last edits: 17/02/2026 (FAH, edits: add code to plot Schoenfeld residuals 
# for Education and PGSs)
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
# breast and prostate cancer), the first 10 genetic PCS, and EA as covariates
# use EA as factor where the low EA will become the offset.
# Create plot of Hazard Ratios.
#
################################################################################

# function to run Cox Proportional Hazards Models with multiple covariates and
# PGSs
cox.model.PGS <- function(filelist,covformula) {
  fit <- coxph(as.formula(paste0("Surv(AGE,",names(filelist[15]),") ~ ",
                                 names(filelist[20])," + ",covformula)), 
               data =  filelist, na.action = na.exclude)
  return(fit)
}

# create the formula with EA, the first 10 genetic PCs, birth decade, and sex as
# covariates, and one with only EA, the first 10 genetic PCs, and birth decade
# year as covariates (for running the analyses for prostate and breast cancer)
mod2sex.formula <- paste0("EA + SEX + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + birthdecade")
mod2nosex.formula <- paste0("EA + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + birthdecade")

# run Cox-PH model 2 which includes the scaled trait-specific PGS in loop with
# foreach in parallel for each of the 18 diseases.
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
                                              "_",Biobank,"_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)


###############################################################################
#
# Create Schoenfeld residual plots for the PGSs and EA
#
################################################################################

# Test proportional hazard assumptions
test.ph.model2 <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  cox.zph(res.cox.model2[[i]])
}

# vector with trait names
trait <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))
# make factor (remove non-relevant traits)
trait <- factor(trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                  "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                  "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                  "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                  "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                  "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                  "K11_APPENDACUT","AUD_SWEDISH"),
                labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                           "Gout","Rheumatoid Arthritis","Breast Cancer",
                           "Atrial Fibrillation","Colorectal Cancer","Asthma",
                           "Coronary Heart Disease","Hip Osteoarthritis",
                           "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                           "Major Depression","Any Cancer","Epilepsy",
                           "Appendicitis","Alcohol Use Disorder"))

# create Schoenfeld residual plots
png(filename = paste0("/scratch/project_2007428/users/FAHagenbeek/output/2classEA/CoxPropHaz_model2/",as.character(Sys.Date()),
                      "_",Biobank,"_EUR_INTERVENE_EducationalAttainment_TestPH_model2_FIG.png"),
    res = 300, width = 500, height = 400, units = "mm")
par(mfrow = c(7,6)) # adjust to number of traits tested
# only include the number of traits tested
plot(test.ph.model2[[1]], var = c(rownames(test.ph.model2[[1]]$table)[grep(".*prs",rownames(test.ph.model2[[1]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[1]))
plot(test.ph.model2[[1]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[1]))
#
plot(test.ph.model2[[2]], var = c(rownames(test.ph.model2[[2]]$table)[grep(".*prs",rownames(test.ph.model2[[2]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[2]))
plot(test.ph.model2[[2]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[2]))
#
plot(test.ph.model2[[3]], var = c(rownames(test.ph.model2[[3]]$table)[grep(".*prs",rownames(test.ph.model2[[3]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[3]))
plot(test.ph.model2[[3]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[3]))
#
plot(test.ph.model2[[4]], var = c(rownames(test.ph.model2[[4]]$table)[grep(".*prs",rownames(test.ph.model2[[4]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[4]))
plot(test.ph.model2[[4]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[4]))
#
plot(test.ph.model2[[5]], var = c(rownames(test.ph.model2[[5]]$table)[grep(".*prs",rownames(test.ph.model2[[5]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[5]))
plot(test.ph.model2[[5]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[5]))
#
plot(test.ph.model2[[6]], var = c(rownames(test.ph.model2[[6]]$table)[grep(".*prs",rownames(test.ph.model2[[6]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[6]))
plot(test.ph.model2[[6]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[6]))
#
plot(test.ph.model2[[7]], var = c(rownames(test.ph.model2[[7]]$table)[grep(".*prs",rownames(test.ph.model2[[7]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[7]))
plot(test.ph.model2[[7]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[7]))
#
plot(test.ph.model2[[8]], var = c(rownames(test.ph.model2[[8]]$table)[grep(".*prs",rownames(test.ph.model2[[8]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[8]))
plot(test.ph.model2[[8]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[8]))
#
plot(test.ph.model2[[9]], var = c(rownames(test.ph.model2[[9]]$table)[grep(".*prs",rownames(test.ph.model2[[9]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[9]))
plot(test.ph.model2[[9]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[9]))
#
plot(test.ph.model2[[10]], var = c(rownames(test.ph.model2[[10]]$table)[grep(".*prs",rownames(test.ph.model2[[10]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[10]))
plot(test.ph.model2[[10]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[10]))
#
plot(test.ph.model2[[11]], var = c(rownames(test.ph.model2[[11]]$table)[grep(".*prs",rownames(test.ph.model2[[11]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[11]))
plot(test.ph.model2[[11]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[11]))
#
plot(test.ph.model2[[12]], var = c(rownames(test.ph.model2[[12]]$table)[grep(".*prs",rownames(test.ph.model2[[12]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[12]))
plot(test.ph.model2[[12]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[12]))
#
plot(test.ph.model2[[13]], var = c(rownames(test.ph.model2[[13]]$table)[grep(".*prs",rownames(test.ph.model2[[13]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[13]))
plot(test.ph.model2[[13]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[13]))
#
plot(test.ph.model2[[14]], var = c(rownames(test.ph.model2[[14]]$table)[grep(".*prs",rownames(test.ph.model2[[14]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[14]))
plot(test.ph.model2[[14]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[14]))
#
plot(test.ph.model2[[15]], var = c(rownames(test.ph.model2[[15]]$table)[grep(".*prs",rownames(test.ph.model2[[15]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[15]))
plot(test.ph.model2[[15]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[15]))
#
plot(test.ph.model2[[16]], var = c(rownames(test.ph.model2[[16]]$table)[grep(".*prs",rownames(test.ph.model2[[16]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[16]))
plot(test.ph.model2[[16]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[16]))
#
plot(test.ph.model2[[17]], var = c(rownames(test.ph.model2[[17]]$table)[grep(".*prs",rownames(test.ph.model2[[17]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[17]))
plot(test.ph.model2[[17]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[17]))
#
plot(test.ph.model2[[18]], var = c(rownames(test.ph.model2[[18]]$table)[grep(".*prs",rownames(test.ph.model2[[18]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[18]))
plot(test.ph.model2[[18]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[18]))
#
plot(test.ph.model2[[19]], var = c(rownames(test.ph.model2[[19]]$table)[grep(".*prs",rownames(test.ph.model2[[19]]$table))]), ylab = "Beta(t) for PGS", main=paste0(trait[19]))
plot(test.ph.model2[[19]], var = c("EA"), ylab = paste0("Beta(t) for education"),main=paste0(trait[19]))
dev.off()
