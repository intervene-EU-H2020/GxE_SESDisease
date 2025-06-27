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
# Script model 4: Run Fine-Gray competing risk (all-cause mortality) model with
# EA, trait-specific PGS, EA * trait-specific PGS interaction, sex (except for
# protate and breast cancer), first 10 genetics PCs + birth decade as covariates
#
# Required input data: biobank-specific INTERVENE combined phenotype and PGS
# files for Fine-Gray analyses
#
# Last edits: 27/06/2024 (FAH, edits: globalize script for use in other
# INTERVENE biobanks and upload to GitHub)
# 
################################################################################

################################################################################
#
# Set up script
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
# packages function as specified above): survival, prodlim & riskRegression =
# computing survival analyses; foreach = run multiple analyses in a loop, also install "doParallel"
# which will allow you to run foreach in parallel across the number of cores you
# desire/have available; plyr = data wrangling.
packages("survival","foreach","doParallel","plyr","riskRegression","prodlim")

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


################################################################################
#
# Run Fine-Gray model with scaled disease-specific PGS, EA, PGSxEA interaction,
# sex (except for breast and prostate cancer), the first 10 genetic PCS, and
# birth decade as covariates for those traits with significant interaction in
# meta-analysis: T2D, AF, CHD, hip OA, knee OA, any cancer, and asthma
#
################################################################################

# run Fine-Gray model 4, which includes the scaled trait-specific PGS in loop
# with foreach in parallel for each of the diseases.
res.FG.model4 <- foreach(i=1:length(INTERVENE.list)) %dopar% {
  FGR(as.formula(paste0("Hist(AGE,event) ~ ", names(INTERVENE.list[[i]][19]), 
                        " + interaction + EA + SEX + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + birthdecade")), 
        data = INTERVENE.list[[i]], cause = 1) 
}

# trait names
trait <- c(unlist(lapply(INTERVENE.list, function(x) { names(x)[15] })))

# function to extract coefficients from model output stored in a list and output
# the coefficients as a data frame (Fine Gray models because they're generated
# with the riskRegression package)
extract.coeffs.FG <- extract.coeffs.FG <- function(model_list, trait_names) {
  
  results <- lapply(seq_along(model_list), function(i) {
    model <- model_list[[i]]
    trait <- trait_names[i]  # Assign correct trait name
    
    # Extract model coefficients
    coef_table <- as.data.frame(summary(model)$coef)
    
    # Extract variable names from row names
    var_names <- rownames(coef_table)
    
    # Rename PRS variable (assumed to be the first variable in the model)
    var_names[1] <- "PRS"
    
    # Create structured output
    df <- data.frame(trait = trait)
    
    for (j in seq_along(var_names)) {
      var <- var_names[j]
      
      df[[paste0(var, "_beta")]] <- coef_table[j, "coef"]
      df[[paste0(var, "_se")]] <- coef_table[j, "se(coef)"]
      df[[paste0(var, "_p")]] <- coef_table[j, "p-value"]
      df[[paste0(var, "_HR")]] <- exp(coef_table[j, "coef"])
      df[[paste0(var, "_HR_lower")]] <- exp(coef_table[j, "coef"] - 1.96 * coef_table[j, "se(coef)"])
      df[[paste0(var, "_HR_upper")]] <- exp(coef_table[j, "coef"] + 1.96 * coef_table[j, "se(coef)"])
    }
    
    return(df)
  })
  
  # Combine results into a single dataframe
  final_results <- bind_rows(results)
  
  return(final_results)
}

# extract model coefficients for each trait with sex as covariate
modcoeffs.FG.model4 <- extract.coeffs.FG(model_list = res.FG.model4,trait_names = trait)

# write table with model coefficients to output as tab-delimited text files
write.table(modcoeffs.FG.model4, file=paste0("[PathToOutputFolder/]",as.character(Sys.Date()),
                                             "_",Biobank,"_INTERVENE_EducationalAttainment_FineGray_model4_Coeffs.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
