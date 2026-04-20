#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# educational attainment [EA]) in risk of 18 common diseases (as
# previously selected in the INTERVENE flagship manuscript:
# https://doi.org/10.1101/2023.06.12.23291186) and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Compare results for model 4 (Education + PGS + interaction +
# covariates) using Cox Proportional Hazard Model (CoxPH) in FinnGen with an
# extended model (Education + PGS + interaction + EA PGS + PGS*EA PGS
# interaction + covariates) in FinnGen
#
# Data: Results CoxPH model 4 FG + extended model 4 FG
#
# Last edits: 20/04/2026 (edits, FAH: last edits before GitHub upload)
# 
################################################################################

################################################################################
#
# Set up script
#
################################################################################

# function to install (if required) and load R packages
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
# packages function as specified in the source file): data.table = package for
# efficiently reading in large data sets; dplyr, forcats, stringr & tidyr = data
# wrangling; googledrive + googlesheets4 = read/write to/from GoogleDrive.
packages("data.table","dplyr","forcats","stringr","tidyr","googledrive",
         "googlesheets4")

# set working directory
setwd("C:/Users/fhk210/OneDrive - Vrije Universiteit Amsterdam/OngoingProjects/SESDiffDiseaseRisk")

# run GoogleDrive - set to FALSE if latest results have already been downloaded!
run_googledrive<-FALSE


################################################################################
#
# Read in CoxPH and FG results for model 4
#
################################################################################

# download latest version of files to local machine - Cox PH
if (run_googledrive==TRUE) {
  #identify folder
  folder_id = drive_get(as_id("18iI9QxxJ7WXrXO6hcNal_amvGmR1F_jv")) # this ID links to the "INTERVENE flagship/GxE_SESDisease/Output_CoxPHmodels_perBB" folder. ID obtained with drive_find(pattern = "Output_Cox")
  
  #find files in folder
  files = drive_ls(folder_id)
  
  #loop dirs and download files inside them
  for (i in seq_along(files$name)) {
    #list files
    i_dir = drive_ls(files[i, ])
    
    #mkdir
    try({dir.create(paste0("output/GoogleDrive/",files$name[i]))})
    
    #download files
    for (file_i in seq_along(i_dir$name)) {
      #fails if already exists
      try({
        drive_download(
          as_id(i_dir$id[file_i]),
          path = paste0("output/GoogleDrive/",files$name[i], "/",i_dir$name[file_i])
        )
      })
    }
  }
}

# read in model 4 - CoxPH full FG R11 sample
FGR11.full <- fread("output/GoogleDrive/FGR11/2024-03-13_FinnGenR11_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt", data.table=FALSE) # results for analyses on education

# read in extended model 4
FGR11.GxG <- fread("output/GoogleDrive/FGR11/2026-02-11_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model4_GxG_FinnGenR11.txt", data.table=FALSE)


################################################################################
#
# Combine results for plotting
#
################################################################################

# Combine results model 4
model4 <- data.frame(trait = c(rep(FGR11.full$trait,3),rep(FGR11.GxG$trait,3)),
                     HR = c(FGR11.full$EAhigh_HR,FGR11.full$PRS_HR,FGR11.full$`PRS:EAhigh_HR`,
                            FGR11.GxG$EAhigh_HR,FGR11.GxG$PRS_HR,FGR11.GxG$`PRS:EAhigh_HR`),
                     lb = c(FGR11.full$EAhigh_HR_lower95,FGR11.full$PRS_HR_lower95,
                            FGR11.full$`PRS:EAhigh_HR_lower95`,FGR11.GxG$EAhigh_HR_lower,
                            FGR11.GxG$PRS_HR_lower,FGR11.GxG$`PRS:EAhigh_HR_lower95`),
                     ub = c(FGR11.full$EAhigh_HR_upper95,FGR11.full$PRS_HR_upper95,
                            FGR11.full$`PRS:EAhigh_HR_upper95`,FGR11.GxG$EAhigh_HR_upper,
                            FGR11.GxG$PRS_HR_upper,FGR11.GxG$`PRS:EAhigh_HR_upper95`),
                     beta = c(FGR11.full$EAhigh_beta,FGR11.full$PRS_beta,FGR11.full$`PRS:EAhigh_beta`,
                              FGR11.GxG$EAhigh_beta,FGR11.GxG$PRS_beta,FGR11.GxG$`PRS:EAhigh_beta`),
                     se = c(FGR11.full$EAhigh_se,FGR11.full$PRS_se,FGR11.full$`PRS:EAhigh_se`,
                            FGR11.GxG$EAhigh_se,FGR11.GxG$PRS_se,FGR11.GxG$`PRS:EAhigh_se`),
                     Test = c(rep("Education",nrow(FGR11.full)), rep("PRS",nrow(FGR11.full)),
                              rep("interaction",nrow(FGR11.full)), rep("Education",nrow(FGR11.GxG)), 
                              rep("PRS",nrow(FGR11.GxG)),rep("interaction",nrow(FGR11.GxG))),
                     Sample = c(rep("NoGxG",3*nrow(FGR11.full)),rep("GxG",3*nrow(FGR11.GxG))))
# traits as factor to rename them and plot them in the order they've been
# plotted in the INTERVENE flagship figures
model4$trait <- factor(model4$trait, levels = c("T1D","C3_PROSTATE","T2D","GOUT",
                                                "RHEUMA_SEROPOS_OTH","C3_BREAST","I9_AF",
                                                "C3_COLORECTAL","J10_ASTHMA","I9_CHD",
                                                "COX_ARTHROSIS","KNEE_ARTHROSIS",
                                                "C3_MELANOMA_SKIN","C3_BRONCHUS_LUNG",
                                                "F5_DEPRESSIO","C3_CANCER","G6_EPLEPSY",
                                                "K11_APPENDACUT","AUD_SWEDISH"),
                       labels = c("Type 1 Diabetes","Prostate Cancer","Type 2 Diabetes",
                                  "Gout","Rheumatoid Arthritis","Breast Cancer",
                                  "* Atrial Fibrillation","* Colorectal Cancer","Asthma",
                                  "Coronary Heart Disease","Hip Osteoarthritis",
                                  "Knee Osteoarthritis","Skin Melanoma","Lung Cancer",
                                  "Major Depression","Any Cancer","Epilepsy",
                                  "Appendicitis","Alcohol Use Disorder"))
#reverse factor order (otherwise it will be plotted in reverse order of the
#flagship paper)
model4$trait <- fct_rev(model4$trait)
# Test as factor
model4$Test <- factor(model4$Test, levels = c("Education","PRS","interaction"), 
                      labels = c("Education","PGS","Interaction"))
# Sample as factor
model4$Sample <- factor(model4$Sample, levels = c("NoGxG","GxG"),
                       labels = c("No GxG","GxG"))

# split by Test 
df12.EA <- model4[which(model4$Test=="Education"),]
df12.PRS <- model4[which(model4$Test=="PGS"),]
df12.inter <- model4[which(model4$Test=="Interaction"),]

# arrange dataframes by trait and Test group 
plot12.df.EA <- df12.EA %>%
  arrange(trait, Sample) 
plot12.df.PRS <- df12.PRS %>%
  arrange(trait, Sample) 
plot12.df.inter <- df12.inter %>%
  arrange(trait, Sample) 

# Ensure each trait has only two rows (one for each group) and calculate the
# difference in effect size for each trait
plot12_grouped_EA <- plot12.df.EA %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Sample == "No GxG"],
    Model2_HR = HR[Sample == "GxG"],
    Model1_HR_lb = lb[Sample == "No GxG"],
    Model2_HR_lb = lb[Sample == "GxG"],
    Model1_HR_ub = ub[Sample == "No GxG"],
    Model2_HR_ub = ub[Sample == "GxG"],
    Model1b = beta[Sample == "No GxG"],
    Model2b = beta[Sample == "GxG"],
    Model1se = se[Sample == "No GxG"],
    Model2se = se[Sample == "GxG"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )
plot12_grouped_PRS <- plot12.df.PRS %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Sample == "No GxG"],
    Model2_HR = HR[Sample == "GxG"],
    Model1_HR_lb = lb[Sample == "No GxG"],
    Model2_HR_lb = lb[Sample == "GxG"],
    Model1_HR_ub = ub[Sample == "No GxG"],
    Model2_HR_ub = ub[Sample == "GxG"],
    Model1b = beta[Sample == "No GxG"],
    Model2b = beta[Sample == "GxG"],
    Model1se = se[Sample == "No GxG"],
    Model2se = se[Sample == "GxG"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )
plot12_grouped_inter <- plot12.df.inter %>%
  group_by(trait) %>%
  reframe(
    Model1_HR = HR[Sample == "No GxG"],
    Model2_HR = HR[Sample == "GxG"],
    Model1_HR_lb = lb[Sample == "No GxG"],
    Model2_HR_lb = lb[Sample == "GxG"],
    Model1_HR_ub = ub[Sample == "No GxG"],
    Model2_HR_ub = ub[Sample == "GxG"],
    Model1b = beta[Sample == "No GxG"],
    Model2b = beta[Sample == "GxG"],
    Model1se = se[Sample == "No GxG"],
    Model2se = se[Sample == "GxG"],
    difference = Model2_HR - Model1_HR,
    Betadiff = Model2b - Model1b,
    SEDiff = sqrt(Model2se**2 + Model2se**2)
  )

# Determine whether difference between PGS effect (beta) low and Upper-level occupation
# groups is significant. Code adapted from:
# https://github.com/intervene-EU-H2020/flagship/blob/main/MetaAnalysis/AgeSpecific_MetaAnalysisandHetero.R
# calculate z-value
plot12_diff_EA <- plot12_grouped_EA %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot12_diff_PRS <- plot12_grouped_PRS %>%
  mutate(ZDiff = Betadiff/SEDiff)
plot12_diff_inter <- plot12_grouped_inter %>%
  mutate(ZDiff = Betadiff/SEDiff)
# calculate p-value
plot12_diff_EA <- plot12_diff_EA %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot12_diff_PRS <- plot12_diff_PRS %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))
plot12_diff_inter <- plot12_diff_inter %>%
  mutate(PvalDiff = 2*pnorm(abs(ZDiff), lower.tail = FALSE))

# manually adjust the ranking to ensure only those who are significant after
# Bonferonni correction remain on top/bottom
# is p-value Bonferroni significant? 
plot12_diff_EA <- plot12_diff_EA %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot12_diff_PRS <- plot12_diff_PRS %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))
plot12_diff_inter <- plot12_diff_inter %>%
  mutate(Bonf = ifelse(PvalDiff <= 0.05/19, 1, 0))

# combine results
plot12_diff_EA$Test <- "Education"
plot12_diff_PRS$Test <- "PGS"
plot12_diff_inter$Test <- "Interaction"
plot12_diff <- rbind(plot12_diff_EA,plot12_diff_PRS,plot12_diff_inter)

# write to file
fwrite(plot12_diff, paste("output/2classEA/FG11/CoxPropHaz_model4/", as.character(Sys.Date()),
                          "_INTERVENE_SESDiffDiseases_Differences_NoGxGvsGxG_model4.csv",sep=""))

################################################################################
#
# Upload results to Google Drive
#
################################################################################

# model 1a
drive_upload(media = "output/2classEA/FG11/CoxPropHaz_model4/2026-02-26_INTERVENE_SESDiffDiseases_Differences_NoGxGvsGxG_model4.csv",
             path = as_id("18iI9QxxJ7WXrXO6hcNal_amvGmR1F_jv"),
             name = "2026-02-2_INTERVENE_SESDiffDiseases_Differences_NoGxGvsGxG_model4.csv",
             type = "spreadsheet")
