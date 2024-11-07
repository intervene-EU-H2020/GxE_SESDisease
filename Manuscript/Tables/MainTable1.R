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
# Script: Create Main Table 1
#
# required input data: FinnGen data release 11 (FGR11) + UK Biobanke
# (UKB) + Generation Scotland (GS) descriptive statistics
#
# Last edits: 06/11/2024 (edits, FAH: final checks and minor tweaks prior to
# upload to GitHub)
# 
################################################################################

################################################################################
#
# Set up script
#
################################################################################

# set working directory (the working directory is the project folder on my VM on
# the FinnGen Sosioeconomic Data Sandbox)
setwd("C:/Users/hagenbee/OneDrive - University of Helsinki/SESdiffDiseaseRisk/")

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
# wrangling.
packages("data.table","dplyr","forcats","stringr","tidyr")


################################################################################
#
# Read in descriptives
#
################################################################################

# read in descriptives
FGR11.N <- fread("output/2classEA/FG11/2024-03-13_INTERVENE_SESDiffDiseases_SampleDescriptives_FinnGenR11.txt",data.table=FALSE)
FGR11.N$Biobank <- "FinnGen"
#
UKB.N <- fread("output/2classEA/UKB/2024-04-04_INTERVENE_SESDiffDiseases_SampleDescriptives_UKB.txt",data.table=FALSE)
UKB.N$Biobank <- "UK Biobank"
#
GS.N <- fread("output/2classEA/GS/2024-07-04_GS_INTERVENE_EducationalAttainment_SampleDescriptives.txt",data.table=FALSE)
GS.N$Biobank <- "Generation Scotland"


################################################################################
#
# As current version of UKB results also includes the traits
# where UKB was in the discovery GWAS, subset those data frames to only include
# the traits where UKB was not in the discovery GWAS: "Type 1
# Diabetes","Prostate Cancer","Gout","Rheumatoid Arthritis","Breast
# Cancer","Epilepsy","Alcohol Use Disorder"
#
################################################################################

UKB.N <- UKB.N[which(UKB.N$trait %in% c("T1D","C3_PROSTATE","GOUT","RHEUMA_SEROPOS_OTH","C3_BREAST","G6_EPLEPSY","AUD_SWEDISH")),]


################################################################################
#
# Create main table 1
#
################################################################################

# calculate total sample size
FGR11.N$Ntotal <- FGR11.N$Ncontrols+FGR11.N$Ncases
UKB.N$Ntotal <- UKB.N$Ncontrols+UKB.N$Ncases
GS.N$Ntotal <- GS.N$Ncontrols+GS.N$Ncases

# split column with N/% females in cases and controls
FGR11.N <- FGR11.N %>%
  separate(Ncontrols_females, into = c("controls_females", "controls_females_percentage"), sep = " \\(", remove = FALSE) %>%
  separate(Ncases_females, into = c("cases_females", "cases_females_percentage"), sep = " \\(", remove = FALSE)
# Convert the columns to appropriate types
FGR11.N$controls_females <- as.numeric(FGR11.N$controls_females)
FGR11.N$cases_females <- as.numeric(FGR11.N$cases_females)
#
UKB.N <- UKB.N %>%
  separate(Ncontrols_females, into = c("controls_females", "controls_females_percentage"), sep = " \\(", remove = FALSE) %>%
  separate(Ncases_females, into = c("cases_females", "cases_females_percentage"), sep = " \\(", remove = FALSE)
# Convert the columns to appropriate types
UKB.N$controls_females <- as.numeric(UKB.N$controls_females)
UKB.N$cases_females <- as.numeric(UKB.N$cases_females)
#
GS.N <- GS.N %>%
  separate(Ncontrols_females, into = c("controls_females", "controls_females_percentage"), sep = " \\(", remove = FALSE) %>%
  separate(Ncases_females, into = c("cases_females", "cases_females_percentage"), sep = " \\(", remove = FALSE)
# Convert the columns to appropriate types
GS.N$controls_females <- as.numeric(GS.N$controls_females)
GS.N$cases_females <- as.numeric(GS.N$cases_females)

# calculate total females
FGR11.N$Nfemales <- FGR11.N$controls_females+FGR11.N$cases_females
UKB.N$Nfemales <- UKB.N$controls_females+UKB.N$cases_females
GS.N$Nfemales <- GS.N$controls_females+GS.N$cases_females

# calculate total high education
FGR11.N$highEA <- FGR11.N$Ncases_highEA+FGR11.N$Ncontrols_highEA
UKB.N$highEA <- UKB.N$Ncases_highEA+UKB.N$Ncontrols_highEA
GS.N$highEA <- GS.N$Ncases_highEA+GS.N$Ncontrols_highEA

# Create descriptive summary for manuscript
T1 <- data.frame(Study = c("FinnGen","UK Biobank","Generation Scotland"),
                 SampleSize = c(max(FGR11.N$Ntotal),max(UKB.N$Ntotal),max(GS.N$Ntotal)),
                 AgeRecruitment_mean_IQR = c(paste0(round(mean(FGR11.N$AgeOnstet_q50),1)," (",round(mean(FGR11.N$AgeOnset_IQR),1),")"),
                                             paste0(round(mean(UKB.N$AgeOnstet_q50),1)," (",round(mean(UKB.N$AgeOnset_IQR),1),")"),
                                             paste0(round(mean(GS.N$AgeOnstet_q50),1)," (",round(mean(GS.N$AgeOnset_IQR),1),")")),
                 MaximumFollowUp_mediaun_IQR = c(paste0(max(FGR11.N$Followup_Median)," (",max(FGR11.N$Followup_IQR),")"),
                                                 paste0(max(UKB.N$Followup_Median)," (",max(UKB.N$Followup_IQR),")"),
                                                 paste0(max(GS.N$Followup_Median)," (",max(GS.N$Followup_IQR),")")),
                 females = c(paste0(round(mean(FGR11.N$Nfemales)),"(",round(mean(FGR11.N$Nfemales)/max(FGR11.N$Ntotal)*100,1),"%)"),
                             paste0(round(mean(UKB.N$Nfemales)),"(",round(mean(UKB.N$Nfemales)/max(UKB.N$Ntotal)*100,1),"%)"),
                             paste0(round(mean(GS.N$Nfemales)),"(",round(mean(GS.N$Nfemales)/max(GS.N$Ntotal)*100,1),"%)")),
                 highEA = c(paste0(round(mean(FGR11.N$highEA)),"(",round(mean(FGR11.N$highEA)/max(FGR11.N$Ntotal)*100,1),"%)"),
                            paste0(round(mean(UKB.N$highEA)),"(",round(mean(UKB.N$highEA)/max(UKB.N$Ntotal)*100,1),"%)"),
                            paste0(round(mean(GS.N$highEA)),"(",round(mean(GS.N$highEA)/max(GS.N$Ntotal)*100,1),"%)")),
                 AcertainmentStrategy = c("Population and hospital","Population","Population"))

# write file 
write.table(T1, file = paste0("output/Tables/",as.character(Sys.Date()),
                               "_INTERVENE_EducationalAttainment_MainTable1.txt"),
            row.names=F, col.names = T, sep="\t",quote = F)
