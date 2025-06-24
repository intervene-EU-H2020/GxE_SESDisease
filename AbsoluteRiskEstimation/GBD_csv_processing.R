#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# education [EA]) in risk of 18 common diseases (as previously
# selected in the INTERVENE flagship manuscript:
# https://doi.org/10.1101/2023.06.12.23291186) and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Extract GBD incidences, prevalences, and mortality rates (based on:
# https://github.com/intervene-EU-H2020/flagship/blob/main/AbsoluteRiskEstimation/GBD_csv_processing.R)
#
# Data: GBD 2019 data downloaded from INTERVENE GitHub page 
# (https://github.com/intervene-EU-H2020/flagship/tree/main/AbsoluteRiskEstimation)
#
# Start script: 08/02/2025 
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
# efficiently reading in large data sets; tidyr & stringr = data wrangling
packages("data.table","tidyr","stringr")


################################################################################
#
# Read in GBD 2019 data
#
################################################################################

# read in GBD data
dat1 <- fread("data/GlobalBurdenDisease2019/IHME-GBD_2019_DATA-da65912d-1.csv")
dat2 <- fread("data/GlobalBurdenDisease2019/IHME-GBD_2019_DATA-da65912d-2.csv")
dat3 <- fread("data/GlobalBurdenDisease2019/IHME-GBD_2019_DATA-da65912d-3.csv")
dat4 <- fread("data/GlobalBurdenDisease2019/IHME-GBD_2019_DATA-da65912d-4.csv")
dat5 <- fread("data/GlobalBurdenDisease2019/IHME-GBD_2019_DATA-da65912d-5.csv")
dat6 <- fread("data/GlobalBurdenDisease2019/IHME-GBD_2019_DATA-da65912d-6.csv")

# combine dataframes
dat_all <- rbind(dat1,dat2,dat3,dat4,dat5,dat6)

# fill in the gaps
dat <- complete(dat_all,location,age,sex,cause,measure,metric,year,fill=list(val=0,upper=0,lower=0))

#prepare GBD stats
gbd_phenos <- c("Total cancers", "Appendicitis", "Asthma", "Atrial fibrillation and flutter", 
                "Ischemic heart disease", "Colon and rectum cancer", "Idiopathic epilepsy", "Gout", 
                "Osteoarthritis hip", "Osteoarthritis knee", "Major depressive disorder", 
                "Malignant skin melanoma", "Rheumatoid arthritis", "Diabetes mellitus type 1", 
                "Diabetes mellitus type 2", "Tracheal, bronchus, and lung cancer","Alcohol use disorders")
gbd_bcpc <-c("Breast cancer","Prostate cancer","All causes")


################################################################################
#
# Extract relevant GBD 2019 data and write to file
#
################################################################################

## Mortality ##

# write mortality for all but breast & prostate cancer and all causes
dat %>% filter(str_detect(metric,"Rate")|str_detect(metric,"Number")) %>% filter(str_detect(measure,"Deaths")) %>% filter(!is.na(val)) %>%
  filter(cause %in% c(gbd_phenos,"All causes")) %>% filter(str_detect(sex,"Both")) %>% select(measure,location,sex,age,cause,metric,year,val,upper,lower) %>%
  fwrite(paste0("data/GlobalBurdenDisease2019/",as.character(Sys.Date()),"_GBD_Mortality.csv"))

# write mortality for breast & prostate cancer and all causes
dat %>% filter(str_detect(metric,"Rate")|str_detect(metric,"Number")) %>%filter(str_detect(measure,"Deaths")) %>%filter(!is.na(val)) %>%
  filter(cause %in% gbd_bcpc) %>% 
  filter(str_detect(cause,"Breast cancer") & str_detect(sex,"Female") | str_detect(cause,"Prostate cancer") & str_detect(sex,"Male") | str_detect(cause,"All causes")) %>%
  select(measure,location,sex,age,cause,metric,year,val,upper,lower) %>%
  fwrite(paste0("data/GlobalBurdenDisease2019/",as.character(Sys.Date()),"_BreastCancerProstateCancer_Mortality.csv"))

## Prevalence ##

# write prevalence for all but breast & prostate cancer and all causes
dat %>% filter(str_detect(metric,"Rate")|str_detect(metric,"Number")) %>% filter(str_detect(measure,"Prevalence")) %>%filter(!is.na(val)) %>%
  filter(cause %in% gbd_phenos) %>% filter(str_detect(sex,"Both")) %>% select(measure,location,sex,age,cause,metric,year,val,upper,lower) %>%
  fwrite(paste0("data/GlobalBurdenDisease2019/",as.character(Sys.Date()),"_GBD_Prevalence.csv"))

# write prevalence for breast & prostate cancer and all causes
dat %>% filter(str_detect(metric,"Rate")|str_detect(metric,"Number")) %>%filter(str_detect(measure,"Prevalence")) %>%filter(!is.na(val)) %>%
  filter(cause %in% gbd_bcpc) %>% 
  filter(str_detect(cause,"Breast cancer") & str_detect(sex,"Female") | str_detect(cause,"Prostate cancer") & str_detect(sex,"Male"))%>%
  select(measure,location,sex,age,cause,metric,year,val,upper,lower) %>%
  fwrite(paste0("data/GlobalBurdenDisease2019/",as.character(Sys.Date()),"_BreastCancerProstateCancer_Prevalence.csv"))

## Incidence ##

# write incidence for all but breast & prostate cancer and all causes
dat %>% filter(str_detect(metric,"Rate")|str_detect(metric,"Number")) %>%filter(str_detect(measure,"Incidence")) %>%filter(!is.na(val)) %>%
  filter(cause %in% gbd_phenos) %>% filter(str_detect(sex,"Both")) %>% select(measure,location,sex,age,cause,metric,year,val,upper,lower) %>%
  fwrite(paste0("data/GlobalBurdenDisease2019/",as.character(Sys.Date()),"_GBD_Incidence.csv"))

# write incidence for breast & prostate cancer and all causes
dat %>% filter(str_detect(metric,"Rate")|str_detect(metric,"Number")) %>%filter(str_detect(measure,"Incidence")) %>%filter(!is.na(val)) %>%
  filter(cause %in% gbd_bcpc) %>% 
  filter(str_detect(cause,"Breast cancer") & str_detect(sex,"Female") | str_detect(cause,"Prostate cancer") & str_detect(sex,"Male")) %>%
  select(measure,location,sex,age,cause,metric,year,val,upper,lower) %>%
  fwrite(paste0("data/GlobalBurdenDisease2019/",as.character(Sys.Date()),"_BreastCancerProstateCancer_Incidence.csv"))