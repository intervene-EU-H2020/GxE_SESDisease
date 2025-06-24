#empty working dir on script start
rm(list=ls())

################################################################################
#
# Project: INTERVENE - Differences by socioeconomic status (SES, as assessed by
# occupation) in risk of 18 common diseases (as previously
# selected in the INTERVENE flagship manuscript:
# https://doi.org/10.1101/2023.06.12.23291186) and alcohol use disorder
#
# Author: F.A. Hagenbeek [FAH] (fiona.hagenbeek@helsinki.fi)
#
# Script: Lifetime risk estimation (based on:
# https://github.com/intervene-EU-H2020/flagship/blob/main/AbsoluteRiskEstimation/04_LifetimeRiskEstimation.R)
#
# Data: Model 6 (PGS as factor: <20%, 20-40%, 40-60% (=reference group), 60-95%, >95%)
# stratified by occupation for Finngen (FGR11)).
#
# Start script: 19/02/2025 
# Last edits: 24/06/2025 (FAH, edits: last checks prior to upload GitHub)
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
# efficiently reading in large data sets; ggplot2 = versatile visualizations;
# dplyr = data wrangling; googledrive + googlesheets4 = read/write to/from
# GoogleDrive.
packages("data.table","ggplot2","dplyr","googledrive","googlesheets4")

# run GoogleDrive - set to FALSE if latest results have already been downloaded!
run_googledrive<-FALSE


################################################################################
#
# Read in full sample hazard ratios per standard deviation and GBD incidences,
# prevalences, and mortality and reorganize
#
################################################################################

# download latest version of files to local machine
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

# read in model 6
FGR11.6 <- fread("output/EmploymentStatus/FG11/CoxPropHaz_model6/2025-03-14_INTERVENE_SESDiffDiseases_Coeffs_CoxPH_model6_FinnGenR11.txt", data.table=FALSE)

# split by low vs high occupation and reorganize hazard ratio files only need to keep c("Phenotype", "Group",
# "Beta", "SE","HR") where group reflect the HR for occupation in the three PGS groups
# Lower-level occupation
low <- FGR11.6[which(FGR11.6$Test=="Lower-level"),] # extract rows for low EA
FGR11.low <- data.frame(trait = rep(low$trait,2), # retain only required columns
                        Beta = c(low$`PGS_groupGroup 1_beta`,low$`PGS_groupGroup 2_beta`,
                                 low$`PGS_groupGroup 4_beta`, low$`PGS_groupGroup 5_beta`),
                        SE = c(low$`PGS_groupGroup 1_se`,low$`PGS_groupGroup 2_se`,
                               low$`PGS_groupGroup 4_se`,low$`PGS_groupGroup 5_se`),
                        HR = c(low$`PGS_groupGroup 1_HR`,low$`PGS_groupGroup 2_HR`,
                               low$`PGS_groupGroup 4_HR`,low$`PGS_groupGroup 5_HR`),
                        Group = c(rep("<20%",8),rep("20-40%",8),rep("60-95%",8),rep(">95%",8)))
# Upper-level occupation
high <- FGR11.6[which(FGR11.6$Test=="Upper-level"),] # extract rows for high EA
FGR11.high <- data.frame(trait = rep(high$trait,2), # retain only required columns
                         Beta = c(high$`PGS_groupGroup 1_beta`,high$`PGS_groupGroup 2_beta`,
                                  high$`PGS_groupGroup 4_beta`, high$`PGS_groupGroup 5_beta`),
                         SE = c(high$`PGS_groupGroup 1_se`,high$`PGS_groupGroup 2_se`,
                                high$`PGS_groupGroup 4_se`,high$`PGS_groupGroup 5_se`),
                         HR = c(high$`PGS_groupGroup 1_HR`,high$`PGS_groupGroup 2_HR`,
                                high$`PGS_groupGroup 4_HR`,high$`PGS_groupGroup 5_HR`),
                         Group = c(rep("<20%",8),rep("20-40%",8),rep("60-95%",8),rep(">95%",8)))
# combine in list
OCC <- list(LowerlevelOccupation = FGR11.low,
                 UpperlevelOccupation = FGR11.high)

#Read in GBD incidence data 
incidencea <- fread("data/GlobalBurdenDisease2019/2025-02-08_GBD_Incidence.csv", data.table=FALSE)
prevalencea <- fread("data/GlobalBurdenDisease2019/2025-02-08_GBD_Prevalence.csv", data.table=FALSE)
mortalitya <- fread("data/GlobalBurdenDisease2019/2025-02-08_GBD_Mortality.csv", data.table=FALSE)
# breast/prostate cancer
incidenceb <- fread("data/GlobalBurdenDisease2019/2025-02-08_BreastCancerProstateCancer_Incidence.csv", data.table=FALSE)
prevalenceb <- fread("data/GlobalBurdenDisease2019/2025-02-08_BreastCancerProstateCancer_Prevalence.csv", data.table=FALSE)
mortalityb <- fread("data/GlobalBurdenDisease2019/2025-02-08_BreastCancerProstateCancer_Mortality.csv", data.table=FALSE)
# from breast/prostate cancer remove sex==both all causes mortality
mortalityb <- mortalityb[-which(mortalityb$sex=="Both"),]
# combine
incidence_comb <- rbind(incidencea,incidenceb)
prevalence_comb <- rbind(prevalencea,prevalenceb)
mortality_comb <- rbind(mortalitya,mortalityb)

################################################################################
#
# Set-up for calculating lifetime risk across all traits, in each country/biobank
#
################################################################################

# define countries, biobanks, and education group
countries <- c("Finland")
biobank <- c("FinnGen")
occupation <- c("LowerlevelOccupation","UpperlevelOccupation")

# define which traits to analyse per biobank
biobank_traits <- list(LowerlevelOccupation = unique(FGR11.low$trait),
                       UpperlevelOccupation = unique(FGR11.high$trait))

# define which matching traits should be pulled from incidence/mortality/prevalence files
GBD_traits <- list(LowerlevelOccupation = c("Diabetes mellitus type 2",
                                            "Atrial fibrillation and flutter","Asthma",
                                            "Ischemic heart disease","Osteoarthritis hip",
                                            "Osteoarthritis knee","Total cancers","Prostate cancer"),
                   UpperlevelOccupation = c("Diabetes mellitus type 2",
                                            "Atrial fibrillation and flutter","Asthma",
                                            "Ischemic heart disease","Osteoarthritis hip",
                                            "Osteoarthritis knee","Total cancers","Prostate cancer"))

################################################################################
#
# Calculating lifetime risk across all traits, in each country/Group in loop
#
################################################################################

for(j in 1:length(OCC)){
  
  print(occupation[j])
  
  # Iterate over traits for this biobank
  for (z in 1:length(biobank_traits[[j]])) {  
    
    print(paste("Processing", biobank_traits[[j]][z], "for", occupation[j]))
    
    # define lifetime risks, age groups based on GBD, 2 repetitions to reflect low vs high EA, groups based on low vs high EA, repetitions reflect number of traits
    lifetimerisks <- data.frame(Age=rep(c("1-4 years","5-9 years","10-14 years","15-19 years","20-24 years","25-29 years","30-34 years","35-39 years","40-44 years","45-49 years","50-54 years","55-59 years","60-64 years","65-69 years","70-74 years","75-79 years"),5),
                                Group=c(rep("<20%",16),rep("20-40%",16),rep("40-60%",16),rep("60-95%",16),rep(">95%",16))) 
    
    #Read in GBD incidence data 
    incidence <- incidence_comb
    incidence <- subset(incidence, location==countries)
    incidence$age <- factor(incidence$age, levels=c("1-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90-94 years", "All ages"))
    incidence <- incidence[order(incidence$age),]
    incidence$val <- as.numeric(incidence$val)
    
    incidence <- subset(incidence, cause==GBD_traits[[j]][z])
    
    #Divide incidence rates by 100,000 to get the incidence as a probability (note: incidence rates are per year)
    incidence$incidence <- incidence$val / 100000
    
    population <- c()
    for(i in unique(incidence$age)){
      subby <- subset(incidence, age==i)
      poppy <- subby$val[1]/(subby$val[2]/100000)
      population <- c(population, poppy)
    }
    
    population[is.na(population)] <- 0
    
    incidence <- subset(incidence, metric=='Rate')
    incidence <- cbind(incidence, population)
    incidence <- incidence[,c("location","age","cause","metric","population","incidence")]
    
    prevalence <- prevalence_comb
    prevalence <- subset(prevalence, location==countries)
    prevalence$val <- as.numeric(prevalence$val)
    
    prevalence <- subset(prevalence, cause==GBD_traits[[j]][z])
    
    #Divide prevalence rates by 100,000 to get the prevalence as a probability (note: prevalence rates are per year)
    prevalence$prevalence <- prevalence$val / 100000
    prevalence <- prevalence[,c("location","age","cause","metric","prevalence")]
    
    #Left join to incidence to calculate hazard 
    incidence <- left_join(incidence, prevalence)
    
    #Use all cause and cause specific mortality incidence rates to calculate the competing risk of death during the age interval
    mortality <- mortality_comb
    mortality <- subset(mortality, location==countries)
    
    mortality <- subset(mortality, cause==GBD_traits[[j]][z] | cause=="All causes")
    
    all_cause_mortality <- subset(mortality, cause=="All causes" & metric=="Rate")
    all_cause_mortality <- all_cause_mortality[,c("location","sex","age","val")]
    colnames(all_cause_mortality)[4] <- c("all_cause_rate")
    
    cause_specific_mortality <- subset(mortality, cause!="All causes" & metric=="Rate")
    cause_specific_mortality <- cause_specific_mortality[,c("location","sex","age","cause","val")]
    colnames(cause_specific_mortality)[5] <- c("cause_specific_rate")
    
    mortality <- left_join(cause_specific_mortality, all_cause_mortality)
    mortality$all_cause_rate <- as.numeric(mortality$all_cause_rate)
    mortality$cause_specific_rate <- as.numeric(mortality$cause_specific_rate)
    
    mortality$mortality_rate <- (mortality$all_cause_rate - mortality$cause_specific_rate)/100000
    mortality <- mortality[,c("location","sex","age","cause","mortality_rate")]
    
    #Merge mortality data to incidence data
    incidence <- left_join(incidence, mortality)
    
    incidence <- subset(incidence, age!="All ages" & age!="80-84 years" & age!="85-89 years" & age!="90-94 years")
    
    #######################################################################################################################################################################################
    
    #Hazard Ratios - maddavat
    
    #Read in the hazard ratios and allocate to variables...
    hazrats <- OCC[[j]]
    
    hazrats <- subset(hazrats, trait==biobank_traits[[j]][z])
    
    #Hazard Ratios
    hr01 <- hazrats[hazrats$Group=="<20%","HR"]
    hr02 <- hazrats[hazrats$Group=="20-40%","HR"]
    hr04 <- hazrats[hazrats$Group=="60-95%","HR"]
    hr05 <- hazrats[hazrats$Group==">95%","HR"]
    
    #Proportions
    props01 <- 0.2
    props02 <- 0.2
    props03 <- 0.2
    props04 <- 0.35
    props05 <- 0.05
    
    #Estimate incidence attributable to different distributions of PRS 
    incidence$i3 <- (incidence$incidence*incidence$population) / ((props03 * incidence$population) + (hr01 * (props01 * incidence$population)) + 
                                                                    (hr02 * (props02 * incidence$population)) + (hr04 * (props04 * incidence$population)) +
                                                                    (hr05 * (props05 * incidence$population))) 
    incidence$i3[is.na(incidence$i3)] <- 0
    incidence$i1 <- incidence$i3 * hr01
    incidence$i2 <- incidence$i3 * hr02
    incidence$i4 <- incidence$i3 * hr04
    incidence$i5 <- incidence$i3 * hr05
    
    
    ###################################################
    
    Groups <- c("<20%","20-40%","40-60%","60-95%", ">95%")
    lifetimerisk <- data.frame(NULL)
    for(i in 1:5){
      #Calculate hazard
      incidence[[paste0("hazard",i)]] <- incidence[[paste0("i",i)]] / (1 - incidence$prevalence)
      
      #Calculate probability of experiencing the endpoint within the age interval. hazard multiplied by 5 as that is the age interval and current probabilities are per year. 
      incidence[[paste0("risk",i)]] <- 1 - exp(-5*incidence[[paste0("hazard",i)]])
      
      #Mortality and risk
      incidence[[paste0("mortandrisk",i)]] <- cumsum(incidence[[paste0("hazard",i)]] + incidence$mortality_rate)
      
      #Survival
      incidence[[paste0("survival",i)]] <- 1
      
      for(l in 2:nrow(incidence)){
        incidence[[paste0("survival",i)]][l] <- exp(-5*incidence[[paste0("mortandrisk",i)]][l-1])
      }
      
      #Calculate lifetime risk as the cumulative sum of the product of survival and risk.
      incidence[[paste0("lifetimerisk",i)]] <- cumsum(incidence[[paste0("survival",i)]]*incidence[[paste0("risk",i)]])*100
      
      result <- data.frame(incidence$age, Groups[i], incidence[[paste0("lifetimerisk",i)]])
      lifetimerisk <- rbind(lifetimerisk, result)
    }
    
    colnames(lifetimerisk) <- c("Age","Group","LifetimeRisk")
    
    #Plot all as well as overall lifetime risk
    lifetimerisk$Age <- factor(lifetimerisk$Age, levels=c("1-4 years","5-9 years","10-14 years","15-19 years","20-24 years","25-29 years","30-34 years","35-39 years","40-44 years","45-49 years","50-54 years","55-59 years","60-64 years","65-69 years","70-74 years","75-79 years"))
    lifetimerisk$age <- rep(c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80),5)
    lifetimerisk$Group <- factor(lifetimerisk$Group, levels=c(">95%","60-95%","40-60%","20-40%", "<20%"))
    
    write.csv(lifetimerisk, paste0("output/LifetimeRisk/model6/",biobank,"/",as.character(Sys.Date()),"_",biobank_traits[[j]][z],"_LifetimeRisk_",occupation[j],"_",biobank,".csv"))
    
    #Not considering confidence intervals
    ggplot(lifetimerisk, aes(age, LifetimeRisk, color=Group, group=Group)) +
      stat_smooth(method = "lm", formula = y ~ poly(x, 8), se = FALSE) +
      geom_point() +
      xlab("Age") + 
      ylab("Cumulative Risk (%)") + 
      theme_bw() +
      labs(color='PRS Group') +
      scale_color_hue(labels = c(">95%","60-95%","40-60%","20-40%", "<20%")) +
      theme(legend.text = element_text(size = 24),
            legend.title = element_text(size = 28),
            axis.title.x = element_text(size = 28),
            axis.text.x = element_text(size = 24),
            axis.title.y = element_text(size = 28),
            axis.text.y = element_text(size = 24))
    ggsave(paste0("output/LifetimeRisk/model6/",biobank,"/",as.character(Sys.Date()),"_",biobank_traits[[j]][z],"_LifetimeRisk_",occupation[j],"_",biobank,".png"), height=6, width=8, dpi=300)
    
  }
}
