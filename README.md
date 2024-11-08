# Pipeline for the INTERVENE GxE SES and complex diseases project
The analyses are broken up into [Part 1](https://github.com/intervene-EU-H2020/GxE_SESDisease/edit/main/README.md#part-1-biobank-specific-general-data-preparation), general data preparation for individual-level analyses in each Biobank, [Part 2](https://github.com/intervene-EU-H2020/GxE_SESDisease/edit/main/README.md#part-2-biobank-specific-analyses), individual-level analyses with Educational Attainment and Occupation in each Biobank, [Part 3](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/README.md#part-3-meta-analyses-across-biobank-studies), meta-analyses done on summary statistics to draw conclusions across biobank studies, and [Part 4](), scripts to creates the (supplemental) tables and figures as included in the manuscript describing this project.

### Dependencies  
These scripts assume you have plink-1.9 and R v4.3.2 or higher installed on your biobank computing system. Required R libraries: data.table, foreach, doParallel, lubridate, tidyverse/dplyr, plyr, forcats, stringr, survival, metafor, googledrive, googlesheets4, ggplot2, viridis, cowplot, grid, gridExtra, and RColorBrewer.

### Contact
Please contact Fiona Hagenbeek (fiona.hagenbeek@helsinki.fi) if you have any questions.  

# Part 1: Biobank-specific general data preparation
## Step 1: Define phenotypes
### Step 1a: Run [UKBPhenotyper.R](https://github.com/intervene-EU-H2020/flagship/blob/main/Phenotyping/UKBBPhenotyper.R)
The script in this step is identical to the [INTERVENE flagship project](https://github.com/intervene-EU-H2020/flagship), but we retain a 19th phenotype for analysis (Alcohol Use Disorder).  
Run [UKBPhenotyper.R](https://github.com/intervene-EU-H2020/flagship/blob/main/Phenotyping/UKBBPhenotyper.R) to define the phenotypes required for these analyses. Note, this will require you to also download the file [UKBB_definitions_demo_TEST.csv](https://github.com/intervene-EU-H2020/flagship/blob/main/Phenotyping/UKBB_definitions_demo_TEST.csv). For more information on running this script see the [flagship README](https://github.com/intervene-EU-H2020/flagship/tree/main?tab=readme-ov-file#step-1a-run-ukbphenotyperr).  
Phenotypes of interest after running this script are (you can subset the file to just these):  
1. C3_CANCER (All cancers)
2. K11_APPENDACUT (Appendicitis)
3. J10_ASTHMA (Asthma)
4. I9_AF (Atrial fibrillation)
5. C3_BREAST (Breast cancer
6. I9_CHD (Coronary heart disease)
7. C3_COLORECTAL (Colorectal cancer)
8. G6_EPLEPSY (Epilepsy)
9. GOUT (Gout)
10. COX_ARTHROSIS (Hip osteoarthritis)
11. KNEE_ARTHROSIS (Knee osteoarthritis)
12. F5_DEPRESSIO (Major depressive disorder)
13. C3_MELANOMA_SKIN (Malignant skin melanoma)
14. C3_PROSTATE (Prostate cancer)
15. RHEUMA_SEROPOS_OTH (Rheumatoid arthritis)
16. T1D (Type 1 diabetes)
17. T2D (Type 2 diabetes)
18. C3_BRONCHUS_LUNG (Lung cancer)
19. AUD_SWEDISH (Alcohol use disorder) = new

### Step 1b: Construct the phenotype file
The phenotype file will include all variables required to run the analyses. The list of required variables can be found [here](https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit).  
**Note, you can currently ignore the requirement to define smoking.** Education can either be mapped to [ISCED 1997](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=International_Standard_Classification_of_Education_(ISCED)#Correspondence_between_ISCED_2011_and_ISCED_1997) or to ISCED 2011 _(the relevant script will recode to 1997)_, an overview of the mapping of raw educational attainment to ISCED codes and the final dichotomized Educational Attainment variable used in the analyses for all Biobanks can be found [here](https://docs.google.com/spreadsheets/d/14VEX5gJQvlaU-JRbQfDydFg4mZggik1r/edit?usp=sharing&ouid=110931529881647544076&rtpof=true&sd=true). Mapped Occupation information can either be added to the phenotype file or be read-in from a separate file. Examples of the occupation mapping in FinnGen and UK Biobank can be found [here](https://docs.google.com/spreadsheets/d/199409EeZBEgS1uYAgJUwMu6Ag54n_d4F/edit?usp=sharing&ouid=110931529881647544076&rtpof=true&sd=true).

## Step 2: Download the adjusted summary statistics
Download the pre-adjusted summary statistics created using MegaPRS that correspond to the genome build of your Biobank. All pre-adjusted summary statistics can be found [here](https://figshare.com/account/home#/projects/202290). Email Fiona Hagenbeek (fiona.hagenbeek@helsinki.fi) for access.  
- hg19 pre-adjusted summary statistics can be found [here](https://figshare.com/s/6d6125fa4c65828f6aab) and hg38 files [here](https://figshare.com/s/c6d94a641825d3b2bebe).
  - Hg19 contains rsids and hg38 contains variant IDs in the CHR_POS_REF_ALT format. 

## Step 3: Merge variant IDs to match those within the .bim file
**Note, only to be performed if the genome build is hg38 and the variant ID structure is CHR_POS_REF_ALT**  
The script in this step is identical to the [INTERVENE flagship project](https://github.com/intervene-EU-H2020/flagship).  
Run [hg38_biobankadjustments.R](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/hg38_biobankadjustments.R) to select the correct CHR_POS_REF_ALT based on the .bim of your biobank. For more information on running this script see the [flagship README](https://github.com/intervene-EU-H2020/flagship/tree/main?tab=readme-ov-file#step-3-merge-variant-ids-to-match-those-within-the-bim-file).

## Step 4: Compute polygenic risk scores (PRSs) with Plink
The scripts in this step are identical to the [INTERVENE flagship project](https://github.com/intervene-EU-H2020/flagship).  
### If your Plink files are not split by chromosome
Run [GeneratePRS.sh](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/GeneratePRS.sh) to create the PRSs for your Biobank. For more information on running this script see the [flagship README](https://github.com/intervene-EU-H2020/flagship?tab=readme-ov-file#step-4-compute-prs-using-plink).  
### If your Plink files are split by chromosome
#### Step 4a: run [GeneratePRS_IndividualChr.sh](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/GeneratePRS_IndividualChr.sh)
Run [GeneratePRS_IndividualChr.sh](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/GeneratePRS_IndividualChr.sh) to create the PRSs per chromosome for your Biobank. For more information on running this script see the [flagship README](https://github.com/intervene-EU-H2020/flagship?tab=readme-ov-file#step-4a-run-script-generateprs_individualchrsh).  
#### Step 4b: run [PRSSummationOverChr.R](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/PRSSummationOverChr.R)
Run [PRSSummationOverChr.R](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/PRSSummationOverChr.R) to sum over the per chromosome PRSs to create a total PRS for each trait in your Biobank. For more information on running this script see the [flagship README](https://github.com/intervene-EU-H2020/flagship?tab=readme-ov-file#step-4b-run-script-prssummationoverchrr).

## Step 5: Construct combined phenotype and polygenic risk score (PRS) R objects 
### Step 5a: Educational Attainment
Run [DataPrep_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/DataPrep_EducationalAttainment.R) to create the EA-specific sample for each trait in your Biobank. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 64 - specify phenotype file location + filename
4. Lines 67 + 76 - specify the location of the folder containing PRS weights
5. Line 81 - please replace the identifier name with that used in your biobank.
6. Lines 94-95 - if Educational Attainment has not been converted to ISCED 1997 from ISCED 2011, replace _"EDUCATION_11"_ in the code that creates the factor with the naming convention for ISCED 2011 education in your biobank; if Educational Attainment has already been converted to ISCED 1997 instead of ISCED 2011, replace it (if applicable) with code to make the ISCED 1997 variable a factor: 
```
pheno$ISCED97 <- factor(pheno$ISCED97, levels = c(1,2,3,4,5,6), # remove the ISCED 1997 levels not available in your biobank
                    labels = c("ISCED 1","ISCED 2","ISCED 3","ISCED 4","ISCED 5", "ISCED 6)) # remove the ISCED 1997 not available in your biobank
```
7. Lines 98-123 - Run if ISCED 2011 has not yet been recoded to ISCED 1997 (otherwise out-comment); remove the ISCED 1997 levels not available in your biobank
8. Lines 132-134 - remove the ISCED 1997 levels not available in your biobank
9. Line 150 - please replace the identifier names with those used in your biobank.
10. Line 154 - if your biobank contains individuals of non-European ancestry/those that have principal components calculated for NON-EUROPEAN ancestry, i.e. within ancestry principal components, not global genetic principal components, please add code to only retain individuals of European ancestry after this line, for example,:
```
pheno <- subset(pheno, ANCESTRY=='EUR')
```
11. Lines 168-224 - Code assumes you have kept the same shorthand names for the phenotypes as within [FinnGen](https://docs.google.com/spreadsheets/d/1DNKd1KzI8WOIfG2klXWskbCSyX6h5gTu/edit#gid=334983519) (column B) and you have kept the same naming structure for the PRS files as when you downloaded them. Please adjust the names of the standard covariates before running this code if the current names do not match the naming convention in your biobank and add additional (technical) covariates as required. Remove any of the traits not applicable in your biobank (i.e., if the biobank was included in GWAS the summary statistics were based on, see Supplementary Table 4 of the [INTERVENE flagship preprint](https://doi.org/10.1101/2023.06.12.23291186).
12. Line 261, 266, 278, 286, 296, 333, 340, 350, 353, 356, 412, 415, 418, 421, 426, 429, 432 + 435 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
13. Lines 300 + 305 - add additional (technical) covariates if required
14. Line 324-328 - If none of the PRSs have been flipped (e.g., all associations are positive) then you must out-comment these lines. 
15. Lines 366-373 - remove birth decades _not_ present in your biobank and _add_ birth decades not included in the code that are included in your biobank. 
**16. Before writing the output to file, please check whether each subgroup for each trait has >=5 individuals!** Remove traits from the list if <5 individuals in a subgroup, e.g., with the following code: 
```
Listname[c(x,y,z)] <- NULL # where x, y, and z are the shorthand names for the phenotypes as in FinnGen
```
17. Lines 443, 445, 447 + 449 - specify the location you want to save the .Rdata files.  
- Output files are "&#42;_INTERVENE_EducationalAttainment_dat.RData", "&#42;_INTERVENE_PGSgroup1_EducationalAttainment_dat.RData", "&#42;_INTERVENE_PGSgroup2_EducationalAttainment_dat.RData", and "&#42;_INTERVENE_PGSgroup3_EducationalAttainment_dat.RData".

### Step 5b: Occupation
Run [DataPrep_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/DataPrep_Occupation.R) to create the occupation-specific sample for each trait in your Biobank. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line 
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 64 - specify phenotype file location + filename
4. Line 68 - If not already included in the phenotype file, specify occupation file location + filename. Otherwise, out-comment this line.
5. Lines 71 + 80 - specify the location of the folder containing PRS weights
6. Line 85 - please replace the identifier name with that used in your biobank.
7. Lines 99-125 - Replace the example coding (based on UKB data) with the codes relevant to your biobank and remove lines related to the occupation category not included in your biobank. _Note, that the UKB data does not include the "Self-employed" category._ If your biobank contains the information to categorize occupation information into a "Self-employed" class, please replace lines 116-117 with the following, where XX, YY, and ZZ are fictional codes in the relevant UKB field:
```
	} 
	else if(!is.na(dat$`20277-0.0`[i]) & (grepl("^XX", dat$`20277-0.0`[i]) | grepl("^YY", dat$`20277-0.0`[i]) | grepl("^ZZ", dat$`20277-0.0`[i]))) {
	Occupation[i] <- "Self-employed"
	}
}
```
And adjust lines 120-121 as follows (alternatively, if not all occupation classes are present in your biobank remove those not included):
```
Occupation <- factor(Occupation, levels = c("Manual worker","Self-employed",
                                            "Lower-level","Upper-level"))
```
8. Lines 128-131 - If occupation information has already been categorized, out-comment these lines, if occupation information has not been categorized, please replace the identifier names with those used in your biobank. 
9. Line 134 - If occupation information has already been categorized, please replace _"phenos"_ with _"pheno"_, and replace the identifier names with those used in your biobank.
10. Line 138 - if your biobank contains individuals of non-European ancestry/those that have principal components calculated for NON-EUROPEAN ancestry, i.e. within ancestry principal components, not global genetic principal components, please add code to only retain individuals of European ancestry after this line, for example,:
```
pheno <- subset(pheno, ANCESTRY=='EUR')
```
11. Lines 152-208 - Code assumes you have kept the same shorthand names for the phenotypes as within [FinnGen](https://docs.google.com/spreadsheets/d/1DNKd1KzI8WOIfG2klXWskbCSyX6h5gTu/edit#gid=334983519) (column B) and you have kept the same naming structure for the PRS files as when you downloaded them. Please adjust the names of the standard covariates before running this code if the current names do not match the naming convention in your biobank and add additional (technical) covariates as required. Remove any of the traits not applicable in your biobank (i.e., if the biobank was included in GWAS the summary statistics were based on, see Supplementary Table 4 of the [INTERVENE flagship preprint](https://doi.org/10.1101/2023.06.12.23291186).
12. Lines 245, 250, 265, 273, 281, 320, 326, 336, 339 + 342 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
13. Lines 283 + 288 - add additional (technical) covariates if required
14. Line 307-311 - If none of the PRSs have been flipped (e.g., all associations are positive) then you must out-comment these lines. 

**15. Before writing the output to file, please check whether each subgroup for each trait has >=5 individuals!** Remove traits from the list if <5 individuals in a subgroup, e.g., with the following code: 
```
Listname[c(x,y,z)] <- NULL # where x, y, and z are the shorthand names for the phenotypes as in FinnGen
```
16. Lines 350-357 - specify the location you want to save the .Rdata files.  
- Output files are "&#42;_INTERVENE_Occupation_dat.RData", "&#42;_INTERVENE_PGSgroup1_Occupation_dat.RData", "&#42;_INTERVENE_PGSgroup2_Occupation_dat.RData", and "&#42;_INTERVENE_PGSgroup3_Occupation_dat.RData".

## Step 6: Calculate descriptives for each of the socioeconomic indices and trait combinations
### Step 6a: Educational Attainment
Run [Descriptives_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/Descriptives_EducationalAttainment.R) to calculate the summary statistics on the phenotype files including Educational Attainment. Please make the following adjustments: 
1. Line 50 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 53 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Lines 62-67 - specify file locations + filenames
4. Line 149, 179, 207 + 235 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
5. Lines 158-163, 188-193, 216-221 + 224-249  - if your Biobank was included in the prostate cancer GWASs or the number of individuals in any subgroup was <5, and you cannot investigate this trait, out-comment or remove these lines
7. Lines 165-170, 195-200, 223-228 + 251-256 - if your Biobank was included in the breast cancer GWASs or the number of individuals in any subgroup was <5, and you cannot investigate this trait, out-comment or remove these lines
6. Lines 173 + 265 - specify the location you want to save the descriptive files.  
- Output files are  "&#42;_INTERVENE_EducationalAttainment_SampleDescriptives.txt" and "&#42;_INTERVENE_EducationalAttainment_SampleDescriptives_byPGS3group.txt"

### Step 6b: Occupation
Run [Descriptives_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/Descriptives_Occupation.R) to calculate the summary statistics on the phenotype files including Occupation. Please make the following adjustments: 
1. Line 50 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 53 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Lines 62-67 - specify file locations + filenames
4. Lines 107-112 - remove lines related to the occupation categories not included in your biobank. As this example is based on UKB, _which does not include the "Self-employed" occupation class_, please add the following two lines of code if your biobank does include the "Self-employed" occupation class:
```
Ncontrols_Selfemployed <- sum(filelist$Occupation[which(filelist[,15]==0)]=="Self-employed")
Ncases_Selfemployed <- sum(filelist$Occupation[which(filelist[,15]==1)]=="Self-employed")
```
5. Lines 115-117 - remove lines related to occupation categories not included in your biobank. As this example is based on UKB, _which does not include the "Self-employed" occupation class_, please add the following line of code if your biobank does include the "Self-employed" occupation class:
```
prevalence_Selfemployed <- round(Ncases_Selfemployed/(Ncases_Selfemployed+Ncontrols_Selfemployed)*100,2)
```
6. Lines 120-143 - remove lines related to occupation categories not included in your biobank. As this example is based on UKB, _which does not include the "Self-employed" occupation class_, please add the following two lines of code if your biobank does include the "Self-employed" occupation class:
```
Ncontrols_females_Selfemployed <- c(paste0(sum(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Self-employed")]==1), 
                                             " (", 
                                             round(prop.table(table(filelist$SEX[which(filelist[,15]==0 & filelist$Occupation=="Self-employed")]))[2]*100,1), 
                                             "%)"))
Ncases_females_Selfemployed <- c(paste0(sum(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Self-employed")]==1), 
                                          " (", 
                                          round(prop.table(table(filelist$SEX[which(filelist[,15]==1 & filelist$Occupation=="Self-employed")]))[2]*100,1), 
                                          "%)"))
```
7. Lines 146-156 - remove lines related to occupation categories not included in your biobank. As this example is based on UKB, _which does not include the "Self-employed" occupation class_, please adjust the code if your biobank does include the "Self-employed" occupation class (example code now assumes all four occupation classes are present in your biobank):
```
dat <- as.data.frame(cbind(trait,Ncontrols,Ncases,Ncontrols_females,
                             Ncases_females,prevalence,AgeOnset_q25,
                             AgeOnstet_q50,AgeOnset_q75,AgeOnset_IQR,
                             Followup_IQR,Followup_Median,Ncontrols_Manualworker,Ncases_Manualworker,
                             Ncontrols_Selfemployed,Ncases_Selfemployed,Ncontrols_Lowerlevel,
                             Ncases_Lowerlevel,Ncontrols_Upperlevel,Ncases_Upperlevel,
                             prevalence_Manualworker,prevalence_Selfemployed,
                             prevalence_Lowerlevel,prevalence_Upperlevel,
                             Ncontrols_females_Manualworker,Ncases_females_Manualworker,
                             Ncontrols_females_Selfemployed,Ncases_females_Selfemployed,
                             Ncontrols_females_Lowerlevel,Ncases_females_Lowerlevel,
                             Ncontrols_females_Upperlevel,Ncases_females_Upperlevel))
```
8. Line 163, 198, 231 + 264 - if running on a single core or a Rstudio session with automatic multi-threading, replace %dopar& with %do%
9. Lines 172-179, 207-214, 240-247 + 273-280 - if your Biobank was included in the prostate cancer GWASs or the number of individuals in any subgroup was <5, and you cannot investigate this trait, out-comment or remove these lines
10. Lines 181-188, 216-223, 249-256 + 282-289 - if your Biobank was included in the breast cancer GWASs or the number of individuals in any subgroup was <5, and you cannot investigate this trait, out-comment or remove these lines
11. Lines 191 and 298 - specify the location you want to save the descriptive files.  
- Output files are  "&#42;_INTERVENE_Occupation_SampleDescriptives.txt" and "&#42;_INTERVENE_Occupation_SampleDescriptives_byPGS3group.txt"

# Part 2: Biobank-specific analyses
## Model 1: Determine the individual effect of the socioeconomic indices or trait-specific polygenic risk score (PRS) on disease risk
### Model 1a SES effect, with Educational Attainment as the socioeconomic index
Run [CoxPHmodel1a_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel1a_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and include sex (except for prostate and breast cancer), birth decade and the first 5 genetic principal components (PCs) as covariates. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 89, 115, 125, 135, 145,155 + 165 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
5. Lines 181 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1a.sex"_ to _"modcoeffs.cox.model1a"_
6. Lines 184-187 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
7. Line 190 - specify the location you want to save the model 1a output.  
- Output file is "&#42;_INTERVENE_EducationalAttainment_CoxPH_model1a_Coeffs.txt"

### Model 1a SES effect, with Occupation as the socioeconomic index
Run [CoxPHmodel1a_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel1a_Occupation.R) to run the Cox proportional hazard models with age at disease onset as timescale, where occupation is classified into "Manual worker", "Lower-level", "Upper-level" and **(optional)** "Self-employed" (reference = Manual worker), and include sex (except for prostate and breast cancer), birth decade and the first 5 genetic PCs as covariates. Please make the following adjustments: 
1. Line 52 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 55 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 64 - specify file location + filename
4. Lines 88, 114, 124, 134, 144, 154 + 164 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
5. Lines 180 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1a.sex"_ to _"modcoeffs.cox.model1a"_
6. Lines 183-186 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
7. Line 189 - specify the location you want to save the model 1a output.  
- Output file is "&#42;_INTERVENE_Occupation_CoxPH_model1a_Coeffs.txt"

### Model 1b PRS effect, without Educational Attainment as the socioeconomic index
Run [CoxPHmodel1b_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel1b_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, with the trait-specific PRS, sex (except for prostate and breast cancer), the first 10 genetic PCs, and birth decade as covariates. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 85-86 - add biobank-specific technical covariates if required
5. Lines 90, 116, 126, 136, 146, 156 + 166 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 182 + 188 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1b.sex"_ to _"modcoeffs.cox.model1b"_
7. Lines 185-186 + 189-190 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 193 - specify the location you want to save the model 1b output.  
- Output file is "&#42;_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt"

### Model 1b PRS effect, without Occupation as the socioeconomic index
Run [CoxPHmodel1b_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel1b_Occupation.R) to run the Cox proportional hazard models with age at disease onset as timescale, with the trait-specific PRS, sex (except for prostate and breast cancer), the first 10 genetic PCs, and birth decade as covariates. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 85-86 - add biobank-specific technical covariates if required
5. Lines 90, 116, 126, 136, 146, 156 + 166 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 182 + 188 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1b.sex"_ to _"modcoeffs.cox.model1b"_
7. Lines 185-186 + 189-190 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 193 - specify the location you want to save the model 1b output.  
- Output file is "&#42;_INTERVENE_Occupation_CoxPH_model1b_Coeffs.txt"

## Model 2: Determine the effect of the socioeconomic indices and the trait-specific polygenic risk score (PRS) together on disease risk 
### Educational Attainment 
Run [CoxPHmodel2_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel2_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and include the trait-specific PRS, sex (except for prostate and breast cancer), birth decade and the first 10 genetic PCs as covariates. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 86-87 - add biobank-specific technical covariates if required
5. Lines 91, 117, 127, 137, 147, 157 + 167 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 174 + 180 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model2.sex"_ to _"modcoeffs.cox.model2"_
7. Lines 186-187 + 190-191 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 194 - specify the location you want to save the model 2 output.  
- Output file is "&#42;_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt"

### Occupation 
Run [CoxPHmodel2_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel2_Occupation.R) to run the Cox proportional hazard models with age at disease onset as timescale, where occupation is classified into "Manual worker", "Lower-level", "Upper-level" and **(optional)** "Self-employed" (reference = Manual worker), and include the trait-specific PRS, sex (except for prostate and breast cancer), birth decade and the first 10 genetic PCs as covariates. Please make the following adjustments:
1. Line 52 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 55 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 64 - specify file location + filename
4. Lines 89-90 - add biobank-specific technical covariates if required
5. Lines 94, 120, 130, 140, 150, 160 + 170 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 186 + 192 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model2.sex"_ to _"modcoeffs.cox.model2"_
7. Lines 189-190 + 193-194 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 197 - specify the location you want to save the model 2 output.  
- Output file is "&#42;_INTERVENE_Occupation_CoxPH_model2_Coeffs.txt"

## Model 3: Determine the effect of the trait-specific polygenic risk score (PRS) per level of the socioeconomic index on disease risk
### Educational Attainment
Run [CoxPHmodel3_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel3_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and include the trait-specific PRS by EA level _(created in this script)_, sex (except for breast and prostate cancer), bith decade, and the first 10 genetic PCS as covariates. Please make the following adjustments: 
1. Line 50 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 53 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 62 - specify file location + filename
4. Lines 91, 96, 130, 156, 166, 176, 186, 196 + 206 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
5. Line 103 - adjust _"23"_ to the total number of columns in the dataset before adding the new variables
6. Lines 126-127 - add biobank-specific technical covariates if required
7. Lines 222 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model3.sex"_ to _"modcoeffs.cox.model3"_
8. Lines 225-228 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
9. Line 231 - specify the location you want to save the model 3 output.  
- Output file is "&#42;_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt"

### Occupation
Run [CoxPHmodel3_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel3_Occupation.R) to run the Cox proportional hazard models with age at disease onset as timescale, where occupation is classified into "Manual worker", "Lower-level", "Upper-level" and **(optional)** "Self-employed" (reference = Manual worker), and include the trait-specific PRS by occupation level _(created in this script)_, sex (except for breast and prostate cancer), bith decade, and the first 10 genetic PCS as covariates. Please make the following adjustments:
1. Line 50 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 53 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 62 - specify file location + filename
4. Lines 75-77 - remove the lines related to the occupation categories not included in your biobank. As this example is based on UKB, _which does not include the "Self-employed" occupation class_, please adjust the code if your biobank does include the "Self-employed" occupation class:
```
Selfemployed <- ifelse(filelist$Occupation=="Self-employed", 1, 0)
```
5. Lines 80-82 - remove the lines related to the occupation categories not included in your biobank. As this example is based on UKB, _which does not include the "Self-employed" occupation class_, please adjust the code if your biobank does include the "Self-employed" occupation class:
```
GxSelfemployed <- filelist[,19] * Selfemployed
```
6. Line 85 - remove the occupation classes not included in your biobank. As this example is based on UKB, _which does not include the "Self-employed" occupation class_, please adjust the code if your biobank does include the "Self-employed" occupation class (the following example assumes all four occupation classes are present in your biobank):
```
out = cbind(GxManualworker,GxSelfemployed,GxLowerlevel,GxUpperlevel)
```
7. Lines 93, 98, 137, 163, 173, 183, 193, 203 + 213 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
8. Line 104-107 - adjust _"21"_ to the total number of columns in the dataset before adding the new variables and remove occupation classes not included in your biobank. As this example is based on UKB, _which does not include the "Self-employed" occupation class_, please adjust the code if your biobank does include the "Self-employed" occupation class (the following example assumes all four occupation classes are present in your biobank):
```
for (i in 1:length(INTERVENE.list)) {
  names(INTERVENE.list[[i]]) <- c(names(INTERVENE.list[[i]][1:21]),
                                  "GxManualworker","GxSelfemployed","GxLowerlevel","GxUpperlevel")
}
```
9. Lines 129-134 - add biobank-specific technical covariates if required and remove the occupation classes not included in your biobank. As this example is based on UKB, _which does not include the "Self-employed" occupation class_, please adjust the code if your biobank does include the "Self-employed" occupation class (the following example assumes all four occupation classes are present in your biobank):
```
mod3sex.formula <- paste0("GxManualworker + GxSelfemployed + GxLowerlevel + GxUpperlevel + 
                          SEX + PC1 + PC2 + PC3 + PC4 + PC5 + 
                          PC6 + PC7 + PC8 + PC9 + PC10")
mod3nosex.formula <- paste0("GxManualworker + GxSelfemployed + GxLowerlevel + GxUpperlevel + 
                          PC1 + PC2 + PC3 + PC4 + PC5 + 
                          PC6 + PC7 + PC8 + PC9 + PC10")
```
10. Lines 229 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model3.sex"_ to _"modcoeffs.cox.model3"_
11. Lines 232-235 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
12. Line 238 - specify the location you want to save the model 3 output.  
- Output file is "&#42;_INTERVENE_Occupation_CoxPH_model3_Coeffs.txt"

## Model 4: Determine the effect of the socioeconomic index, the trait-specific polygenic risk score (PRS), and their interaction on disease risk
### Educational Attainment
Run [CoxPHmodel4_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel4_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and include EA, the trait-specific PRS, the EA * trait-specific PRS interaction, sex (except for prostate and breast cancer), the first 10 genetics PCs, and birth decade as covariates. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 88-89 - add biobank-specific technical covariates if required
5. Lines 93, 119, 129, 139, 149, 159 + 169 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 185 + 191 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model4.sex"_ to _"modcoeffs.cox.model4"_
7. Lines 188-189 + 192-193 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 196 - specify the location you want to save the model 4 output.  
- Output file is "&#42;_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt"

### Occupation
Run [CoxPHmodel4_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel4_Occupation.R) to run the Cox proportional hazard models with age at disease onset as timescale, where occupation is classified into "Manual worker", "Lower-level", "Upper-level" and **(optional)** "Self-employed" (reference = Manual worker), and include occupation, the trait-specific PRS, the occupation * trait-specific PRS interaction, sex (except for prostate and breast cancer), the first 10 genetics PCs, and birth decade as covariates. Please make the following adjustments:
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 89-90 - add biobank-specific technical covariates if required
5. Lines 94, 120, 130, 140, 150, 160 + 170 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 186 + 192 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model4.sex"_ to _"modcoeffs.cox.model4"_
7. Lines 189-190 + 193-194 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 197 - specify the location you want to save the model 4 output.  
- Output file is "&#42;_INTERVENE_Occupation_CoxPH_model4_Coeffs.txt"

## Model 5: Determine the effect of the socioeconomic indices on disease risk in each of the three trait-specific polygenic risk score (PRS) groups
### Educational Attainment
Run [CoxPHmodel5_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel5_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and include sex (except for prostate and breast cancer), birth decade, and the first 5 genetic PCs as covariates in each of the groups stratified by PRS ("<25%", "25-75%", and ">75%"). Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Lines 63-65 - specify file location + filename
4. Lines 91, 117, 127, 137, 147, 157, 167, 198 + 225 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
5. Lines 183 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1a.Group1.sex"_ to _"modcoeffs.cox.model1a.Group1"_
6. Lines 186-189, 213-216 + 240-243 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
7. Lines 210 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1a.Group2.sex"_ to _"modcoeffs.cox.model1a.Group2"_
8. Lines 237 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1a.Group3.sex"_ to _"modcoeffs.cox.model1a.Group3"_
9. Line 192, 219 + 246 - specify the location you want to save the model 5 output.  
- Output files are "&#42;_INTERVENE_EducationalAttainment_CoxPH_model5_Coeffs_PGSGroup1.txt", "&#42;_INTERVENE_EducationalAttainment_CoxPH_model5_Coeffs_PGSGroup2.txt", and "&#42;_INTERVENE_EducationalAttainment_CoxPH_model5_Coeffs_PGSGroup3.txt"

### Occupation
Run [CoxPHmodel5_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel5_Occupation.R) to run the Cox proportional hazard models with age at disease onset as timescale, where occupation is classified into "Manual worker", "Lower-level", "Upper-level" and (optional) "Self-employed" (reference = Manual worker), and include sex (except for prostate and breast cancer), birth decade, and the first 5 genetic PCs as covariates in each of the groups stratified by PRS ("<25%", "25-75%", and ">75%"). Please make the following adjustments:
1. Line 52 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Lines 64-66 - specify file location + filename
4. Lines 92, 118, 128, 138, 148, 158, 168, 199 + 226 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
5. Lines 184 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1a.Group1.sex"_ to _"modcoeffs.cox.model1a.Group1"_
6. Lines 187-190, 214-217 + 241-244 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
7. Lines 211 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1a.Group2.sex"_ to _"modcoeffs.cox.model1a.Group2"_
8. Lines 238 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1a.Group3.sex"_ to _"modcoeffs.cox.model1a.Group3"_
9. Line 193, 220 + 247 - specify the location you want to save the model 5 output.  
- Output files are "&#42;_INTERVENE_Occupation_CoxPH_model5_Coeffs_PGSGroup1.txt", "&#42;_INTERVENE_Occupation_CoxPH_model5_Coeffs_PGSGroup2.txt", and "&#42;_INTERVENE_Occupation_CoxPH_model5_Coeffs_PGSGroup3.txt"


# Part 3: Meta-analyses across biobank studies
Please note that the meta-analysis scripts are only provided for Educational Attainment. 
## Model 1: Meta-analyze the individual effect of the socioeconomic indices or trait-specific polygenic risk score (PRS) on disease risk
### Model 1a SES effect, with Educational Attainment as the socioeconomic index
Run [MetaAnalysismodel1a_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel1a_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including sex (except for prostate and breast cancer), birth decade and the first 5 genetic principal components (PCs) as covariates. This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a_anyN.csv"

### Model 1b PRS effect, without Educational Attainment as the socioeconomic index
Run [MetaAnalysismodel1b_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel1b_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, with the trait-specific PRS, sex (except for prostate and breast cancer), the first 10 genetic PCs, and birth decade as covariates. This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b_anyN.csv"

## Model 2: Meta-analyze the effect of the socioeconomic indices and the trait-specific polygenic risk score (PRS) together on disease risk 
Run [MetaAnalysismodel2_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel2_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including the trait-specific PRS, sex (except for prostate and breast cancer), birth decade and the first 10 genetic PCs as covariates. This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2_anyN.csv"

## Compare the meta-analyzed effects of the socioeconomic indices and the trait-specific polygenic risk scores (PRS) as obtained from model 1 (unadjusted) with those obtained in model 2 (adjusted). 
Comparisons are done with two-sided Wald tests after Bonferroni correction for multiple testing of 19 phenotypes (p < 2.63x10-03).
Run [MetaAnalysismodel1vs2differences_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel1vs2differences_EducationalAttainment.R) to compare the meta-analyzed estimates from model 1 and model 2 to determine whether analyzing the socioeconomic indices and PRSs jointly significantly differ from analyzing them separately. This script downloads the summary statistics per biobank study and across biobank studies (meta-analysis) from Google Drive and also uploads the result of the comparisons to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv"

## Model 3: Meta-analyze the effect of the trait-specific polygenic risk score (PRS) per level of the socioeconomic index on disease risk
Run [MetaAnalysismodel3_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel3_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including the trait-specific PRS by EA level _(created in this script)_, sex (except for breast and prostate cancer), bith decade, and the first 10 genetic PCS as covariates. This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_anyN.csv"

## Compare the meta-analyzed effects the trait-specific polygenic risk score (PRS) per level of the socioeconomic index on disease risk between the levels of the socioeconomic index
Comparisons are done with two-sided Wald tests after Bonferroni correction for multiple testing of 19 phenotypes (p < 2.63x10-03).
Run [MetaAnalysismodel3differences_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel3differences_EducationalAttainment.R) to compare the meta-analyzed estimates effects the trait-specific polygenic risk score (PRS) per level of the socioeconomic index on disease risk between the levels of the socioeconomic index to determine whether the effect of the PRS differs significantly between the levels of the socioeconomic index. This script downloads the summary statistics per biobank study and across biobank studies (meta-analysis) from Google Drive and also uploads the result of the comparisons to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_Differences.csv"

## Model 4: Meta-analyze the effect of the socioeconomic index, the trait-specific polygenic risk score (PRS), and their interaction on disease risk
Run [MetaAnalysismodel4_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel4_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including EA, the trait-specific PRS, the EA * trait-specific PRS interaction, sex (except for prostate and breast cancer), the first 10 genetics PCs, and birth decade as covariates. This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4_anyN.csv"

## Model 5: Meta-analyze the effect of the socioeconomic indices on disease risk in each of the three trait-specific polygenic risk score (PRS) groups
Run [MetaAnalysismodel5_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel5_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including sex (except for prostate and breast cancer), birth decade, and the first 5 genetic PCs as covariates in each of the groups stratified by PRS ("<25%", "25-75%", and ">75%"). This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model5_anyN.csv"
