# Scripts for the INTERVENE GxE SES and complex diseases project
The analyses are broken up into [Part 1](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/README.md#part-1-biobank-specific-general-data-preparation), general data preparation for individual-level analyses in each Biobank, [Part 2](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/README.md#part-2-biobank-specific-analyses), individual-level analyses with Educational Attainment and Occupation in each Biobank, [Part 3](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/README.md#part-3-meta-analyses-across-biobank-studies), meta-analyses done on summary statistics to draw conclusions across biobank studies, [Part 4](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/README.md#part-4-absolute-risk-estimation), calculation of cummulative risk incidences, [Part 5](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/README.md#part-5-prediction-comparison-in-each-biobank-study), prediction comparison in each biobank study, and [Part 6](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/README.md#part-6-create-supplemental-tables-and-figures-as-included-in-the-manuscript-for-this-project), scripts to creates the (supplemental) tables and figures as included in the manuscript describing this project.

### Dependencies  
These scripts assume you have plink-1.9 and R v4.3.2 or higher installed on your biobank computing system. Required R libraries: data.table, foreach, doParallel, lubridate, tidyverse/tidyr, dplyr, plyr, forcats, stringr, survival, metafor, Hmisc, pROC, nricens, googledrive, googlesheets4, ggplot2, viridis, cowplot, grid, gridExtra, and RColorBrewer.

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

## Step 4: Compute polygenic scores (PGSs) with Plink
The scripts in this step are identical to the [INTERVENE flagship project](https://github.com/intervene-EU-H2020/flagship).  
### If your Plink files are not split by chromosome
Run [GeneratePRS.sh](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/GeneratePRS.sh) to create the PGSs for your Biobank. For more information on running this script see the [flagship README](https://github.com/intervene-EU-H2020/flagship?tab=readme-ov-file#step-4-compute-prs-using-plink).  
### If your Plink files are split by chromosome
#### Step 4a: run [GeneratePRS_IndividualChr.sh](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/GeneratePRS_IndividualChr.sh)
Run [GeneratePRS_IndividualChr.sh](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/GeneratePRS_IndividualChr.sh) to create the PGSs per chromosome for your Biobank. For more information on running this script see the [flagship README](https://github.com/intervene-EU-H2020/flagship?tab=readme-ov-file#step-4a-run-script-generateprs_individualchrsh).  
#### Step 4b: run [PRSSummationOverChr.R](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/PRSSummationOverChr.R)
Run [PRSSummationOverChr.R](https://github.com/intervene-EU-H2020/flagship/blob/main/PRS/PRSSummationOverChr.R) to sum over the per chromosome PGSs to create a total PGS for each trait in your Biobank. For more information on running this script see the [flagship README](https://github.com/intervene-EU-H2020/flagship?tab=readme-ov-file#step-4b-run-script-prssummationoverchrr).

## Step 5: Construct combined phenotype and polygenic score (PGS) R objects 
### Step 5a: Educational Attainment
Run [DataPrep_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/DataPrep_EducationalAttainment.R) to create the EA-specific sample for each trait in your Biobank. Please make the following adjustments: 
1. Line 50 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 53 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify phenotype file location + filename
4. Lines 66 + 75 - specify the location of the folder containing PGS weights
5. Line 80 - please replace the identifier name with that used in your biobank.
6. Lines 92-93 - if Educational Attainment has not been converted to ISCED 1997 from ISCED 2011, replace _"EDUCATION_11"_ in the code that creates the factor with the naming convention for ISCED 2011 education in your biobank; if Educational Attainment has already been converted to ISCED 1997 instead of ISCED 2011, replace it (if applicable) with code to make the ISCED 1997 variable a factor: 
```
pheno$ISCED97 <- factor(pheno$ISCED97, levels = c(1,2,3,4,5,6), # remove the ISCED 1997 levels not available in your biobank
                    labels = c("ISCED 1","ISCED 2","ISCED 3","ISCED 4","ISCED 5", "ISCED 6)) # remove the ISCED 1997 not available in your biobank
```
7. Lines 96-122 - Run if ISCED 2011 has not yet been recoded to ISCED 1997 (otherwise out-comment); remove the ISCED 1997 levels not available in your biobank
8. Lines 131-133 - remove the ISCED 1997 levels not available in your biobank
9. Line 149 - please replace the identifier names with those used in your biobank.
10. Line 154 - if your biobank contains individuals of non-European ancestry/those that have principal components calculated for NON-EUROPEAN ancestry, i.e., within ancestry principal components, not global genetic principal components, please add code to only retain individuals of European ancestry after this line (*In case of multiple ancestries in a Biobank, generate each file separately per ancestry, in which case the following has to be adapted to only retain the ancestry of interest*), for example,:
```
pheno <- subset(pheno, ANCESTRY=='EUR')
```
11. Lines 167-223 - Code assumes you have kept the same shorthand names for the phenotypes as within [FinnGen](https://docs.google.com/spreadsheets/d/1DNKd1KzI8WOIfG2klXWskbCSyX6h5gTu/edit#gid=334983519) (column B) and you have kept the same naming structure for the PGS files as when you downloaded them. Please adjust the names of the standard covariates before running this code if the current names do not match the naming convention in your biobank and add additional (technical) covariates as required. Remove any of the traits not applicable in your biobank (i.e., if the biobank was included in GWAS the summary statistics were based on, see Supplementary Data 10 of the [INTERVENE flagship manuscript](https://doi.org/10.1038/s41467-024-48938-2).
12. Line 260, 265, 277, 285, 295, 386 + 391 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
13. Lines 299 + 304 - add additional (technical) covariates if required
14. Line 323-327 - If none of the PGSs have been flipped (e.g., all associations are positive) then you must out-comment these lines. 
15. Lines 343-350 - remove birth decades _not_ present in your biobank and _add_ birth decades not included in the code that are included in your biobank. 
**16. Before writing the output to file, please check whether each subgroup for each trait has >=5 individuals!** Remove traits from the list if <5 individuals in a subgroup, e.g., with the following code: 
```
Listname[c(x,y,z)] <- NULL # where x, y, and z are the shorthand names for the phenotypes as in FinnGen
```
17. Lines 399 - specify the location you want to save the .Rdata file. *In case of multiple ancestries in a Biobank, generate each file separately per ancestry, and add abbreviation of the ancestry between the name of the biobank and "INTERVENE" on line 400.*
- Output files is "&#42;_INTERVENE_EducationalAttainment_dat.RData".

### Step 5b: Educational Attainment - Split data into 80% training and 20% test for prediction models
Run [DataPrep_EducationalAttainment_TrainTestSplit.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/DataPrep_EducationalAttainment_TrainTestSplit.R) to create the EA-specific samples (80% train + 20% test) for each trait in your Biobank. Please make the following adjustments: 
1. Line 50 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 53 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify phenotype file location + filename
4. Lines 66 + 75 - specify the location of the folder containing PGS weights
5. Line 80 - please replace the identifier name with that used in your biobank.
6. Lines 92-93 - if Educational Attainment has not been converted to ISCED 1997 from ISCED 2011, replace _"EDUCATION_11"_ in the code that creates the factor with the naming convention for ISCED 2011 education in your biobank; if Educational Attainment has already been converted to ISCED 1997 instead of ISCED 2011, replace it (if applicable) with code to make the ISCED 1997 variable a factor: 
```
pheno$ISCED97 <- factor(pheno$ISCED97, levels = c(1,2,3,4,5,6), # remove the ISCED 1997 levels not available in your biobank
                    labels = c("ISCED 1","ISCED 2","ISCED 3","ISCED 4","ISCED 5", "ISCED 6)) # remove the ISCED 1997 not available in your biobank
```
7. Lines 96-122 - Run if ISCED 2011 has not yet been recoded to ISCED 1997 (otherwise out-comment); remove the ISCED 1997 levels not available in your biobank
8. Lines 131-133 - remove the ISCED 1997 levels not available in your biobank
9. Line 149 - please replace the identifier names with those used in your biobank.
10. Line 154 - if your biobank contains individuals of non-European ancestry/those that have principal components calculated for NON-EUROPEAN ancestry, i.e. within ancestry principal components, not global genetic principal components, please add code to only retain individuals of European ancestry after this line, for example,:
```
pheno <- subset(pheno, ANCESTRY=='EUR')
```
11. Lines 177-162 + 428-446 - Code assumes you have kept the same shorthand names for the phenotypes as within [FinnGen](https://docs.google.com/spreadsheets/d/1DNKd1KzI8WOIfG2klXWskbCSyX6h5gTu/edit#gid=334983519) (column B) and you have kept the same naming structure for the PGS files as when you downloaded them. Please adjust the names of the standard covariates before running this code if the current names do not match the naming convention in your biobank and add additional (technical) covariates as required. Remove any of the traits not applicable in your biobank (i.e., if the biobank was included in GWAS the summary statistics were based on, see Supplementary Data 10 of the [INTERVENE flagship manuscript](https://doi.org/10.1038/s41467-024-48938-2).
12. Line 277, 282, 294, 302, 312, 403, 408, 450, 455, 467, 475, 485, 525 + 530 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
13. Lines 316, 321, 489 + 494 - add additional (technical) covariates if required
14. Lines 340-344 + 512-517 - If none of the PGSs have been flipped (e.g., all associations are positive) then you must out-comment these lines. 
15. Lines 360-367 - remove birth decades _not_ present in your biobank and _add_ birth decades not included in the code that are included in your biobank. 
**16. Before writing the output to file, please check whether each subgroup for each trait has >=5 individuals!** Remove traits from the list if <5 individuals in a subgroup, e.g., with the following code: 
```
Listname[c(x,y,z)] <- NULL # where x, y, and z are the shorthand names for the phenotypes as in FinnGen
```
17. Lines 416 + 538 - specify the locations you want to save the .Rdata files.  
- Output files are "&#42;_INTERVENE_EducationalAttainment_dat_80percent.RData" and "&#42;_INTERVENE_EducationalAttainment_dat_20percent.RData".

### Step 5c: Occupation
Run [DataPrep_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/DataPrep_Occupation.R) to create the occupation-specific sample for each trait in your Biobank. Please make the following adjustments: 
1. Line 52 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line 
2. Line 55 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 65 - specify phenotype file location + filename
4. Line 69 - If not already included in the phenotype file, specify occupation file location + filename. Otherwise, out-comment this line.
5. Lines 72 + 81 - specify the location of the folder containing PGS weights
6. Line 86 - please replace the identifier name with that used in your biobank.
7. Lines 100-123 - Replace the example coding (based on UKB data) with the codes relevant to your biobank and remove lines related to the occupation category not included in your biobank. 
8. Lines 126-129 - If occupation information has already been categorized, out-comment these lines, if occupation information has not been categorized, please replace the identifier names with those used in your biobank. 
9. Line 132 - If occupation information has already been categorized, please replace _"phenos"_ with _"pheno"_, and replace the identifier names with those used in your biobank.
10. Line 136 - if your biobank contains individuals of non-European ancestry/those that have principal components calculated for NON-EUROPEAN ancestry, i.e., within ancestry principal components, not global genetic principal components, please add code to only retain individuals of European ancestry after this line, for example,:
```
pheno <- subset(pheno, ANCESTRY=='EUR')
```
11. Lines 150-228 - Code assumes you have kept the same shorthand names for the phenotypes as within [FinnGen](https://docs.google.com/spreadsheets/d/1DNKd1KzI8WOIfG2klXWskbCSyX6h5gTu/edit#gid=334983519) (column B) and you have kept the same naming structure for the PGS files as when you downloaded them. Please adjust the names of the standard covariates before running this code if the current names do not match the naming convention in your biobank and add additional (technical) covariates as required. Remove any of the traits not applicable in your biobank (i.e., if the biobank was included in GWAS the summary statistics were based on, see Supplementary Data 10 of the [INTERVENE flagship manuscript](https://doi.org/10.1038/s41467-024-48938-2).
12. Lines 243, 248, 260, 268 + 277 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
13. Lines 281 + 286 - add additional (technical) covariates if required
14. Line 305-309 - If none of the PGSs have been flipped (e.g., all associations are positive) then you must out-comment these lines. 

**15. Before writing the output to file, please check whether each subgroup for each trait has >=5 individuals!** Remove traits from the list if <5 individuals in a subgroup, e.g., with the following code: 
```
Listname[c(x,y,z)] <- NULL # where x, y, and z are the shorthand names for the phenotypes as in FinnGen
```
16. Line 323 - specify the location you want to save the .Rdata file.  
- Output file is "&#42;_INTERVENE_Occupation_dat.RData".

## Step 6: Calculate descriptives for each of the socioeconomic indices and trait combinations
### Step 6a: Educational Attainment
Run [Descriptives_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/Descriptives_EducationalAttainment.R) to calculate the summary statistics on the phenotype files including Educational Attainment. Please make the following adjustments: 
1. Line 49 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 52 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 61 - specify file location + filename
4. Line 143 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
5. Lines 152-157  - if your Biobank was included in the prostate cancer GWASs or the number of individuals in any subgroup was <5, and you cannot investigate this trait, out-comment or remove these lines
7. Lines 159-164 - if your Biobank was included in the breast cancer GWASs or the number of individuals in any subgroup was <5, and you cannot investigate this trait, out-comment or remove these lines
6. Line 164 - specify the location you want to save the descriptive file. *If the descriptives were generated for the 80-20% split files, change the file name on line 168 to reflect this by adding "_80percent" or "_20percent" at the end of the file name, respectively.*
- Output files is  "&#42;_INTERVENE_EducationalAttainment_SampleDescriptives.txt"

### Step 6b: Occupation
Run [Descriptives_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/Descriptives_Occupation.R) to calculate the summary statistics on the phenotype files including Occupation. Please make the following adjustments: 
1. Line 50 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 53 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 62 - specify file location + filename
4. Line 144 - if running on a single core or a Rstudio session with automatic multi-threading, replace %dopar& with %do%
5. Lines 153-158 - if your Biobank was included in the prostate cancer GWASs or the number of individuals in any subgroup was <5, and you cannot investigate this trait, out-comment or remove these lines
6. Lines 160-165 - if your Biobank was included in the breast cancer GWASs or the number of individuals in any subgroup was <5, and you cannot investigate this trait, out-comment or remove these lines
7. Line 168 - specify the location you want to save the descriptive file.  
- Output files is  "&#42;_INTERVENE_Occupation_SampleDescriptives.txt"

## Step 7: Create plots of the distribution of the scaled polygenic scores by level of the socioeconomic index
### Step 7a: Educational Attainment
Run [ComparePGSDistribution_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/DataPrep/ComparePGSDistribution_EducationalAttainment.R) to create the plots of the distribution of the scaled polygenic scores by Educational Attainment level. Please make the following adjustments: 
1. Line 52 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 55 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 65 - specify phenotype file location + filename
4. Lines 68 + 77 - specify the location of the folder containing PGS weights
5. Line 82 - please replace the identifier name with that used in your biobank.
6. Lines 95-96 - if Educational Attainment has not been converted to ISCED 1997 from ISCED 2011, replace _"EDUCATION_11"_ in the code that creates the factor with the naming convention for ISCED 2011 education in your biobank; if Educational Attainment has already been converted to ISCED 1997 instead of ISCED 2011, replace it (if applicable) with code to make the ISCED 1997 variable a factor: 
```
pheno$ISCED97 <- factor(pheno$ISCED97, levels = c(1,2,3,4,5,6), # remove the ISCED 1997 levels not available in your biobank
                    labels = c("ISCED 1","ISCED 2","ISCED 3","ISCED 4","ISCED 5", "ISCED 6)) # remove the ISCED 1997 not available in your biobank
```
7. Lines 99-124 - Run if ISCED 2011 has not yet been recoded to ISCED 1997 (otherwise out-comment); remove the ISCED 1997 levels not available in your biobank
8. Lines 132-135 - remove the ISCED 1997 levels not available in your biobank
9. Line 151 - please replace the identifier names with those used in your biobank.
10. Line 155 - if your biobank contains individuals of non-European ancestry/those that have principal components calculated for NON-EUROPEAN ancestry, i.e. within ancestry principal components, not global genetic principal components, please add code to only retain individuals of European ancestry after this line, for example,:
```
pheno <- subset(pheno, ANCESTRY=='EUR')
```
17. Line 192 - specify the location you want to save the plots.  
- Output file is "&#42;_INTERVENE_FIG_CompareScaledPGSDistribution_ByEducationalAttainment.pdf".

# Part 2: Biobank-specific analyses
## Model 1: Determine the individual effect of the socioeconomic indices or trait-specific polygenic score (PGS) on disease risk
### Model 1a SES effect, with Educational Attainment as the socioeconomic index
Run [CoxPHmodel1a_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel1a_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and include sex (except for prostate and breast cancer), birth decade and the first 5 genetic principal components (PCs) as covariates. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 89, 115, 125, 135, 145,155 + 165 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
5. Lines 181 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1a.sex"_ to _"modcoeffs.cox.model1a"_
6. Lines 184-187 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
7. Line 190 - specify the location you want to save the model 1a output. *In case of multiple ancestries in a Biobank, generate each file separately per ancestry, and add the abbreviation of the ancestry between the name of the biobank and "INTERVENE" on line 191.* 
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

### Model 1b PGS effect, without Educational Attainment as the socioeconomic index
Run [CoxPHmodel1b_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel1b_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, with the trait-specific PGS, sex (except for prostate and breast cancer), the first 10 genetic PCs, and birth decade as covariates. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 85-86 - add biobank-specific technical covariates if required
5. Lines 90, 116, 126, 136, 146, 156 + 166 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 182 + 188 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1b.sex"_ to _"modcoeffs.cox.model1b"_
7. Lines 185-186 + 189-190 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 193 - specify the location you want to save the model 1b output. *In case of multiple ancestries in a Biobank, generate each file separately per ancestry, and add the abbreviation of the ancestry between the name of the biobank and "INTERVENE" on line 194.*  
- Output file is "&#42;_INTERVENE_EducationalAttainment_CoxPH_model1b_Coeffs.txt"

### Model 1b PGS effect, without Occupation as the socioeconomic index
Run [CoxPHmodel1b_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel1b_Occupation.R) to run the Cox proportional hazard models with age at disease onset as timescale, with the trait-specific PGS, sex (except for prostate and breast cancer), the first 10 genetic PCs, and birth decade as covariates. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 85-86 - add biobank-specific technical covariates if required
5. Lines 90, 116, 126, 136, 146, 156 + 166 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 182 + 188 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model1b.sex"_ to _"modcoeffs.cox.model1b"_
7. Lines 185-186 + 189-190 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 193 - specify the location you want to save the model 1b output.  
- Output file is "&#42;_INTERVENE_Occupation_CoxPH_model1b_Coeffs.txt"

## Model 2: Determine the effect of the socioeconomic indices and the trait-specific polygenic score (PGS) together on disease risk 
### Educational Attainment 
Run [CoxPHmodel2_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel2_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and include the trait-specific PGS, sex (except for prostate and breast cancer), birth decade and the first 10 genetic PCs as covariates. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 86-87 - add biobank-specific technical covariates if required
5. Lines 91, 117, 127, 137, 147, 157 + 167 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 174 + 180 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model2.sex"_ to _"modcoeffs.cox.model2"_
7. Lines 186-187 + 190-191 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 194 - specify the location you want to save the model 2 output. *In case of multiple ancestries in a Biobank, generate each file separately per ancestry, and add the abbreviation of the ancestry between the name of the biobank and "INTERVENE" on line 195.*  
- Output file is "&#42;_INTERVENE_EducationalAttainment_CoxPH_model2_Coeffs.txt"

### Occupation 
Run [CoxPHmodel2_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel2_Occupation.R) to run the Cox proportional hazard models with age at disease onset as timescale, where occupation is classified into "Manual worker", "Lower-level", "Upper-level" and **(optional)** "Self-employed" (reference = Manual worker), and include the trait-specific PGS, sex (except for prostate and breast cancer), birth decade and the first 10 genetic PCs as covariates. Please make the following adjustments:
1. Line 52 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 55 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 64 - specify file location + filename
4. Lines 89-90 - add biobank-specific technical covariates if required
5. Lines 94, 120, 130, 140, 150, 160 + 170 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 186 + 192 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model2.sex"_ to _"modcoeffs.cox.model2"_
7. Lines 189-190 + 193-194 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 197 - specify the location you want to save the model 2 output.  
- Output file is "&#42;_INTERVENE_Occupation_CoxPH_model2_Coeffs.txt"

## Model 3: Determine the effect of the trait-specific polygenic score (PGS) stratified by level of the socioeconomic index on disease risk
### Educational Attainment
Run [CoxPHmodel3_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel3_EducationalAttainment.R) to run the Education stratified (where EA is dichotomized into low vs high EA) Cox proportional hazard models with age at disease onset as timescale, and include the trait-specific PGS, sex (except for breast and prostate cancer), bith decade, and the first 10 genetic PCS as covariates. Please make the following adjustments: 
1. Line 50 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 53 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 62 - specify file location + filename
4. Lines 71, 76, 119, 126, 153, 163, 173, 183 + 203 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
5. Lines 115-116 - add biobank-specific technical covariates if required
6. Lines 219-222 + lines 230-231 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model3.sex.low"_ and _"modcoeffs.cox.model3.sex.high"_ to _"modcoeffs.cox.model3.low"_ and _"modcoeffs,cox.model3.high"_ 
7. Lines 223-227 + lines 232-233 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 234 - if you cannot run the analyses for prostate and breast cancer, remove _"modcoeffs.cox.model3.nosex.low"_ and _"modcoeffs.cox.model3.nosex.high"_
9. Line 231 - specify the location you want to save the model 3 output. *In case of multiple ancestries in a Biobank, generate each file separately per ancestry, and add abbreviation of the ancestry between the name of the biobank and "INTERVENE" on line 238.*  
- Output file is "&#42;_INTERVENE_EducationalAttainment_CoxPH_model3_Coeffs.txt"

### Occupation
Run [CoxPHmodel3_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel3_Occupation.R) to run the Occupation stratified (where occupation is dichotomized into lower-level vs upper-level occupation) Cox proportional hazard models with age at disease onset as timescale, and include the trait-specific PGS, sex (except for breast and prostate cancer), bith decade, and the first 10 genetic PCS as covariates. Please make the following adjustments:
1. Line 49 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 52 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 61 - specify file location + filename
4. Lines 70, 75, 152, 162, 172, 182, 192 + 202 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
5. Lines 113-114 - add biobank-specific technical covariates if required 
10. Lines 218-221, 229-230 + 233 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model3.sex.low"_ to _"modcoeffs.cox.model3.low"_ and _"modcoefffs.cox.model3.sex.high"_ to _"modcoeffs.cox.model3.high"_
11. Lines 223-226 + 231-232 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
12. Line 239 - specify the location you want to save the model 3 output.  
- Output file is "&#42;_INTERVENE_Occupation_CoxPH_model3_Coeffs.txt"

## Model 4: Determine the effect of the socioeconomic index, the trait-specific polygenic score (PGS), and their interaction on disease risk
### Educational Attainment
Run [CoxPHmodel4_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel4_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and include EA, the trait-specific PGS, the EA * trait-specific PGS interaction, sex (except for prostate and breast cancer), the first 10 genetics PCs, and birth decade as covariates. Please make the following adjustments: 
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 88-89 - add biobank-specific technical covariates if required
5. Lines 93, 119, 129, 139, 149, 159 + 169 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 185 + 191 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model4.sex"_ to _"modcoeffs.cox.model4"_
7. Lines 188-189 + 192-193 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 196 - specify the location you want to save the model 4 output. *In case of multiple ancestries in a Biobank, generate each file separately per ancestry, and add the abbreviation of the ancestry between the name of the biobank and "INTERVENE" on line 197.*  
- Output file is "&#42;_INTERVENE_EducationalAttainment_CoxPH_model4_Coeffs.txt"

### Occupation
Run [CoxPHmodel4_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel4_Occupation.R) to run the Cox proportional hazard models with age at disease onset as timescale, where occupation is classified into "Manual worker", "Lower-level", "Upper-level" and **(optional)** "Self-employed" (reference = Manual worker), and include occupation, the trait-specific PGS, the occupation * trait-specific PGS interaction, sex (except for prostate and breast cancer), the first 10 genetics PCs, and birth decade as covariates. Please make the following adjustments:
1. Line 51 - if you're running this on a single core or a Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 54 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Line 63 - specify file location + filename
4. Lines 89-90 - add biobank-specific technical covariates if required
5. Lines 94, 120, 130, 140, 150, 160 + 170 - if running on a single core or a Rstudio session with automatic multi-threading, replace _%dopar%_ with _%do%_
6. Lines 186 + 192 - if you cannot run the analyses for prostate and breast cancer, rename _"modcoefffs.cox.model4.sex"_ to _"modcoeffs.cox.model4"_
7. Lines 189-190 + 193-194 - if you cannot run the analyses for prostate and breast cancer, out-comment or remove these lines
8. Line 197 - specify the location you want to save the model 4 output.  
- Output file is "&#42;_INTERVENE_Occupation_CoxPH_model4_Coeffs.txt"

## Model 5: Determine the effect of the socioeconomic indices on disease risk in each of the three trait-specific polygenic score (PGS) groups
### Educational Attainment
Run [CoxPHmodel5_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel5_EducationalAttainment.R) to run the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and include sex (except for prostate and breast cancer), birth decade, and the first 5 genetic PCs as covariates in each of the groups stratified by PGS ("<25%", "25-75%", and ">75%").  
**Please note that these analyses have been discontinued.**

### Occupation
Run [CoxPHmodel5_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/CoxModels/CoxPHmodel5_Occupation.R) to run the Cox proportional hazard models with age at disease onset as timescale, where occupation is classified into "Manual worker", "Lower-level", "Upper-level" and (optional) "Self-employed" (reference = Manual worker), and include sex (except for prostate and breast cancer), birth decade, and the first 5 genetic PCs as covariates in each of the groups stratified by PGS ("<25%", "25-75%", and ">75%").  
**Please note that these analyses have been discontinued.**


# Part 3: Meta-analyses across biobank studies
Please note that the meta-analysis scripts are only provided for Educational Attainment. 
## Model 1: Meta-analyze the individual effect of the socioeconomic indices or trait-specific polygenic score (PGS) on disease risk
### Model 1a SES effect, with Educational Attainment as the socioeconomic index
Run [MetaAnalysismodel1a_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel1a_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including sex (except for prostate and breast cancer), birth decade and the first 5 genetic principal components (PCs) as covariates. This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1a_anyN.csv"

### Model 1b PGS effect, without Educational Attainment as the socioeconomic index
Run [MetaAnalysismodel1b_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel1b_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, with the trait-specific PGS, sex (except for prostate and breast cancer), the first 10 genetic PCs, and birth decade as covariates. This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1b_anyN.csv"

## Model 2: Meta-analyze the effect of the socioeconomic indices and the trait-specific polygenic score (PGS) together on disease risk 
Run [MetaAnalysismodel2_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel2_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including the trait-specific PGS, sex (except for prostate and breast cancer), birth decade and the first 10 genetic PCs as covariates. This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model2_anyN.csv"

## Compare the meta-analyzed effects of the socioeconomic indices and the trait-specific polygenic scores (PGS) as obtained from model 1 (unadjusted) with those obtained in model 2 (adjusted). 
Comparisons are done with two-sided Wald tests after Bonferroni correction for multiple testing of 19 phenotypes (p < 2.63x10-03).
Run [MetaAnalysismodel1vs2differences_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel1vs2differences_EducationalAttainment.R) to compare the meta-analyzed estimates from model 1 and model 2 to determine whether analyzing the socioeconomic indices and PGSs jointly significantly differ from analyzing them separately. This script downloads the summary statistics per biobank study and across biobank studies (meta-analysis) from Google Drive and also uploads the result of the comparisons to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model1vs2_Differences.csv"

## Model 3: Meta-analyze the effect of the trait-specific polygenic score (PGS) per level of the socioeconomic index on disease risk
Run [MetaAnalysismodel3_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel3_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including the trait-specific PGS by EA level _(created in this script)_, sex (except for breast and prostate cancer), bith decade, and the first 10 genetic PCS as covariates. This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model3_anyN.csv"

## Compare the meta-analyzed effects the trait-specific polygenic score (PGS) per level of the socioeconomic index on disease risk between the levels of the socioeconomic index
Comparisons are done with two-sided Wald tests after Bonferroni correction for multiple testing of 19 phenotypes (p < 2.63x10-03).
Run [MetaAnalysismodel3differences_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel3differences_EducationalAttainment.R) to compare the meta-analyzed estimates effects the trait-specific polygenic score (PGS) per level of the socioeconomic index on disease risk between the levels of the socioeconomic index to determine whether the effect of the PGS differs significantly between the levels of the socioeconomic index. This script downloads the summary statistics per biobank study and across biobank studies (meta-analysis) from Google Drive and also uploads the result of the comparisons to Google Drive.   
**Please note that these analyses have been discontinued.**

## Model 4: Meta-analyze the effect of the socioeconomic index, the trait-specific polygenic score (PGS), and their interaction on disease risk
Run [MetaAnalysismodel4_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel4_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including EA, the trait-specific PGS, the EA * trait-specific PGS interaction, sex (except for prostate and breast cancer), the first 10 genetics PCs, and birth decade as covariates. This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this project, we meta-analysed across the FinnGen study, the UK Biobank, and Generation Scotland.
- Output file is "&#42;_INTERVENE_EducationalAttainment_FEMetaAnalysis_FinnGenR11_UKB_GenScot_model4_anyN.csv"

## Meta-analyze models 2 and 4 in the UK Biobank and Generation Scotland only
Run [MetaAnalysismodel2and4_EducationalAttainment_UKB_GenScot_only.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel2and4_EducationalAttainment_UKB_GenScot_only.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including the trait-specific PGS, sex (except for prostate and breast cancer), birth decade and the first 10 genetic PCs as covariates (model 2) and to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including EA, the trait-specific PGS, the EA * trait-specific PGS interaction, sex (except for prostate and breast cancer), the first 10 genetics PCs, and birth decade as covariates (model 4). This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive. In this script, we only meta-analysed across the UK Biobank and Generation Scotland so we could use the resulting estimates to compare the predictive accuracy of model 2 vs. model 4 in the FinnGen study.  
**Please note that these analyses have been discontinued.**

## Model 5: Meta-analyze the effect of the socioeconomic indices on disease risk in each of the three trait-specific polygenic score (PGS) groups
Run [MetaAnalysismodel5_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Meta-analyses/MetaAnalysismodel5_EducationalAttainment.R) to run the fixed-effect meta-analysis across biobank studies using the beta coefficients from the Cox proportional hazard models with age at disease onset as timescale, where EA is dichotomized into low vs high EA (reference = low EA), and including sex (except for prostate and breast cancer), birth decade, and the first 5 genetic PCs as covariates in each of the groups stratified by PGS ("<25%", "25-75%", and ">75%"). This script downloads the summary statistics per biobank study from Google Drive and also uploads the resulting meta-analysis to Google Drive.   
**Please note that these analyses have been discontinued.**

# Part 4: Absolute Risk Estimation
Absolute risk estimation is only performed in the FinnGen study.
## Step 1: Extract mortality, prevalence, and incidence of the 19 complex diseases from the 2019 Global Burden of Disease (GBD) Study 
Download the 2019 GBD data from [INTERVENE flagship project](https://github.com/intervene-EU-H2020/flagship/tree/main/AbsoluteRiskEstimation).  
Run [GBD_csv_processing.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/AbsoluteRiskEstimation/GBD_csv_processing.R) to extract the mortality, prevalence, and incidence for each of the 19 complex diseases from the 2019 GBD study.

## Step 2: Estimate cumulative incidences
### Educational Attainment
Run [AbsoluteRiskEstimation_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/AbsoluteRiskEstimation/AbsoluteRiskEstimation_EducationalAttainment.R) to calculate the cumulative risk incidences by low and high educational attainment and polygenic score (PGS) strata for the 7 complex diseases with significant interactions between disease-specific PGSs and educational attainment or occupation in the FinnGen study. Aside from the mortality, prevalence, and incidence of the complex diseases from the 2019 GBD study as obtained in step 1, this script also requires the results of the PGS-stratified Cox Proportional Hazard models in FinnGen (see ADD REF).
### Occupation
Run [AbsoluteRiskEstimation_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/AbsoluteRiskEstimation/AbsoluteRiskEstimation_Occupation.R) to calculate the cumulative risk incidences by lower- and upper-level occupation and polygenic score (PGS) strata for the 7 complex diseases with significant interactions between disease-specific PGSs and educational attainment or occupation in the FinnGen study. Aside from the mortality, prevalence, and incidence of the complex diseases from the 2019 GBD study as obtained in step 1, this script also requires the results of the PGS-stratified Cox Proportional Hazard models in FinnGen (see ADD REF).

## Step 3: Obtain confidence intervals for cumulative incidences
### Educational Attainment
Run [AbsoluteRiskEstimation_Bootstrap_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/AbsoluteRiskEstimation/AbsoluteRiskEstimation_Bootstrap_EducationalAttainment.R) to calculate confidence intervals for the cumulative risk incidences obtained in step 2. Aside from the mortality, prevalence, and incidence of the complex diseases from the 2019 GBD study as obtained in step 1 and cumulative incidences as obtained in step 2, this script also requires the results of the PGS-stratified Cox Proportional Hazard models in FinnGen (see ADD REF).
### Occupation
Run [AbsoluteRiskEstimation_Bootstrap_Occupation.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/AbsoluteRiskEstimation/AbsoluteRiskEstimation_Bootstrap_Occupation.R) to calculate confidence intervals for the cumulative risk incidences obtained in step 2. Aside from the mortality, prevalence, and incidence of the complex diseases from the 2019 GBD study as obtained in step 1and cumulative incidences as obtained in step 2, this script also requires the results of the PGS-stratified Cox Proportional Hazard models in FinnGen (see ADD REF).

# Part 5: Prediction comparison in each biobank study
## Compare prediction model 0a vs model 1a
Run [PredictionComparison_Model0avsModel1a_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Prediction/PredictionComparison_Model0avsModel1a_EducationalAttainment.R) to compare the prediction accuracy of model 0a (covariates only [sex if relevant, birth year, first 5 genetic principal components]) with model 1a (main effect of education) by comparing the models Receiver Operating Characteristic (ROC) Area Under the Curve(AUC), continuous Net Reclassification Index (NRI), and Integrated Discrimination Index (IDI). When predicting in 20% of the FinnGen study and the UK Biobank: the output of the logistic regression models 0a and 1a from 80% of the FinnGen study (download output files from [here](https://drive.google.com/drive/folders/1pxDlg6Mt610pdBChhRt8Tx2A7DgUtIdw?usp=sharing). Please make the following adjustments:
1. Line 61 - if you're running this on a single core or an Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 64 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Lines 73, 78, and 81 - specify file location + filename
4. Lines 249 and 310 - specify location output folder
- Output files are "&#42;_INTERVENE_EducationalAttainment_AUCcomparison_Model0a-1a.txt" and "&#42;_INTERVENE_EducationalAttainment_NRI_IDI_Model0a-1a.txt"

## Compare prediction model 0b vs model 1b
Run [PredictionComparison_Model0bvsModel1b_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Prediction/PredictionComparison_Model0bvsModel1b_EducationalAttainment.R) to compare the prediction accuracy of model 0b (covariates only [sex if relevant, birth year, first 10 genetic principal components]) with model 1b (main effect of disease-specific PGS) by comparing the models Receiver Operating Characteristic (ROC) Area Under the Curve(AUC), continuous Net Reclassification Index (NRI), and Integrated Discrimination Index (IDI). When predicting in 20% of the FinnGen study and the UK Biobank: the output of the logistic regression models 0b and 1b from 80% of the FinnGen study (download output files from [here](https://drive.google.com/drive/folders/1pxDlg6Mt610pdBChhRt8Tx2A7DgUtIdw?usp=sharing). Please make the following adjustments:
1. Line 61 - if you're running this on a single core or an Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 64 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Lines 73, 78, and 81 - specify file location + filename
4. Lines 247 and 308 - specify location output folder
- Output files are "&#42;_INTERVENE_EducationalAttainment_AUCcomparison_Model0b-1b.txt" and "&#42;_INTERVENE_EducationalAttainment_NRI_IDI_Model0b-1b.txt"

## Compare prediction model 1a vs model 2
Run [PredictionComparison_Model1avsModel2_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Prediction/PredictionComparison_Model1avsModel2_EducationalAttainment.R) to compare the prediction accuracy of model 1a (main effect of education) with model 2 (main effects of education and disease-specific PGS) by comparing the models Receiver Operating Characteristic (ROC) Area Under the Curve(AUC), continuous Net Reclassification Index (NRI), and Integrated Discrimination Index (IDI). When predicting in 20% of the FinnGen study and the UK Biobank: the output of the logistic regression models 1a and 2 from 80% of the FinnGen study (download output files from [here](https://drive.google.com/drive/folders/1pxDlg6Mt610pdBChhRt8Tx2A7DgUtIdw?usp=sharing). Please make the following adjustments:
1. Line 61 - if you're running this on a single core or an Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 64 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Lines 73, 78, and 81 - specify file location + filename
4. Lines 212 and 272 - specify location output folder
- Output files are "&#42;_INTERVENE_EducationalAttainment_AUCcomparison_Model1a-2.txt" and "&#42;_INTERVENE_EducationalAttainment_NRI_IDI_Model1a-2.txt"

## Compare prediction model 1b vs model 2
Run [PredictionComparison_Model1bvsModel2_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Prediction/PredictionComparison_Model1bvsModel2_EducationalAttainment.R) to compare the prediction accuracy of model 1b (main effect of disease-specific PGS) with model 2 (main effects of education and disease-specific PGS) by comparing the models Receiver Operating Characteristic (ROC) Area Under the Curve(AUC), continuous Net Reclassification Index (NRI), and Integrated Discrimination Index (IDI). When predicting in 20% of the FinnGen study and the UK Biobank: the output of the logistic regression models 1b and 2 from 80% of the FinnGen study (download output files from [here](https://drive.google.com/drive/folders/1pxDlg6Mt610pdBChhRt8Tx2A7DgUtIdw?usp=sharing). Please make the following adjustments:
1. Line 61 - if you're running this on a single core or an Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 64 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Lines 73, 78, and 81 - specify file location + filename
4. Lines 249 and 305 - specify location output folder
- Output files are "&#42;_INTERVENE_EducationalAttainment_AUCcomparison_Model1b-2.txt" and "&#42;_INTERVENE_EducationalAttainment_NRI_IDI_Model1b-2.txt"

## Compare prediction model 2 vs model 4
Run [PredictionComparison_Model2vsModel4_EducationalAttainment.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Prediction/PredictionComparison_Model2vsModel4_EducationalAttainment.R) to compare the prediction accuracy of model 2 (main effects of education and disease-specific PGS) with model 4 (main effects of education and disease-specific PGS + their interaction) by comparing the models Receiver Operating Characteristic (ROC) Area Under the Curve(AUC), continuous Net Reclassification Index (NRI), and Integrated Discrimination Index (IDI). When predicting in 20% of the FinnGen study and the UK Biobank: the output of the logistic regression models 2 and 4 from 80% of the FinnGen study (download output files from [here](https://drive.google.com/drive/folders/1pxDlg6Mt610pdBChhRt8Tx2A7DgUtIdw?usp=sharing). Please make the following adjustments:
1. Line 61 - if you're running this on a single core or an Rstudio session with automatic multi-threading, you can choose to out-command this line
2. Line 64 - replace with the name of your biobank (don't include spaces in the biobank name)
3. Lines 73, 78, and 81 - specify file location + filename
4. Lines 252 and 311 - specify location output folder
- Output files are "&#42;_INTERVENE_EducationalAttainment_AUCcomparison_Model2-4.txt" and "&#42;_INTERVENE_EducationalAttainment_NRI_IDI_Model2-4.txt"

# Part 6: Create (supplemental) tables and figures as included in the manuscript for this project. 
Please note that the manuscript only includes the results for Educational Attainment. 
## (Supplemental) Tables: Scripts that create the Main Table 1 and the supplemental tables including the results from this project. 
Run [MainTable1.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Manuscript/Tables/MainTable1.R) to create the Main Table 1 in the manuscript, which contains the general descriptive statistics across biobank studies.
- Output file is "&#42;_INTERVENE_EducationalAttainment_MainTable1.txt"

Run [SupplementaryTables.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Manuscript/Tables/SupplementaryTables.R) to create the eight supplemental tables for the manuscript, namely: Table S4: descriptive statistics per cohohort, Table S5: results model 1: per cohort + meta-analysis; Table S6: results model 2: per cohort + meta-analysis; Table S7: significance test difference effect education and PGS model 1vs2; Table S8: results model 3 (per cohort + meta-analysis); Table S9 significance test difference effect PGS high vs low EA (model3); Table S10: results model 5: per cohort + meta-analysis; and Table S11: descriptive statistics per cohort and polygenic score strata.
- Output files are "&#42;_INTERVENE_EducationalAttainment_TableS4.txt", "&#42;_INTERVENE_EducationalAttainment_TableS5.txt", "&#42;_INTERVENE_EducationalAttainment_TableS6.txt", "&#42;_INTERVENE_EducationalAttainment_TableS7.txt", "&#42;_INTERVENE_EducationalAttainment_TableS8.txt", "&#42;_INTERVENE_EducationalAttainment_TableS9.txt", "&#42;_INTERVENE_EducationalAttainment_TableS10.txt", and "&#42;_INTERVENE_EducationalAttainment_TableS11.txt".

## (Supplemental) Figures: Scripts that create the Main Figures 1 and 2 and the supplemental figures depicting the results from this project. 
Please note that Main Figures 1 and 2 are completed in InkScape after creating the base figures in R.

Run [MainFigure1.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Manuscript/Figures/MainFigure1.R) to create the base for Main Figure 1 in the manuscript, which contains 2 panels: A: meta-analyzed results model 1b; B: meta-analyzed PGS results model 1a.
- Output file is "&#42;_INTERVENE_SESDiffDiseases_MainFigure1.png"

Run [MainFigure2.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Manuscript/Figures/MainFigure2.R) to create the base for Main Figure 2 in the manuscript, which contains the meta-analyzed results of model 3, including the comparison of the PGS estimates between the education groups.
- Output file is "&#42;_INTERVENE_SESDiffDiseases_MainFigure2.png"

Run [SupplementaryFigures.R](https://github.com/intervene-EU-H2020/GxE_SESDisease/blob/main/Manuscript/Figures/SupplementaryFigures.R) to create the four supplemental figures for the manuscript, namely: Figure S1: comparison of the hazard ratios per biobank study and the fixed-effect meta-analysis of model 1a and 1b; Figure S2: comparison of the hazard ratios per biobank study and the fixed-effect meta-analysis of model 2; Figure S3: comparison of the hazard ratios per biobank study and the fixed-effect meta-analysis of model 3; and Figure S4: comparison of the hazard ratios per biobank study and the fixed-effect meta-analysis of model 5.
- Output files are "&#42;_INTERVENE_EducationalAttainment_FigureS1.tiff", "&#42;_INTERVENE_EducationalAttainment_FigureS2.tiff", "&#42;_INTERVENE_EducationalAttainment_FigureS3.tiff", and "&#42;_INTERVENE_EducationalAttainment_FigureS4.tiff".
