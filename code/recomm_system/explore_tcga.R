library(data.table)
library(reshape2)
library(plyr)

explore_biotab <- function()
{
  foldername <- "/Users/blahiri/healthcare/data/tcga/37b4840f-20a4-49a0-bd7e-6e2026a82eab/Clinical/Biotab"
  #data <- fread(paste(foldername, "/", "nationwidechildrens.org_clinical_patient_gbm.txt", sep = ""), sep = "\t", header = TRUE) 
  
  files <- list.files(foldername)
  for (file in files)
  {
    data <- fread(paste(foldername, "/", file, sep = ""), sep = "\t", header = TRUE)
    cat(paste("\n\n", "file = ", file, ", nrow(data) = ", nrow(data), "\n", sep = ""))
    print(colnames(data))
  }
}

explore_cgds <- function()
{
  library(cgdsr)
  mycgds = CGDS("http://www.cbioportal.org/public-portal/")
  getCancerStudies(mycgds) #returns data with 88 rows and these 3 columns: (cancer_study_id, name, description)
  # Get available case lists (collection of samples) for a given cancer study
  #mycancerstudy = getCancerStudies(mycgds)[2,1] #returns the cancer_study_id laml_tcga
  mycancerstudy = getCancerStudies(mycgds)[29,1] #returns the cancer_study_id gbm_tcga
  #mycaselist = getCaseLists(mycgds,mycancerstudy)[1,1] #returns the case_list_id laml_tcga_3way_complete. Description says "All tumor samples that have mRNA, CNA and sequencing data (163 samples)"
  mycaselist = getCaseLists(mycgds,mycancerstudy)[2,1] #returns the case_list_id gbm_tcga_all. Description says "All tumor samples (613 samples)"
  # Get clinical data for caselist
  getClinicalData(mycgds,mycaselist)

  #The columns are the following:
  #[1] "AGE"                                     "DAYS_TO_BIRTH"                           "DAYS_TO_DEATH"                           "DAYS_TO_INITIAL_PATHOLOGIC_DIAGNOSIS"    "DFS_MONTHS"                             
  #[6] "DFS_STATUS"                              "ECOG_SCORE"                              "ETHNICITY"                               "GENDER"                                  "HISTORY_NEOADJUVANT_TRTYN"              
  #[11] "HISTORY_OTHER_MALIGNANCY"                "ICD_10"                                  "ICD_O_3_HISTOLOGY"                       "ICD_O_3_SITE"                            "INITIAL_PATHOLOGIC_DX_YEAR"             
  #[16] "KARNOFSKY_PERFORMANCE_SCORE"             "LAST_CONTACT_DAYS_TO"                    "METHOD_OF_SAMPLE_PROCUREMENT"            "NEW_TUMOR_EVENT_AFTER_INITIAL_TREATMENT" "OS_MONTHS"                              
  #[21] "OS_STATUS"                               "PERFORMANCE_STATUS_TIMING"               "PHARMACEUTICAL_TX_ADJUVANT"              "PROSPECTIVE_COLLECTION"                  "RACE"                                   
  #[26] "RADIATION_TREATMENT_ADJUVANT"            "RETROSPECTIVE_COLLECTION"                "SPECIMEN_SECOND_LONGEST_DIMENSION"       "TISSUE_SOURCE_SITE"     "TREATMENT_OUTCOME_FIRST_COURSE"         
  #[31] "TUMOR_STATUS"                            "TUMOR_TISSUE_SITE"                       "VITAL_STATUS"

  #DFS: Disease-Free Survival
  #ECOG: Eastern Cooperative Oncology Group: to quantify cancer patients' general well-being and activities of daily life: values range from 0 (Best) to 5 (Death)
  #Neoadjuvant treatment: administration of therapeutic agents before a main treatment
  #ICD 10 codes here are C71.9 and C71.2. C71.9 stands for primary or metastatic malignant neoplasm affecting the brain. C71.2 stands for primary malignant neoplasm of temporal lobe.
  #ICD-O-3: International Classification of Diseases for Oncology, 3rd edition. WHO has a system of classification of the tumors of the central nervous system. /0 indicates "benign" tumor, /3 malignant tumor and /1 borderline tumor. ICD_O_3_HISTOLOGY has only one value in this set: 9440/3 indicates Glioblastoma. ICD_O_3_SITE is mostly C71.9.
  #Karnofsky performance score is to quantify cancer patients' general well-being and activities of daily life. 100 is normal, 0 is dead, standard intervals of 10.
  #METHOD_OF_SAMPLE_PROCUREMENT: Values are Tumor resection, Excisional Biopsy, Other method, specify:, Fine needle aspiration biopsy, Incisional Biopsy
  #OS: Overall Survival, median is 11.83 months. OS_STATUS is 539 DECEASED, 53 LIVING (8.9% LIVING).
  #PERFORMANCE_STATUS_TIMING: Values are Post-Adjuvant Therapy, Pre-Adjuvant Therapy and Pre-Operative. Adjuvant therapy, also called adjuvant care, is treatment 
  #that is given in addition to the primary, main or initial treatment.
  #TISSUE_SOURCE_SITE: A Tissue Source Site (TSS) collects samples (tissue, cell or blood) and clinical metadata, which are then sent to a BCR. Most are 06, which stands for Henry Ford Hospital. 
  #Details are at https://tcga-data.nci.nih.gov/datareports/codeTablesReport.htm?codeTable=tissue%20source%20site
  #TREATMENT_OUTCOME_FIRST_COURSE: Possible values are Complete Remission/Response, Partial Remission/Response, Progressive Disease, Stable Disease.
  #TUMOR_STATUS: TUMOR FREE:27, WITH TUMOR: 512
  #TUMOR_TISSUE_SITE: Brain for all 594.
  #VITAL_STATUS: 139 Alive, 453 Dead (23.4% Alive).
}

explore_gatech <- function()
{
  #Explore the dataset obtained from Kunal (GA Tech)
  foldername <- "/Users/blahiri/healthcare/data/tcga/Raw_Data"
  patients <- fread(paste(foldername, "/", "clinical_patient_gbm.txt", sep = ""), sep = "\t", header = TRUE) #576 patients
  drugs <- fread(paste(foldername, "/", "clinical_drug_gbm.txt", sep = ""), sep = "\t", header = TRUE) #1427 records
  radiation <- fread(paste(foldername, "/", "clinical_radiation_gbm.txt", sep = ""), sep = "\t", header = TRUE) #519 records
  follow_up <- fread(paste(foldername, "/", "clinical_follow_up_v1.0_gbm.txt", sep = ""), sep = "\t", header = TRUE) #614 records

  length(intersect(patients$bcr_patient_barcode, drugs$bcr_patient_barcode)) #has some drug data for 420 patients out of 576
  length(intersect(patients$bcr_patient_barcode, radiation$bcr_patient_barcode)) #has some radiation data for 422 patients out of 576

  table(patients$vital_status) #137 alive, 437 dead, 23.78% alive
  has_death_data <- subset(patients, (!days_to_death %in% c("[Not Applicable]", "[Not Available]"))) 
  nrow(has_death_data) #437 (437/576 = 75.86% patients): Note: the remaining 137 are alive patients
  fivenum(as.numeric(has_death_data $days_to_death)) #3  154  359  588 3880 - median survival is marginally less than a year


  sort(table(drugs$drug_name), decreasing = TRUE) 
  #The most commonly used drugs are: Temodar, Temozolomide, Avastin, Dexamethasone, CCNU, Temodor, Bevacizumab. Need some fuzzy matching with drugs as "Temozolomide"
  #is often spelt as "Temozolamide", and so on.

  table(radiation$course_number) #valid values are 01, 02, 03 and 04
 
  sort(table(radiation$radiation_dosage), decreasing = TRUE) #6000 is the most common value

  sort(table(radiation$units), decreasing = TRUE) #cGy is the most common unit (0.01 of a gray, or 1 rad), sometimes mCi is also used

  sort(table(as.factor(radiation$radiation_type)), decreasing = TRUE) #values are EXTERNAL BEAM, RADIOISOTOPES, IMPLANTS, External and COMBINATION

  radiation_numeric <- subset(radiation, (!(days_to_radiation_therapy_start %in% c("[Not Available]", "[Completed]")) & 
                                          !(days_to_radiation_therapy_end %in% c("[Not Available]", "[Completed]")))) #Has 488 records, so 488/519 = 94% records have complete duration info. 
  rad_duration <- as.numeric(radiation_numeric$days_to_radiation_therapy_end) - as.numeric(radiation_numeric$days_to_radiation_therapy_start)
  print(fivenum(rad_duration)) #The 5-number statistics for radiation duration is  0  30  42  44 345
  z_rad_duration <- (rad_duration - mean(rad_duration))/sd(rad_duration)
  qqnorm(z_rad_duration); abline(0,1) #Normal distribution is not a good fit
  
  hist(patients$age_at_initial_pathologic_diagnosis)
  z_age <- (patients$age_at_initial_pathologic_diagnosis - mean(patients$age_at_initial_pathologic_diagnosis))/sd(patients$age_at_initial_pathologic_diagnosis)
  qqnorm(z_age); abline(0,1) #Age is normally distributed

  drugs_numeric <- subset(drugs, (!(days_to_drug_therapy_start %in% c("[Not Available]", "[Completed]")) & 
                                  !(days_to_drug_therapy_end %in% c("[Not Available]", "[Completed]")))) #Has 1182 records, so 1182/1427 = 83% records have complete duration info.
  print(fivenum(as.numeric(drugs_numeric$days_to_drug_therapy_end) - as.numeric(drugs_numeric$days_to_drug_therapy_start)))
  #The 5-number statistics for drug duration is 0 34 57 132 3440
  

  #Focus on patient with bcr_patient_barcode = "TCGA-12-0653" and extract the complete history
  pat <- subset(patients, (bcr_patient_barcode == 'TCGA-12-0653'))

  #bcr_patient_barcode = TCGA-12-0653, age_at_initial_pathologic_diagnosis = 65, anatomic_neoplasm_subdivision = [Not Available], 
  #bcr_patient_uuid = a7b55dae-c9b8-45c6-986f-fb1055f1c67b, date_of_form_completion = 2012-7-14, days_to_birth = -23952, days_to_death = 320, 
  #days_to_initial_pathologic_diagnosis = 0, days_to_last_followup = [Not Available], eastern_cancer_oncology_group = [Not Available], ethnicity = NOT HISPANIC OR LATINO,
  #gender = MALE, histological_type = Untreated primary (de novo) GBM, history_of_neoadjuvant_treatment = No, icd_10 = C71.9, icd_o_3_histology = 9440/3, 
  #icd_o_3_site = C71.9, informed_consent_verified = YES, initial_pathologic_diagnosis_method = Excisional Biopsy, karnofsky_performance_score = 60,
  #patient_id = 653, performance_status_scale_timing = Pre-Adjuvant Therapy, person_neoplasm_cancer_status = WITH TUMOR, prior_glioma = NO, race = WHITE, tissue_source_site = Brain, 
  #vital_status = DECEASED, year_of_initial_pathologic_diagnosis = 2002

  drug <- subset(drugs, (bcr_patient_barcode == 'TCGA-12-0653'))
  #There were 7 different records from drug administration data for this patient. Gliadel Wafers was applied once, Temozolomide was applied twice, CCNU was applied twice, O6B6
  #twice. The values of [days_to_drug_therapy_start, days_to_drug_therapy_end] were [0, 0], [14, 110], [110, 155], [155, 210], [210, 267], [281, 283], [281, 283]. Patient died 
  #on day 320.
  
  rad <- subset(radiation, (bcr_patient_barcode == 'TCGA-12-0653'))
  #There was one radiation record for this patient.
  #bcr_radiation_barcode = TCGA-12-0653-R33742, anatomic_treatment_site = Primary Tumor Field, bcr_radiation_uuid = C68458FC-E0C6-4522-A2E3-52B15B4BC790, 
  #course_number = [Not Available], days_to_radiation_therapy_end = 75, days_to_radiation_therapy_start = 14, radiation_treatment_ongoing = NO, radiation_type = External.
  #Radiation was given in the period [14, 75], which means it was given alongwith the drug Temozolomide which was given in [14, 110].
 
  setkey(patients, bcr_patient_barcode) 
  setkey(drugs, bcr_patient_barcode)
  patients_drugs <- merge(x = patients, y = drugs)
  all_combined <- merge(x = as.data.frame(patients_drugs), y = as.data.frame(radiation), by = "bcr_patient_barcode")
  write.csv(all_combined, paste(foldername, "/", "clinical_all_combined_gbm.csv", sep = ""))
}

process_names_further <- function(processed_name, top_k_names)
{
  if (processed_name %in% top_k_names) 
    return(processed_name)
  return('Other_drug')
}

transform_drug_names <- function()
{
  foldername <- "/Users/blahiri/healthcare/data/tcga/Raw_Data/v3"
  drugs <- as.data.frame(fread(paste(foldername, "/", "clinical_drug_gbm.txt", sep = ""), sep = "\t", header = TRUE))
  drug_names <- read.csv(paste(foldername, "/", "drug_names.csv", sep = ""))
  drugs <- merge(x = drugs, y = drug_names, by.x = "drug_name", by.y = "name_in_raw_data", all.x = TRUE)

  #Keep the top k processed names as they are. Convert everything else to 'Other'.
  ordered_names <- sort(table(drugs$processed_name), decreasing = TRUE)
  k <- 9
  top_k_names <- rownames(as.data.frame(ordered_names))[1:k]
  drugs$processed_name <- apply(drugs, 1, function(row)process_names_further(row["processed_name"], top_k_names))
  write.csv(drugs, paste(foldername, "/", "clinical_drug_processed_names_gbm.csv", sep = ""))

  drugs[drugs == "[Not Available]"] <- NA
  drugs[drugs == "[Completed]"] <- NA

  #At this point we only focus on whether a drug was given to a patient or not, so if a drug was given to same patient multiple times, 
  #we only take the combination once

  drugs <- unique(drugs[, c("bcr_patient_barcode", "processed_name")])
  drugs$dummy <- 1
  drugs.wide <- dcast(drugs, bcr_patient_barcode ~ processed_name, value.var = "dummy")
  drugs.wide[is.na(drugs.wide)] <- 0

  #420 records: verified from Postgres: these are the patients who had some drug ever
  write.csv(drugs.wide, paste(foldername, "/", "clinical_drug_widened_gbm.csv", sep = ""))
  drugs.wide
}


process_radiation_names <- function(radiation_type)
{
  ifelse((radiation_type == 'EXTERNAL BEAM'), 'EXTERNAL BEAM',  'Other_radiation')
}


transform_radiation_data <- function()
{
  foldername <- "/Users/blahiri/healthcare/data/tcga/Raw_Data/v3"
  radiation <- as.data.frame(fread(paste(foldername, "/", "clinical_radiation_gbm.txt", sep = ""), sep = "\t", header = TRUE))
  
  radiation <- radiation[, c("bcr_patient_barcode", "days_to_radiation_therapy_end", 
                             "days_to_radiation_therapy_start", "radiation_type")]
  radiation[radiation == "[Not Available]"] <- NA
  radiation[radiation == "[Completed]"] <- NA
  radiation$processed_name <- apply(radiation, 1, function(row)process_radiation_names(row["radiation_type"]))

  #At this point we only focus on whether a radiation was given to a patient or not, so if a radiation was given to same patient multiple times, 
  #we only take the combination once

  radiation <- unique(radiation[, c("bcr_patient_barcode", "processed_name")])
  radiation$dummy <- 1
  radiation.wide <- dcast(radiation, bcr_patient_barcode ~ processed_name, value.var = "dummy")

  #For some patients, no radiation type is available at all: a column called “NA” was being created for these patients. We simply put them under “Other”.
  radiation.wide$Other_radiation[which(radiation.wide[,"NA"] == 1)] <- 1
  radiation.wide <- radiation.wide[, c("bcr_patient_barcode", "EXTERNAL BEAM", "Other_radiation")]
  radiation.wide[is.na(radiation.wide)] <- 0

  #422 records: verified from Postgres: these are the patients who had some radiation ever
  write.csv(radiation.wide, paste(foldername, "/", "clinical_radiation_widened_gbm.csv", sep = ""))
  radiation.wide
}

combine_wide_formats <- function()
{
  foldername <- "/Users/blahiri/healthcare/data/tcga/Raw_Data/v3"
  patients <- as.data.frame(fread(paste(foldername, "/", "clinical_patient_gbm.txt", sep = ""), sep = "\t", header = TRUE))
  patients <- patients[, c("bcr_patient_barcode", "age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "histological_type", "history_of_neoadjuvant_treatment", 
                                   "initial_pathologic_diagnosis_method", "karnofsky_performance_score", "person_neoplasm_cancer_status", "prior_glioma", 
                                   "race", "days_to_death", "vital_status")]
  drugs.wide <- read.csv(paste(foldername, "/", "clinical_drug_widened_gbm.csv", sep = ""))
  radiation.wide <- read.csv(paste(foldername, "/", "clinical_radiation_widened_gbm.csv", sep = ""))
  patients_drugs <- merge(patients, drugs.wide, all.x = TRUE)
  all_combined <- merge(x = patients_drugs, y = radiation.wide, by = "bcr_patient_barcode", all.x = TRUE)

  #For drug and radiation variables (including days_to_death), replace NAs by 0. For demographic and case history variables, replace [Not Available] or [Not Applicable] by NA.
  demog_ch_vars <- c("age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "race", "histological_type", "history_of_neoadjuvant_treatment", "initial_pathologic_diagnosis_method",                            "karnofsky_performance_score", 
                     "person_neoplasm_cancer_status", "prior_glioma", "days_to_death")
  drug_radiation_vars <- c("Avastin", "BCNU", "CCNU", "CPT.11", "Dexamethasone", "Gliadel.Wafer",  
                           "Other_drug", "Tarceva", "Temozolomide", "VP.16", "EXTERNAL.BEAM", 
                           "Other_radiation")
  
  for (variable in demog_ch_vars)
  {
    all_combined[, variable][(all_combined[, variable] == '[Not Available]')] <- NA
    all_combined[, variable][(all_combined[, variable] == '[Not Applicable]')] <- NA
  }
  for (variable in drug_radiation_vars)
  {
    cat(paste("variable = ", variable, "\n", sep = ""))
    all_combined[, variable][is.na(all_combined[, variable])] <- 0
  }

  all_combined <- imputation(all_combined)
  cat("Remove crappy columns like the first with row numbers, X.x, X.y, etc\n")
  write.csv(all_combined, paste(foldername, "/", "clinical_all_combined_gbm.csv", sep = ""))
} 

#A quick imputation where we fill in the missing values according to the distribution of the variables

imputation <- function(all_combined)
{
  vars <- c("ethnicity", "history_of_neoadjuvant_treatment", "initial_pathologic_diagnosis_method", "person_neoplasm_cancer_status", "race", "karnofsky_performance_score")
  for (var in vars)
  {
    vector <- all_combined[, var] 
    na_removed <- vector[!is.na(vector)]
    freq_dist <- table(na_removed)
    levels <- names(freq_dist)
    freq_dist <- as.numeric(table(na_removed))/sum(as.numeric(table(na_removed)))
    names(freq_dist) <- levels
    cum_freq_dist <- cumsum(freq_dist)
    n_vector <- length(vector)
    n_levels <- length(levels)

    for (i in 1:n_vector)
    {
      if (is.na(vector[i]))
      {
         rand <- runif(1)
         if (rand <= cum_freq_dist[1])
         {
           sampled_level <- levels[1]
         }
         else
         {
          for (j in 1:(n_levels - 1))
          {
            if (rand > cum_freq_dist[j] & rand <= cum_freq_dist[j+1])
            {
              sampled_level <- levels[j+1]
              break
            }
          }
         } #end else
         #cat(paste("rand = ", rand, ", sampled_level = ", sampled_level, "\n", sep = ""))
         vector[i] <- sampled_level
      } #end if (is.na(vector[i]))
    } #end for (i in 1:n_vector)
    all_combined[, var] <- vector
  }
  all_combined
}

create_dummy_variables <- function()
{
  foldername <- "/Users/blahiri/healthcare/data/tcga/Raw_Data/v2"
  all_combined  <- read.csv(paste(foldername, "/", "clinical_all_combined_gbm.csv", sep = ""))
  transformed <- all_combined

  transformed[, "ethnicityHISPANIC OR LATINO"] <- as.numeric(all_combined$ethnicity == 'HISPANIC OR LATINO')
  transformed$genderMALE <- as.numeric(all_combined$gender == 'MALE')
  transformed[, "histological_typeTreated primary GBM"] <- as.numeric(all_combined$histological_type == 'Treated primary GBM')
  transformed[, "histological_typeGlioblastoma Multiforme (GBM)"] <- as.numeric(all_combined$histological_type == 'Glioblastoma Multiforme (GBM)')
  transformed[, "history_of_neoadjuvant_treatmentNo"] <- as.numeric(all_combined$history_of_neoadjuvant_treatment == 'No')
  transformed[, "initial_pathologic_diagnosis_methodExcisional Biopsy"] <- as.numeric(all_combined$initial_pathologic_diagnosis_method == 'Excisional Biopsy')
  transformed[, "initial_pathologic_diagnosis_methodIncisional Biopsy"] <- as.numeric(all_combined$initial_pathologic_diagnosis_method == 'Incisional Biopsy')
  transformed[, "initial_pathologic_diagnosis_methodOther method, specify:"] <- as.numeric(all_combined$initial_pathologic_diagnosis_method == 'Other method, specify:')
  transformed[, "initial_pathologic_diagnosis_methodFine needle aspiration biopsy"] <- as.numeric(all_combined$initial_pathologic_diagnosis_method == 'Fine needle aspiration biopsy')
  transformed[, "person_neoplasm_cancer_statusTUMOR FREE"] <- as.numeric(all_combined$person_neoplasm_cancer_status == 'TUMOR FREE')
  transformed[, "prior_gliomaYES"] <- as.numeric(all_combined$prior_glioma == 'YES')
  transformed[, "raceBLACK OR AFRICAN AMERICAN"] <- as.numeric(all_combined$race == 'BLACK OR AFRICAN AMERICAN')
  transformed[, "raceASIAN"] <- as.numeric(all_combined$race == 'ASIAN')

  write.csv(transformed, paste(foldername, "/", "transformed.csv", sep = ""))
}





