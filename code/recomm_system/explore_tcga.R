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
  length(intersect(patients$bcr_patient_barcode, radiation$bcr_patient_barcode)) #has some radiation data for 420 patients out of 576

  table(patients$vital_status) #137 alive, 437 dead, 23.78% alive
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
  return('Other')
}

transform_drug_names <- function()
{
  foldername <- "/Users/blahiri/healthcare/data/tcga/Raw_Data"
  drugs <- as.data.frame(fread(paste(foldername, "/", "clinical_drug_gbm.txt", sep = ""), sep = "\t", header = TRUE))
  drug_names <- read.csv(paste(foldername, "/", "drug_names.csv", sep = ""))
  drugs <- merge(x = drugs, y = drug_names, by.x = "drug_name", by.y = "name_in_raw_data", all.x = TRUE)

  #Keep the top k processed names as they are. Convert everything else to 'Other'.
  ordered_names <- sort(table(drugs$processed_name), decreasing = TRUE)
  k <- 19
  top_k_names <- rownames(as.data.frame(ordered_names))[1:k]
  drugs$processed_name <- apply(drugs, 1, function(row)process_names_further(row["processed_name"], top_k_names))
  write.csv(drugs, paste(foldername, "/", "clinical_drug_processed_names_gbm.csv", sep = ""))

  drugs <- subset(drugs, (!(days_to_drug_therapy_start %in% c("[Not Available]", "[Completed]")) & 
                          !(days_to_drug_therapy_end %in% c("[Not Available]", "[Completed]"))))

  #There are instances of same patient having same medicine, with same start and end date, only differing in dose. Since we 
  #are not considering dosage at this point, let's keep only one of these.
  drugs <- unique(drugs[, c("bcr_patient_barcode", "processed_name", "days_to_drug_therapy_start", "days_to_drug_therapy_end")])
  drugs <- drugs[with(drugs, order(bcr_patient_barcode, days_to_drug_therapy_start)), ]
  drugs$drug_duration <- as.numeric(drugs$days_to_drug_therapy_end) - as.numeric(drugs$days_to_drug_therapy_start) + 1
  
  drugs <- drugs[, c("bcr_patient_barcode", "processed_name", "drug_duration")]
  #If the same patient has multiple episodes of the same drug, then add the durations.
  drugs.wide <- dcast(drugs, bcr_patient_barcode ~ processed_name, value.var = "drug_duration", fun.aggregate = sum)
  write.csv(drugs.wide, paste(foldername, "/", "clinical_drug_widened_gbm.csv", sep = ""))
  drugs.wide
}

transform_radiation_data <- function()
{
  foldername <- "/Users/blahiri/healthcare/data/tcga/Raw_Data"
  radiation <- as.data.frame(fread(paste(foldername, "/", "clinical_radiation_gbm.txt", sep = ""), sep = "\t", header = TRUE))
  
  radiation <- radiation[, c("bcr_patient_barcode", "days_to_radiation_therapy_end", 
                             "days_to_radiation_therapy_start", "radiation_type")]
  radiation <- subset(radiation, (!(days_to_radiation_therapy_start %in% c("[Not Available]", "[Completed]")) & 
                                          !(days_to_radiation_therapy_end %in% c("[Not Available]", "[Completed]"))))
  radiation <- subset(radiation, (radiation_type != "[Not Available]"))

  #If there are instances of same patient having same radiation type, with same start and end date, let's keep only one of these.
  radiation <- unique(radiation[, c("bcr_patient_barcode", "radiation_type", "days_to_radiation_therapy_start", "days_to_radiation_therapy_end")])
  radiation$rad_duration <- as.numeric(radiation$days_to_radiation_therapy_end) - as.numeric(radiation$days_to_radiation_therapy_start) + 1
  radiation <- radiation[, c("bcr_patient_barcode", "rad_duration", "radiation_type")]
  radiation <- radiation[with(radiation, order(bcr_patient_barcode)), ]
  radiation.wide <- dcast(radiation, bcr_patient_barcode ~ radiation_type, value.var = "rad_duration", fun.aggregate = sum)
  write.csv(radiation.wide, paste(foldername, "/", "clinical_radiation_widened_gbm.csv", sep = ""))
  radiation.wide
}

combine_wide_formats <- function()
{
  foldername <- "/Users/blahiri/healthcare/data/tcga/Raw_Data"
  patients <- as.data.frame(fread(paste(foldername, "/", "clinical_patient_gbm.txt", sep = ""), sep = "\t", header = TRUE))
  patients <- patients[, c("bcr_patient_barcode", "age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "histological_type", "history_of_neoadjuvant_treatment", 
                                   "initial_pathologic_diagnosis_method", "karnofsky_performance_score", "person_neoplasm_cancer_status", "prior_glioma", 
                                   "race", "vital_status")]
  drugs.wide <- read.csv(paste(foldername, "/", "clinical_drug_widened_gbm.csv", sep = ""))
  radiation.wide <- read.csv(paste(foldername, "/", "clinical_radiation_widened_gbm.csv", sep = ""))
  patients_drugs <- merge(patients, drugs.wide)
  all_combined <- merge(x = patients_drugs, y = radiation.wide, by = "bcr_patient_barcode")
  write.csv(all_combined, paste(foldername, "/", "clinical_all_combined_gbm.csv", sep = ""))
} 

process_vital_status <- function(status)
{
  if (status == 'Alive' || status == 'LIVING') 
    return('Alive')
  return('Dead')
}

construct_bn <- function()
{
  library(bnlearn)
  file_path <- "/Users/blahiri/healthcare/data/tcga/Raw_Data"
  dense_matrix <- read.csv(paste(file_path, "/", "clinical_all_combined_gbm.csv", sep = ""))

  #Filter out rows that create incomplete data
  dense_matrix <- subset(dense_matrix, (karnofsky_performance_score != "[Not Available]")) 
  dense_matrix <- subset(dense_matrix, (person_neoplasm_cancer_status != "[Not Available]")) 

  dense_matrix <- subset(dense_matrix, (person_neoplasm_cancer_status != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (ethnicity != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (race != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (history_of_neoadjuvant_treatment != "[Not Available]"))
  
  dense_matrix$vital_status <- apply(dense_matrix, 1, function(row)process_vital_status(row["vital_status"])) 
  dense_matrix <- dense_matrix[,!(names(dense_matrix) %in% c("bcr_patient_barcode"))]

  demog_vars <- c("age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "race")
  case_history_vars <- c("histological_type", "history_of_neoadjuvant_treatment", "initial_pathologic_diagnosis_method", "karnofsky_performance_score", 
                         "person_neoplasm_cancer_status", "prior_glioma")
  drug_vars <- colnames(dense_matrix)[12:31]
  radiation_vars <- colnames(dense_matrix)[32:37]

  factor_vars <- c(demog_vars, case_history_vars, "vital_status")
  numeric_vars <- c(drug_vars, radiation_vars, "age_at_initial_pathologic_diagnosis", "karnofsky_performance_score")
  factor_vars <- factor_vars[!factor_vars %in% numeric_vars]

  for (column in factor_vars)
  {
    dense_matrix[, column] <- factor(dense_matrix[, column], levels = unique(dense_matrix[, column]))
  }
  for (column in numeric_vars)
  {
    dense_matrix[, column] <- as.numeric(dense_matrix[, column])
  }

  #Create the blacklist
  blacklist <- expand.grid(demog_vars, demog_vars)

  df <- expand.grid(case_history_vars, case_history_vars)
  blacklist <- rbind(blacklist, df)

  df <- expand.grid(drug_vars, drug_vars)
  blacklist <- rbind(blacklist, df)

  df <- expand.grid(radiation_vars, radiation_vars)
  blacklist <- rbind(blacklist, df)

  #There should be no outgoing arrows from vital_status, only incoming arrows to it
  df <- data.frame("Var1" = "vital_status", "Var2" = c(factor_vars, numeric_vars))
  blacklist <- rbind(blacklist, df)

  #There should be no incoming arrows to any of the demographic variables, only outgoing arrows
  df <- expand.grid(c(case_history_vars, drug_vars, radiation_vars), demog_vars)
  blacklist <- rbind(blacklist, df)

  colnames(blacklist) <- c("from", "to")

  #Construct the network structure
  res = hc(dense_matrix , blacklist = blacklist, optimized = FALSE)
  #Fit the parameters of the Bayesian network, conditional on its structure
  fitted = bn.fit(res, dense_matrix)
  list("res" = res, "fitted" = fitted)
}

#Turn all drug and radiation-related variables to discrete ones
construct_bn_mostly_discrete <- function()
{
  library(bnlearn)
  file_path <- "/Users/blahiri/healthcare/data/tcga/Raw_Data"
  dense_matrix <- read.csv(paste(file_path, "/", "clinical_all_combined_gbm.csv", sep = ""))

  #Filter out rows that create incomplete data
  dense_matrix <- subset(dense_matrix, (karnofsky_performance_score != "[Not Available]")) 
  dense_matrix <- subset(dense_matrix, (person_neoplasm_cancer_status != "[Not Available]")) 

  dense_matrix <- subset(dense_matrix, (person_neoplasm_cancer_status != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (ethnicity != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (race != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (history_of_neoadjuvant_treatment != "[Not Available]"))
  
  dense_matrix$vital_status <- apply(dense_matrix, 1, function(row)process_vital_status(row["vital_status"])) 
  dense_matrix <- dense_matrix[,!(names(dense_matrix) %in% c("bcr_patient_barcode"))]

  demog_vars <- c("age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "race")
  case_history_vars <- c("histological_type", "history_of_neoadjuvant_treatment", "initial_pathologic_diagnosis_method", "karnofsky_performance_score", 
                         "person_neoplasm_cancer_status", "prior_glioma")
  drug_vars <- colnames(dense_matrix)[12:31]
  radiation_vars <- colnames(dense_matrix)[32:37]

  for (drug_var in drug_vars)
  {
    dense_matrix[, drug_var] <- (dense_matrix[, drug_var] > 0)
  }
  for (radiation_var in radiation_vars)
  {
    dense_matrix[, radiation_var] <- (dense_matrix[, radiation_var] > 0)
  }

  factor_vars <- c(demog_vars, case_history_vars, drug_vars, radiation_vars, "vital_status")
  numeric_vars <- c("age_at_initial_pathologic_diagnosis", "karnofsky_performance_score")
  factor_vars <- factor_vars[!factor_vars %in% numeric_vars]

  for (column in factor_vars)
  {
    dense_matrix[, column] <- factor(dense_matrix[, column], levels = unique(dense_matrix[, column]))
  }
  for (column in numeric_vars)
  {
    dense_matrix[, column] <- as.numeric(dense_matrix[, column])
  }

  #Create the blacklist
  blacklist <- expand.grid(demog_vars, demog_vars)

  df <- expand.grid(case_history_vars, case_history_vars)
  blacklist <- rbind(blacklist, df)

  df <- expand.grid(drug_vars, drug_vars)
  blacklist <- rbind(blacklist, df)

  df <- expand.grid(radiation_vars, radiation_vars)
  blacklist <- rbind(blacklist, df)

  #There should be no outgoing arrows from vital_status, only incoming arrows to it
  df <- data.frame("Var1" = "vital_status", "Var2" = c(factor_vars, numeric_vars))
  blacklist <- rbind(blacklist, df)

  #There should be no incoming arrows to any of the demographic variables, only outgoing arrows
  df <- expand.grid(c(case_history_vars, drug_vars, radiation_vars), demog_vars)
  blacklist <- rbind(blacklist, df)

  #There should be no arrows from drug variables to case history variables: other direction is OK
  df <- expand.grid(drug_vars, case_history_vars)
  blacklist <- rbind(blacklist, df)

  #There should be no arrows from radiation variables to case history variables: other direction is OK
  df <- expand.grid(radiation_vars, case_history_vars)
  blacklist <- rbind(blacklist, df)

  colnames(blacklist) <- c("from", "to")

  #whitelist <- data.frame("from" = c("OTHER..SPECIFY.IN.NOTES", "CCNU", "BCNU", "person_neoplasm_cancer_status"),
  #                        "to" = c("CCNU", "age_at_initial_pathologic_diagnosis", "person_neoplasm_cancer_status", "karnofsky_performance_score"))
  #print(whitelist)

  #sink(paste(file_path, "/", "debug_text.txt", sep = ""))
  #Construct the network structure
  res = hc(dense_matrix , blacklist = blacklist, 
             #whitelist = whitelist, 
             debug = FALSE)
  #Fit the parameters of the Bayesian network, conditional on its structure
  fitted = bn.fit(res, dense_matrix, debug = FALSE)
  #plot(res, highlight = c("vital_status", mb(res, "vital_status")))
  #sink()

  #Values returned by cpquery are different in different runs since it is generated by logic sampligs. Values in CPT tables are 
  #MLE estimates, and hence are derived simply from counts.
  prob <-  cpquery(fitted, (vital_status == 'Alive'), (person_neoplasm_cancer_status == 'WITH TUMOR') & (Irinotecan == 'TRUE')) #0.84375 whereas CPT table says 0.83333333 
  cat(paste("prob = ", prob, "\n", sep = ""))

  ss <- subset(dense_matrix, (person_neoplasm_cancer_status == 'WITH TUMOR') & (Irinotecan == 'TRUE'))
  fss <- subset(dense_matrix, (vital_status == 'Alive') & (person_neoplasm_cancer_status == 'WITH TUMOR') & (Irinotecan == 'TRUE'))  #0.833333333333333
  cat(paste("from counts, ", nrow(fss)/nrow(ss), "\n", sep = ""))

  prob <-  cpquery(fitted, (Avastin == 'TRUE'), (initial_pathologic_diagnosis_method == 'Tumor resection') & (OTHER..SPECIFY.IN.NOTES == 'FALSE'))
  cat(paste("prob = ", prob, "\n", sep = "")) #0.071498 whereas CPT table says 0.07185629

  ss <- subset(dense_matrix, (initial_pathologic_diagnosis_method == 'Tumor resection') & (OTHER..SPECIFY.IN.NOTES == 'FALSE'))
  fss <- subset(dense_matrix, (Avastin == 'TRUE') & (initial_pathologic_diagnosis_method == 'Tumor resection') & (OTHER..SPECIFY.IN.NOTES == 'FALSE')) #0.0718562874251497
  cat(paste("from counts, ", nrow(fss)/nrow(ss), "\n", sep = ""))

  prob <-  cpquery(fitted, (EXTERNAL.BEAM == 'FALSE'), (Gliadel.Wafer == 'FALSE'))
  cat(paste("prob = ", prob, "\n", sep = "")) #0.0112845 whereas CPT table says 0.01310044

  prob <- cpquery(fitted, (vital_status == 'Alive'), (age_at_initial_pathologic_diagnosis > 50) & (age_at_initial_pathologic_diagnosis < 60) & (ethnicity == 'NOT HISPANIC OR LATINO'))
  cat(paste("prob = ", prob, "\n", sep = "")) #Returns 0.27122464312547 for age in (50, 60), returns 0 when age is set to a fixed value: example of a continuos parent of a discrete node

  prob <- cpquery(fitted, (vital_status == 'Alive'), (Avastin == 'TRUE'))
  cat(paste("prob = ", prob, "\n", sep = "")) #Returns 0.316728167281673: example of a CP query where event and evidence variables do not have an arc between in the structure 

  list("res" = res, "fitted" = fitted)
}

#Test the accuracy of the model by randomly performing conditional probability queries, 
#for queries which have a good enough support 
test_bn <- function(fitted)
{
  node_names <- names(fitted)
  combined_table <- data.frame(matrix(ncol = 5))
  colnames(combined_table) <- c("event", "evidence1", "evidence2", "cpquery_result", "actual_cp")
  threshold <- 0.1
  rownumber <- 1

  for (node_name in node_names)
  {
    #fitted[[<node_name>]] is an object of class bn.fit.dnode, or bn.fit.gnode or bn.fit.cgnode
    if (class(fitted[[node_name]]) == 'bn.fit.dnode')
    {
      cptable <- as.data.frame(fitted[[node_name]][["prob"]]) 
      if (ncol(cptable) == 4)
      {
        n_cptable <- nrow(cptable)
        cols_cptable <- colnames(cptable)
        for (i in 1:n_cptable)
        {
          if (is.finite(cptable[i, "Freq"]) & cptable[i, "Freq"] >= threshold)
          {
            event <- paste("(",  cols_cptable[1], " == '", cptable[i, 1], "')", sep = "")
            combined_table[rownumber, "event"] <- event

            evidence1 <- paste("(",  cols_cptable[2], " == '", cptable[i, 2], "')", sep = "")
            combined_table[rownumber, "evidence1"] <- evidence1

            evidence2 <- paste("(",  cols_cptable[3], " == '", cptable[i, 3], "')", sep = "")
            combined_table[rownumber, "evidence2"] <- evidence2

            combined_table[rownumber, "actual_cp"] <- cptable[i, "Freq"]

            cpquery_expn <- paste("cpquery(fitted, ", event, ", ", evidence1, " & ", evidence2, ")", sep = "")
            cond_prob <- eval(parse(text = cpquery_expn))
            combined_table[rownumber, "cpquery_result"] <- cond_prob

            rownumber <- rownumber + 1
          }
        }
      }
    }
  }
  combined_table$percent_error <- 100*(abs(combined_table$cpquery_result - combined_table$actual_cp))/combined_table$actual_cp
  print(combined_table)
}


create_bs_by_over_and_undersampling <- function(dense_matrix)
{
  set.seed(1)
  n_dense_matrix <- nrow(dense_matrix)
  size_each_part <- n_dense_matrix/2

  majority_set <- subset(dense_matrix, (vital_status == 'Dead'))
  n_majority <- nrow(majority_set)
  sample_majority_ind <- sample(1:n_majority, size_each_part, replace = FALSE)
  sample_majority <- majority_set[sample_majority_ind, ]
    
  minority_set <- subset(dense_matrix, (vital_status == 'Alive'))
  n_minority <- nrow(minority_set)
  rep_times <- size_each_part%/%nrow(minority_set)
  oversampled_minority_set <- minority_set
  if (rep_times > 1)
  {
   for (i in 1:(rep_times - 1))
   {
    oversampled_minority_set <- rbind(oversampled_minority_set, minority_set)
   }
  }
  rem_sample_id <- sample(1:n_minority, size_each_part%%nrow(minority_set), replace = FALSE)
  rem_sample <- minority_set[rem_sample_id, ]
  oversampled_minority_set <- rbind(oversampled_minority_set, rem_sample)

  bal_dense_matrix <- rbind(sample_majority, oversampled_minority_set)
  print(table(bal_dense_matrix$vital_status))
  return(bal_dense_matrix)
}


classify <- function()
{
  file_path <- "/Users/blahiri/healthcare/data/tcga/Raw_Data"
  dense_matrix <- read.csv(paste(file_path, "/", "clinical_all_combined_gbm.csv", sep = ""))

  #Filter out rows that create incomplete data
  dense_matrix <- subset(dense_matrix, (karnofsky_performance_score != "[Not Available]")) 
  dense_matrix <- subset(dense_matrix, (person_neoplasm_cancer_status != "[Not Available]")) 

  dense_matrix <- subset(dense_matrix, (person_neoplasm_cancer_status != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (ethnicity != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (race != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (history_of_neoadjuvant_treatment != "[Not Available]"))
  
  dense_matrix$vital_status <- apply(dense_matrix, 1, function(row)process_vital_status(row["vital_status"])) 
  dense_matrix <- dense_matrix[,!(names(dense_matrix) %in% c("bcr_patient_barcode"))]

  demog_vars <- c("age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "race")
  case_history_vars <- c("histological_type", "history_of_neoadjuvant_treatment", "initial_pathologic_diagnosis_method", "karnofsky_performance_score", 
                         "person_neoplasm_cancer_status", "prior_glioma")
  drug_vars <- colnames(dense_matrix)[12:31]
  radiation_vars <- colnames(dense_matrix)[32:37]

  for (drug_var in drug_vars)
  {
    dense_matrix[, drug_var] <- (dense_matrix[, drug_var] > 0)
  }
  for (radiation_var in radiation_vars)
  {
    dense_matrix[, radiation_var] <- (dense_matrix[, radiation_var] > 0)
  }

  factor_vars <- c(demog_vars, case_history_vars, drug_vars, radiation_vars, "vital_status")
  numeric_vars <- c("age_at_initial_pathologic_diagnosis", "karnofsky_performance_score")
  factor_vars <- factor_vars[!factor_vars %in% numeric_vars]

  for (column in factor_vars)
  {
    dense_matrix[, column] <- factor(dense_matrix[, column], levels = unique(dense_matrix[, column]))
  }
  for (column in numeric_vars)
  {
    dense_matrix[, column] <- as.numeric(dense_matrix[, column])
  }
  library(e1071)
  set.seed(1)
  x <- dense_matrix[,!(names(dense_matrix) %in% c("vital_status"))]
  y <- dense_matrix[, "vital_status"]
  train = sample(1:nrow(x), 0.5*nrow(x))

  test = (-train)
  df.train <- create_bs_by_over_and_undersampling(dense_matrix[train, ])
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))
  str_formula <- "vital_status ~ "
   for (column in colnames(dense_matrix))
   {
       if (column != 'vital_status')
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   
   tune.out = tune.rpart(as.formula(str_formula), data = df.train, minsplit = c(5, 10, 15), maxdepth = c(1, 3, 5, 7))
   bestmod <- tune.out$best.model
   
   ypred <- predict(bestmod, newdata = dense_matrix[test, ], type = "class")
   print(table(dense_matrix[test, "vital_status"], ypred, dnn = list('actual', 'predicted')))
   #Error in Dead class = 0.3793103, error in Alive class = 0.3235294.
   #tune.out$best.model$variable.importance
   tune.out
}
