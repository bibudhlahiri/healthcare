library(data.table)

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

construct_bn <- function()
{
  library(bnlearn)
  file_path <- "/Users/blahiri/healthcare/data/tcga/Raw_Data"
  dense_matrix <- read.csv(paste(foldername, "/", "clinical_all_combined_gbm.csv", sep = ""))
  dense_matrix <- dense_matrix[, c("age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "histological_type", "history_of_neoadjuvant_treatment", 
                                   "initial_pathologic_diagnosis_method", "karnofsky_performance_score", "person_neoplasm_cancer_status", "prior_glioma", 
                                   "race", "vital_status", "days_to_drug_therapy_end", "days_to_drug_therapy_start", "drug_name", "regimen_indication.x",
                                   "therapy_type", "anatomic_treatment_site", "days_to_radiation_therapy_end", "days_to_radiation_therapy_start",
                                   "radiation_type")]  #1812 rows
  dense_matrix <- subset(dense_matrix, (karnofsky_performance_score != "[Not Available]")) #1535 rows
  dense_matrix <- subset(dense_matrix, (person_neoplasm_cancer_status != "[Not Available]")) #1479 rows
  dense_matrix <- subset(dense_matrix, (!(days_to_drug_therapy_start %in% c("[Not Available]", "[Completed]")) & 
                                          !(days_to_drug_therapy_end %in% c("[Not Available]", "[Completed]")))) #1229 rows
  dense_matrix$drug_duration <- as.numeric(dense_matrix$days_to_drug_therapy_end) - as.numeric(dense_matrix$days_to_drug_therapy_start)
  dense_matrix <- dense_matrix[,!(names(dense_matrix) %in% c("days_to_drug_therapy_start", "days_to_drug_therapy_end"))]

  dense_matrix <- subset(dense_matrix, (anatomic_treatment_site != "[Not Available]")) #1222
  dense_matrix <- subset(dense_matrix, (!(days_to_radiation_therapy_start %in% c("[Not Available]", "[Completed]")) & 
                                          !(days_to_radiation_therapy_end %in% c("[Not Available]", "[Completed]")))) #1211
  dense_matrix$rad_duration <- as.numeric(dense_matrix$days_to_radiation_therapy_end) - as.numeric(dense_matrix$days_to_radiation_therapy_start)
  dense_matrix <- dense_matrix[,!(names(dense_matrix) %in% c("days_to_radiation_therapy_start", "days_to_radiation_therapy_start"))]
  dense_matrix <- subset(dense_matrix, (radiation_type != "[Not Available]")) #1211

  demog_vars <- c("age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "race")
  case_history_vars <- c("histological_type", "history_of_neoadjuvant_treatment", "initial_pathologic_diagnosis_method", "karnofsky_performance_score", 
                         "person_neoplasm_cancer_status", "prior_glioma")
  drug_vars <- c("drug_duration", "drug_name", "regimen_indication.x", "therapy_type", "anatomic_treatment_site")
  radiation_vars <- c("radiation_type", "rad_duration")
  factor_vars <- c(demog_vars, case_history_vars, drug_vars, radiation_vars)
  numeric_vars <- c("age_at_initial_pathologic_diagnosis", "karnofsky_performance_score", "drug_duration", "rad_duration")
  factor_vars <- factor_vars[!factor_vars %in% numeric_vars]
  for (column in factor_vars)
  {
    dense_matrix[, column] <- as.factor(dense_matrix[, column])
  }
  for (column in numeric_vars)
  {
    dense_matrix[, column] <- as.numeric(dense_matrix[, column])
  }

  blacklist <- expand.grid(demog_vars, demog_vars)

  df <- expand.grid(case_history_vars, case_history_vars)
  blacklist <- rbind(blacklist, df)

  df <- expand.grid(drug_vars, drug_vars)
  blacklist <- rbind(blacklist, df)

  df <- expand.grid(radiation_vars, radiation_vars)
  blacklist <- rbind(blacklist, df)

  colnames(blacklist) <- c("from", "to")

  res = hc(dense_matrix , blacklist = blacklist, optimized = FALSE)
  fitted = bn.fit(res, dense_matrix)
  res
}

