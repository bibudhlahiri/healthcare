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
