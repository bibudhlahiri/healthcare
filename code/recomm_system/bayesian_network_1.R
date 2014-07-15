

compute_conditional_mutual_info <- function()
{
  file_path <- "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/"
  #file_path <- "/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/"

  load(file = paste(file_path, "cpt_conditions_proc_one.RData", sep = ""), envir = .GlobalEnv)
  loaded_cpt_conditions_proc_one <- cpt_conditions_proc_one

  load(file = paste(file_path, "cpt_conditions_proc_zero.RData", sep = ""), envir = .GlobalEnv)
  loaded_cpt_conditions_proc_zero <- cpt_conditions_proc_zero
 
   #Get all procedure codes.
  tcpc <- read.csv(paste(file_path, "transformed_claim_prcdr_codes.csv", sep = ""))
  procedure_codes <- sort(unique(tcpc$prcdr_cd))
  procedure_codes <- paste("proc_", procedure_codes, sep = "")

  n_procedure_codes <- length(procedure_codes)
  dense_matrix <- read.csv(paste(file_path, "dense_matrix.csv", sep = ""))
  n_benefs <- nrow(dense_matrix)
  cmi_values <- list()
  

  #Create a list of named vectors where each entry in the list is for a procedure, and the names of the named vectors are sorted pairs of 
  #conditions, e.g., chron_diabetes and diag_V5841 will form a name chron_diabetes_diag_V5841 where the conditional mutual information between these
  #two conditions will be stored for a specific procedure. 

  chronic_conds <- paste("chron_", substr(chronic_conditions, 4, nchar(chronic_conditions)), sep = "")
  chronic_conds <- sort(chronic_conds)
  tcdc <- read.csv(paste(file_path, "transformed_claim_diagnosis_codes.csv", sep = ""))
  diagnosis_codes <- sort(unique(tcdc$dgns_cd))
  diagnosis_codes <- paste("diag_", diagnosis_codes, sep = "")
  all_conditions <- append(chronic_conds, diagnosis_codes)
  n_all_conditions <- length(all_conditions)

  for (procedure_code in procedure_codes)
  {
   for (i in 1:n_all_conditions)
   {
    for (j in (i+1):n_all_conditions)
    {
      cmi <- 0
      #Sum over all the 8 possible combinations of (Ai, Aj and C). P(Ai/C) and P(Aj/C) are taken from pre-computed lists to avoid repeated computations.
      #Ai = 0, Aj = 0, C = 0
      p_ai_0_aj_0_c_0 <- nrow(subset(dense_matrix, (all_conditions[i] == 0 & all_conditions[j] == 0 & procedure_code == 0)))/n_benefs)
      p_c_0 <- nrow(subset(dense_matrix, (procedure_code == 0)))/n_benefs)
      p_ai_0_aj_0_given_c_0 <- p_ai_0_aj_0_c_0/p_c_0
      p_ai_0_given_c_0 <- 
      
      cpt_conditions <- loaded_cpt_conditions_proc_zero[[procedure_code]]
      cond_prob <- 1 - as.numeric(cpt_conditions[as.character(all_conditions[i])])
      if (is.na(cond_prob))
      {
        cond_prob <- 1
      }


    }
   }
  }

}

