#Tree-augmented version of Bayes theorem where the Bayesian network is built as a tree, the procedures performed as the root.
chronic_conditions <- c("sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr", "sp_copd", "sp_depressn", 
                          "sp_diabetes", "sp_ischmcht", "sp_osteoprs", "sp_ra_oa", "sp_strketia")

library(RPostgreSQL)

compute_conditional_mutual_info <- function()
{
  

}

create_data <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- "select * from transformed_claim_diagnosis_codes tcdc where tcdc.clm_thru_year = '2008'"
  res <- dbSendQuery(con, statement)
  transformed_claim_diagnosis_codes <- fetch(res, n = -1)
  write.csv(transformed_claim_diagnosis_codes, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/transformed_claim_diagnosis_codes.csv")

  statement <- "select a.desynpuf_id, a.clm_id, a.clm_from_dt, a.clm_thru_dt, a.claim_type, a.prcdr_cd, a.clm_thru_year
                from  (select tcpc.*, (select count(*) from transformed_claim_diagnosis_codes tcdc where tcpc.prcdr_cd = tcdc.dgns_cd)
                       from transformed_claim_prcdr_codes tcpc 
                       where tcpc.clm_thru_year = '2008') a
                where a.count = 0"
  res <- dbSendQuery(con, statement)
  transformed_claim_prcdr_codes <- fetch(res, n = -1)
  write.csv(transformed_claim_prcdr_codes, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/transformed_claim_prcdr_codes.csv")

  statement <- "select * from beneficiary_summary_2008" 
  res <- dbSendQuery(con, statement)
  beneficiary_summary_2008 <- fetch(res, n = -1)
  write.csv(beneficiary_summary_2008, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/beneficiary_summary_2008.csv")

  dbDisconnect(con)
}


build_dense_matrix <- function()
{
  #file_path <- "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/"
  file_path <- "/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/"
  
  beneficiaries <- read.csv(paste(file_path, "beneficiary_summary_2008.csv", sep = ""))
  beneficiaries <- beneficiaries[, c("desynpuf_id", "sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr", "sp_copd", "sp_depressn", 
                                     "sp_diabetes", "sp_ischmcht", "sp_osteoprs", "sp_ra_oa", "sp_strketia")]
  chronic_conds <- paste("chron_", substr(chronic_conditions, 4, nchar(chronic_conditions)), sep = "")
  colnames(beneficiaries) <- c("desynpuf_id", chronic_conds)
  beneficiaries[, chronic_conds] <- as.numeric(beneficiaries[, chronic_conds] == '1')
  dense_matrix <- beneficiaries

  tcdc <- read.csv(paste(file_path, "transformed_claim_diagnosis_codes.csv", sep = ""))
  diagnosis_codes <- unique(tcdc$dgns_cd)  
  #diagnosis_codes <- diagnosis_codes[1:100]
  n_diagnosis_codes <- length(diagnosis_codes)

  loopc <- 0

  library(foreach)
  library(doMC)
  registerDoMC(8)

  cat(sprintf('Running with %d worker(s)\n', getDoParWorkers()))
  (name <- getDoParName())
  (ver <- getDoParVersion())
  if (getDoParRegistered())
   cat(sprintf('Currently using %s [%s]\n', name, ver))

  #for (diagnosis_code in diagnosis_codes)
  diag_conds_for_benefs <- foreach (i = 1:n_diagnosis_codes, .combine = cbind) %dopar%
  {
    diagnosis_code <- diagnosis_codes[i]
    tcdc_this_diag <- subset(tcdc, (dgns_cd == diagnosis_code))
    benefs_this_cond <- data.frame(patient_id = unique(tcdc_this_diag$desynpuf_id), temp = 1)

    column <- paste("diag_", diagnosis_code, sep = "")
    colnames(benefs_this_cond) <- c("patient_id", column)

    benef_tcdc <- merge(x = beneficiaries, y = benefs_this_cond, by.x = "desynpuf_id", by.y = "patient_id",  all.x = TRUE)
    benef_tcdc[, column] <- as.numeric(!is.na(benef_tcdc[, column]))

    #columns <- colnames(dense_matrix)
    #dense_matrix <- cbind(dense_matrix, benef_tcdc[, column])
    #colnames(dense_matrix) <- c(columns, column)

    loopc <- loopc + 1
    if (loopc %% 20 == 0)
    {
      cat(paste("for diagnosis_code, loopc = ", loopc, ", time = ", Sys.time(), "\n", sep = ""))
    }
    benef_tcdc[, column]
  }
  columns <- colnames(dense_matrix)
  dense_matrix <- cbind(dense_matrix, diag_conds_for_benefs)
  colnames(dense_matrix) <- c(columns, paste("diag_", diagnosis_codes, sep = "")) 

  #Do the same for procedures. Take procedure codes from file because they do not overlap with diagnosis codes.
  tcpc <- read.csv(paste(file_path, "transformed_claim_prcdr_codes.csv", sep = ""))
  procedure_codes <- sort(unique(tcpc$prcdr_cd))  #594 unique procedure codes which do not overlap with diagnosis codes
  #procedure_codes <- procedure_codes[1:100]
  n_procedure_codes <- length(procedure_codes)
  loopc <- 0

  #for (procedure_code in procedure_codes)
  procs_for_benefs <- foreach (i = 1:n_procedure_codes, .combine = cbind) %dopar%
  {
    procedure_code <- procedure_codes[i]
    tcpc_this_proc <- subset(tcpc, (prcdr_cd == procedure_code))
    benefs_this_proc <- data.frame(patient_id = unique(tcpc_this_proc$desynpuf_id), temp = 1)

    column <- paste("proc_", procedure_code, sep = "")
    colnames(benefs_this_proc) <- c("patient_id", column)

    benef_tcpc <- merge(x = beneficiaries, y = benefs_this_proc, by.x = "desynpuf_id", by.y = "patient_id",  all.x = TRUE)
    benef_tcpc[, column] <- as.numeric(!is.na(benef_tcpc[, column]))

    #columns <- colnames(dense_matrix)
    #dense_matrix <- cbind(dense_matrix, benef_tcpc[, column])
    #colnames(dense_matrix) <- c(columns, column)

    loopc <- loopc + 1
    if (loopc %% 20 == 0)
    {
      cat(paste("for procedure_code, loopc = ", loopc, ", time = ", Sys.time(), "\n", sep = ""))
    }
    benef_tcpc[, column]
  }
  columns <- colnames(dense_matrix)
  dense_matrix <- cbind(dense_matrix, procs_for_benefs)
  colnames(dense_matrix) <- c(columns, paste("proc_", procedure_codes, sep = ""))
  write.csv(dense_matrix, paste(file_path, "dense_matrix.csv", sep = ""))
  dense_matrix
}  
