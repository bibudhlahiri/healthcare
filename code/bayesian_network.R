#Tree-augmented version of Bayes theorem where the Bayesian network is built as a tree, the procedures performed as the root.
chronic_conditions <- c("sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr", "sp_copd", "sp_depressn", 
                          "sp_diabetes", "sp_ischmcht", "sp_osteoprs", "sp_ra_oa", "sp_strketia")

library(RPostgreSQL)

#Temporarily taking top 5 only from diagnosed codes, procedure codes and prescribed drugs
create_data <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  #Taking the top 2000 diagnoses codes only, they account for 87.66% of all diagnoses
  statement <- "select * from transformed_claim_diagnosis_codes tcdc1 where tcdc1.clm_thru_year = '2008'
                and tcdc1.dgns_cd in (select tcdc.dgns_cd
                                      from transformed_claim_diagnosis_codes tcdc
                                      where tcdc.clm_thru_year = '2008'
                                      and exists (select 1 from beneficiary_summary_2008 b where b.desynpuf_id = tcdc.desynpuf_id)
                                      group by tcdc.dgns_cd
                                      order by count(*) desc
                                      limit 2000)
                and tcdc1.dgns_cd in ('4019', '25000', '2724', '4011', 'V5869')"
  res <- dbSendQuery(con, statement)
  transformed_claim_diagnosis_codes <- fetch(res, n = -1)
  write.csv(transformed_claim_diagnosis_codes, "/Users/blahiri/healthcare/documents/recommendation_system/transformed_claim_diagnosis_codes.csv")

  #Take only the procedures whose codes do not match with any diagnoses codes
  statement <- "select a.desynpuf_id, a.clm_id, a.clm_from_dt, a.clm_thru_dt, a.claim_type, a.prcdr_cd, a.clm_thru_year
                from  (select tcpc.*, (select count(*) from transformed_claim_diagnosis_codes tcdc where tcpc.prcdr_cd = tcdc.dgns_cd)
                       from transformed_claim_prcdr_codes tcpc 
                       where tcpc.clm_thru_year = '2008') a
                where a.count = 0
                and a.prcdr_cd in ('9904', '8154', '3893', '3995', '4516')"
  res <- dbSendQuery(con, statement)
  transformed_claim_prcdr_codes <- fetch(res, n = -1)
  write.csv(transformed_claim_prcdr_codes, "/Users/blahiri/healthcare/documents/recommendation_system/transformed_claim_prcdr_codes.csv")

  statement <- "select b1.desynpuf_id, b1.sp_alzhdmta as chron_alzhdmta_2008, b1.sp_chf as chron_chf_2008, 
                b1.sp_chrnkidn as chron_chrnkidn_2008, b1.sp_cncr as chron_cncr_2008, b1.sp_copd as chron_copd_2008, b1.sp_depressn as chron_depressn_2008, 
                b1.sp_diabetes as chron_diabetes_2008, b1.sp_ischmcht as chron_ischmcht_2008, b1.sp_osteoprs as chron_osteoprs_2008, b1.sp_ra_oa as chron_ra_oa_2008, 
                b1.sp_strketia as chron_strketia_2008, b2.sp_alzhdmta as chron_alzhdmta_2009, b2.sp_chf as chron_chf_2009, 
                b2.sp_chrnkidn as chron_chrnkidn_2009, b2.sp_cncr as chron_cncr_2009, b2.sp_copd as chron_copd_2009, b2.sp_depressn as chron_depressn_2009, 
                b2.sp_diabetes as chron_diabetes_2009, b2.sp_ischmcht as chron_ischmcht_2009, b2.sp_osteoprs as chron_osteoprs_2009, b2.sp_ra_oa as chron_ra_oa_2009, 
                b2.sp_strketia as chron_strketia_2009
                from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                where b1.desynpuf_id = b2.desynpuf_id" 
  res <- dbSendQuery(con, statement)
  beneficiary_summary_2008 <- fetch(res, n = -1)
  write.csv(beneficiary_summary_2008, "/Users/blahiri/healthcare/documents/recommendation_system/beneficiary_summary_2008_2009.csv")

  statement <- "select distinct b1.desynpuf_id, nc.substancename
                from beneficiary_summary_2008 b1, prescription_drug_events pde1, ndc_codes nc
                where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
                and nc.substancename is not null
                and pde1.desynpuf_id = b1.desynpuf_id
                and to_char(pde1.srvc_dt, 'YYYY') = '2008'
                and nc.substancename in ('LOVASTATIN', 'GEMFIBROZIL', 'SULFASALAZINE', 'LOSARTAN POTASSIUM', 'VALSARTAN')
                order by b1.desynpuf_id"
  res <- dbSendQuery(con, statement)
  prescribed_drugs <- fetch(res, n = -1)
  write.csv(prescribed_drugs, "/Users/blahiri/healthcare/documents/recommendation_system/prescribed_drugs.csv")

  dbDisconnect(con)
}


build_dense_matrix_sequentially <- function()
{
  file_path <- "/Users/blahiri/healthcare/documents/recommendation_system/"
  #file_path <- "/home/impadmin/bibudh/healthcare/documents/recommendation_system/"
  
  beneficiaries <- read.csv(paste(file_path, "beneficiary_summary_2008_2009.csv", sep = ""))
  columns <- colnames(beneficiaries)
  chronic_conds <- columns[columns != 'desynpuf_id']
  beneficiaries[, chronic_conds] <- as.numeric(beneficiaries[, chronic_conds] == '1')
  dense_matrix <- beneficiaries

  tcdc <- read.csv(paste(file_path, "transformed_claim_diagnosis_codes.csv", sep = ""))
  diagnosis_codes <- unique(tcdc$dgns_cd)  
  #diagnosis_codes <- diagnosis_codes[1:5]
  n_diagnosis_codes <- length(diagnosis_codes)

  loopc <- 0

  for (diagnosis_code in diagnosis_codes)
  {
    #This is the part that takes most of the time 
    loopc <- loopc + 1

    tcdc_this_diag <- subset(tcdc, (dgns_cd == diagnosis_code))
    benefs_this_cond <- data.frame(patient_id = unique(tcdc_this_diag$desynpuf_id), temp = 1)

    column <- paste("diag_", diagnosis_code, sep = "")
    colnames(benefs_this_cond) <- c("patient_id", column)

    benef_tcdc <- merge(x = beneficiaries, y = benefs_this_cond, by.x = "desynpuf_id", by.y = "patient_id",  all.x = TRUE)
    benef_tcdc[, column] <- as.numeric(!is.na(benef_tcdc[, column]))

    if (loopc == 1)
    {
      diag_conds_for_benefs <- benef_tcdc[, column]
    }
    else
    {
      diag_conds_for_benefs <- cbind(diag_conds_for_benefs, benef_tcdc[, column])
    }
    if (loopc %% 20 == 0)
    {
      cat(paste("for diagnosis_code, loopc = ", loopc, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  columns <- colnames(dense_matrix)
  dense_matrix <- cbind(dense_matrix, diag_conds_for_benefs)
  colnames(dense_matrix) <- c(columns, paste("diag_", diagnosis_codes, sep = "")) 

  #Do the same for procedures. Take procedure codes from file because they do not overlap with diagnosis codes.
  tcpc <- read.csv(paste(file_path, "transformed_claim_prcdr_codes.csv", sep = ""))
  procedure_codes <- sort(unique(tcpc$prcdr_cd))  #594 unique procedure codes which do not overlap with diagnosis codes
  #procedure_codes <- procedure_codes[1:5]
  n_procedure_codes <- length(procedure_codes)
  loopc <- 0

  for (procedure_code in procedure_codes)
  {
    loopc <- loopc + 1

    tcpc_this_proc <- subset(tcpc, (prcdr_cd == procedure_code))
    benefs_this_proc <- data.frame(patient_id = unique(tcpc_this_proc$desynpuf_id), temp = 1)

    column <- paste("proc_", procedure_code, sep = "")
    colnames(benefs_this_proc) <- c("patient_id", column)

    benef_tcpc <- merge(x = beneficiaries, y = benefs_this_proc, by.x = "desynpuf_id", by.y = "patient_id",  all.x = TRUE)
    benef_tcpc[, column] <- as.numeric(!is.na(benef_tcpc[, column]))

    if (loopc == 1)
    {
      procs_for_benefs <- benef_tcpc[, column]
    }
    else
    {
      procs_for_benefs <- cbind(procs_for_benefs, benef_tcpc[, column])
    }
    if (loopc %% 20 == 0)
    {
      cat(paste("for procedure_code, loopc = ", loopc, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  columns <- colnames(dense_matrix)
  dense_matrix <- cbind(dense_matrix, procs_for_benefs)
  colnames(dense_matrix) <- c(columns, paste("proc_", procedure_codes, sep = ""))

  #Do the same for prescribed drugs. 
  pde <- read.csv(paste(file_path, "prescribed_drugs.csv", sep = ""))
  prescribed_drugs <- sort(unique(pde$substancename))  
  #prescribed_drugs <- prescribed_drugs[1:5]
  n_prescribed_drugs <- length(prescribed_drugs)
  loopc <- 0

  for (prescribed_drug in prescribed_drugs)
  {
    loopc <- loopc + 1

    pde_this_drug <- subset(pde, (substancename == prescribed_drug))
    benefs_this_drug <- data.frame(patient_id = unique(pde_this_drug$desynpuf_id), temp = 1)

    column <- paste("drug_", prescribed_drug, sep = "")
    colnames(benefs_this_drug) <- c("patient_id", column)

    benef_pde <- merge(x = beneficiaries, y = benefs_this_drug, by.x = "desynpuf_id", by.y = "patient_id",  all.x = TRUE)
    benef_pde[, column] <- as.numeric(!is.na(benef_pde[, column]))

    if (loopc == 1)
    {
      drugs_for_benefs <- benef_pde[, column]
    }
    else
    {
      drugs_for_benefs <- cbind(drugs_for_benefs, benef_pde[, column])
    }
    if (loopc %% 20 == 0)
    {
      cat(paste("for prescribed_drug, loopc = ", loopc, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  columns <- colnames(dense_matrix)
  dense_matrix <- cbind(dense_matrix, drugs_for_benefs)
  colnames(dense_matrix) <- c(columns, paste("drug_", prescribed_drugs, sep = ""))
 
  
  #require(MASS) 
  #library(bigmemory)
  #dense_matrix <- as.big.matrix(dense_matrix)
  #library(ff)
  #dense_matrix <- as.ffdf(dense_matrix)
  #cat(paste("About to write matrix, time = ", Sys.time(), "\n", sep = ""))
  #write.table(dense_matrix, paste(file_path, "dense_matrix.csv", sep = ""))
  #write.matrix(dense_matrix, paste(file_path, "dense_matrix.csv", sep = ""))
  #save(dense_matrix, file = paste(file_path, "dense_matrix.RData", sep = "")) 
  #write.big.matrix(dense_matrix, paste(file_path, "dense_matrix.csv", sep = ""))
  write.csv(dense_matrix, paste(file_path, "dense_matrix.csv", sep = ""))
  #write.csv.ffdf(dense_matrix, file = paste(file_path, "dense_matrix.csv", sep = ""))
  #cat(paste("Wrote matrix, time = ", Sys.time(), "\n", sep = ""))
  
  #Loading a list creates an object with the same name as the file, but the value is the name of the object.
  #load(file = paste(file_path, "dense_matrix.RData", sep = ""), envir = .GlobalEnv)
  #loaded_dense_matrix <- dense_matrix
  dense_matrix
}  


build_dense_matrix_in_parallel <- function()
{
  #file_path <- "/Users/blahiri/healthcare/documents/recommendation_system/"
  file_path <- "/home/impadmin/bibudh/healthcare/documents/recommendation_system/"
  
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

construct_bn <- function()
{
  library(bnlearn)
  file_path <- "/Users/blahiri/healthcare/documents/recommendation_system/"
  #file_path <- "/home/impadmin/bibudh/healthcare/documents/recommendation_system/"
  dense_matrix <- read.csv(paste(file_path, "dense_matrix.csv", sep = ""))
  dense_matrix <- dense_matrix[,!(names(dense_matrix) %in% c("X.1", "X", "desynpuf_id"))]
  columns <- colnames(dense_matrix)
  print(columns)

  chronic_conditions <- columns[substr(columns, 1, 6) == 'chron_']
  blacklist <- expand.grid(chronic_conditions, chronic_conditions)
  diagnosed_conditions <- columns[substr(columns, 1, 5) == 'diag_']
  blacklist <- rbind(blacklist, expand.grid(diagnosed_conditions, diagnosed_conditions))
  procedures <- columns[substr(columns, 1, 5) == 'proc_']
  blacklist <- rbind(blacklist, expand.grid(procedures, procedures))
  drugs <- columns[substr(columns, 1, 5) == 'drug_']
  blacklist <- rbind(blacklist, expand.grid(drugs, drugs))
  colnames(blacklist) <- c("from", "to")
  print(blacklist)

  for (column in columns)
  {
    dense_matrix[, column] <- as.factor(dense_matrix[, column])
  }
  res = hc(dense_matrix, blacklist = blacklist)
  plot(res)
  res
}

#plot(res, ylim = c(0,800), xlim = ylim, radius = 125)
cond_prob_tables <- function()
{
  #First, find the CPTs between the chronic conditions and the diagnosed conditions
  file_path <- "/Users/blahiri/healthcare/documents/recommendation_system/"
  #file_path <- "/home/impadmin/bibudh/healthcare/documents/recommendation_system/"
  dense_matrix <- read.csv(paste(file_path, "dense_matrix.csv", sep = ""))
  dense_matrix <- dense_matrix[,!(names(dense_matrix) %in% c("X.1", "X", "desynpuf_id"))]
  columns <- colnames(dense_matrix)

  chronic_conditions <- columns[substr(columns, 1, 6) == 'chron_' & substr(columns, nchar(columns)-4, nchar(columns)) == '_2008']
  diagnosed_conditions <- columns[substr(columns, 1, 5) == 'diag_']
  for (chronic_condition in chronic_conditions)
  {
    for (diagnosed_condition in diagnosed_conditions)
    {
      M <- table(dense_matrix[, chronic_condition], dense_matrix[, diagnosed_condition])
      #Xsq <- chisq.test(M)
      #if (Xsq$p.value < 0.05)
      #{
        cat(paste("chronic_condition = ", chronic_condition, ", diagnosed_condition = ", diagnosed_condition, "\n", sep = ""))
        #print(M) 
        #print(Xsq)
        t <- M/rowSums(M)
        print(t)
      #}
    }
  }

  procedures <- columns[substr(columns, 1, 5) == 'proc_']
  for (diagnosed_condition in diagnosed_conditions)
  {
    for (procedure in procedures)
    {
      M <- table(dense_matrix[, diagnosed_condition], dense_matrix[, procedure])
      cat(paste("diagnosed_condition = ", diagnosed_condition, ", procedure = ", procedure, "\n", sep = ""))
      t <- M/rowSums(M)
      print(t)
    }
  }

  drugs <- columns[substr(columns, 1, 5) == 'drug_']
  for (diagnosed_condition in diagnosed_conditions)
  {
    for (drug in drugs)
    {
      M <- table(dense_matrix[, diagnosed_condition], dense_matrix[, drug])
      cat(paste("diagnosed_condition = ", diagnosed_condition, ", drug = ", drug, "\n", sep = ""))
      t <- M/rowSums(M)
      print(t)
    }
  }

  chronic_conditions <- columns[substr(columns, 1, 6) == 'chron_' & substr(columns, nchar(columns)-4, nchar(columns)) == '_2009']
  for (drug in drugs) 
  {
    for (chronic_condition in chronic_conditions)
    {
      M <- table(dense_matrix[, drug], dense_matrix[, chronic_condition])
      cat(paste("drug = ", drug, ", chronic_condition = ", chronic_condition, "\n", sep = ""))
      t <- M/rowSums(M)
      print(t)
    }
  }

  for (procedure in procedures) 
  {
    for (chronic_condition in chronic_conditions)
    {
      M <- table(dense_matrix[, procedure], dense_matrix[, chronic_condition])
      cat(paste("procedure = ", procedure, ", chronic_condition = ", chronic_condition, "\n", sep = ""))
      t <- M/rowSums(M)
      print(t)
    }
  }

}
