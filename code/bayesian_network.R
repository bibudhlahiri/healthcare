#Tree-augmented version of Bayes theorem where the Bayesian network is built as a tree, the procedures performed as the root.
chronic_conditions <- c("sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr", "sp_copd", "sp_depressn", 
                          "sp_diabetes", "sp_ischmcht", "sp_osteoprs", "sp_ra_oa", "sp_strketia")

library(RPostgreSQL)

compute_conditional_mutual_info <- function()
{
  

}

build_dense_matrix <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF") 

  statement <- "select desynpuf_id, sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr, sp_copd, sp_depressn, 
                sp_diabetes, sp_ischmcht, sp_osteoprs, sp_ra_oa, sp_strketia
                from beneficiary_summary_2008
                order by desynpuf_id"
  res <- dbSendQuery(con, statement)
  beneficiaries <- fetch(res, n = -1)
  chronic_conds <- paste("chron_", substr(chronic_conditions, 4, nchar(chronic_conditions)), sep = "")
  colnames(beneficiaries) <- c("desynpuf_id", chronic_conds)
  beneficiaries[, chronic_conds] <- as.numeric(beneficiaries[, chronic_conds] == '1')
  dense_matrix <- beneficiaries

  statement <- "select distinct tcdc.dgns_cd
                from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
                where b.desynpuf_id = tcdc.desynpuf_id
                and tcdc.clm_thru_year = '2008'
                order by tcdc.dgns_cd"
  res <- dbSendQuery(con, statement)
  diagnosis_codes <- (fetch(res, n = -1))$dgns_cd
  diagnosis_codes <- diagnosis_codes[1:5]
  print(diagnosis_codes)

  for (diagnosis_code in diagnosis_codes)
  {
    statement <- paste("select a.desynpuf_id, case when a.count_", diagnosis_code, " > 0 then 1 else 0 end as diag_", diagnosis_code, 
                       " from (select b.desynpuf_id, (select count(*) from transformed_claim_diagnosis_codes tcdc
                              where b.desynpuf_id = tcdc.desynpuf_id and tcdc.clm_thru_year = '2008'
                              and tcdc.dgns_cd = '", diagnosis_code, "') count_", diagnosis_code, 
                        " from beneficiary_summary_2008 b
                        order by b.desynpuf_id) a", sep = "")
    res <- dbSendQuery(con, statement)
    data_this_diag <- fetch(res, n = -1)
    column <- paste("diag_", diagnosis_code, sep = "")
    columns <- colnames(dense_matrix)
    dense_matrix <- cbind(dense_matrix, data_this_diag[, c(column)])
    colnames(dense_matrix) <- c(columns, column) 
  }

  #Do the same for procedures
  statement <- "select distinct tcpc.prcdr_cd
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                where tcpc.desynpuf_id = b.desynpuf_id
                and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                order by tcpc.prcdr_cd"
  res <- dbSendQuery(con, statement)
  procedure_codes <- (fetch(res, n = -1))$prcdr_cd
  procedure_codes <- procedure_codes[1:5]
  print(procedure_codes)

  for (procedure_code in procedure_codes)
  {
    statement <- paste("select a.desynpuf_id, case when a.count_", procedure_code, " > 0 then 1 else 0 end as proc_", procedure_code, 
                       " from (select b.desynpuf_id, (select count(*) from transformed_claim_prcdr_codes tcpc
                              where b.desynpuf_id = tcpc.desynpuf_id and tcpc.clm_thru_year = '2008'
                              and tcpc.prcdr_cd = '", procedure_code, "') count_", procedure_code, 
                        " from beneficiary_summary_2008 b
                        order by b.desynpuf_id) a", sep = "")
    res <- dbSendQuery(con, statement)
    data_this_proc <- fetch(res, n = -1)
    column <- paste("proc_", procedure_code, sep = "")
    columns <- colnames(dense_matrix)
    dense_matrix <- cbind(dense_matrix, data_this_proc[, c(column)])
    colnames(dense_matrix) <- c(columns, column) 
  }

  
  dbDisconnect(con)
  dense_matrix
}  
