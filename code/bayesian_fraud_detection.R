library(RPostgreSQL)

prepare_data_all_together <- function()
{
 con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- "select b.desynpuf_id, sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr, sp_copd, sp_depressn, 
                sp_diabetes, sp_ischmcht, sp_osteoprs, sp_ra_oa, sp_strketia, tcpc.prcdr_cd
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                where tcpc.desynpuf_id = b.desynpuf_id
                and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                order by tcpc.desynpuf_id"
  res <- dbSendQuery(con, statement)
  #chronic conditions and procedures
  ccp_data_raw <- fetch(res, n = -1)
  n_ccp_data_raw <- nrow(ccp_data_raw)
  #n_ccp_data_raw <- 200
  desynpuf_id <- '-1'
  ccp_data <- data.frame()
  j <- 1
  columns <- c("desynpuf_id", "sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr", "sp_copd", "sp_depressn", 
                      "sp_diabetes", "sp_ischmcht", "sp_osteoprs", "sp_ra_oa", "sp_strketia")
  for (i in 1:n_ccp_data_raw)
  {
    if (ccp_data_raw[i, "desynpuf_id"] != desynpuf_id)
    {
      desynpuf_id <- ccp_data_raw[i, "desynpuf_id"]
      for (column in columns)
      {
        ccp_data[j, column] <- ccp_data_raw[i, column]
      }
      colname <- paste("proc_", ccp_data_raw[i, "prcdr_cd"], sep = "") 
      ccp_data[j, colname] <- 1
      j <- j + 1
    }
    if (i %% 200 == 0)
    {
      cat(paste("i = ", i, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  dbDisconnect(con)
  ccp_data[is.na(ccp_data)] <- 0
  write.csv(ccp_data, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/ccp_data.csv") 
  ccp_data
}

prepare_prior_for_precodures <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  #Our interest population is the beneficiaries registered in 2008 who underwent some procedures in 2008
  statement <- "select count(distinct b.desynpuf_id)
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                where tcpc.desynpuf_id = b.desynpuf_id
                and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'"
  n_benefs <- as.numeric(dbGetQuery(con, statement))

  statement <- "select tcpc.prcdr_cd, count(distinct b.desynpuf_id)
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                where tcpc.desynpuf_id = b.desynpuf_id
                and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                group by tcpc.prcdr_cd
                order by count(distinct b.desynpuf_id) desc"
  res <- dbSendQuery(con, statement)
  procedure_priors <- fetch(res, n = -1)
  procedure_priors$prior_prob <- procedure_priors$count/n_benefs
  dbDisconnect(con)
  write.csv(procedure_priors, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  procedure_priors
}

prepare_conditionals_for_chronic_conditions <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  chronic_conditions <- c("sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr", "sp_copd", "sp_depressn", 
                          "sp_diabetes", "sp_ischmcht", "sp_osteoprs", "sp_ra_oa", "sp_strketia")
  procedure_priors <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  for (chronic_condition in chronic_conditions)
  {
    statement <- paste("select tcpc.prcdr_cd, case when ", chronic_condition, " = '2' then 0 else 1 end as ", chronic_condition,
                        ", count(distinct b.desynpuf_id)
                        from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                        where tcpc.desynpuf_id = b.desynpuf_id
                        and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                        group by tcpc.prcdr_cd, ", chronic_condition,
                        " order by count(distinct b.desynpuf_id) desc", sep = "")
    res <- dbSendQuery(con, statement)
    cpt_this_cc <- fetch(res, n = -1)
    n_cpt_this_cc <- nrow(cpt_this_cc)
  }
  dbDisconnect(con)
} 

