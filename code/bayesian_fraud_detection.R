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
  #chronic_conditions <- c("sp_alzhdmta")
  procedure_priors <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  procedures <- procedure_priors$prcdr_cd
  loopc <- 1

  for (chronic_condition in chronic_conditions)
  {
    cat(paste("chronic_condition = ", chronic_condition, "\n", sep = ""))
    dummy <- expand.grid(procedures, c(1, 0))
    cat(paste("length(procedures) = ", length(procedures), ", nrow(dummy) = ", nrow(dummy), "\n", sep = ""))
    colnames(dummy) <- c("prcdr_cd", chronic_condition)
    
    statement <- paste("select tcpc.prcdr_cd, case when ", chronic_condition, " = '2' then 0 else 1 end as ", chronic_condition,
                        ", count(distinct b.desynpuf_id)
                        from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                        where tcpc.desynpuf_id = b.desynpuf_id
                        and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                        group by tcpc.prcdr_cd, ", chronic_condition, sep = "")
    res <- dbSendQuery(con, statement)
    cpt_this_cc <- fetch(res, n = -1)
    n_cpt_this_cc <- nrow(cpt_this_cc)
    cpt_cc <- merge(x = dummy, y = cpt_this_cc, by.x = c(chronic_condition, "prcdr_cd"), by.y = c(chronic_condition, "prcdr_cd"), all.x =  TRUE)
    cpt_cc <- cpt_cc[order(cpt_cc[, "prcdr_cd"]),]
    cat(paste("nrow(cpt_cc) = ", nrow(cpt_cc), "\n", sep = ""))
    if (loopc == 1)
    {
      grand_cpt_cc <- cpt_cc
    }
    else
    {
      grand_cpt_cc <- cbind(grand_cpt_cc, cpt_cc)
    }
    loopc <- loopc + 1
  }
  dbDisconnect(con)
  grand_cpt_cc[is.na(grand_cpt_cc)] <- 0

  columns <- colnames(grand_cpt_cc)
  loopc <- 1
  for (column in columns)
  {
    if (column == 'count')
    {
      chronic_condition <- substr(columns[loopc - 2], 4, nchar(columns[loopc - 2]))
      newcol <- paste("count_", chronic_condition, sep = "")
      grand_cpt_cc[, newcol] <- grand_cpt_cc[, loopc]
      #grand_cpt_cc <- grand_cpt_cc[,-c(loopc)]  
    }
    loopc <- loopc + 1
  }
  columns_to_retain <- c(chronic_conditions, "prcdr_cd", paste("count_", substr(chronic_conditions, 4, nchar(chronic_conditions)), sep = ""))
  print(columns_to_retain)
  grand_cpt_cc <- grand_cpt_cc[, columns_to_retain]
  #n_procs <- length(procedures)
  #for (i in 1:n_procs)
  #{
  #  this_prcdr_cd <- grand_cpt_cc[2*i-1, "prcdr_cd"]
  #  benefs_done_this_proc <- subset(procedure_priors, (prcdr_cd == this_prcdr_cd))
    
  #}
  grand_cpt_cc <- merge(grand_cpt_cc, procedure_priors[, c("prcdr_cd", "count", "prior_prob")], by = "prcdr_cd", all.x = TRUE)
  for (chronic_condition in chronic_conditions)
  {
    cpcol <- paste("cond_prob_", substr(chronic_condition, 4, nchar(chronic_condition)), sep = "")
    countcol <- paste("count_", substr(chronic_condition, 4, nchar(chronic_condition)), sep = "")
    grand_cpt_cc[, cpcol] <- grand_cpt_cc[, countcol]/grand_cpt_cc$count
  }
  write.csv(grand_cpt_cc, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/grand_cpt_cc.csv")
  grand_cpt_cc
} 

