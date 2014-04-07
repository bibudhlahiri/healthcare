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


chronic_conditions <- c("sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr", "sp_copd", "sp_depressn", 
                          "sp_diabetes", "sp_ischmcht", "sp_osteoprs", "sp_ra_oa", "sp_strketia")

prepare_conditionals_for_chronic_conditions <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
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
    }
    loopc <- loopc + 1
  }
  columns_to_retain <- c(chronic_conditions, "prcdr_cd", paste("count_", substr(chronic_conditions, 4, nchar(chronic_conditions)), sep = ""))
  print(columns_to_retain)
  grand_cpt_cc <- grand_cpt_cc[, columns_to_retain]
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

#Of all the people who underwent a given procedure, how many were diagnosed with a given condition, how many were not?
#We will build count_ and cond_prob_ columns for the diagnoses codes, like we did for the chronic conditions
prepare_conditionals_for_diagnosed_conditions_1 <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  procedure_priors <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  procedure_priors <- procedure_priors[,!(names(procedure_priors) %in% c("X"))]

  procedure_priors <- procedure_priors[order(procedure_priors[, "prcdr_cd"]),]
  loopc <- 1

  statement <- "select distinct tcdc.dgns_cd
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b, transformed_claim_diagnosis_codes tcdc
                where tcpc.desynpuf_id = b.desynpuf_id
                and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                and b.desynpuf_id = tcdc.desynpuf_id
                and tcdc.clm_thru_year = to_char(tcpc.clm_thru_dt, 'YYYY')
                order by tcdc.dgns_cd"
  res <- dbSendQuery(con, statement)
  diag_conditions <- fetch(res, n = -1)
  diag_conditions <- diag_conditions$dgns_cd
  cat(paste("length(diag_conditions) = ", length(diag_conditions), "\n", sep = ""))

  #diag_conditions <- diag_conditions[1:3]
  n_diag_conditions <- length(diag_conditions)

  #grand_cpt_dc <- foreach (i=1:n_diag_conditions, .combine = cbind) %dopar% 
  for (this_diag_condition in diag_conditions)
  {
    statement <- paste("select tcpc.prcdr_cd, count(distinct tcdc.desynpuf_id)
                  from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b, transformed_claim_diagnosis_codes tcdc
                  where tcpc.desynpuf_id = b.desynpuf_id
                  and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                  and b.desynpuf_id = tcdc.desynpuf_id
                  and tcdc.clm_thru_year = to_char(tcpc.clm_thru_dt, 'YYYY')
                  and tcdc.dgns_cd = '", this_diag_condition, "' ", 
                  "group by tcpc.prcdr_cd
                  order by tcpc.prcdr_cd", sep = "")
    res <- dbSendQuery(con, statement)
    cpt_this_dc <- fetch(res, n = -1)
    cpt_dc <- merge(x = procedure_priors, y = cpt_this_dc, by.x = "prcdr_cd", by.y = "prcdr_cd", all.x =  TRUE)
    cpt_dc[is.na(cpt_dc)] <- 0
    one_col_count <- paste("count_", this_diag_condition, "_1", sep = "")
    zero_col_count <- paste("count_", this_diag_condition, "_0", sep = "")
    colnames(cpt_dc) <- c("prcdr_cd", "count", "prior_prob", one_col_count)
    cpt_dc[, zero_col_count] <- cpt_dc$count - cpt_dc[, one_col_count]

    one_col_cond_prob <- paste("cond_prob_", this_diag_condition, "_1", sep = "")
    zero_col_cond_prob <- paste("cond_prob_", this_diag_condition, "_0", sep = "")
    cpt_dc[, one_col_cond_prob] <- cpt_dc[, one_col_count]/cpt_dc$count
    cpt_dc[, zero_col_cond_prob] <- cpt_dc[, zero_col_count]/cpt_dc$count
    
    if (loopc == 1)
    {
      grand_cpt_dc <- cpt_dc
    }
    else
    {
      grand_cpt_dc <- cbind(grand_cpt_dc, cpt_dc)
    }
    loopc <- loopc + 1
    if (loopc %% 10 == 0)
    {
      cat(paste("loopc = ", loopc, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  grand_cpt_dc <- grand_cpt_dc[,!(names(grand_cpt_dc) %in% c("count", "prior_prob"))]
  
  grand_cpt_dc <- grand_cpt_dc[, c("prcdr_cd", paste("count_", diag_conditions, "_1", sep = ""),  
                                   paste("count_", diag_conditions, "_0", sep = ""), paste("cond_prob_", diag_conditions, "_1", sep = ""),
                                   paste("cond_prob_", diag_conditions, "_0", sep = ""))]
  dbDisconnect(con)
  write.csv(grand_cpt_dc, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/grand_cpt_dc.csv")
  grand_cpt_dc
}

#Faster version using lists
prepare_conditionals_for_diagnosed_conditions <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  procedure_priors <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  n_procs <- nrow(procedure_priors)
  cpt_dc <- list()
  #n_procs <- 10

  for (i in 1:n_procs)
  {
    this_proc <- procedure_priors[i, "prcdr_cd"]
    proc_count <- procedure_priors[i, "count"]
    statement <- paste("select tcdc.dgns_cd, count(distinct tcdc.desynpuf_id)
                  from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b, transformed_claim_diagnosis_codes tcdc
                  where tcpc.desynpuf_id = b.desynpuf_id
                  and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                  and b.desynpuf_id = tcdc.desynpuf_id
                  and tcdc.clm_thru_year = to_char(tcpc.clm_thru_dt, 'YYYY')
                  and tcpc.prcdr_cd = '", this_proc, "' ", 
                  "group by tcdc.dgns_cd 
                  order by count(distinct tcdc.desynpuf_id) desc", sep = "")
    res <- dbSendQuery(con, statement)
    cpt_this_dc <- fetch(res, n = -1)
    diag_cpts <- cpt_this_dc$count/proc_count
    names(diag_cpts) <- cpt_this_dc$dgns_cd
    this_proc <- paste("proc_", this_proc, sep = "")
    cpt_dc[[this_proc]] <- diag_cpts
    if (i %% 10 == 0)
    {
      cat(paste("i = ", i, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  dbDisconnect(con)
  cpt_dc
}

compute_posteriors <- function()
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
  ccp_data <- fetch(res, n = -1)
  n_ccp_data <- nrow(ccp_data)
  for (chronic_condition in chronic_conditions)
  {
    ccp_data[, chronic_condition] <- as.numeric(ccp_data[, chronic_condition] == '1') 
  }

  procedure_priors <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  grand_cpt_cc <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/grand_cpt_cc.csv")

  #n_ccp_data <- 100
  for (i in 1:n_ccp_data)
  {
    this_prcdr_cd <- ccp_data[i, "prcdr_cd"]
    prior <- (subset(procedure_priors, (prcdr_cd == this_prcdr_cd)))$prior_prob
    likelihood <- 1
    
    for (chronic_condition in chronic_conditions)
    {
      this_cc_value <- ccp_data[i, chronic_condition]
      relev_row_for_cpt <- subset(grand_cpt_cc, (prcdr_cd == this_prcdr_cd))
     
      relev_colname <- paste("cond_prob_", substr(chronic_condition, 4, nchar(chronic_condition)), sep = "")
      if (this_cc_value == 0)
      {
        relev_cond_prob <- relev_row_for_cpt[1, relev_colname]
      }
      else
      {
        relev_cond_prob <- relev_row_for_cpt[2, relev_colname]
      }
      likelihood <- likelihood*relev_cond_prob
      
      #cat(paste("chronic_condition = ", chronic_condition, ", this_cc_value = ", this_cc_value, 
      #          ", relev_cond_prob = ", relev_cond_prob, 
      #          ", likelihood = ", likelihood, "\n", sep = ""))
    }
    ccp_data[i, "prior"] <- prior
    ccp_data[i, "likelihood"] <- likelihood
    #cat(paste("i = ", i, ", this_prcdr_cd = ", this_prcdr_cd, ", prior = ", prior, ", likelihood = ", likelihood, "\n", sep = ""))
    if (i %% 100 == 0)
    {
      cat(paste("i = ", i, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  ccp_data$posterior <- ccp_data$prior*ccp_data$likelihood
  write.csv(ccp_data, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/ccp_data.csv")
  dbDisconnect(con)
  ccp_data
}

analyze_posteriors <- function()
{
  ccp_data <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/ccp_data.csv")
  threshold <- as.numeric(quantile(ccp_data$posterior, c(.01)))
  low_posterior <- subset(ccp_data, (posterior <= threshold))

  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF") 
  statement <- "select * from procedure_codes"
  res <- dbSendQuery(con, statement)
  pc_codes <- fetch(res, n = -1)
  dbDisconnect(con)
  print(pc_codes)
  low_posterior$prcdr_cd <- as.character(low_posterior$prcdr_cd)
  print(class(low_posterior$prcdr_cd))
  print(class(pc_codes$procedure_code))
  colnames(pc_codes) <- c("prcdr_cd", "long_desc", "short_desc")
  outlier_data <- merge(x = low_posterior, y = pc_codes, 
                        #by.x = "prcdr_cd", by.y = "procedure_code", 
                        all.x =  TRUE)
  write.csv(outlier_data, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/outlier_data.csv")

  tab <- table(low_posterior$prcdr_cd)
  df_low_posterior <- as.data.frame(tab) 
  colnames(df_low_posterior) <- c("prcdr_cd", "instances")
  df_low_posterior <- df_low_posterior[order(-df_low_posterior[, "instances"]),]
  #Pick the top k procedure codes where most low-posterior-probability events happen
}
