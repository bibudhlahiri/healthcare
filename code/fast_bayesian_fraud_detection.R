library(RPostgreSQL)
library(sqldf)
chronic_conditions <- c("sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr", "sp_copd", "sp_depressn", 
                          "sp_diabetes", "sp_ischmcht", "sp_osteoprs", "sp_ra_oa", "sp_strketia")


#Create a list to compute P(F1,...Fn| P = 1), where P is the procedure performed and F1,...Fn is the set of chronic and diagnosed conditions.
prepare_conditionals_for_conditions_proc_one <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  procedure_priors <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  n_procs <- nrow(procedure_priors)
  cpt_conditions_proc_one <- list()
  #n_procs <- 10
  cat(paste("n_procs = ", n_procs, "\n", sep = ""))
  for (i in 1:n_procs)
  {
    this_proc <- procedure_priors[i, "prcdr_cd"]
    proc_count <- procedure_priors[i, "count"]

    #Diagnosed conditions
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
    names(diag_cpts) <- paste("diag_", cpt_this_dc$dgns_cd, sep = "")
    
    #Chronic conditions
    chronic_cpts <- c()
    names_for_chronic_cpts <- c()
    for (chronic_condition in chronic_conditions)
    {
      statement <- paste("select count(distinct b.desynpuf_id)
                    from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                    where tcpc.desynpuf_id = b.desynpuf_id
                    and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                    and ", chronic_condition, " = '1' ",
                    "and tcpc.prcdr_cd = '", this_proc, "' ", sep = "")
      proc_and_cc <- as.numeric(dbGetQuery(con, statement))
      if (proc_and_cc > 0)
      {
        chronic_cpts <- append(chronic_cpts, proc_and_cc)
        names_for_chronic_cpts <- append(names_for_chronic_cpts, chronic_condition)
      }
    }
    chronic_cpts <- chronic_cpts/proc_count

    this_proc <- paste("proc_", this_proc, sep = "")
    if (length(chronic_cpts) > 0)
    {
      names(chronic_cpts) <- paste("chron_", substr(names_for_chronic_cpts, 4, nchar(names_for_chronic_cpts)), sep = "")
      cpt_conditions_proc_one[[this_proc]] <- append(diag_cpts, chronic_cpts)
    }
    else
    {
      cpt_conditions_proc_one[[this_proc]] <- diag_cpts
    }
    if (i %% 100 == 0)
    {
      cat(paste("i = ", i, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  dbDisconnect(con)
  save(cpt_conditions_proc_one, file = "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/cpt_conditions_proc_one.RData") 
  load(file = "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/cpt_conditions_proc_one.RData", envir = .GlobalEnv)
  #Loading a list creates an object with the same name as the file, but the value is the name of the object.
  loaded_cpt_conditions <- cpt_conditions_proc_one
}


#Create a list to compute P(F1,...Fn| P = 0), where P is the procedure (not) performed and F1,...Fn is the set of chronic and diagnosed conditions.
prepare_conditionals_for_conditions_proc_zero <- function()
{
  
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  procedure_priors <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  n_procs <- nrow(procedure_priors)
  cpt_conditions_proc_zero <- list()
  n_procs <- 20
  cat(paste("n_procs = ", n_procs, "\n", sep = ""))

  statement <- "select count(distinct b.desynpuf_id)
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                where tcpc.desynpuf_id = b.desynpuf_id
                and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'"
  n_benefs <- as.numeric(dbGetQuery(con, statement))

  statement <- "select * from transformed_claim_diagnosis_codes tcdc where tcdc.clm_thru_year = '2008'"
  res <- dbSendQuery(con, statement)
  transformed_claim_diagnosis_codes <- fetch(res, n = -1)

  statement <- "select * from transformed_claim_prcdr_codes tcpc where to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'"
  res <- dbSendQuery(con, statement)
  transformed_claim_prcdr_codes <- fetch(res, n = -1)

  statement <- "select * from beneficiary_summary_2008" 
  res <- dbSendQuery(con, statement)
  beneficiary_summary_2008 <- fetch(res, n = -1)

  for (i in 1:n_procs)
  {
    this_proc <- procedure_priors[i, "prcdr_cd"]
    proc_count <- procedure_priors[i, "count"]

    #Diagnosed conditions for people who did NOT do this procedure but did some other procedure
    #statement <- paste("select tcdc.dgns_cd, count(distinct b.desynpuf_id)
    cpt_this_dc <- sqldf(paste("select tcdc.dgns_cd, count(distinct b.desynpuf_id)
                        from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
                        where tcdc.desynpuf_id = b.desynpuf_id
                        and tcdc.clm_thru_year = '2008'
                        and not exists (select 1 from transformed_claim_prcdr_codes tcpc ", 
                                        #where to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                                        " where tcpc.desynpuf_id = b.desynpuf_id
                                        and tcpc.prcdr_cd = '", this_proc, "') 
                        and exists (select 1 from transformed_claim_prcdr_codes tcpc1 ", 
                                    #where to_char(tcpc1.clm_thru_dt, 'YYYY') = '2008'
                                   " where tcpc1.desynpuf_id = b.desynpuf_id
                                   and tcpc1.prcdr_cd <> '", this_proc, "')
                                   group by tcdc.dgns_cd
                                   order by count(distinct b.desynpuf_id) desc", sep = ""), drv = "SQLite", dbname = ":memory:")
    #res <- dbSendQuery(con, statement)
    #cpt_this_dc <- fetch(res, n = -1)
    diag_cpts <- cpt_this_dc$count/(n_benefs - proc_count)
    names(diag_cpts) <- paste("diag_", cpt_this_dc$dgns_cd, sep = "")
    
    #Chronic conditions for people who did NOT do this procedure but did some other procedure
    chronic_cpts <- c()
    names_for_chronic_cpts <- c()
    for (chronic_condition in chronic_conditions)
    {
      #statement <- paste("select count(*)
      proc_and_cc <- sqldf(paste("select count(*)
                          from beneficiary_summary_2008 b
                          where ", chronic_condition, " = '1'
                          and not exists (select 1 from transformed_claim_prcdr_codes tcpc ", 
                                          #where to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                                          " where tcpc.desynpuf_id = b.desynpuf_id
                                          and tcpc.prcdr_cd = '", this_proc, "')  
                          and exists (select 1 from transformed_claim_prcdr_codes tcpc1 ", 
                                      #where to_char(tcpc1.clm_thru_dt, 'YYYY') = '2008'
                                      " where tcpc1.desynpuf_id = b.desynpuf_id
                                      and tcpc1.prcdr_cd <> '", this_proc, "')", 
                         sep = ""), drv = "SQLite", dbname = ":memory:")
      #proc_and_cc <- as.numeric(dbGetQuery(con, statement))
      proc_and_cc <- as.numeric(proc_and_cc)
      if (proc_and_cc > 0)
      {
        chronic_cpts <- append(chronic_cpts, proc_and_cc)
        names_for_chronic_cpts <- append(names_for_chronic_cpts, chronic_condition)
      }
    }
     
    chronic_cpts <- chronic_cpts/(n_benefs - proc_count)

    this_proc <- paste("proc_", this_proc, sep = "")
    if (length(chronic_cpts) > 0)
    {
      names(chronic_cpts) <- paste("chron_", substr(names_for_chronic_cpts, 4, nchar(names_for_chronic_cpts)), sep = "")
      cpt_conditions_proc_zero[[this_proc]] <- append(diag_cpts, chronic_cpts)
    }
    else
    {
      cpt_conditions_proc_zero[[this_proc]] <- diag_cpts
    }
    if (i %% 10 == 0)
    {
      cat(paste("i = ", i, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  dbDisconnect(con)
  save(cpt_conditions_proc_zero, file = "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/cpt_conditions_proc_zero.RData") 
  load(file = "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/cpt_conditions_proc_zero.RData", envir = .GlobalEnv)
  #Loading a list creates an object with the same name as the file, but the value is the name of the object.
  loaded_cpt_conditions <- cpt_conditions_proc_zero
}


