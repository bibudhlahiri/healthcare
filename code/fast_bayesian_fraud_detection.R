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


create_data <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- "select * from transformed_claim_diagnosis_codes tcdc where tcdc.clm_thru_year = '2008'"
  res <- dbSendQuery(con, statement)
  transformed_claim_diagnosis_codes <- fetch(res, n = -1)
  write.csv(transformed_claim_diagnosis_codes, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/transformed_claim_diagnosis_codes.csv")

  statement <- "select * from transformed_claim_prcdr_codes tcpc where to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'"
  res <- dbSendQuery(con, statement)
  transformed_claim_prcdr_codes <- fetch(res, n = -1)
  write.csv(transformed_claim_prcdr_codes, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/transformed_claim_prcdr_codes.csv")

  statement <- "select * from beneficiary_summary_2008" 
  res <- dbSendQuery(con, statement)
  beneficiary_summary_2008 <- fetch(res, n = -1)
  write.csv(beneficiary_summary_2008, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/beneficiary_summary_2008.csv")

  dbDisconnect(con)
}


prepare_conditionals_for_conditions_proc_zero <- function()
{
  
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  procedure_priors <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  n_procs <- nrow(procedure_priors)
  cpt_conditions_proc_zero <- list()
  #n_procs <- 10
  cat(paste("n_procs = ", n_procs, "\n", sep = ""))

  statement <- "select count(distinct b.desynpuf_id)
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                where tcpc.desynpuf_id = b.desynpuf_id
                and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'"
  n_benefs <- as.numeric(dbGetQuery(con, statement))

  for (i in 1:n_procs)
  {
    this_proc <- procedure_priors[i, "prcdr_cd"]
    proc_count <- procedure_priors[i, "count"]

    #Diagnosed conditions for people who did NOT do this procedure but did some other procedure
    statement <- paste("select tcdc.dgns_cd, count(distinct b.desynpuf_id)
                        from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
                        where tcdc.desynpuf_id = b.desynpuf_id
                        and tcdc.clm_thru_year = '2008'
                        and not exists (select 1 from transformed_claim_prcdr_codes tcpc
                                        where to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                                        and tcpc.desynpuf_id = b.desynpuf_id
                                        and tcpc.prcdr_cd = '", this_proc, "') 
                        and exists (select 1 from transformed_claim_prcdr_codes tcpc1
                        where to_char(tcpc1.clm_thru_dt, 'YYYY') = '2008'
                        and tcpc1.desynpuf_id = b.desynpuf_id
                        and tcpc1.prcdr_cd <> '", this_proc, "')
                        group by tcdc.dgns_cd
                        order by count(distinct b.desynpuf_id) desc", sep = "")
    res <- dbSendQuery(con, statement)
    cpt_this_dc <- fetch(res, n = -1)
    diag_cpts <- cpt_this_dc$count/(n_benefs - proc_count)
    names(diag_cpts) <- paste("diag_", cpt_this_dc$dgns_cd, sep = "")
    
    #Chronic conditions for people who did NOT do this procedure but did some other procedure
    chronic_cpts <- c()
    names_for_chronic_cpts <- c()
    for (chronic_condition in chronic_conditions)
    {
      statement <- paste("select count(*)
                          from beneficiary_summary_2008 b
                          where ", chronic_condition, " = '1'
                          and not exists (select 1 from transformed_claim_prcdr_codes tcpc
                                          where to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                                          and tcpc.desynpuf_id = b.desynpuf_id
                                          and tcpc.prcdr_cd = '", this_proc, "') 
                          and exists (select 1 from transformed_claim_prcdr_codes tcpc1
                                      where to_char(tcpc1.clm_thru_dt, 'YYYY') = '2008'
                                      and tcpc1.desynpuf_id = b.desynpuf_id
                                      and tcpc1.prcdr_cd <> '", this_proc, "')", 
                         sep = "")
      proc_and_cc <- as.numeric(dbGetQuery(con, statement))
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


#Create a list to compute P(F1,...Fn| P = 0), where P is the procedure (not) performed and F1,...Fn is the set of chronic and diagnosed conditions.
#SQLite and sqldf take about same time as directly querying Postgres. However, SQLite and sqldf do not present connection problems when parallelized. 
prepare_conditionals_for_conditions_proc_zero_with_sqldf <- function()
{
  library(foreach)
  library(doMC)
  registerDoMC(8)

  cat(sprintf('Running with %d worker(s)\n', getDoParWorkers()))
  (name <- getDoParName())
  (ver <- getDoParVersion())
  if (getDoParRegistered())
   cat(sprintf('Currently using %s [%s]\n', name, ver))
  
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  procedure_priors <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  #procedure_priors <- procedure_priors[1:20, ]
  n_procs <- nrow(procedure_priors)
  cpt_conditions_proc_zero <- list()
  cat(paste("n_procs = ", n_procs, "\n", sep = ""))

  statement <- "select count(distinct b.desynpuf_id)
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                where tcpc.desynpuf_id = b.desynpuf_id
                and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'"
  n_benefs <- as.numeric(dbGetQuery(con, statement))

  statement <- "select * from transformed_claim_diagnosis_codes tcdc where tcdc.clm_thru_year = '2008'"
  res <- dbSendQuery(con, statement)
  transformed_claim_diagnosis_codes <- fetch(res, n = -1)
  cat(paste("object.size(transformed_claim_diagnosis_codes) = ", object.size(transformed_claim_diagnosis_codes), "\n", sep = "")) 

  statement <- "select * from transformed_claim_prcdr_codes tcpc where to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'"
  res <- dbSendQuery(con, statement)
  transformed_claim_prcdr_codes <- fetch(res, n = -1)
  cat(paste("object.size(transformed_claim_prcdr_codes) = ", object.size(transformed_claim_prcdr_codes), "\n", sep = ""))

  statement <- "select * from beneficiary_summary_2008" 
  res <- dbSendQuery(con, statement)
  beneficiary_summary_2008 <- fetch(res, n = -1)
  cat(paste("object.size(beneficiary_summary_2008) = ", object.size(beneficiary_summary_2008), "\n", sep = ""))

  cpt_conditions_proc_zero <- foreach(i=1:n_procs) %dopar%
  {
    this_proc <- procedure_priors[i, "prcdr_cd"]
    proc_count <- procedure_priors[i, "count"]

    ptm <- proc.time()
    #Diagnosed conditions for people who did NOT do this procedure but did some other procedure
    cpt_this_dc <- sqldf(paste("select tcdc.dgns_cd, count(distinct b.desynpuf_id)
                        from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
                        where tcdc.desynpuf_id = b.desynpuf_id
                        and not exists (select 1 from transformed_claim_prcdr_codes tcpc ", 
                                        " where tcpc.desynpuf_id = b.desynpuf_id
                                        and tcpc.prcdr_cd = '", this_proc, "') 
                        and exists (select 1 from transformed_claim_prcdr_codes tcpc1 ", 
                                   " where tcpc1.desynpuf_id = b.desynpuf_id
                                   and tcpc1.prcdr_cd <> '", this_proc, "')
                                   group by tcdc.dgns_cd
                                   order by count(distinct b.desynpuf_id) desc", sep = ""), drv = "SQLite", dbname = ":memory:")
    t <- proc.time() - ptm
    cat("time for first query\n")
    print(t)

    diag_cpts <- cpt_this_dc$count/(n_benefs - proc_count)
    names(diag_cpts) <- paste("diag_", cpt_this_dc$dgns_cd, sep = "")
    
    #Chronic conditions for people who did NOT do this procedure but did some other procedure
    chronic_cpts <- c()
    names_for_chronic_cpts <- c()
    for (chronic_condition in chronic_conditions)
    {
      ptm <- proc.time()
      proc_and_cc <- sqldf(paste("select count(*)
                          from beneficiary_summary_2008 b
                          where ", chronic_condition, " = '1'
                          and not exists (select 1 from transformed_claim_prcdr_codes tcpc ", 
                                          " where tcpc.desynpuf_id = b.desynpuf_id
                                          and tcpc.prcdr_cd = '", this_proc, "')  
                          and exists (select 1 from transformed_claim_prcdr_codes tcpc1 ", 
                                      " where tcpc1.desynpuf_id = b.desynpuf_id
                                      and tcpc1.prcdr_cd <> '", this_proc, "')", 
                         sep = ""), drv = "SQLite", dbname = ":memory:")
      t <- proc.time() - ptm
      cat("time for second query\n")
      print(t)

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
    cpt_conditions_proc_zero[[this_proc]]
  } #end for (i in 1:n_procs)
  names(cpt_conditions_proc_zero) <- paste("proc_", procedure_priors$prcdr_cd, sep = "")
  dbDisconnect(con)
  save(cpt_conditions_proc_zero, file = "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/cpt_conditions_proc_zero.RData") 
  load(file = "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/cpt_conditions_proc_zero.RData", envir = .GlobalEnv)
  #Loading a list creates an object with the same name as the file, but the value is the name of the object.
  loaded_cpt_conditions <- cpt_conditions_proc_zero
}


prepare_conditionals_for_conditions_proc_zero_for_cluster <- function()
{
  library(foreach)
  library(doMC)
  registerDoMC(16)

  cat(sprintf('Running with %d worker(s)\n', getDoParWorkers()))
  (name <- getDoParName())
  (ver <- getDoParVersion())
  if (getDoParRegistered())
   cat(sprintf('Currently using %s [%s]\n', name, ver))
  
  procedure_priors <- read.csv("/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/procedure_priors.csv")
  #procedure_priors <- procedure_priors[1:50, ]
  n_procs <- nrow(procedure_priors)
  cpt_conditions_proc_zero <- list()
  cat(paste("n_procs = ", n_procs, "\n", sep = ""))

  transformed_claim_diagnosis_codes <- read.csv("/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/transformed_claim_diagnosis_codes.csv")

  transformed_claim_prcdr_codes <- read.csv("/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/transformed_claim_prcdr_codes.csv")

  beneficiary_summary_2008 <- read.csv("/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/beneficiary_summary_2008.csv")

  n_benefs <- sqldf("select count(distinct b.desynpuf_id)
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                where tcpc.desynpuf_id = b.desynpuf_id", drv = "SQLite", dbname = ":memory:")
  n_benefs <- as.numeric(n_benefs)

  cat(paste("calling foreach at ", Sys.time(), "\n", sep = ""))
  cpt_conditions_proc_zero <- foreach(i=1:n_procs) %dopar%
  {
    this_proc <- procedure_priors[i, "prcdr_cd"]
    proc_count <- procedure_priors[i, "count"]

    #Diagnosed conditions for people who did NOT do this procedure but did some other procedure
    cpt_this_dc <- sqldf(paste("select tcdc.dgns_cd, count(distinct b.desynpuf_id)
                        from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
                        where tcdc.desynpuf_id = b.desynpuf_id
                        and not exists (select 1 from transformed_claim_prcdr_codes tcpc ", 
                                        " where tcpc.desynpuf_id = b.desynpuf_id
                                        and tcpc.prcdr_cd = '", this_proc, "') 
                        and exists (select 1 from transformed_claim_prcdr_codes tcpc1 ", 
                                   " where tcpc1.desynpuf_id = b.desynpuf_id
                                   and tcpc1.prcdr_cd <> '", this_proc, "')
                                   group by tcdc.dgns_cd
                                   order by count(distinct b.desynpuf_id) desc", sep = ""), drv = "SQLite", dbname = ":memory:")

    diag_cpts <- cpt_this_dc$count/(n_benefs - proc_count)
    names(diag_cpts) <- paste("diag_", cpt_this_dc$dgns_cd, sep = "")
    
    #Chronic conditions for people who did NOT do this procedure but did some other procedure
    chronic_cpts <- c()
    names_for_chronic_cpts <- c()
    for (chronic_condition in chronic_conditions)
    {
      proc_and_cc <- sqldf(paste("select count(*)
                          from beneficiary_summary_2008 b
                          where ", chronic_condition, " = '1'
                          and not exists (select 1 from transformed_claim_prcdr_codes tcpc ", 
                                          " where tcpc.desynpuf_id = b.desynpuf_id
                                          and tcpc.prcdr_cd = '", this_proc, "')  
                          and exists (select 1 from transformed_claim_prcdr_codes tcpc1 ", 
                                      " where tcpc1.desynpuf_id = b.desynpuf_id
                                      and tcpc1.prcdr_cd <> '", this_proc, "')", 
                         sep = ""), drv = "SQLite", dbname = ":memory:")

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
    cpt_conditions_proc_zero[[this_proc]]
  } #end for (i in 1:n_procs)
  cat(paste("returned from foreach at ", Sys.time(), "\n", sep = ""))
  names(cpt_conditions_proc_zero) <- paste("proc_", procedure_priors$prcdr_cd, sep = "")
  save(cpt_conditions_proc_zero, file = "/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/cpt_conditions_proc_zero.RData") 
  load(file = "/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/cpt_conditions_proc_zero.RData", envir = .GlobalEnv)
  #Loading a list creates an object with the same name as the file, but the value is the name of the object.
  loaded_cpt_conditions <- cpt_conditions_proc_zero
}


prepare_conditionals_for_conditions_proc_zero_without_sqldf <- function()
{
  library(foreach)
  library(doMC)
  registerDoMC(16)

  cat(sprintf('Running with %d worker(s)\n', getDoParWorkers()))
  (name <- getDoParName())
  (ver <- getDoParVersion())
  if (getDoParRegistered())
   cat(sprintf('Currently using %s [%s]\n', name, ver))
 
  file_path <- "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/"
  #file_path <- "/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/"
 
  procedure_priors <- read.csv(paste(file_path, "procedure_priors.csv", sep = ""))
  #procedure_priors <- procedure_priors[1:50, ]
  n_procs <- nrow(procedure_priors)
  cpt_conditions_proc_zero <- list()
  cat(paste("n_procs = ", n_procs, "\n", sep = ""))

  transformed_claim_diagnosis_codes <- read.csv(paste(file_path, "transformed_claim_diagnosis_codes.csv", sep = ""))

  transformed_claim_prcdr_codes <- read.csv(paste(file_path, "transformed_claim_prcdr_codes.csv", sep = ""))

  beneficiary_summary_2008 <- read.csv(paste(file_path, "beneficiary_summary_2008.csv", sep = ""))

  n_benefs <- sqldf("select count(distinct b.desynpuf_id)
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                where tcpc.desynpuf_id = b.desynpuf_id", drv = "SQLite", dbname = ":memory:")
  n_benefs <- as.numeric(n_benefs)

  cat(paste("calling foreach at ", Sys.time(), "\n", sep = ""))
  cpt_conditions_proc_zero <- foreach(i=1:n_procs) %dopar%
  {
    this_proc <- procedure_priors[i, "prcdr_cd"]
    proc_count <- procedure_priors[i, "count"]

    #Diagnosed conditions for people who did NOT do this procedure but did some other procedure
    cpt_this_dc <- sqldf(paste("select tcdc.dgns_cd, count(distinct b.desynpuf_id)
                        from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
                        where tcdc.desynpuf_id = b.desynpuf_id
                        and not exists (select 1 from transformed_claim_prcdr_codes tcpc ", 
                                        " where tcpc.desynpuf_id = b.desynpuf_id
                                        and tcpc.prcdr_cd = '", this_proc, "') 
                        and exists (select 1 from transformed_claim_prcdr_codes tcpc1 ", 
                                   " where tcpc1.desynpuf_id = b.desynpuf_id
                                   and tcpc1.prcdr_cd <> '", this_proc, "')
                                   group by tcdc.dgns_cd
                                   order by count(distinct b.desynpuf_id) desc", sep = ""), drv = "SQLite", dbname = ":memory:")

    diag_cpts <- cpt_this_dc$count/(n_benefs - proc_count)
    names(diag_cpts) <- paste("diag_", cpt_this_dc$dgns_cd, sep = "")
    
    #Chronic conditions for people who did NOT do this procedure but did some other procedure
    chronic_cpts <- c()
    names_for_chronic_cpts <- c()
    for (chronic_condition in chronic_conditions)
    {
      proc_and_cc <- sqldf(paste("select count(*)
                          from beneficiary_summary_2008 b
                          where ", chronic_condition, " = '1'
                          and not exists (select 1 from transformed_claim_prcdr_codes tcpc ", 
                                          " where tcpc.desynpuf_id = b.desynpuf_id
                                          and tcpc.prcdr_cd = '", this_proc, "')  
                          and exists (select 1 from transformed_claim_prcdr_codes tcpc1 ", 
                                      " where tcpc1.desynpuf_id = b.desynpuf_id
                                      and tcpc1.prcdr_cd <> '", this_proc, "')", 
                         sep = ""), drv = "SQLite", dbname = ":memory:")

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
    cpt_conditions_proc_zero[[this_proc]]
  } #end for (i in 1:n_procs)
  cat(paste("returned from foreach at ", Sys.time(), "\n", sep = ""))
  names(cpt_conditions_proc_zero) <- paste("proc_", procedure_priors$prcdr_cd, sep = "")
  save(cpt_conditions_proc_zero, file = "/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/cpt_conditions_proc_zero.RData") 
  load(file = "/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/cpt_conditions_proc_zero.RData", envir = .GlobalEnv)
  #Loading a list creates an object with the same name as the file, but the value is the name of the object.
  loaded_cpt_conditions <- cpt_conditions_proc_zero
}


get_cpt_conditions_proc_zero_this_proc <- function(i, procedure_priors, transformed_claim_diagnosis_codes, 
                                                   beneficiary_summary_2008, transformed_claim_prcdr_codes, n_benefs)
  {
    this_proc <- procedure_priors[i, "prcdr_cd"]
    proc_count <- procedure_priors[i, "count"]

    #Diagnosed conditions for people who did NOT do this procedure but did some other procedure
    cpt_this_dc <- sqldf(paste("select tcdc.dgns_cd, count(distinct b.desynpuf_id)
                        from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
                        where tcdc.desynpuf_id = b.desynpuf_id
                        and not exists (select 1 from transformed_claim_prcdr_codes tcpc ", 
                                        " where tcpc.desynpuf_id = b.desynpuf_id
                                        and tcpc.prcdr_cd = '", this_proc, "') 
                        and exists (select 1 from transformed_claim_prcdr_codes tcpc1 ", 
                                   " where tcpc1.desynpuf_id = b.desynpuf_id
                                   and tcpc1.prcdr_cd <> '", this_proc, "')
                                   group by tcdc.dgns_cd
                                   order by count(distinct b.desynpuf_id) desc", sep = ""), drv = "SQLite", dbname = ":memory:")

    diag_cpts <- cpt_this_dc$count/(n_benefs - proc_count)
    names(diag_cpts) <- paste("diag_", cpt_this_dc$dgns_cd, sep = "")
    
    #Chronic conditions for people who did NOT do this procedure but did some other procedure
    chronic_cpts <- c()
    names_for_chronic_cpts <- c()
    for (chronic_condition in chronic_conditions)
    {
      proc_and_cc <- sqldf(paste("select count(*)
                          from beneficiary_summary_2008 b
                          where ", chronic_condition, " = '1'
                          and not exists (select 1 from transformed_claim_prcdr_codes tcpc ", 
                                          " where tcpc.desynpuf_id = b.desynpuf_id
                                          and tcpc.prcdr_cd = '", this_proc, "')  
                          and exists (select 1 from transformed_claim_prcdr_codes tcpc1 ", 
                                      " where tcpc1.desynpuf_id = b.desynpuf_id
                                      and tcpc1.prcdr_cd <> '", this_proc, "')", 
                         sep = ""), drv = "SQLite", dbname = ":memory:")

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
      return(append(diag_cpts, chronic_cpts))
    }
    else
    {
      return(diag_cpts)
    }
  } 


prepare_conditionals_for_conditions_proc_zero_with_parallel <- function()
{
  library(parallel)
  #file_path <- "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/"
  file_path <- "/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/"

  
  procedure_priors <- read.csv(paste(file_path, "procedure_priors.csv", sep = ""))
  procedure_priors <- procedure_priors[1:20, ]
  n_procs <- nrow(procedure_priors)
  cpt_conditions_proc_zero <- list()
  cat(paste("n_procs = ", n_procs, "\n", sep = ""))

  transformed_claim_diagnosis_codes <- read.csv(paste(file_path, "transformed_claim_diagnosis_codes.csv", sep = ""))
  transformed_claim_prcdr_codes <- read.csv(paste(file_path, "transformed_claim_prcdr_codes.csv", sep = ""))
  beneficiary_summary_2008 <- read.csv(paste(file_path, "beneficiary_summary_2008.csv", sep = ""))

  n_benefs <- sqldf("select count(distinct b.desynpuf_id)
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                where tcpc.desynpuf_id = b.desynpuf_id", drv = "SQLite", dbname = ":memory:")
  n_benefs <- as.numeric(n_benefs)

  cat(paste("calling mclapply at ", Sys.time(), "\n", sep = ""))
  
  #More than 5 cores creates a problem with mclapply. However, 2 minutes with 20 procedures, so estimated time for mclapply for whole job is 48 hours!
  cpt_conditions_proc_zero <- mclapply(X = 1:n_procs, FUN = get_cpt_conditions_proc_zero_this_proc, 
                                       procedure_priors, transformed_claim_diagnosis_codes, beneficiary_summary_2008, 
                                       transformed_claim_prcdr_codes, n_benefs, mc.preschedule = TRUE, mc.cores = 5)
  
  cat(paste("returned from mclapply at ", Sys.time(), "\n", sep = ""))
 
  names(cpt_conditions_proc_zero) <- paste("proc_", procedure_priors$prcdr_cd, sep = "")
  save(cpt_conditions_proc_zero, file = paste(file_path, "cpt_conditions_proc_zero.RData", sep = "")) 
  load(file = paste(file_path, "cpt_conditions_proc_zero.RData", sep = ""), envir = .GlobalEnv)
  #Loading a list creates an object with the same name as the file, but the value is the name of the object.
  loaded_cpt_conditions <- cpt_conditions_proc_zero
}

prepare_conditionals_for_conditions_proc_zero_with_snow <- function()
{
  library(snow)
  #file_path <- "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/"
  file_path <- "/home/impadmin/bibudh/healthcare/documents/fraud_detection/bayesian/"
  
  procedure_priors <- read.csv(paste(file_path, "procedure_priors.csv", sep = ""))
  procedure_priors <- procedure_priors[1:20, ]
  n_procs <- nrow(procedure_priors)
  cpt_conditions_proc_zero <- list()
  cat(paste("n_procs = ", n_procs, "\n", sep = ""))

  transformed_claim_diagnosis_codes <- read.csv(paste(file_path, "transformed_claim_diagnosis_codes.csv", sep = ""))
  transformed_claim_prcdr_codes <- read.csv(paste(file_path, "transformed_claim_prcdr_codes.csv", sep = ""))
  beneficiary_summary_2008 <- read.csv(paste(file_path, "beneficiary_summary_2008.csv", sep = ""))

  n_benefs <- sqldf("select count(distinct b.desynpuf_id)
                     from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
                     where tcpc.desynpuf_id = b.desynpuf_id", drv = "SQLite", dbname = ":memory:")
  n_benefs <- as.numeric(n_benefs)

  cl <- makeCluster(spec = rep("localhost", 8), count = 8, type = "SOCK")
  clusterExport(cl, list("sqldf", "chronic_conditions"))
 
  cat(paste("calling parLapply at ", Sys.time(), "\n", sep = "")) 
  #However, 2 minutes with 20 procedures, so estimated time for parLapply for whole job is 48 hours!
  cpt_conditions_proc_zero <- parLapply(cl, X = 1:n_procs, FUN = get_cpt_conditions_proc_zero_this_proc, 
                                       procedure_priors, transformed_claim_diagnosis_codes, beneficiary_summary_2008, 
                                       transformed_claim_prcdr_codes, n_benefs)
  cat(paste("returned from parLapply at ", Sys.time(), "\n", sep = ""))
  stopCluster(cl)
  
  names(cpt_conditions_proc_zero) <- paste("proc_", procedure_priors$prcdr_cd, sep = "")
  save(cpt_conditions_proc_zero, file = paste(file_path, "cpt_conditions_proc_zero.RData", sep = "")) 
  load(file = paste(file_path, "cpt_conditions_proc_zero.RData", sep = ""), envir = .GlobalEnv)
  #Loading a list creates an object with the same name as the file, but the value is the name of the object.
  loaded_cpt_conditions <- cpt_conditions_proc_zero
}

sample_patients_and_procedures <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- "select b.desynpuf_id, sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr, sp_copd, sp_depressn, 
                sp_diabetes, sp_ischmcht, sp_osteoprs, sp_ra_oa, sp_strketia, tcpc.prcdr_cd
                from beneficiary_summary_2008 b, transformed_claim_prcdr_codes tcpc
                where to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                and tcpc.desynpuf_id = b.desynpuf_id
                order by b.desynpuf_id, tcpc.prcdr_cd"
  res <- dbSendQuery(con, statement)
  patients_and_procs <- fetch(res, n = -1)
  sample_size <- 1000
  cat(paste("sample_size = ", sample_size, "\n", sep = ""))
  set.seed(1)
  sample <- patients_and_procs[sample(nrow(patients_and_procs), sample_size),]
  write.csv(sample, "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/sample_patients_and_procs.csv")
  dbDisconnect(con)
}

#Generate the list of named vectors data structure where each element of the list is for one patient. The named vector contains 
#the list of conditions diagnosed and chronic conditions. We will do it ONLY for the patients who have been sampled in sample_patients_and_procedures(). 
#We will parallelize the work of computing posteriors on combinations of patients and procedures later.
summarize_patients <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  spp <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/bayesian/sample_patients_and_procs.csv")
  patients <- unique(spp$desynpuf_id)
  #patient_clause in query ensures these are patients who have undergone some procedure
  patient_clause <- paste("('", paste(patients, collapse = "', '"), "')", sep = "")
  
  statement <- paste("select distinct b.desynpuf_id, sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr, sp_copd, sp_depressn, 
                      sp_diabetes, sp_ischmcht, sp_osteoprs, sp_ra_oa, sp_strketia, tcdc.dgns_cd
                      from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
                      where tcdc.desynpuf_id = b.desynpuf_id
                      and tcdc.clm_thru_year = '2008' and b.desynpuf_id in ",
                      patient_clause, 
                      " order by b.desynpuf_id, tcdc.dgns_cd", sep = "")
  res <- dbSendQuery(con, statement)
  patients_data <- fetch(res, n = -1)
  patients_data <- patients_data[1:200, ]
  n_patients_data <- nrow(patients_data)
  cat(paste("n_patients_data = ", n_patients_data, "\n", sep = ""))
  
  patients_summarized <- list()
  patient_id <- "-1"
  for (i in 1:n_patients_data)
  {
    if (patients_data[i, "desynpuf_id"] != patient_id)
    {
      #New patient. Add the vector for the old patient to patients_summarized.
      if (patient_id != "-1")
      {
        patients_summarized[[patient_id]] <- conds_this_patient
      }
      #Start a vector, put all the chronic conditions this patient has, and the first diagnostic condition. 
      patient_id <- patients_data[i, "desynpuf_id"]
      conds_this_patient <- c()
      for (chronic_condition in chronic_conditions)
      {
         if (patients_data[i, chronic_condition] == '1')
         {
           conds_this_patient <- append(conds_this_patient, paste("chron_", substr(chronic_condition, 4, nchar(chronic_condition)), sep = ""))
         } 
      }
      conds_this_patient <- append(conds_this_patient, paste("diag_", patients_data[i, "dgns_cd"], sep = ""))
    }
    else
    {
     #Continuing patient. Add the diagnostic condition to the existing vector.
     conds_this_patient <- append(conds_this_patient, paste("diag_", patients_data[i, "dgns_cd"], sep = ""))
    }
  }
  #Add the last one to list
  patients_summarized[[patient_id]] <- conds_this_patient
  dbDisconnect(con) 
  save(patients_summarized, file = "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/patients_summarized.RData") 
  load(file = "/Users/blahiri/healthcare/documents/fraud_detection/bayesian/patients_summarized.RData", envir = .GlobalEnv)
  #Loading a list creates an object with the same name as the file, but the value is the name of the object.
  loaded_patients_data <- patients_summarized
}


