#Get all features and compute their information gain, the response being whether inpatient cost increased between 2008 and 2009
prepare_data_for_feature_selection <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- paste("select distinct('d_' || tcdc.dgns_cd) as feature
                      from beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                      where b2.DESYNPUF_ID = tcdc.DESYNPUF_ID
                      and tcdc.clm_thru_year = '2008'
                      order by feature", sep = "")
  res <- dbSendQuery(con, statement)
  #10635 diagnosis codes
  dgns_codes <- fetch(res, n = -1)
  cat(paste("nrow(dgns_codes) = ", nrow(dgns_codes), "\n", sep = ""))

  statement <- "select distinct('s_' || nc.substancename) as feature
                from beneficiary_summary_2009 b2, prescription_drug_events pde, ndc_codes nc
                where (b2.desynpuf_id = pde.desynpuf_id and to_char(pde.srvc_dt, 'YYYY') = '2008')
                and nc.substancename is not null
                and pde.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
                order by feature"
  res <- dbSendQuery(con, statement)
  #1806 substance codes
  substance_codes <- fetch(res, n = -1)
  cat(paste("nrow(substance_codes) = ", nrow(substance_codes), "\n", sep = ""))
  
  #Total 12441 features
  features <- rbind(dgns_codes, substance_codes)
  n_features <- nrow(features)
  cat(paste("n_features = ", n_features, "\n", sep = ""))

  for (i in 1:n_features)
  {
    v <- cont_table_for_feature(con, features[i, "feature"])
    features[i, "a1"] <- v[1]
    features[i, "a2"] <- v[2]
    features[i, "a3"] <- v[3]
    features[i, "a4"] <- v[4]
    if (i %% 200 == 0)
    {
      cat(paste("i = ", i, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  dbDisconnect(con)
  write.csv(gs, "/Users/blahiri/healthcare/documents/features_for_selection.csv")
}

get_n_had_condition <- function(con, feature_code)
{
  prefix <- substr(feature_code, 1, 1)
  feature_code <- substr(feature_code, 3, nchar(as.character(feature_code)))
  if (prefix == 'd')
  {
    statement <- paste("select count(distinct b2.DESYNPUF_ID)
                        from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                        where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                        and tcdc.DESYNPUF_ID = b2.DESYNPUF_ID
                        and to_char(tcdc.clm_thru_dt, 'YYYY') = '2008'
                        and tcdc.dgns_cd = '", feature_code, "'", sep = "")
    return(as.numeric(dbGetQuery(con, statement)))
  }
  if (prefix == 's')
  {
    statement <- paste("select count(distinct b2.DESYNPUF_ID)
                        from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, prescription_drug_events pde, ndc_codes nc
                        where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                        and (b2.desynpuf_id = pde.desynpuf_id and to_char(pde.srvc_dt, 'YYYY') = '2008')
                        and nc.substancename = '", feature_code, "' 
                        and pde.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code", sep = "")
    return(as.numeric(dbGetQuery(con, statement)))
  }  
}

get_n_had_condition_and_cost_increased <- function(con, feature_code)
{
  prefix <- substr(feature_code, 1, 1)
  feature_code <- substr(feature_code, 3, nchar(as.character(feature_code)))
  if (prefix == 'd')
  {
    statement <- paste("select count(distinct b2.DESYNPUF_ID)
                        from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                        where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                        and tcdc.DESYNPUF_ID = b2.DESYNPUF_ID 
                        and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)
                        and to_char(tcdc.clm_thru_dt, 'YYYY') = '2008'
                        and tcdc.dgns_cd = '", feature_code, "'", sep = "")
    return(as.numeric(dbGetQuery(con, statement)))
  }
  if (prefix == 's')
  {
    statement <- paste("select count(distinct b2.DESYNPUF_ID)
                        from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, prescription_drug_events pde, ndc_codes nc
                        where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                        and (b2.desynpuf_id = pde.desynpuf_id and to_char(pde.srvc_dt, 'YYYY') = '2008')
                        and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)
                        and nc.substancename = '", feature_code, "' 
                        and pde.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code", sep = "")
    return(as.numeric(dbGetQuery(con, statement)))
  }  
}


get_n_did_not_have_condition_but_cost_increased <- function(con, feature_code)
{
  prefix <- substr(feature_code, 1, 1)
  feature_code <- substr(feature_code, 3, nchar(as.character(feature_code)))
  if (prefix == 'd')
  {
    statement <- paste("select count(distinct b2.DESYNPUF_ID)
                        from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                        where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                        and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)
                        and not exists (select 1 from transformed_claim_diagnosis_codes tcdc
                                        where tcdc.DESYNPUF_ID = b2.DESYNPUF_ID
                                        and to_char(tcdc.clm_thru_dt, 'YYYY') = '2008'
                                        and tcdc.dgns_cd = '", feature_code,  "')", sep = "")
    return(as.numeric(dbGetQuery(con, statement)))
  }
  if (prefix == 's')
  {
    statement <- paste("select count(distinct b2.DESYNPUF_ID)
                        from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                        where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                        and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)
                        and not exists (select 1 from prescription_drug_events pde, ndc_codes nc
                                        where pde.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
                                        and pde.DESYNPUF_ID = b2.DESYNPUF_ID
                                        and to_char(pde.srvc_dt, 'YYYY') = '2008'
                                        and nc.substancename = '", feature_code, "')", sep = "")
    return(as.numeric(dbGetQuery(con, statement)))
  }  
}



cont_table_for_feature <- function(con, feature_code)
{
  #The first two are the row totals
  n_had_condition <- get_n_had_condition(con, feature_code)
  n_did_not_have_condition <- 114538 - n_had_condition

  a1 <- get_n_had_condition_and_cost_increased(con, feature_code)
  a2 <- n_had_condition - a1
  a3 <- get_n_did_not_have_condition_but_cost_increased(con, feature_code)
  a4 <- n_did_not_have_condition - a3
  cat(paste("feature_code = ", feature_code, ", a1 = ", a1, ", a2 = ", a2, ", a3 = ", a3, ", a4 = ", a4, "\n", sep = ""))
  return(c(a1, a2, a3, a4))
}


