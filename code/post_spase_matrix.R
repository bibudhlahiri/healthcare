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

  statement <- "select 'd_' || tcdc.dgns_cd as feature, count(distinct b2.DESYNPUF_ID) as n_had_condition
                from beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                where tcdc.DESYNPUF_ID = b2.DESYNPUF_ID
                and to_char(tcdc.clm_thru_dt, 'YYYY') = '2008'
                group by feature"
  res <- dbSendQuery(con, statement)
  dgns_codes_present <- fetch(res, n = -1)
  dgns_codes <- merge(dgns_codes, dgns_codes_present, all.x = TRUE)

  statement <- "select 'd_' || tcdc.dgns_cd as feature, count(distinct b2.DESYNPUF_ID) as n_had_condition_and_cost_increased
                from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                and tcdc.DESYNPUF_ID = b2.DESYNPUF_ID 
                and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)
                and to_char(tcdc.clm_thru_dt, 'YYYY') = '2008'
                group by feature"
  res <- dbSendQuery(con, statement)
  dgns_codes_present_and_cost_inc <- fetch(res, n = -1)
  dgns_codes <- merge(dgns_codes, dgns_codes_present_and_cost_inc, all.x = TRUE)

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

  statement <- "select 's_' || nc.substancename as feature, count(distinct b2.DESYNPUF_ID) as n_had_condition
                from beneficiary_summary_2009 b2, prescription_drug_events pde, ndc_codes nc
                where (b2.desynpuf_id = pde.desynpuf_id and to_char(pde.srvc_dt, 'YYYY') = '2008')
                and nc.substancename is not null
                and pde.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
                group by feature"
  res <- dbSendQuery(con, statement)
  substance_codes_present <- fetch(res, n = -1)
  substance_codes <- merge(substance_codes, substance_codes_present, all.x = TRUE)

  statement <- "select 's_' || nc.substancename as feature, count(distinct b2.DESYNPUF_ID) as n_had_condition_and_cost_increased
                from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, prescription_drug_events pde, ndc_codes nc
                where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                and (b2.desynpuf_id = pde.desynpuf_id and to_char(pde.srvc_dt, 'YYYY') = '2008')
                and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)
                and nc.substancename is not null
                and pde.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
                group by feature"
  res <- dbSendQuery(con, statement)
  substance_codes_present_and_cost_inc <- fetch(res, n = -1)
  substance_codes <- merge(substance_codes, substance_codes_present_and_cost_inc, all.x = TRUE)
  
  #Total 12441 features
  features <- rbind(dgns_codes, substance_codes)
  features[is.na(features)] <- 0
  n_features <- nrow(features)
  cat(paste("n_features = ", n_features, "\n", sep = ""))

  features$a1 <- features$n_had_condition_and_cost_increased
  features$a2 <- features$n_had_condition - features$a1
  features$a3 <- 16248 - features$a1
  features$a4 <- 98290 - features$a2
  features <- features[, !(names(features) %in% c("n_had_condition_and_cost_increased"))]
  dbDisconnect(con)
  write.csv(features, "/Users/blahiri/healthcare/documents/features_for_selection.csv")
  features
}

#Given a vector of frequencies of diff categories, computes the entropy.
my_entropy <- function(x)
{
  p <- x/sum(x)
  len <- length(p)
  sum <- 0
  for (i in 1:len)
  {
    if (p[i] > 0)
    {
      sum <- sum - p[i]*log(p[i], 2)
    }
  }
  sum
}

info_gain_for_feature <- function(entropy_session_category, a1, a2, a3, a4)
{
  spec_cond_entropy_gram_present <- my_entropy(c(a1, a2))
  spec_cond_entropy_gram_absent <- my_entropy(c(a3, a4))
  prob_gram_present <- (a1 + a2)/(a1 + a2 + a3 + a4)
  prob_gram_absent <- (a3 + a4)/(a1 + a2 + a3 + a4)
  cond_entropy <- prob_gram_present*spec_cond_entropy_gram_present + prob_gram_absent*spec_cond_entropy_gram_absent
  return(entropy_session_category - cond_entropy)
}

#Computes info gain and writes back the data frame.
compute_info_gain <- function()
{
  entropy_patient_category <- my_entropy(c(16248, 98290))
  features <- read.csv("/Users/blahiri/healthcare/documents/features_for_selection.csv")
  features$info_gain <- apply(features, 1, function(row)info_gain_for_feature(entropy_patient_category, as.numeric(row["a1"]), as.numeric(row["a2"]), 
                        as.numeric(row["a3"]), as.numeric(row["a4"])))
  features <- features[order(-features[,"info_gain"]),]
  write.csv(features, "/Users/blahiri/healthcare/documents/features_for_selection.csv")
}

df <- data.frame()

populate_data_frame <- function(obs_id, feature_id)
{
  df[obs_id, feature_id] <<- 1
}

prepare_data_post_feature_selection <- function(sample_size = 1000, how_many = 30)
{
   con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                    host = "localhost", port="5432", dbname = "DE-SynPUF")

   statement <- "select b1.DESYNPUF_ID,
                 case when (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip) then 'increased' else 'did_not_increase'
                 end as change_type
                 from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                 where b1.DESYNPUF_ID = b2.DESYNPUF_ID"
   res <- dbSendQuery(con, statement)
   beneficiaries <- fetch(res, n = -1)
   cat(paste("nrow(beneficiaries) = ", nrow(beneficiaries), "\n", sep = ""))
   sampled <- sample(1:nrow(beneficiaries), sample_size)
   beneficiaries <- beneficiaries[sampled, ]
   
   features <- read.csv("/Users/blahiri/healthcare/documents/features_for_selection.csv")
   features <- features[1:how_many, ]
   features$feature <- substr(features$feature, 3, nchar(as.character(features$feature)))
   patient_clause <- paste("('", paste(beneficiaries$desynpuf_id, collapse = "', '"), "')", sep = "")
   clause <- paste("('", paste(features$feature, collapse = "', '"), "')", sep = "")
   print(clause)

   statement <- paste("select b2.desynpuf_id, 'd_' || tcdc.dgns_cd as feature
                      from beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                      where b2.DESYNPUF_ID = tcdc.DESYNPUF_ID
                      and tcdc.clm_thru_year = '2008' ", 
                      " and b2.desynpuf_id in ", patient_clause, 
                      " and tcdc.dgns_cd in ", clause, 
                      " order by b2.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement)
  data <- fetch(res, n = -1)
  
  print(data[1:5, ])
  
  features <- unique(data$feature)
  n_features <- length(features)

  observations <- unique(data$desynpuf_id)
  n_observations <- length(observations)
  n_data <- nrow(data)
  cat(paste("n_data = ", n_data, "\n", sep = ""))

  apply(data, 1, function(row)populate_data_frame(row["desynpuf_id"], row["feature"]))
  
  df$desynpuf_id <- rownames(df)
  df <- merge(beneficiaries, df, all.x = TRUE)
  df[is.na(df)] <- 0
  cat(paste("nrow(df) = ", nrow(df), "\n", sep = ""))
  write.csv(df, "/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  dbDisconnect(con)
  df
}



