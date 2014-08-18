library(reshape2)
library(plyr)

create_data <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")

  statement <- "select b1.desynpuf_id, b1.sp_alzhdmta as chron_alzhdmta_2008, b1.sp_chf as chron_chf_2008, 
                b1.sp_chrnkidn as chron_chrnkidn_2008, b1.sp_cncr as chron_cncr_2008, b1.sp_copd as chron_copd_2008, b1.sp_depressn as chron_depressn_2008, 
                b1.sp_diabetes as chron_diabetes_2008, b1.sp_ischmcht as chron_ischmcht_2008, b1.sp_osteoprs as chron_osteoprs_2008, b1.sp_ra_oa as chron_ra_oa_2008, 
                b1.sp_strketia as chron_strketia_2008, b2.sp_alzhdmta as chron_alzhdmta_2009, b2.sp_chf as chron_chf_2009, 
                b2.sp_chrnkidn as chron_chrnkidn_2009, b2.sp_cncr as chron_cncr_2009, b2.sp_copd as chron_copd_2009, b2.sp_depressn as chron_depressn_2009, 
                b2.sp_diabetes as chron_diabetes_2009, b2.sp_ischmcht as chron_ischmcht_2009, b2.sp_osteoprs as chron_osteoprs_2009, b2.sp_ra_oa as chron_ra_oa_2009, 
                b2.sp_strketia as chron_strketia_2009
                from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                where b1.desynpuf_id = b2.desynpuf_id
                order by b1.desynpuf_id" 
  res <- dbSendQuery(con, statement)
  beneficiary_summary_2008 <- fetch(res, n = -1)
  write.csv(beneficiary_summary_2008, "/Users/blahiri/healthcare/documents/recommendation_system/reduced_features/beneficiary_summary_2008_2009.csv")

  statement <- "select distinct b.desynpuf_id, ccs_category_code
                from diagnosis_codes dc, dxref dr, transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b, beneficiary_summary_2009 b1
                where (upper(dc.short_desc) = dr.icd9_cm_desc and ltrim(dr.icd9_cm_code, '0') = dc.diagnosis_code)
                and tcdc.dgns_cd = dc.diagnosis_code
                and tcdc.clm_thru_year = '2008'
                and b.desynpuf_id = tcdc.desynpuf_id
                and b.desynpuf_id = b1.desynpuf_id
                order by b.desynpuf_id"
  res <- dbSendQuery(con, statement)
  transformed_claim_diagnosis_codes <- fetch(res, n = -1)
  write.csv(transformed_claim_diagnosis_codes, "/Users/blahiri/healthcare/documents/recommendation_system/reduced_features/transformed_claim_diagnosis_codes.csv") 
              

  #Take only the procedures whose codes do not match with any diagnoses codes
  statement <- "select distinct tcpc.desynpuf_id, pr.ccs_category_code
                from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b, beneficiary_summary_2009 b1, procedure_codes pc, prref pr
                where tcpc.clm_thru_year = '2008'
                and tcpc.prcdr_cd = pc.procedure_code
                and (upper(pc.short_desc) = pr.icd9_cm_desc and ltrim(pr.icd9_cm_code, '0') = pc.procedure_code)
                and b.desynpuf_id = b1.desynpuf_id
                and b.desynpuf_id = tcpc.desynpuf_id
                order by tcpc.desynpuf_id"
  res <- dbSendQuery(con, statement)
  transformed_claim_prcdr_codes <- fetch(res, n = -1)
  write.csv(transformed_claim_prcdr_codes, "/Users/blahiri/healthcare/documents/recommendation_system/reduced_features/transformed_claim_prcdr_codes.csv") 

  #Take only the top 200 substances, too keep the number of features limited
  statement <- "select distinct b1.desynpuf_id, nc1.substancename
                from beneficiary_summary_2008 b1, prescription_drug_events pde1, ndc_codes nc1, beneficiary_summary_2009 b3
                where pde1.hipaa_ndc_labeler_product_code = nc1.hipaa_ndc_labeler_product_code
                and nc1.substancename is not null
                and pde1.desynpuf_id = b1.desynpuf_id
                and to_char(pde1.srvc_dt, 'YYYY') = '2008'
                and b1.desynpuf_id = b3.desynpuf_id
                and nc1.substancename in (select nc2.substancename
                                          from beneficiary_summary_2008 b2, prescription_drug_events pde2, ndc_codes nc2, beneficiary_summary_2009 b4
                                          where pde2.hipaa_ndc_labeler_product_code = nc2.hipaa_ndc_labeler_product_code
                                          and nc2.substancename is not null
                                          and pde2.desynpuf_id = b2.desynpuf_id
                                          and to_char(pde2.srvc_dt, 'YYYY') = '2008'
                                          and b2.desynpuf_id = b4.desynpuf_id
                                          group by nc2.substancename
                                          order by count(distinct b2.desynpuf_id) desc
                                          limit 200)
               order by b1.desynpuf_id"
  res <- dbSendQuery(con, statement)
  prescribed_drugs <- fetch(res, n = -1)
  write.csv(prescribed_drugs, "/Users/blahiri/healthcare/documents/recommendation_system/reduced_features/prescribed_drugs.csv") 

  dbDisconnect(con)
}


build_dense_matrix <- function()
{
  file_path <- "/Users/blahiri/healthcare/documents/recommendation_system/reduced_features/"
  
  beneficiaries <- read.csv(paste(file_path, "beneficiary_summary_2008_2009.csv", sep = ""))
  columns <- colnames(beneficiaries)
  chronic_conds <- columns[columns != 'desynpuf_id']
  beneficiaries[, chronic_conds] <- as.numeric(beneficiaries[, chronic_conds] == '1')
  dense_matrix <- beneficiaries

  tcdc <- read.csv(paste(file_path, "transformed_claim_diagnosis_codes.csv", sep = ""))
  tcdc$ccs_category_code <- paste("diag_", tcdc$ccs_category_code, sep = "")
  tcdc$dummy <- 1
  tcdc_wide <- dcast(tcdc, desynpuf_id ~ ccs_category_code, value.var = "dummy")
  tcdc_wide[is.na(tcdc_wide)] <- 0
  dense_matrix <- merge(x = dense_matrix, y = tcdc_wide, by.x = "desynpuf_id", by.y = "desynpuf_id",  all.x = TRUE)

  tcpc <- read.csv(paste(file_path, "transformed_claim_prcdr_codes.csv", sep = ""))
  tcpc$ccs_category_code <- paste("proc_", tcpc$ccs_category_code, sep = "")
  tcpc$dummy <- 1
  tcpc_wide <- dcast(tcpc, desynpuf_id ~ ccs_category_code, value.var = "dummy")
  tcpc_wide[is.na(tcpc_wide)] <- 0
  dense_matrix <- merge(x = dense_matrix, y = tcpc_wide, by.x = "desynpuf_id", by.y = "desynpuf_id",  all.x = TRUE)

  drugs <- read.csv(paste(file_path, "prescribed_drugs.csv", sep = ""))
  drugs$substancename <- paste("drug_", drugs$substancename, sep = "")
  drugs$dummy <- 1
  drugs_wide <- dcast(drugs, desynpuf_id ~ substancename, value.var = "dummy")
  drugs_wide[is.na(drugs_wide)] <- 0
  dense_matrix <- merge(x = dense_matrix, y = drugs_wide, by.x = "desynpuf_id", by.y = "desynpuf_id",  all.x = TRUE)

  write.csv(dense_matrix, paste(file_path, "dense_matrix.csv", sep = ""))
  dense_matrix
}

