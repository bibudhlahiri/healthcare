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

  dense_matrix[is.na(dense_matrix)] <- 0
  write.csv(dense_matrix, paste(file_path, "dense_matrix.csv", sep = ""))
  dense_matrix
}

sample_dense_matrix <- function()
{
  set.seed(1)
  file_path <- "/Users/blahiri/healthcare/documents/recommendation_system/reduced_features/"
  dense_matrix <- read.csv(paste(file_path, "dense_matrix.csv", sep = ""))
  dense_matrix <- dense_matrix[, !(colnames(dense_matrix) %in% c("desynpuf_id"))]
  
  columns <- colnames(dense_matrix)
  sampled_rows <- sample(1:nrow(dense_matrix), 5000)
  dense_matrix <- dense_matrix[sampled_rows, ]
  for (column in columns)
  {
    u <- unique(dense_matrix[, column])
    if (length(u) < 2)
    {
      cat(paste("dropping column ", column, "\n", sep = ""))
      dense_matrix <- dense_matrix[, !(colnames(dense_matrix) %in% c(column))]
    }
  }
  write.csv(dense_matrix, paste(file_path, "dense_matrix_sample.csv", sep = ""))
}

construct_bn <- function()
{
  set.seed(1)
  library(bnlearn)
  file_path <- "/Users/blahiri/healthcare/documents/recommendation_system/reduced_features/"
  dense_matrix <- read.csv(paste(file_path, "dense_matrix_sample.csv", sep = ""))
  columns <- colnames(dense_matrix)
  for (column in columns)
  {
    dense_matrix[, column] <- as.factor(dense_matrix[, column])
  }

  cat(paste("Starting hill climbing, time = ", Sys.time(), "\n", sep = ""))
  res = hc(dense_matrix)
  cat(paste("Ended hill climbing, time = ", Sys.time(), "\n", sep = ""))
  fitted = bn.fit(res, dense_matrix)
  cat(paste("Ended fitting, time = ", Sys.time(), "\n", sep = ""))
  #hc_for_optimal_treatment(fitted, columns)

  if (FALSE)
  {
    sampled_columns <- sample(1:length(columns), 5)
    subset <- columns[sampled_columns]
    evidence <- paste("(", subset, " == '1')", sep = "", collapse = " & ")
    evidence <- paste("(chron_chf_2008 == '1') & ", evidence, sep = "")
    #cpquery uses logic sampling by default
    cpquery_expn <- paste("cpquery(fitted, (chron_chf_2009 == '0'), ", evidence, ")", sep = "")
    cond_prob <- eval(parse(text = cpquery_expn))
    cat("Current subset:\n")
    print(subset)
    cat(paste("cond_prob = ", cond_prob, "\n", sep = ""))
    cat(paste("Ended hc_for_optimal_treatment, time = ", Sys.time(), "\n", sep = ""))
  }
  #res
  return(fitted)
}

#Compute conditional probabilities for chronic conditions getting cured, given treatment options.
#These will be used for choosing the initial states during hill climbing for selecting optimal subset
#of treatment options.
#Not working very well since the denom is often small, less than 10 for 5,000 patients

compute_cond_probs_for_chrons_2009 <- function()
{
  file_path <- "/Users/blahiri/healthcare/documents/recommendation_system/reduced_features/"
  dense_matrix <- read.csv(paste(file_path, "dense_matrix_sample.csv", sep = ""))
  columns <- colnames(dense_matrix)
  chronic_conds_2009 <- columns[(substr(columns, 1, 6) == 'chron_') & (substr(columns, nchar(columns)-3, nchar(columns)) == '2009')] 
  procedures <- columns[substr(columns, 1, 5) == 'proc_']
  drugs <- columns[substr(columns, 1, 5) == 'drug_']
  treatment_options <- append(procedures, drugs)
  
  #treatment_options <- treatment_options[1:5]
  #chronic_conds_2009 <- chronic_conds_2009[1:5]

  cond_probs_for_chrons_2009 <-  data.frame(matrix(nrow = length(chronic_conds_2009)*length(treatment_options), ncol = 3)) 
  colnames(cond_probs_for_chrons_2009) <- c("cond", "treatment_option", "cond_prob")
  row_number <- 1

  for (cond in chronic_conds_2009)
  {
    for (treatment_option in treatment_options)
    {
      #Compute P(cond = 0|treatment_option = 1)
      denom <- nrow(subset(dense_matrix, (dense_matrix[, treatment_option] == 1)))
      cond_2008 <- gsub("9", "8", cond)
      numerator <- nrow(subset(dense_matrix, (dense_matrix[,cond_2008] == 1) & (dense_matrix[,cond] == 0) & (dense_matrix[,treatment_option] == 1)))
      cond_prob <- numerator/denom
      cond_probs_for_chrons_2009[row_number, "cond"] <- cond
      cond_probs_for_chrons_2009[row_number, "treatment_option"] <- treatment_option
      cond_probs_for_chrons_2009[row_number, "cond_prob"] <- cond_prob
      row_number <- row_number + 1
      cat(paste("cond = ", cond, ", treatment_option = ", treatment_option, ", numerator = ", numerator, 
                ", denom = ", denom, ", cond_prob = ", cond_prob, "\n", sep = ""))
    }
  }
  cond_probs_for_chrons_2009 <- cond_probs_for_chrons_2009[with(cond_probs_for_chrons_2009, order(cond, -cond_prob)), ]
  write.csv(cond_probs_for_chrons_2009, paste(file_path, "cond_probs_for_chrons_2009.csv", sep = ""))
}

#Challenges currently:
#(1) Good initial state for local search algorithms: try 5 features with highest info gain
#(2) Each CP query is taking about 2 minutes with 5 features
hc_for_optimal_treatment <- function(fitted, columns)
{
 library(FSelector)
 procedures <- columns[substr(columns, 1, 5) == 'proc_']
 drugs <- columns[substr(columns, 1, 5) == 'drug_']
 treatment_options <- append(procedures, drugs)

 evaluator <- function(subset) {
    #Conditional probability query with a random subset of treatment_options being used as the evidence
    evidence <- paste("(", subset, " == '1')", sep = "", collapse = " & ")
    evidence <- paste("(chron_chf_2008 == '1') & ", evidence, sep = "")
    #cpquery uses logic sampling by default
    cpquery_expn <- paste("cpquery(fitted, (chron_chf_2009 == '0'), ", evidence, ")", sep = "")
    cond_prob <- eval(parse(text = cpquery_expn))
    cat("Current subset:\n")
    print(subset)
    cat(paste("cond_prob = ", cond_prob, "\n", sep = ""))
    return(cond_prob)
  }

 subset <- hill.climbing.search(treatment_options, evaluator) 
 cat("The most optimal treatment options are\n")
 print(subset)
}



