library(rpart)
library(e1071)
library(RPostgreSQL)
library(ggplot2)
library(plyr)
library(randomForest)
library(RRF)
library(ada)
library(gbm)
library(party)

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
                       (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip) cost_year1, 
                       b1.bene_sex_ident_cd, 
                       extract(year from age(to_date('2009-01-01', 'YYYY-MM-DD'), b1.bene_birth_dt)) age_year2, 
                       case when (b1.sp_alzhdmta = '2' and b2.sp_alzhdmta = '1') then 1 else 0 end as dev_alzhdmta,
                       case when (b1.sp_chf = '2' and b2.sp_chf = '1') then 1 else 0 end as dev_chf,
                       case when (b1.sp_chrnkidn = '2' and b2.sp_chrnkidn = '1') then 1 else 0 end as dev_chrnkidn,
                       case when (b1.sp_cncr = '2' and b2.sp_cncr = '1') then 1 else 0 end as dev_cncr,
                       case when (b1.sp_copd = '2' and b2.sp_copd = '1') then 1 else 0 end as dev_copd,
		       case when (b1.sp_depressn = '2' and b2.sp_depressn = '1') then 1 else 0 end as dev_depressn,
		       case when (b1.sp_diabetes = '2' and b2.sp_diabetes = '1') then 1 else 0 end as dev_diabetes,
		       case when (b1.sp_ischmcht = '2' and b2.sp_ischmcht = '1') then 1 else 0 end as dev_ischmcht,
		       case when (b1.sp_osteoprs = '2' and b2.sp_osteoprs = '1') then 1 else 0 end as dev_osteoprs,
		       case when (b1.sp_ra_oa = '2' and b2.sp_ra_oa = '1') then 1 else 0 end as dev_ra_oa,
		       case when (b1.sp_strketia = '2' and b2.sp_strketia = '1') then 1 else 0 end as dev_strketia,
                 case when (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip) then 'increased' else 'did_not_increase'
                 end as change_type
                 from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                 where b1.DESYNPUF_ID = b2.DESYNPUF_ID"
   res <- dbSendQuery(con, statement)
   beneficiaries <- fetch(res, n = -1)
   cat(paste("nrow(beneficiaries) = ", nrow(beneficiaries), "\n", sep = ""))
   set.seed(1)
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
}

create_balanced_sample <- function(df_cac)
 {
   minority_set <- subset(df_cac, (change_type =='increased'))
   n_minority <- nrow(minority_set)
   bs_minority_ind <- sample(1:n_minority, n_minority, replace = TRUE)
   bootstrap_minority <- minority_set[bs_minority_ind, ]
   
   majority_set <- subset(df_cac, (change_type =='did_not_increase'))
   n_majority <- nrow(majority_set)
   sample_majority_ind <- sample(1:n_majority, n_minority, replace = TRUE)
   sample_majority <- majority_set[sample_majority_ind, ]

   bal_df_cac <- rbind(bootstrap_minority, sample_majority)
   return(bal_df_cac)
 }

create_bs_by_over_and_undersampling <- function(df_cac)
{
  set.seed(1)
  n_df_cac <- nrow(df_cac)
  size_each_part <- n_df_cac/2

  majority_set <- subset(df_cac, (change_type == 'did_not_increase'))
  n_majority <- nrow(majority_set)
  cat(paste("n_majority = ", n_majority, ", n_df_cac = ", n_df_cac, ", size_each_part = ", size_each_part, "\n", sep = ""))
  sample_majority_ind <- sample(1:n_majority, size_each_part, replace = FALSE)
  sample_majority <- majority_set[sample_majority_ind, ]
    
  minority_set <- subset(df_cac, (change_type == 'increased'))
  n_minority <- nrow(minority_set)
  rep_times <- size_each_part%/%nrow(minority_set)
  oversampled_minority_set <- minority_set
  for (i in 1:(rep_times - 1))
  {
    oversampled_minority_set <- rbind(oversampled_minority_set, minority_set)
  }
  rem_sample_id <- sample(1:n_minority, size_each_part%%nrow(minority_set), replace = FALSE)
  rem_sample <- minority_set[rem_sample_id, ]
  oversampled_minority_set <- rbind(oversampled_minority_set, rem_sample)

  bal_df_cac <- rbind(sample_majority, oversampled_minority_set)
  print(table(bal_df_cac$change_type))
  return(bal_df_cac)
}

test_create_bs_by_over_and_undersampling <- function()
{
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  create_bs_by_over_and_undersampling(df_cac)
}

train_validate_test_rpart <- function()
 {
   set.seed(1)
   df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
   for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

   #df_cac <- create_balanced_sample(df_cac)
   df_cac <- create_bs_by_over_and_undersampling(df_cac)
   x <- df_cac[,!(names(df_cac) %in% c("desynpuf_id", "change_type", "X"))]
   y <- df_cac[, "change_type"]
      train = sample(1:nrow(df_cac), 0.5*nrow(df_cac))
   test = (-train)
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(df_cac) - length(train)), "\n", sep = ""))

   str_formula <- "change_type ~ "
   for (column in colnames(x))
   {
     str_formula <- paste(str_formula, column, " + ", sep = "")
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   
   tune.out = tune.rpart(as.formula(str_formula), data = df_cac[train, ], minsplit = c(5, 10, 15), maxdepth = c(1, 3, 5, 7))
   bestmod <- tune.out$best.model

   ypred = predict(bestmod, x[train, ], type = "class")
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(y[train], ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   ypred <- predict(bestmod, newdata = x[test, ], type = "class")
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y[test], ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

   tune.out
 }


train_validate_test_svm <- function()
{
  set.seed(1)
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  #df_cac <- create_balanced_sample(df_cac)
  df_cac <- create_bs_by_over_and_undersampling(df_cac)
  x <- df_cac[,!(names(df_cac) %in% c("desynpuf_id", "change_type", "X"))]
  y <- df_cac[, "change_type"]
  train = sample(1:nrow(x), 0.5*nrow(x))

  test = (-train)
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))
  tab <- table(y[train])
  positive_class_weight <- as.numeric(tab["did_not_increase"]/tab["increased"])
  cat(paste("positive_class_weight = ", positive_class_weight, "\n", sep = ""))
 
  tune.out = tune.svm(x[train, ], y[train], type = "C-classification", kernel = "linear", 
                      class.weights = c(increased = positive_class_weight), 
                      #cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)
                      cost = c(1)
                      )
  #tune.out = tune.svm(x[train, ], y[train], kernel = "radial", 
  #                    class.weights = c(increased = positive_class_weight), 
  #                    #cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), gamma = c(0.125, 0.25, 0.5, 1, 2, 3, 4, 5)
  #                   cost = c(1), gamma = c(0.25)
  #                    )
  bestmod <- tune.out$best.model

  ypred = predict(bestmod, x[train, ])
  cat("Confusion matrix for training data\n")
  cont_tab <-  table(y[train], ypred, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))


  cat("Confusion matrix for test data\n")
  ypred = predict(bestmod, x[test, ])
  cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

  tune.out
 }


train_validate_test_nn_single_hidden_layer <- function()
{
   set.seed(1)
   df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
   for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

   df_cac <- create_balanced_sample(df_cac)
   
   x <- df_cac[,!(names(df_cac) %in% c("desynpuf_id", "change_type", "X"))]
   y <- df_cac[, "change_type"]
   train = sample(1:nrow(df_cac), 0.5*nrow(df_cac))
   test = (-train)
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(df_cac) - length(train)), "\n", sep = ""))

   str_formula <- "change_type ~ "
   for (column in colnames(x))
   {
     str_formula <- paste(str_formula, column, " + ", sep = "")
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   
   tune.out = tune.nnet(as.formula(str_formula), data = df_cac[train, ], size = seq(2, 12, 2))
   bestmod <- tune.out$best.model

   ypred = predict(bestmod, x[train, ], type = "class")
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(y[train], ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   ypred <- predict(bestmod, newdata = x[test, ], type = "class")
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y[test], ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

   tune.out
 }



train_balanced_sample_rpart <- function(training_size = 1000)
 {
   set.seed(1)
   df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
   for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

   train = sample(1:nrow(df_cac), training_size)
   test = (-train)
   df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
   df.test <- df_cac[test, ]
   
   cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

   str_formula <- "change_type ~ "
   for (column in colnames(df.train))
   {
     if (column != 'desynpuf_id' & column != 'change_type' & column != 'X')
     {
       str_formula <- paste(str_formula, column, " + ", sep = "")
     }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   print(str_formula)
   
   tune.out = tune.rpart(as.formula(str_formula), data = df.train, minsplit = c(5, 10, 15), maxdepth = c(1, 3, 5, 7))
   bestmod <- tune.out$best.model

   ypred = predict(bestmod, df.train[, !(names(df.train) %in% c("change_type"))], type = "class")
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(df.train[, "change_type"], ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   ypred = predict(bestmod, df.test[, !(names(df.test) %in% c("change_type"))], type = "class")
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(df.test[, "change_type"], ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

   tune.out
 }


train_balanced_sample_svm <- function(training_size = 1000)
 {
   set.seed(1)
   df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")

   train = sample(1:nrow(df_cac), training_size)
   test = (-train)
   df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
   df.test <- df_cac[test, ]

   x.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "change_type", "X"))]
   y.train <- df.train[, "change_type"]

   x.test <- df.test[,!(names(df.test) %in% c("desynpuf_id", "change_type", "X"))]
   y.test <- df.test[, "change_type"]
   
   cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

   tune.out = tune.svm(x.train, y.train, type = "C-classification", kernel = "linear", 
                       cost = c(0.1, 1, 5)
                      )

   bestmod <- tune.out$best.model

   ypred = predict(bestmod, x.train)
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(y.train, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   ypred = predict(bestmod, x.test)
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

   tune.out
 }


train_balanced_sample_gbm <- function(training_size = 1000)
{
  library(gbm)
  library(pracma)
  set.seed(1)
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

  train = sample(1:nrow(df_cac), training_size)
  test = (-train)
  df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
  df.test <- df_cac[test, ]

  x.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "change_type", "X"))]
  y.train <- df.train[, "change_type"]
  y.train <- as.numeric(y.train == 'increased')

  x.test <- df.test[,!(names(df.test) %in% c("desynpuf_id", "change_type", "X"))]
  y.test <- df.test[, "change_type"]
  y.test <- as.numeric(y.test == 'increased')
   
  cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

  n_iter <- 5000
  print(colnames(x.train))
  #bernoulli loss function
  boost.cac <- gbm.fit(x.train, y.train, 
                       #distribution = "bernoulli", 
                       distribution = "adaboost",
                       n.trees = n_iter, interaction.depth = 2, verbose = FALSE)

  yhat.boost = predict(boost.cac, newdata = x.train, n.trees = n_iter)
  #Responses are on log odds scale, so take the sigmoid function to get the probabilities of positive class back
  yhat <- sigmoid(yhat.boost)
  yhat <- as.numeric(yhat >= 0.5)

  cat("Confusion matrix for training data\n")
  cont_tab <-  table(y.train, yhat, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
  yhat.boost = predict(boost.cac, newdata = x.test, n.trees = n_iter)
  yhat <- sigmoid(yhat.boost)
  yhat <- as.numeric(yhat >= 0.5)
  cat("Confusion matrix for test data\n")
  cont_tab <-  table(y.test, yhat, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

  #The k-th tree can be viewed by 
  #pretty.gbm.tree(boost.cac, i.tree = k)
  #There were 44 predictors of which 37 had non-zero influence
  #print(boost.cac)
  #The following displays and lists the relative influences.
  #summary(boost.cac)
  boost.cac
} 

classify_rf <- function()
{
  set.seed(1)
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  df_cac <- create_bs_by_over_and_undersampling(df_cac)
  for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }
  x <- df_cac[,!(names(df_cac) %in% c("desynpuf_id", "change_type", "X"))]
  y <- df_cac[, "change_type"]

  #cac.rf <- randomForest(x, y, prox = TRUE, keep.forest = TRUE) 
  rf <- RRF(x, y, flagReg = 0)
  impRF <- rf$importance
  impRF <- impRF[, "MeanDecreaseGini"]
  imp <- impRF/(max(impRF)) #normalize the importance score
  gamma <- 0.5
  coefReg <- (1-gamma) + gamma*imp #weighted average
  cac.rf <- RRF(x, y, coefReg=coefReg, flagReg=1)
  print(cac.rf$feaSet)

  predicted <-  cac.rf$predicted
  cat("Confusion matrix\n")
  cont_tab <-  table(df_cac$change_type, predicted, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))
  #varImpPlot(cac.rf)
  #The following can be used to view the k-th tree in the RF
  #getTree(cac.rf, k, labelVar = TRUE)
  return(cac.rf) 
}


classify_bagging <- function()
{
  set.seed(1)
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  df_cac <- create_bs_by_over_and_undersampling(df_cac)
  for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }
  x <- df_cac[,!(names(df_cac) %in% c("desynpuf_id", "change_type", "X"))]
  y <- df_cac[, "change_type"]

  n_vars <- length(colnames(x))
  cac.rf <- randomForest(x, y, mtry = n_vars) 

  predicted <-  cac.rf$predicted
  cat("Confusion matrix\n")
  cont_tab <-  table(df_cac$change_type, predicted, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))
  varImpPlot(cac.rf)
  #The following can be used to view the k-th tree in the RF
  #getTree(cac.rf, k, labelVar = TRUE)
  return(cac.rf) 
}


train_balanced_sample_rf <- function(training_size = 1000)
{
  set.seed(1)
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

  train = sample(1:nrow(df_cac), training_size)
  test = (-train)
  df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
  df.test <- df_cac[test, ]

  x.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "change_type", "X"))]
  y.train <- df.train[, "change_type"]

  x.test <- df.test[,!(names(df.test) %in% c("desynpuf_id", "change_type", "X"))]
  y.test <- df.test[, "change_type"]
   
  cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

  rf <- RRF(x.train, y.train, flagReg = 0)
  impRF <- rf$importance
  impRF <- impRF[, "MeanDecreaseGini"]
  imp <- impRF/(max(impRF)) #normalize the importance score

  k <- 5
  fold_id <- ceiling(runif(nrow(x.train), 0.000001, k))
  #gammas <- c(0.4, 0.5, 0.6)
  gammas <- seq(0.4, 1, 0.1)
  cv_errors <- data.frame()
  row_number <- 1
   
  for (gamma in gammas)
  {
    for (i in 1:k)
    {  
      train_this_fold <- which(fold_id != i)
      validation <- which(fold_id == i)

      #A penalty coefficient for each feature, rather than a single penalty
      coefReg <- (1-gamma) + gamma*imp #weighted average
      cac.rf <- RRF(x.train[train_this_fold,], y.train[train_this_fold], coefReg = coefReg, flagReg=1)
    
      predicted <-  predict(cac.rf, newdata = x.train[validation, ], type = "response")
      cv_errors[row_number, "gamma"] <- gamma
      cv_errors[row_number, "fold_id"] <- i
      n_wrong_preds <- sum(as.numeric(y.train[validation] != predicted))
      n_tot_preds <- length(y.train[validation])
      error_this_fold <- n_wrong_preds/n_tot_preds
      
      cv_errors[row_number, "validation_error"] <- error_this_fold
      cat(paste("gamma = ", gamma, ", i = ", i, ", error_this_fold = ", error_this_fold, "\n", sep = ""))
      row_number <- row_number + 1
    }
  }

  print(cv_errors)
  cv_errors <- aggregate(x = cv_errors$validation_error, by = list(cv_errors$gamma), FUN = "mean", na.rm = TRUE)
  colnames(cv_errors) <- c("gamma", "cv_error")
  print(cv_errors)
  best_gamma <- cv_errors[which.min(cv_errors$cv_error), "gamma"]
  coefReg <- (1 - best_gamma) + best_gamma*imp
  bestmod <- RRF(x.train, y.train, coefReg = coefReg, flagReg=1)

  yhat = predict(bestmod, newdata = x.train, type = "response")

  cat("Confusion matrix for training data\n")
  cont_tab <-  table(y.train, yhat, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
  yhat = predict(bestmod, newdata = x.test, type = "response")
  cat("Confusion matrix for test data\n")
  cont_tab <-  table(y.test, yhat, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

  cv_errors
} 

train_balanced_sample_bagging <- function(training_size = 1000)
{
  set.seed(1)
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

  train = sample(1:nrow(df_cac), training_size)
  test = (-train)
  df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
  df.test <- df_cac[test, ]

  x.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "change_type", "X"))]
  y.train <- df.train[, "change_type"]

  x.test <- df.test[,!(names(df.test) %in% c("desynpuf_id", "change_type", "X"))]
  y.test <- df.test[, "change_type"]
   
  cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

  n_vars <- length(colnames(x.train))
  cac.bagg <- randomForest(x.train, y.train, mtry = n_vars)

  yhat = predict(cac.bagg, newdata = x.train, type = "response")

  cat("Confusion matrix for training data\n")
  cont_tab <-  table(y.train, yhat, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
  yhat = predict(cac.bagg, newdata = x.test, type = "response")
  cat("Confusion matrix for test data\n")
  cont_tab <-  table(y.test, yhat, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

  cac.bagg
} 


train_balanced_sample_citree <- function(training_size = 1000)
{
  set.seed(1)
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

  train = sample(1:nrow(df_cac), training_size)
  test = (-train)
  df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
  df.test <- df_cac[test, ]

  x.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "change_type", "X"))]
  y.train <- df.train[, "change_type"]

  x.test <- df.test[,!(names(df.test) %in% c("desynpuf_id", "change_type", "X"))]
  y.test <- df.test[, "change_type"]
   
  cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

  str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
  df.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "X"))]
  cac.ct <- ctree(change_type ~ ., data = df.train)

  yhat = predict(cac.ct, newdata = x.train)

  cat("Confusion matrix for training data\n")
  cont_tab <-  table(y.train, yhat, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
  yhat = predict(cac.ct, newdata = x.test)
  cat("Confusion matrix for test data\n")
  cont_tab <-  table(y.test, yhat, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

  cac.ct
} 





pca <- function()
{
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  change_type <- df_cac$change_type
  df_cac <- df_cac[,!(names(df_cac) %in% c("desynpuf_id", "change_type", "X"))]
  pc <- prcomp(df_cac, scale = TRUE)
  
  png("./figures/df_cac_biplot.png",  width = 1200, height = 960, units = "px")
  biplot(pc, col = c("blue", "red"))
  dev.off()
  
  #Print the coefficients in the first principal component in decreasing order. 
  #print(sort(pc$rotation[, "PC1"], decreasing = TRUE))
  proj_along_first_two_pcs <- cbind(data.frame(pc$x[, c("PC1", "PC2")]), change_type = change_type)
  print(table(proj_along_first_two_pcs$change_type))  
  png("./figures/df_cac_first_two_pc.png",  width = 1200, height = 960, units = "px")
  p <- ggplot(proj_along_first_two_pcs, aes(x = PC1, y = PC2)) + aes(shape = change_type) + geom_point(aes(colour = change_type), size = 2) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Projections along first two PCs for positive and negative patients")
  print(p)
  dev.off() 
  pc
}



adaboost <- function(training_size = 4000)
{
  set.seed(1)
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

  train = sample(1:nrow(df_cac), training_size)
  test = (-train)
  df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
  df.test <- df_cac[test, ]

  x.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "change_type", "X"))]
  y.train <- df.train[, "change_type"]

  x.test <- df.test[,!(names(df.test) %in% c("desynpuf_id", "change_type", "X"))]
  y.test <- df.test[, "change_type"]
   
  cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

  stump = rpart.control(cp = -1, maxdepth = 1, minsplit = 0)
  four = rpart.control(cp = -1, maxdepth = 2, minsplit = 0)
  
  #The loss function is the default exponential function. 
  gdis <- ada(x = x.train, y = y.train, iter = 500, type = "discrete"
              , bag.frac = 1.0,
              #nu = 0.5
              , control = stump
              #control = four
             )

   ypred = predict(gdis, x.train, type = "vector")
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(y.train, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   ypred <- predict(gdis, newdata = x.test, type = "vector")
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

  filename <- paste("./figures/perf_discrete_adaboost.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")
  plot(gdis, kappa = FALSE, test = FALSE)
  dev.off()
  return(gdis)
}

train_balanced_sample_nn_single_hidden_layer <- function(training_size = 4000)
 {
   set.seed(1)
   df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
   for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

   train = sample(1:nrow(df_cac), training_size)
   test = (-train)
   df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
   df.test <- df_cac[test, ]

   x.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "change_type", "X"))]
   y.train <- df.train[, "change_type"]

   x.test <- df.test[,!(names(df.test) %in% c("desynpuf_id", "change_type", "X"))]
   y.test <- df.test[, "change_type"]
   
   cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

   str_formula <- "change_type ~ "
   for (column in colnames(df.train))
   {
     if (column != 'desynpuf_id' & column != 'change_type' & column != 'X')
     {
       str_formula <- paste(str_formula, column, " + ", sep = "")
     }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   print(str_formula)
   
   tune.out = tune.nnet(as.formula(str_formula), data = df.train, size = seq(2, 12, 2), decay = 5e-4, maxit = 200)
   bestmod <- tune.out$best.model

   ypred = predict(bestmod, x.train, type = "class")
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(y.train, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   ypred = predict(bestmod, x.test, type = "class")
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

   tune.out
 }


train_balanced_sample_lr <- function(training_size = 4000)
 {
   set.seed(1)
   df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
   for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

   train = sample(1:nrow(df_cac), training_size)
   test = (-train)
   df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
   df.test <- df_cac[test, ]

   x.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "change_type", "X"))]
   y.train <- df.train[, "change_type"]

   x.test <- df.test[,!(names(df.test) %in% c("desynpuf_id", "change_type", "X"))]
   y.test <- df.test[, "change_type"]
   
   cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

   str_formula <- "change_type ~ "
   for (column in colnames(df.train))
   {
     if (column != 'desynpuf_id' & column != 'change_type' & column != 'X')
     {
       str_formula <- paste(str_formula, column, " + ", sep = "")
     }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   cac.logr <- glm(as.formula(str_formula), family = binomial("logit"), data = df.train)

   ypred = predict(cac.logr, x.train, type = "response")
   ypred <-  ifelse(ypred >= 0.5, 'increased', 'did_not_increase')
   #print(contrasts(df.train$change_type))
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(y.train, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   ypred = predict(cac.logr, x.test, type = "response")
   ypred <-  ifelse(ypred >= 0.5, 'increased', 'did_not_increase')
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))
   
   cac.logr
 }


train_balanced_sample_nb <- function(training_size = 4000)
 {
   set.seed(1)
   df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
   for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

   train = sample(1:nrow(df_cac), training_size)
   test = (-train)
   df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
   df.test <- df_cac[test, ]

   x.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "change_type", "X"))]
   y.train <- df.train[, "change_type"]

   x.test <- df.test[,!(names(df.test) %in% c("desynpuf_id", "change_type", "X"))]
   y.test <- df.test[, "change_type"]
   
   cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

   str_formula <- "change_type ~ "
   for (column in colnames(df.train))
   {
     if (column != 'desynpuf_id' & column != 'change_type' & column != 'X')
     {
       str_formula <- paste(str_formula, column, " + ", sep = "")
     }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   cac.nb <- naiveBayes(as.formula(str_formula), data = df.train)

   ypred <- predict(cac.nb, x.train, type = "class")
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(y.train, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   ypred = predict(cac.nb, x.test, type = "class")
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))
   
   cac.nb
 }


train_balanced_sample_knn <- function(training_size = 4000)
 {
   set.seed(1)
   df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
   for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
     }
   }

   train = sample(1:nrow(df_cac), training_size)
   test = (-train)
   df.train <- create_bs_by_over_and_undersampling(df_cac[train, ])
   df.test <- df_cac[test, ]

   x.train <- df.train[,!(names(df.train) %in% c("desynpuf_id", "change_type", "X"))]
   y.train <- df.train[, "change_type"]

   x.test <- df.test[,!(names(df.test) %in% c("desynpuf_id", "change_type", "X"))]
   y.test <- df.test[, "change_type"]
   
   cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

   if (FALSE)
   {
    #tune.out = tune.knn(x.train, y.train, k = 1:10)
    #best_k <- tune.out$best.model$k
   }
    ypred <- knn(train = x.train, test = x.train, cl = y.train, k = 20)
    cat("Confusion matrix for training data\n")
    cont_tab <-  table(y.train, ypred, dnn = list('actual', 'predicted'))
    print(cont_tab)
    FNR <- cont_tab[2,1]/sum(cont_tab[2,])
    FPR <- cont_tab[1,2]/sum(cont_tab[1,])
    training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
    cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   ypred <- knn(train = x.train, test = x.test, cl = y.train, k = 20)
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))

   #tune.out
 }


prepare_data_for_stacking <- function()
{
  set.seed(1)
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  
  df_cac <- create_bs_by_over_and_undersampling(df_cac)

  x <- df_cac[,!(names(df_cac) %in% c("desynpuf_id", "change_type", "X"))]
  y <- df_cac[, "change_type"]

  k <- 5

  pred_by_algos <-  data.frame()
  fold_id <- ceiling(runif(nrow(x), 0.000001, k))
  
  for (i in 1:k)
  { 
    #In iteration i, the ones with fold_id == i form the training set, and the remaining ones form the validation set. 
    #All the learners are learnt from the training set, and applied on the validation set, and the results from the different 
    #classification algorithms for each validation point are kept stored.
    train <- which(fold_id != i)
    validation <- which(fold_id == i)
    cat(paste("i = ", i, ", length(train) = ", length(train), ", length(validation) = ", length(validation), "\n", sep = ""))

    #SVM: Do not need class weights as training data is a balanced sample. Doing it separately from other algorithms as it cannot handle factors.
    cac.svm <- svm(x[train, ], y[train], kernel = "linear", type = "C-classification",
                   cost = 1)
    predicted <-  predict(cac.svm, newdata = x[validation, ])
    pred_by_algos[validation, "svm_class"] <- predicted
   }
   for (column in colnames(df_cac))
   {
     if (column != 'desynpuf_id' & column != 'X' & column != 'cost_year1' & column != 'age_year2')
     {
       df_cac[, column] <- as.factor(df_cac[, column])
       if (column != 'change_type')
       {
         x[, column] <- as.factor(x[, column])
       }
     }
   }
   for (i in 1:k)
   { 
    train <- which(fold_id != i)
    validation <- which(fold_id == i)
    cat(paste("i = ", i, ", length(train) = ", length(train), ", length(validation) = ", length(validation), "\n", sep = ""))
    str_formula <- "change_type ~ "
    for (column in colnames(df_cac))
    {
     if (column != 'desynpuf_id' & column != 'change_type' & column != 'X')
     {
       str_formula <- paste(str_formula, column, " + ", sep = "")
     }
    }
    str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)

    #Decision tree
    cac.rpart <- rpart(as.formula(str_formula), data = df_cac[train, ],  
                            minsplit = 5, maxdepth = 7)
    predicted <-  predict(cac.rpart, newdata = x[validation, ], type = "class")
    pred_by_algos[validation, "rpart_class"] <- predicted

    #GBM
    n_iter <- 5000
    y.train <- as.numeric(y[train] == 'increased')
    boost.cac <- gbm.fit(x[train,], y.train, distribution = "bernoulli", n.trees = n_iter, bag.fraction = 0.5, interaction.depth = 2, verbose = FALSE)
    predicted = predict(boost.cac, newdata = x[validation, ], n.trees = n_iter)
    predicted <- sigmoid(predicted)
    predicted <- as.numeric(predicted >= 0.5)
    predicted <- ifelse(predicted == 1, 'increased', 'did_not_increase')
    pred_by_algos[validation, "gbm_class"] <- predicted

    #Conditional inference tree
    df.train <- df_cac[train, !(names(df_cac) %in% c("desynpuf_id", "X"))]
    cac.ct <- ctree(change_type ~ ., data = df.train)
    predicted = predict(cac.ct, newdata = x[validation, ])
    pred_by_algos[validation, "citree_class"] <- predicted

    #Logistic Regression
    cac.logr <- glm(as.formula(str_formula), family = binomial("logit"), data = df_cac[train, ])
    ypred = predict(cac.logr, x[validation, ], type = "response")
    predicted <-  ifelse(ypred >= 0.5, 'increased', 'did_not_increase')
    pred_by_algos[validation, "lr_class"] <- predicted

    #Naive Bayes
    cac.nb <- naiveBayes(as.formula(str_formula), data = df_cac[train, ])
    predicted <- predict(cac.nb, x[validation, ], type = "class")
    pred_by_algos[validation, "nb_class"] <- predicted
 
    #Neural network
    cac.nnet <- nnet(as.formula(str_formula), data = df.train, size = 12, decay = 5e-4, maxit = 200)
    predicted = predict(cac.nnet, x[validation, ], type = "class")
    pred_by_algos[validation, "nn_class"] <- predicted
  }
  pred_by_algos$true_class <- df_cac[,"change_type"]
  write.csv(pred_by_algos, "/Users/blahiri/healthcare/documents/pred_by_algos.csv")
  pred_by_algos
}



apply_stacking_by_rpart <- function()
{
  pred_by_algos <- read.csv("/Users/blahiri/healthcare/documents/pred_by_algos.csv")
  set.seed(1)
  x <- pred_by_algos[,!(names(pred_by_algos) %in% c("true_class"))]
  y <- pred_by_algos[,"true_class"]

  for (column in colnames(x))
  {
    x[, column] <- as.factor(x[, column])
  }
  y <- as.factor(y)
  
  train = sample(1:nrow(x), 0.5*nrow(x))
  test = (-train)
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))
 
  tune.out = tune.rpart(true_class ~ 
                        svm_class + rpart_class + gbm_class + citree_class + lr_class + nb_class + nn_class,
                        data = pred_by_algos[train, ], minsplit = c(5, 10, 15), maxdepth = c(1, 3, 5, 7))
  bestmod <- tune.out$best.model

  ypred = predict(bestmod, x[train, ], type = "class")
  cat("Confusion matrix for training data\n")
  cont_tab <-  table(y[train], ypred, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))

  ypred = predict(bestmod, x[test, ], type = "class")
  cat("Confusion matrix for test data\n")
  cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
  print(cont_tab)
  FNR <- cont_tab[2,1]/sum(cont_tab[2,])
  FPR <- cont_tab[1,2]/sum(cont_tab[1,])
  test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
  cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))
  #pred_by_algos[test, "predicted"] <- ypred
  
  #false_negatives <- subset(pred_by_algos, (true_class == 'Bot' & predicted == 'User'))
  tune.out
}

