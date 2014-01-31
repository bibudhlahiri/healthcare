library(rpart)
library(e1071)
library(RPostgreSQL)
library(ggplot2)
library(plyr)

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
  n_df_cac <- nrow(df_cac)
  size_each_part <- n_df_cac/2

  majority_set <- subset(df_cac, (change_type =='did_not_increase'))
  n_majority <- nrow(minority_set)
  sample_majority_ind <- sample(1:n_majority, n_minority, replace = TRUE)
  sample_majority <- majority_set[sample_majority_ind, ]
  
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
  df_cac <- create_balanced_sample(df_cac)
  x <- df_cac[,!(names(df_cac) %in% c("desynpuf_id", "change_type", "X"))]
  y <- df_cac[, "change_type"]
  train = sample(1:nrow(x), 0.5*nrow(x))

  test = (-train)
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))
  tab <- table(y[train])
  positive_class_weight <- as.numeric(tab["did_not_increase"]/tab["increased"])
  cat(paste("positive_class_weight = ", positive_class_weight, "\n", sep = ""))
 
  #tune.out = tune.svm(x[train, ], y[train], type = "C-classification", kernel = "linear", 
                      #class.weights = c(positive = positive_class_weight) , 
  #                    cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100))
  tune.out = tune.svm(x[train, ], y[train], kernel = "radial", 
                      class.weights = c(increased = positive_class_weight), 
                      cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), gamma = c(0.125, 0.25, 0.5, 1, 2, 3, 4, 5)
                      )
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


train_balanced_sample_rpart <- function(training_size = 1000)
 {
   set.seed(1)
   df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
   
   train = sample(1:nrow(df_cac), training_size)
   test = (-train)
   df.train <- create_balanced_sample(df_cac[train, ])
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
   
   print(df_cac[train, ][1:5, ])
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

