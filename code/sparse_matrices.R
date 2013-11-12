library(ggplot2)
library(plyr)
library(reshape2)
library(RPostgreSQL)
require(scales)
library(gridExtra)
require(hash)
library(fork)



 feature_arrangement <- function()
 {
   con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
   statement <- paste("select b1.DESYNPUF_ID, tcdc.dgns_cd, 
                       case when b2.MEDREIMB_IP > b1.MEDREIMB_IP then 1 else 0
                       end as change_type
                       from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                       where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                       and b1.DESYNPUF_ID = tcdc.DESYNPUF_ID
                       and to_char(tcdc.clm_thru_dt, 'YYYY') = '2008'
                       and tcdc.dgns_cd in (select tcdc1.dgns_cd
                                            from transformed_claim_diagnosis_codes tcdc1
                                            where to_char(tcdc1.clm_thru_dt, 'YYYY') = '2008'
                                            group by tcdc1.dgns_cd
                                            order by count(distinct tcdc1.DESYNPUF_ID) desc
                                            limit 20)", sep = "")
  res <- dbSendQuery(con, statement);
  df <- fetch(res, n = -1) 
  all_dgns_codes <- unique(df$dgns_cd)
  n_dgns_codes <- length(all_dgns_codes)
  n_beneficiaries <- length(unique(df$DESYNPUF_ID))
  sparse_df <- data.frame();
  #colnames(sparse_df) <- all_dgns_codes 
  n_df <- nrow(df)
  row_index <- 1
  patient_id <- df[1, "desynpuf_id"]
  for (i in 1:n_df)
  {
    if (df[i, "desynpuf_id"] != patient_id)
    {
       row_index <- row_index + 1
       patient_id <- df[i, "desynpuf_id"]
    }
    sparse_df[row_index, "patient_id"] <- df[i, "desynpuf_id"]
    sparse_df[row_index, "change_type"] <- df[i, "change_type"]
    sparse_df[row_index, df[i, "dgns_cd"]] <- 1   
    if (i %% 2000 == 0)
    {
      cat(paste("i = ", i, ", ", Sys.time(), "\n"))
    }
  }
  sparse_df[is.na(sparse_df)] <- 0
  print(sparse_df)
  dbDisconnect(con)
}

#Keeping the hashes as global variables as they need to be accessed from both lookup_bene_num() 
#and create_sparse_feature_matrix()
hash_bene <- hash()
hash_dgns <- hash()
reverse_hash_dgns <- hash()

hash_features <- hash()
reverse_hash_features <- hash()


lookup_bene_num <- function(desynpuf_id)
{
  return(hash_bene[[desynpuf_id]])
}

lookup_dgns_num <- function(dgns_cd)
{
  return(hash_dgns[[dgns_cd]])
}

lookup_feature_num <- function(feature)
{
  return(hash_features[[feature]])
}


lookup_dgns_code <- function(dgns_num)
{
  return(reverse_hash_dgns[[dgns_num]])
}

lookup_feature_code <- function(feature_num)
{
  return(reverse_hash_features[[feature_num]])
}




create_sparse_feature_matrix_old <- function()
 {
   library(Matrix)
   library(glmnet)
   con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
   #Use limit 500 in internal query for quick debugging
   statement <- paste("select distinct a.DESYNPUF_ID, a.dgns_cd, a.change_type
                       from (select b1.DESYNPUF_ID, tcdc.dgns_cd, 
                             case when b2.MEDREIMB_IP > b1.MEDREIMB_IP then 1 else 0
                             end as change_type
                             from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                             where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                             and b1.DESYNPUF_ID = tcdc.DESYNPUF_ID
                             and to_char(tcdc.clm_thru_dt, 'YYYY') = '2008') a 
                       order by a.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement);
  df <- fetch(res, n = -1)
  cat(paste("nrow(df) = ", nrow(df), ", ncol(df) = ", ncol(df), "\n", sep = ""))

   all_beneficiaries <- unique(df$desynpuf_id)
   all_dgns_codes <- unique(df$dgns_cd)
   n_dgns_codes <- length(all_dgns_codes)
   n_beneficiaries <- length(all_beneficiaries)
   cat(paste("n_beneficiaries = ", n_beneficiaries, ", n_dgns_codes = ", n_dgns_codes, "\n", sep = ""))

   hash_bene <<- hash(all_beneficiaries, 1:n_beneficiaries)
   hash_dgns <<- hash(all_dgns_codes, 1:n_dgns_codes)
   reverse_hash_dgns <<- hash(1:n_dgns_codes, all_dgns_codes)

   df$bene_num <- apply(df, 1, function(row)lookup_bene_num(row["desynpuf_id"]))
   df$dgns_num <- apply(df, 1, function(row)lookup_dgns_num(row["dgns_cd"]))
   sparse_mat <- sparseMatrix(i = df$bene_num, j = df$dgns_num, x = 1, dimnames=list(1:n_beneficiaries,1:n_dgns_codes))
   cat(paste("nrow(sparse_mat) = ", nrow(sparse_mat), ", ncol(sparse_mat) = ", ncol(sparse_mat), "\n", sep = ""))
  

  #Create the response vector
  statement <- paste("select distinct a.DESYNPUF_ID, a.change_type
                       from (select b1.DESYNPUF_ID, tcdc.dgns_cd, 
                             case when b2.MEDREIMB_IP > b1.MEDREIMB_IP then 1 else 0
                             end as change_type
                             from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                             where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                             and b1.DESYNPUF_ID = tcdc.DESYNPUF_ID
                             and to_char(tcdc.clm_thru_dt, 'YYYY') = '2008') a 
                       order by a.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement)
  resp <- fetch(res, n = -1)
  cat(paste("nrow(resp) = ", nrow(resp), ", ncol(resp) = ", ncol(resp), "\n", sep = ""))
  dbDisconnect(con)
  
  if (FALSE)
  {
   fit <- glmnet(sparse_mat, resp$change_type, family="binomial")
   #With limit 500, at lambda = 0.037610, 43 covariates have been used, and %Dev is 0.80080. 
   predicted <- predict(fit, newx = sparse_mat, s = 0.037610, type = "nonzero")
   imp_predictors <- apply(predicted, 1, function(row)lookup_dgns_code(as.character(row["X1"])))
   return(imp_predictors)
  }
  
   cvob1 = cv.glmnet(sparse_mat, resp$change_type, family="binomial", type.measure = "class", nfolds = 10)
   index_min_xval_error <- which.min(cvob1$cvm)
   cat(paste("Min xval error = ", min(cvob1$cvm), " occurs at lambda = ", cvob1$lambda[index_min_xval_error], " for ", 
              cvob1$nzero[index_min_xval_error], " covariates\n", sep = ""))
   #plot(cvob1)
   dgns_numbers <- unlist(predict(cvob1, newx = sparse_mat, s = "lambda.min", type = "nonzero"))
   imp_predictors <- data.frame(dgns_numbers)
   #What are the diagnoses codes that have nonzero coefficients for the lambda which minimizes the cross-validation error?
   imp_predictors$dgns_codes <- apply(imp_predictors, 1, function(row)lookup_dgns_code(as.character(row["dgns_numbers"])))
   return(cvob1)
}


 get_n_dgns_year1 <- function(con, interesting_patients)
 {
  cat(paste("PID of child process = ", Sys.getpid(), "\n", sep = ""))
  statement <- paste("select b2.DESYNPUF_ID, (select count(distinct dgns_cd) 
                        from transformed_claim_diagnosis_codes tcdc
                        where tcdc.DESYNPUF_ID = b2.DESYNPUF_ID
                       and to_char(tcdc.clm_thru_dt, 'YYYY') = '2008') as n_dgns_year1
                       from beneficiary_summary_2009 b2
                       order by b2.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement)
  df_n <- fetch(res, n = -1)
  df_n1 <- merge(x = interesting_patients, y = df_n, all.x = TRUE, by.x = "desynpuf_id", by.y = "desynpuf_id")
  rm(df_n)  
  df_n1$bene_num <- apply(df_n1, 1, function(row)lookup_bene_num(row["desynpuf_id"]))
  df_n1 <- df_n1[, c("bene_num", "n_dgns_year1")]
  mat_n1 <- data.matrix(df_n1) 
  rm(df_n1)
  print(mat_n1[1:10, ])
  sparse_mat_n1 <- Matrix(mat_n1, sparse = TRUE)
  rm(mat_n1)
  cat(paste("nrow(sparse_mat_n1) = ", nrow(sparse_mat_n1), ", ncol(sparse_mat_n1) = ", ncol(sparse_mat_n1), "\n", sep = ""))
  return(sparse_mat_n1)
 }


create_sparse_feature_matrix <- function()
 {
   library(Matrix)
   library(glmnet)
  
   con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
   
  statement <- paste("select b2.desynpuf_id, 'd_' || tcdc.dgns_cd as feature
                      from beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                      where b2.DESYNPUF_ID = tcdc.DESYNPUF_ID
                      and tcdc.clm_thru_year = '2008'
                      order by b2.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement)
  d1 <- fetch(res, n = -1)
  cat(paste("nrow(d1) = ", nrow(d1), ", time = ", Sys.time(), "\n", sep = ""))
  diagnoses_codes <- unique(d1$feature)
  
  statement <- paste("select b2.desynpuf_id, 's_' || nc.substancename as feature
                      from beneficiary_summary_2009 b2, prescription_drug_events pde, ndc_codes nc
                      where (b2.desynpuf_id = pde.desynpuf_id and to_char(pde.srvc_dt, 'YYYY') = '2008')
                      and nc.substancename is not null
                      and pde.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code", sep = "")
  res <- dbSendQuery(con, statement)
  d2 <- fetch(res, n = -1)
  cat(paste("nrow(d2) = ", nrow(d2), ", time = ", Sys.time(), "\n", sep = ""))
  substance_names <- unique(d2$feature)

  features <- c(diagnoses_codes, substance_names)
  n_features <- length(features)
  cat(paste("n_features = ", n_features, "\n", sep = ""))

  hash_features <<- hash(features, 1:n_features)
  reverse_hash_features <<- hash(1:n_features, features)
  
  d <- rbind(d1, d2)
  rm(d1, d2)
  d <- d[order(d[,"desynpuf_id"]),]
  interesting_patients <- unique(d$desynpuf_id)
  n_beneficiaries <- length(interesting_patients)
  cat(paste("n_beneficiaries = ", n_beneficiaries, "\n", sep = ""))
  hash_bene <<- hash(interesting_patients, 1:n_beneficiaries)

  statement <- paste("select b1.DESYNPUF_ID,  
                             case when (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip) then 1 else 0
                             --case when b2.total_expense > b1.total_expense then 1 else 0
                             end as change_type
                      from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                      where b1.DESYNPUF_ID = b2.DESYNPUF_ID 
                      order by b1.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement)
  all_patients <- fetch(res, n = -1)
  dbDisconnect(con) 

  interesting_patients <- data.frame(desynpuf_id = interesting_patients)
  interesting_patients <- merge(x = interesting_patients, y = all_patients, all.x = TRUE, by.x = "desynpuf_id", by.y = "desynpuf_id")
  rm(all_patients)
  cat(paste("nrow(interesting_patients) = ", nrow(interesting_patients), "\n", sep = ""))

  d$bene_num <- apply(d, 1, function(row)lookup_bene_num(row["desynpuf_id"]))
  d$feature_num <- apply(d, 1, function(row)lookup_feature_num(row["feature"]))

  sparse_mat <- sparseMatrix(i = d$bene_num, j = d$feature_num, x = 1, dimnames = list(1:n_beneficiaries,1:n_features))
  trg_model <- train_validate_test(sparse_mat, interesting_patients$change_type)
  return(trg_model)

  if (FALSE)
  {
    cat(paste("Before cbind, nrow(sparse_mat) = ", nrow(sparse_mat), ", ncol(sparse_mat) = ", ncol(sparse_mat), "\n", sep = ""))
    sparse_mat_n1 <- fork(get_n_dgns_year1(con, interesting_patients))
    gc()
    cat(paste("PID of parent process = ", Sys.getpid(), "\n", sep = ""))
    sparse_mat <- cbind(sparse_mat, sparse_mat_n1[, 2])
    rm(sparse_mat_n1)
    cat(paste("After cbind, nrow(sparse_mat) = ", nrow(sparse_mat), ", ncol(sparse_mat) = ", ncol(sparse_mat), "\n", sep = ""))
  }
  

  if (FALSE)
  {
   cvob1 = cv.glmnet(sparse_mat, interesting_patients$change_type, family="binomial", 
                    #alpha = 0, 
                    type.measure = "class", nfolds = 10)
   index_min_xval_error <- which.min(cvob1$cvm)
   cat(paste("index_min_xval_error = ", index_min_xval_error, ", min xval error = ", min(cvob1$cvm), " occurs at lambda = ", cvob1$lambda[index_min_xval_error], 
            " for ", cvob1$nzero[index_min_xval_error], " covariates\n", sep = ""))
   errors <- data.frame(cv_error = cvob1$cvm, nonzero_covariates = cvob1$nzero)
   print(errors)
  }

  if (FALSE)
  {
   #Take the sequence of lambda values used in CV, build models with them and check the training errors of those models.
   lambdas <- cvob1$lambda
   nlambda = length(lambdas) 
   trg_model <- glmnet(sparse_mat, interesting_patients$change_type, family="binomial", 
                       #alpha = 0, 
                       nlambda = nlambda, lambda = lambdas)

   #predicted is a 86157 x 100 matrix. Each row gives the predicted values for a patient for different values of lambda, one per column.
   predicted <- predict(trg_model, newx = sparse_mat, s = lambdas, type = "class")
   errors <- data.frame()
   for (i in 1:nlambda)
   {
     correct_predictions <- xor(interesting_patients$change_type, as.numeric(predicted[, i]))
     n_correct_predictions <- sum(correct_predictions)
     errors[i, "lambda"] <- lambdas[i]
     errors[i, "trg_error"] <- n_correct_predictions/length(interesting_patients$change_type)
     errors[i, "cv_error"] <- cvob1$cvm[i]
     errors[i, "nonzero_covariates"] <- cvob1$nzero[i]
   }
   print(errors)
  }

  if (FALSE)
  {
   #Given ncovariates, a number of covariates that the user wants in the final model, find the model from the K models, 
   #where the number of non-zero covariates was closest to ncovariates. Next, get the non-zero covariates from that model,
   #and perform reverse lookup in hashtables to retrieve them.

   ncovariates <- 100
   errors$diff <- abs(errors$nonzero_covariates - ncovariates)
   index_min_diff <- which.min(errors$diff)
   cat(paste("Closest value is ", errors[index_min_diff, "nonzero_covariates"], " for lambda = ", lambdas[index_min_diff], "\n", sep = ""))
   feature_numbers <- unlist(predict(cvob1, newx = sparse_mat, s = lambdas[index_min_diff], type = "nonzero"))
   imp_predictors <- data.frame(feature_numbers)
   imp_predictors$feature_codes <- apply(imp_predictors, 1, function(row)lookup_feature_code(as.character(row["feature_numbers"])))
   dbDisconnect(con)
   return(imp_predictors)
  }

  if (FALSE)
  {
   #How to get the most important ncovariates predictors?
   #glmnet (not CV) gives the beta vector for each lambda. So, take the lambda for which cross-validation error is minimum, apply glmnet with that lambda, and
   #get beta for that lambda value
   #trg_model <- glmnet(sparse_mat, interesting_patients$change_type, family="binomial", lambda = cvob1$lambda)


   #Work with trg_model$beta
   colname <- paste('s', (index_min_xval_error - 1), sep = "")
   coeffs <- trg_model$beta[, colname]
   positive_coeffs <- coeffs[which(coeffs > 0)]
   positive_coeffs <- sort(positive_coeffs, decreasing = TRUE)
   imp_predictors <- data.frame(coeffs = positive_coeffs)
   imp_predictors$feature_nums_for_pos_coeffs <- as.numeric(rownames(imp_predictors)) 
   #Reverse lookup hash table to get the names of the covariates with positive coefficients
   imp_predictors$feature_codes <- apply(imp_predictors, 1, function(row)lookup_feature_code(as.character(row["feature_nums_for_pos_coeffs"])))
   write.csv(imp_predictors, "../documents/positive_coeffs.csv")
   decode_imp_predictors("positive_coeffs.csv", "positive_coeffs_decoded.csv")
  
   negative_coeffs <- coeffs[which(coeffs < 0)]
   negative_coeffs <- sort(negative_coeffs)
   imp_predictors <- data.frame(coeffs = negative_coeffs)
   imp_predictors$feature_nums_for_neg_coeffs <- as.numeric(rownames(imp_predictors)) 
   #Reverse lookup hash table to get the names of the covariates with negative coefficients
   imp_predictors$feature_codes <- apply(imp_predictors, 1, function(row)lookup_feature_code(as.character(row["feature_nums_for_neg_coeffs"])))
   write.csv(imp_predictors, "../documents/negative_coeffs.csv")
   decode_imp_predictors("negative_coeffs.csv", "negative_coeffs_decoded.csv")
  }
}

analyze_glm_errors <- function()
{
  errors <- read.csv("../documents/errors_glmnet.csv") 

  filename <- paste("./figures/lambda_for_feature_selection.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")

  p <- ggplot(errors, aes(x = lambda, y = nonzero_covariates)) + geom_line(size=2) + 
        labs(x = "Lambda") + ylab("Number of features selected") + 
         theme(axis.text = element_text(colour = 'blue', size = 10)) +
         theme(axis.title = element_text(colour = 'red', size = 12))
  print(p)
  dev.off()
     
  filename <- paste("./figures/errors_glmnet.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")

  errors <- errors[, c("trg_error", "cv_error", "test_error", "nonzero_covariates")]
  df_long <- melt(errors, id = "nonzero_covariates")
  p <- ggplot(df_long, aes(x = nonzero_covariates, y = value, colour = variable)) + geom_line(size=2) + 
        labs(x = "Number of features selected") + ylab("Error") + 
         theme(axis.text = element_text(colour = 'blue', size = 10)) +
         theme(axis.title = element_text(colour = 'red', size = 12))
  print(p)
  dev.off()

  filename <- paste("./figures/errors_glmnet_zoomed.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")

  df_long <- subset(df_long, (nonzero_covariates <= 150)) 
  p <- ggplot(df_long, aes(x = nonzero_covariates, y = value, colour = variable)) + geom_line(size=2) + 
        labs(x = "Number of features selected") + ylab("Error") + 
         theme(axis.text = element_text(colour = 'blue', size = 10)) +
         theme(axis.title = element_text(colour = 'red', size = 12))
  print(p)
  dev.off()

  
}

decode_imp_predictors <- function(ip_file, op_file)
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  imp_predictors <- read.csv(paste("../documents/", ip_file, sep = ""))
  imp_predictors$feature_codes <- substr(imp_predictors$feature_codes, 3, nchar(as.character(imp_predictors$feature_codes)))
  statement <- paste("select distinct diagnosis_code as feature_codes, long_desc
                      from diagnosis_codes", sep = "")
  res <- dbSendQuery(con, statement)
  all_feature_codes <- fetch(res, n = -1)
  imp_predictors <- merge(x = imp_predictors, y = all_feature_codes, all.x = TRUE)
  imp_predictors <- imp_predictors[, c("feature_codes", "coeffs", "long_desc")]
  imp_predictors$abs_coeffs <- abs(imp_predictors$coeffs)
  imp_predictors <- imp_predictors[order(-imp_predictors[,"abs_coeffs"]),]  
  imp_predictors$long_desc <- ifelse(is.na(imp_predictors$long_desc), imp_predictors$feature_codes, imp_predictors$long_desc)
  dbDisconnect(con)
  write.csv(imp_predictors, paste("../documents/", op_file, sep = ""))
}

#The following method is a revised version, based on Hastie and Tibshirani. It makes the training and test set separate 
#from the very beginning and performs CV only on the training set.
train_validate_test <- function(x, y)
{
  grid = 10^seq(10, -2, length = 100)

  set.seed(1)
  train = sample(1:nrow(x), nrow(x)/2)
  test = (-train)
  y.test = y[test]
  
  lasso.mod = glmnet(x[train, ], y[train], alpha = 1, lambda = grid, family="binomial")

  cv.out = cv.glmnet(x[train, ], y[train], alpha = 1, family="binomial", type.measure = "class")
  bestlam = cv.out$lambda.min
  
  #Note: The model built on the training dataset is applied on the test set, using the value of lambda found by CV
  lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test,], type = "class")
  correct_predictions <- xor(y.test, as.numeric(lasso.pred))
  n_correct_predictions <- sum(correct_predictions)
  test_error <- n_correct_predictions/length(y.test)
  cat(paste("test_error for best lambda = ", test_error, "\n", sep = ""))

  if (FALSE)
  {
   trg_model <- glmnet(x[train, ], y[train], family="binomial", lambda = cv.out$lambda)
   index_min_xval_error <- which.min(cv.out$cvm)
   #Work with trg_model$beta
   colname <- paste('s', (index_min_xval_error - 1), sep = "")
   cat(paste("index_min_xval_error = ", index_min_xval_error, ", colname = ", colname, "\n", sep = ""))
   coeffs <- trg_model$beta[, colname]
   print(coeffs)
   positive_coeffs <- coeffs[which(coeffs > 0)]
   positive_coeffs <- sort(positive_coeffs, decreasing = TRUE)
   imp_predictors <- data.frame(coeffs = positive_coeffs)
   imp_predictors$feature_nums_for_pos_coeffs <- as.numeric(rownames(imp_predictors)) 
   #Reverse lookup hash table to get the names of the covariates with positive coefficients
   imp_predictors$feature_codes <- apply(imp_predictors, 1, function(row)lookup_feature_code(as.character(row["feature_nums_for_pos_coeffs"])))
   write.csv(imp_predictors, "../documents/positive_coeffs.csv")
   decode_imp_predictors("positive_coeffs.csv", "positive_coeffs_decoded.csv")
  
   negative_coeffs <- coeffs[which(coeffs < 0)]
   negative_coeffs <- sort(negative_coeffs)
   imp_predictors <- data.frame(coeffs = negative_coeffs)
   imp_predictors$feature_nums_for_neg_coeffs <- as.numeric(rownames(imp_predictors)) 
   #Reverse lookup hash table to get the names of the covariates with negative coefficients
   imp_predictors$feature_codes <- apply(imp_predictors, 1, function(row)lookup_feature_code(as.character(row["feature_nums_for_neg_coeffs"])))
   write.csv(imp_predictors, "../documents/negative_coeffs.csv")
   decode_imp_predictors("negative_coeffs.csv", "negative_coeffs_decoded.csv")
  }

  #Get a table of training error, CV error and test error
  lambdas <- cv.out$lambda
  nlambda = length(lambdas) 
  trg_model <- glmnet(x[train, ], y[train], family = "binomial", lambda = lambdas)

  #predicted is a 86157 x 100 matrix. Each row gives the predicted values for a patient for different values of lambda, one per column.
  prediction_on_trg <- predict(trg_model, newx = x[train, ], s = lambdas, type = "class")
  prediction_on_test <- predict(trg_model, newx = x[test, ], s = lambdas, type = "class")

  errors <- data.frame()
  for (i in 1:nlambda)
   {
     wrong_predictions_on_trg <- xor(y[train], as.numeric(prediction_on_trg[, i]))
     n_wrong_predictions_on_trg <- sum(wrong_predictions_on_trg)
     errors[i, "lambda"] <- lambdas[i]
     errors[i, "trg_error"] <- n_wrong_predictions_on_trg/length(y[train])

     wrong_predictions_on_test <- xor(y.test, as.numeric(prediction_on_test[, i]))
     n_wrong_predictions_on_test <- sum(wrong_predictions_on_test)
     errors[i, "test_error"] <- n_wrong_predictions_on_test/length(y.test)

     errors[i, "cv_error"] <- cv.out$cvm[i]
     errors[i, "nonzero_covariates"] <- cv.out$nzero[i]
   }
  print(errors)
  write.csv(errors, "../documents/errors_glmnet.csv")
}


get_n_among_increased <- function(con, feature_code)
{
  cat(paste("feature_code = ", feature_code, "\n", sep = ""))
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
                        and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)
                        and (b2.desynpuf_id = pde.desynpuf_id and to_char(pde.srvc_dt, 'YYYY') = '2008')
                        and nc.substancename = '", feature_code, "' 
                        and pde.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code", sep = "")
    return(as.numeric(dbGetQuery(con, statement)))
  }  
}

get_n_among_did_not_increase <- function(con, feature_code)
{
  #cat(paste("feature_code = ", feature_code, "\n", sep = ""))
  prefix <- substr(feature_code, 1, 1)
  feature_code <- substr(feature_code, 3, nchar(as.character(feature_code)))
  if (prefix == 'd')
  {
    statement <- paste("select count(distinct b2.DESYNPUF_ID)
                        from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, transformed_claim_diagnosis_codes tcdc
                        where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                        and tcdc.DESYNPUF_ID = b2.DESYNPUF_ID
                        and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) <= (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)
                        and to_char(tcdc.clm_thru_dt, 'YYYY') = '2008'
                        and tcdc.dgns_cd = '", feature_code, "'", sep = "")
    return(as.numeric(dbGetQuery(con, statement)))
  }
  if (prefix == 's')
  {
    statement <- paste("select count(distinct b2.DESYNPUF_ID)
                        from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, prescription_drug_events pde, ndc_codes nc
                        where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                        and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) <= (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)
                        and (b2.desynpuf_id = pde.desynpuf_id and to_char(pde.srvc_dt, 'YYYY') = '2008')
                        and nc.substancename = '", feature_code, "' 
                        and pde.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code", sep = "")
    return(as.numeric(dbGetQuery(con, statement)))
  }  
}



explain_coefficients <- function()
{  
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- paste("select count(distinct b2.DESYNPUF_ID) as n_increased
                      from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                      where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                      and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) > (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)", sep = "")
  n_increased <- as.numeric(dbGetQuery(con, statement))

  statement <- paste("select count(distinct b2.DESYNPUF_ID) as n_did_not_increase
                       from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                       where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                       and (b2.medreimb_ip + b2.benres_ip + b2.pppymt_ip) <= (b1.medreimb_ip + b1.benres_ip + b1.pppymt_ip)", sep = "")
  n_did_not_increase <- as.numeric(dbGetQuery(con, statement))
  
  positive_coeffs <- read.csv("/Users/blahiri/healthcare/documents/inpatient_cost/positive_coeffs.csv")
  #positive_coeffs <- positive_coeffs[1:5, ]
  positive_coeffs$n_among_increased <- apply(positive_coeffs, 1, function(row)get_n_among_increased(con, as.character(row["feature_codes"])))
  positive_coeffs$f_among_increased <- positive_coeffs$n_among_increased/n_increased

  positive_coeffs$n_among_did_not_increase <- apply(positive_coeffs, 1, function(row)get_n_among_did_not_increase(con, as.character(row["feature_codes"])))
  positive_coeffs$f_among_did_not_increase <- positive_coeffs$n_among_did_not_increase/n_did_not_increase

  print(positive_coeffs)
  write.csv(positive_coeffs, "/Users/blahiri/healthcare/documents/inpatient_cost/positive_coeffs_explained.csv")

  negative_coeffs <- read.csv("/Users/blahiri/healthcare/documents/inpatient_cost/negative_coeffs.csv")
  #negative_coeffs <- negative_coeffs[1:5, ]
  negative_coeffs$n_among_increased <- apply(negative_coeffs, 1, function(row)get_n_among_increased(con, as.character(row["feature_codes"])))
  negative_coeffs$f_among_increased <- negative_coeffs$n_among_increased/n_increased

  negative_coeffs$n_among_did_not_increase <- apply(negative_coeffs, 1, function(row)get_n_among_did_not_increase(con, as.character(row["feature_codes"])))
  negative_coeffs$f_among_did_not_increase <- negative_coeffs$n_among_did_not_increase/n_did_not_increase

  print(negative_coeffs)
  write.csv(negative_coeffs, "/Users/blahiri/healthcare/documents/inpatient_cost/negative_coeffs_explained.csv")

  dbDisconnect(con)
} 

visualization_for_report <- function()
{
  positive_coeffs_explained <- read.csv("/Users/blahiri/healthcare/documents/inpatient_cost/positive_coeffs_explained.csv")
  positive_coeffs_explained <- positive_coeffs_explained[, c("feature_codes", "f_among_increased", "f_among_did_not_increase")]
  colnames(positive_coeffs_explained) <- c("feature_codes", "increased", "did_not_increase")

  path <- "/Users/blahiri/healthcare/documents/Healthcare_expenditure/v2/visualizations/positive_coeffs_explained/"
  n_positive_coeffs_explained <- nrow(positive_coeffs_explained)

  for (i in 1:n_positive_coeffs_explained)
  {
   feature_code <- positive_coeffs_explained[i, "feature_codes"]
   cat(paste("feature_code = ", feature_code, "\n", sep = ""))
   df <- positive_coeffs_explained[i, ]
   molten_data <- melt(df, id = c("feature_codes")) 
   print(molten_data)
   filename  <- paste(path, feature_code, ".png", sep = "")
   png(file = filename, width = 800, height = 600)
   p <- ggplot(molten_data, aes(x = variable, y = value)) + geom_bar(width = 0.5, fill = "#FF6666", stat="identity") + 
           labs(x = "Whether cost increased") +  
           labs(y = "Fraction with presence of covariate") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
   print(p)
   dev.off() 
  }

  negative_coeffs_explained <- read.csv("/Users/blahiri/healthcare/documents/inpatient_cost/negative_coeffs_explained.csv")
  negative_coeffs_explained <- negative_coeffs_explained[, c("feature_codes", "f_among_increased", "f_among_did_not_increase")]
  colnames(negative_coeffs_explained) <- c("feature_codes", "increased", "did_not_increase")

  path <- "/Users/blahiri/healthcare/documents/Healthcare_expenditure/v2/visualizations/negative_coeffs_explained/"
  n_negative_coeffs_explained <- nrow(negative_coeffs_explained)

  for (i in 1:n_negative_coeffs_explained)
  {
   feature_code <- negative_coeffs_explained[i, "feature_codes"]
   cat(paste("feature_code = ", feature_code, "\n", sep = ""))
   df <- negative_coeffs_explained[i, ]
   molten_data <- melt(df, id = c("feature_codes")) 
   print(molten_data)
   filename  <- paste(path, feature_code, ".png", sep = "")
   png(file = filename, width = 800, height = 600)
   p <- ggplot(molten_data, aes(x = variable, y = value)) + geom_bar(width = 0.5, fill = "#FF6666", stat="identity") + 
           labs(x = "Whether cost increased") +  
           labs(y = "Fraction with presence of covariate") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
   print(p)
   dev.off() 
  }
} 


get_n_had_condition <- function(con, feature_code)
{
  cat(paste("feature_code = ", feature_code, "\n", sep = ""))
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
  cat(paste("feature_code = ", feature_code, "\n", sep = ""))
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
  cat(paste("feature_code = ", feature_code, "\n", sep = ""))
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


derive_conditional_probabilities <- function()
{  
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  
  positive_coeffs <- read.csv("/Users/blahiri/healthcare/documents/inpatient_cost/positive_coeffs.csv")
  #positive_coeffs <- positive_coeffs[1:5, ]
  positive_coeffs$n_had_condition <- apply(positive_coeffs, 1, function(row)get_n_had_condition(con, as.character(row["feature_codes"])))
  positive_coeffs$n_did_not_have_condition <- 114538 - positive_coeffs$n_had_condition
  positive_coeffs$n_had_condition_and_cost_increased <- apply(positive_coeffs, 1, function(row)get_n_had_condition_and_cost_increased(con, as.character(row["feature_codes"])))
  positive_coeffs$n_did_not_have_condition_but_cost_increased <- 
                  apply(positive_coeffs, 1, function(row)get_n_did_not_have_condition_but_cost_increased(con, as.character(row["feature_codes"])))
  positive_coeffs$f_had_condition_and_cost_increased <- positive_coeffs$n_had_condition_and_cost_increased/positive_coeffs$n_had_condition
  positive_coeffs$f_did_not_have_condition_but_cost_increased <- positive_coeffs$n_did_not_have_condition_but_cost_increased/positive_coeffs$n_did_not_have_condition

  print(positive_coeffs)
  write.csv(positive_coeffs, "/Users/blahiri/healthcare/documents/inpatient_cost/positive_coeffs_conditional_probabilities.csv")


  negative_coeffs <- read.csv("/Users/blahiri/healthcare/documents/inpatient_cost/negative_coeffs.csv")
  negative_coeffs <- negative_coeffs[1:5, ]
  negative_coeffs$n_had_condition <- apply(negative_coeffs, 1, function(row)get_n_had_condition(con, as.character(row["feature_codes"])))
  negative_coeffs$n_did_not_have_condition <- 114538 - negative_coeffs$n_had_condition
  negative_coeffs$n_had_condition_and_cost_increased <- apply(negative_coeffs, 1, function(row)get_n_had_condition_and_cost_increased(con, as.character(row["feature_codes"])))
  negative_coeffs$n_did_not_have_condition_but_cost_increased <- 
                  apply(negative_coeffs, 1, function(row)get_n_did_not_have_condition_but_cost_increased(con, as.character(row["feature_codes"])))
  negative_coeffs$f_had_condition_and_cost_increased <- negative_coeffs$n_had_condition_and_cost_increased/negative_coeffs$n_had_condition
  negative_coeffs$f_did_not_have_condition_but_cost_increased <- negative_coeffs$n_did_not_have_condition_but_cost_increased/negative_coeffs$n_did_not_have_condition

  print(negative_coeffs)
  write.csv(negative_coeffs, "/Users/blahiri/healthcare/documents/inpatient_cost/negative_coeffs_conditional_probabilities.csv")

  dbDisconnect(con)
}


visualize_conditonal_probabilities <- function()
{
  pccp <- read.csv("/Users/blahiri/healthcare/documents/inpatient_cost/positive_coeffs_conditional_probabilities.csv")
  pccp <- pccp[, c("feature_codes", "f_had_condition_and_cost_increased", "f_did_not_have_condition_but_cost_increased")]
  colnames(pccp) <- c("feature_codes", "Yes", "No")

  path <- "/Users/blahiri/healthcare/documents/Healthcare_expenditure/v2/visualizations/positive_coeffs_conditional_probabilities/"
  n_pccp <- nrow(pccp)

  for (i in 1:n_pccp)
  {
   feature_code <- pccp[i, "feature_codes"]
   cat(paste("feature_code = ", feature_code, "\n", sep = ""))
   df <- pccp[i, ]
   molten_data <- melt(df, id = c("feature_codes")) 
   print(molten_data)
   filename  <- paste(path, feature_code, ".png", sep = "")
   png(file = filename, width = 800, height = 600)
   p <- ggplot(molten_data, aes(x = variable, y = value)) + geom_bar(width = 0.5, fill = "#FF6666", stat="identity") + 
           labs(x = "Whether benef had condition") +  
           labs(y = "Conditional probability of cost increase") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
   print(p)
   dev.off() 
  }

  nccp <- read.csv("/Users/blahiri/healthcare/documents/inpatient_cost/negative_coeffs_conditional_probabilities.csv")
  nccp <- nccp[, c("feature_codes", "f_had_condition_and_cost_increased", "f_did_not_have_condition_but_cost_increased")]
  colnames(nccp) <- c("feature_codes", "Yes", "No")

  path <- "/Users/blahiri/healthcare/documents/Healthcare_expenditure/v2/visualizations/negative_coeffs_conditional_probabilities/"
  n_nccp <- nrow(nccp)

  for (i in 1:n_nccp)
  {
   feature_code <- nccp[i, "feature_codes"]
   cat(paste("feature_code = ", feature_code, "\n", sep = ""))
   df <- nccp[i, ]
   molten_data <- melt(df, id = c("feature_codes")) 
   print(molten_data)
   filename  <- paste(path, feature_code, ".png", sep = "")
   png(file = filename, width = 800, height = 600)
   p <- ggplot(molten_data, aes(x = variable, y = value)) + geom_bar(width = 0.5, fill = "#FF6666", stat="identity") + 
           labs(x = "Whether benef had condition") +  
           labs(y = "Conditional probability of cost increase") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
   print(p)
   dev.off() 
  }
  
}

