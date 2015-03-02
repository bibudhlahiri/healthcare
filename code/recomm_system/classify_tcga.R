process_vital_status <- function(status)
{
  if (status == 'Alive' || status == 'LIVING') 
    return('TRUE')
  return('FALSE')
}


create_bs_by_over_and_undersampling <- function(dense_matrix)
{
  set.seed(1)
  n_dense_matrix <- nrow(dense_matrix)
  size_each_part <- n_dense_matrix/2

  majority_set <- subset(dense_matrix, (vital_status == 'FALSE'))
  n_majority <- nrow(majority_set)
  sample_majority_ind <- sample(1:n_majority, size_each_part, replace = FALSE)
  sample_majority <- majority_set[sample_majority_ind, ]
    
  minority_set <- subset(dense_matrix, (vital_status == 'TRUE'))
  n_minority <- nrow(minority_set)
  rep_times <- size_each_part%/%nrow(minority_set)
  oversampled_minority_set <- minority_set
  if (rep_times > 1)
  {
   for (i in 1:(rep_times - 1))
   {
    oversampled_minority_set <- rbind(oversampled_minority_set, minority_set)
   }
  }
  rem_sample_id <- sample(1:n_minority, size_each_part%%nrow(minority_set), replace = FALSE)
  rem_sample <- minority_set[rem_sample_id, ]
  oversampled_minority_set <- rbind(oversampled_minority_set, rem_sample)

  bal_dense_matrix <- rbind(sample_majority, oversampled_minority_set)
  print(table(bal_dense_matrix$vital_status))
  return(bal_dense_matrix)
}

prepare_data <- function()
{
  file_path <- "/Users/blahiri/healthcare/data/tcga/Raw_Data"
  dense_matrix <- read.csv(paste(file_path, "/", "clinical_all_combined_gbm.csv", sep = ""))

  #Filter out rows that create incomplete data
  dense_matrix <- subset(dense_matrix, (karnofsky_performance_score != "[Not Available]")) 
  dense_matrix <- subset(dense_matrix, (person_neoplasm_cancer_status != "[Not Available]")) 

  dense_matrix <- subset(dense_matrix, (person_neoplasm_cancer_status != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (ethnicity != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (race != "[Not Available]"))
  dense_matrix <- subset(dense_matrix, (history_of_neoadjuvant_treatment != "[Not Available]"))
  
  dense_matrix$vital_status <- apply(dense_matrix, 1, function(row)process_vital_status(row["vital_status"])) 
  dense_matrix <- dense_matrix[,!(names(dense_matrix) %in% c("bcr_patient_barcode"))]

  demog_vars <- c("age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "race")
  case_history_vars <- c("histological_type", "history_of_neoadjuvant_treatment", "initial_pathologic_diagnosis_method", "karnofsky_performance_score", 
                         "person_neoplasm_cancer_status", "prior_glioma")
  drug_vars <- colnames(dense_matrix)[12:31]
  radiation_vars <- colnames(dense_matrix)[32:37]

  for (drug_var in drug_vars)
  {
    dense_matrix[, drug_var] <- (dense_matrix[, drug_var] > 0)
  }
  for (radiation_var in radiation_vars)
  {
    dense_matrix[, radiation_var] <- (dense_matrix[, radiation_var] > 0)
  }

  factor_vars <- c(demog_vars, case_history_vars, drug_vars, radiation_vars, "vital_status")
  numeric_vars <- c("age_at_initial_pathologic_diagnosis", "karnofsky_performance_score")
  factor_vars <- factor_vars[!factor_vars %in% numeric_vars]

  for (column in factor_vars)
  {
    dense_matrix[, column] <- factor(dense_matrix[, column], levels = unique(dense_matrix[, column]))
  }
  for (column in numeric_vars)
  {
    dense_matrix[, column] <- as.numeric(dense_matrix[, column])
  }
  dense_matrix
}

classify_rpart <- function()
{
  library(e1071)
  set.seed(1)
  dense_matrix <- prepare_data()
  x <- dense_matrix[,!(names(dense_matrix) %in% c("vital_status"))]
  y <- dense_matrix[, "vital_status"]
  train = sample(1:nrow(x), 0.5*nrow(x))

  test = (-train)
  df.train <- create_bs_by_over_and_undersampling(dense_matrix[train, ])
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))
  str_formula <- "vital_status ~ "
   for (column in colnames(dense_matrix))
   {
       if (column != 'vital_status')
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   
   tune.out = tune.rpart(as.formula(str_formula), data = df.train, minsplit = c(5, 10, 15), maxdepth = c(1, 3, 5, 7))
   bestmod <- tune.out$best.model
   
   ypred <- predict(bestmod, newdata = dense_matrix[test, ], type = "class")
   print(table(dense_matrix[test, "vital_status"], ypred, dnn = list('actual', 'predicted')))
   #Error in Dead class = 0.3793103, error in Alive class = 0.3235294.
   #tune.out$best.model$variable.importance
   tune.out
}


classify_lr <- function()
{
  library(e1071)
  set.seed(1)
  dense_matrix <- prepare_data()
  x <- dense_matrix[,!(names(dense_matrix) %in% c("vital_status"))]
  y <- dense_matrix[, "vital_status"]
  train = sample(1:nrow(x), 0.5*nrow(x))

  test = (-train)

  df.train <- dense_matrix[train, ]
  df.train <- create_bs_by_over_and_undersampling(df.train)

  df.test <- dense_matrix[test, ]

  
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))
 
  #For logistic regression, the factor predictors need at least two distinct values to be 
  #present in the training data. Sample balancing does not change the distribution of the predictors much, 
  #so we drop factor predictors that do not have only one distinct value in training data.

   for (column in colnames(dense_matrix))
   {
       if (class(df.train[, column]) == 'factor')
       {
         if (length(unique(df.train[, column])) == 1)
         {
            cat(paste("Dropping column = ", column, "\n", sep = ""))
            df.train <- df.train[,!(names(df.train) %in% c(column))]
         }
       }
   }
   cat("colnames after first dropping\n")
   print(colnames(df.train))

   str_formula <- "vital_status ~ "
   for (column in colnames(df.train))
   {
       if (column != 'vital_status')
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)

   #Diagnostics for rank-deficiency
   dm <- model.matrix(as.formula(str_formula), df.train)
   dupes <- duplicated(t(dm))
   print((colnames(df.train))[which(dupes)]) #EXTERNAL.BEAM
   print(summary(df.train$EXTERNAL.BEAM))
   library(caret)
   lincomb <- findLinearCombos(dm)
   print((colnames(df.train))[lincomb$remove])
   print(summary(df.train[, 8]))
   print(summary(df.train[, 29]))
   df.train <- df.train[, -lincomb$remove]
   cat("colnames after second dropping\n")
   print(colnames(df.train))
   

   str_formula <- "vital_status ~ "
   for (column in colnames(df.train))
   {
       if (column != 'vital_status')
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   
   vital.logr = glm(as.formula(str_formula), family = binomial("logit"), data = df.train)

   x.train <- df.train[,!(names(df.train) %in% c("vital_status"))]
   y.train <- df.train[, "vital_status"]

   #Note: Using names(df.train) for df.test as columns have been dropped from df.train only
   x.test <- df.test[, names(x.train)]
   y.test = y[test]
   
   ypred <- predict(vital.logr, x.train, type = "response")
   ypred <-  ifelse(ypred >= 0.5, 'TRUE', 'FALSE')
   
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(y.train, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   cat("colnames for x.test\n")
   print(colnames(x.test))
   ypred = predict(vital.logr, x.test, type = "response")
   ypred <-  ifelse(ypred >= 0.5, 'TRUE', 'FALSE')
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))
   
   vital.logr
}
