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
  data_package <- list("dense_matrix" = dense_matrix, "drug_vars" = drug_vars, "radiation_vars" = radiation_vars)
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


classify_lr <- function(dense_matrix)
{
  set.seed(1)
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
  #so we drop factor predictors that have only one distinct value in training data.

  dropped_columns <- c()
  for (column in colnames(dense_matrix))
  {
       if (class(df.train[, column]) == 'factor')
       {
         if (length(unique(df.train[, column])) == 1)
         {
            cat(paste("Dropping column = ", column, "\n", sep = ""))
            df.train <- df.train[,!(names(df.train) %in% c(column))]
            dropped_columns <- c(dropped_columns, column)
         }
       }
   }

   str_formula <- "vital_status ~ "
   for (column in colnames(df.train))
   {
       if (column != 'vital_status')
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)

   if (FALSE)
   {
   #Diagnostics for rank-deficiency
   dm <- model.matrix(as.formula(str_formula), df.train)
   cat("colnames(dm) = \n")
   print(colnames(dm))

   cat("colnames(df.train) = \n")
   print(colnames(df.train))

   library(caret)
   lincomb <- findLinearCombos(dm)
   print(lincomb$remove)
   print(dm[, "initial_pathologic_diagnosis_methodOther method, specify:"])
   print(dm[, "ProcarbazineTRUE"])
   #Vincristine has perfect correlation with Procarbazine
   print(dm[, "VincristineTRUE"])
   print(table(df.train$Procarbazine, df.train$vital_status, dnn = list('Procarbazine', 'vital_status')))
   print(table(df.train$Vincristine, df.train$vital_status, dnn = list('Vincristine', 'vital_status')))
   
   df.train <- df.train[, -lincomb$remove]

   str_formula <- "vital_status ~ "
   for (column in colnames(df.train))
   {
       if (column != 'vital_status')
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   }
   
   #Problem: With regular logistic regression, only age_at_initial_pathologic_diagnosis and karnofsky_performance_score are statistically significant predictors, 
   #although other predictors (e.g., person_neoplasm_cancer_statusTUMOR FREE) have high values of coefficients. One reason may be scale (?).

   #vital.logr = glm(as.formula(str_formula), family = binomial("logit"), data = df.train)
   vital.logr = bayesglm(as.formula(str_formula), family = binomial("logit"), data = df.train, drop.unused.levels = FALSE)
   display(vital.logr)

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
   
   ypred = predict(vital.logr, x.test, type = "response")
   ypred <-  ifelse(ypred >= 0.5, 'TRUE', 'FALSE')
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))
   
   model_package <- list("dropped_columns" = dropped_columns , "vital.logr" = vital.logr, "lincomb" = lincomb)
}


evaluate_node <- function(bit_string, treatment_options, n_options, dropped_columns, demog_ch_vars, vital.logr, dense_matrix)
{
  columns <- setdiff(c(demog_ch_vars, treatment_options), dropped_columns)
  test_row <- data.frame(matrix(ncol = length(columns)))
  colnames(test_row) <- columns
  
  #Some random values set for demo and case history variables

  test_row[1, "age_at_initial_pathologic_diagnosis"] <- 45
  test_row[1, "ethnicity"] <- 'NOT HISPANIC OR LATINO'
  test_row[1, "gender"] <- 'FEMALE'
  test_row[1, "race"] <- 'WHITE'
  test_row[1, "histological_type"] <- 'Untreated primary (de novo) GBM'
  test_row[1, "history_of_neoadjuvant_treatment"] <- 'No'
  test_row[1, "initial_pathologic_diagnosis_method"] <- 'Tumor resection'
  test_row[1, "karnofsky_performance_score"] <- 50
  test_row[1, "person_neoplasm_cancer_status"] <- 'WITH TUMOR'
  test_row[1, "prior_glioma"] <- 'NO'

  for (j in 1:n_options)
   {
     if (bit_string[j])
     {
       test_row[1, treatment_options[j]] = 'TRUE'
     }
     else
     {
       test_row[1, treatment_options[j]] = 'FALSE'
     }
   }
  for (column in columns)
  {
    if (column != "vital_status" & is.factor(dense_matrix[, column]))
    { 
      test_row[, column] <- factor(test_row[, column], levels = levels(dense_matrix[, column]))
    }
  }
  cond_prob = predict(vital.logr, test_row, type = "response")
}

convert_bit_string_to_evidence <- function(bit_string, treatment_options, n_options)
{
  evidence <- ""
  for (j in 1:n_options)
  {
     if (bit_string[j])
     {
       starts_with <- ifelse((evidence == ""), "", " & ")
       evidence <- paste(evidence, starts_with, "(", treatment_options[j], " == 'TRUE')", sep = "")
     }
  }
  evidence
}

custom_hill_climbing_for_optimal <- function(drug_vars, radiation_vars, dropped_columns, vital.logr, dense_matrix)
{
  treatment_options <- sort(append(drug_vars, radiation_vars))
  n_options <- length(treatment_options)
  bit_string <- rep(FALSE, length(treatment_options))

  #pick an option at random, to start with. A node is an assignment of values to the evidence variables.
  current <- sample(treatment_options, 1)
  bit_string[which(treatment_options == current)] <- TRUE
  
  #Fix some values for the demographic and case history variables, for now
  demog_ch_vars <- c("age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "race", "histological_type", "history_of_neoadjuvant_treatment", 
                     "initial_pathologic_diagnosis_method", "karnofsky_performance_score", "person_neoplasm_cancer_status", "prior_glioma")
  current_val <- evaluate_node(bit_string, treatment_options, n_options, dropped_columns, demog_ch_vars, vital.logr, dense_matrix)
 
  while (TRUE)
  {
    cat(paste("current evidence = ", convert_bit_string_to_evidence(bit_string, treatment_options, n_options), ", current_val = ", current_val, "\n\n", sep = ""))
    #Generate all neighbors of current by a successor function. The neighbors are generated by toggling one 
    #bit in bit_string at a time
    max_score_from_neighbor <- 0
    for (i in 1:n_options)
    {
        #Generate a neighbor by toggling the i-th bit of bit_string
        neighbor_bit_string <- bit_string
        neighbor_bit_string[i] <- !bit_string[i]
        #All 0s can be generated as a neighbor but it is not a valid evidence, so skip it
        if (sum(neighbor_bit_string) > 0)
        {
          #Generate the evidence corresponding to the neighbor
          this_neighbor_val <- evaluate_node(neighbor_bit_string, treatment_options, n_options, dropped_columns, demog_ch_vars, vital.logr, dense_matrix)
          #cat(paste("this_neighbor_val = ", this_neighbor_val, "\n", sep = ""))
          if (this_neighbor_val > max_score_from_neighbor)
          {
            max_score_from_neighbor <- this_neighbor_val
            highest_scoring_neighbor <- neighbor_bit_string
          }
        }
    }
    #Now, all neighbors are processed for current node
    if (max_score_from_neighbor <= current_val)
    {
      cat(paste("Reached maxima at ", current_val, "\n", sep = "")) 
      return(bit_string)
    }
    #Setting current to highest scoring neighbor for the next iteration. bit_string represents the current node
    bit_string <- highest_scoring_neighbor
    current_val <- max_score_from_neighbor
    cat(paste("score rising to = ", max_score_from_neighbor, ", new current is ", convert_bit_string_to_evidence(bit_string, treatment_options, n_options), "\n\n", sep = ""))
    if (max_score_from_neighbor == 1)
    {
      return(highest_scoring_neighbor)
    }
  }
}

run_show <- function()
{
  data_package <-  prepare_data()
  dense_matrix <- data_package[["dense_matrix"]]
  model_package <- classify_lr(dense_matrix)
  vital.logr <- model_package[["vital.logr"]]
  dropped_columns <- model_package[["dropped_columns"]]
  lincomb <- model_package[["lincomb"]]
  drug_vars <- data_package[["drug_vars"]]
  radiation_vars <- data_package[["radiation_vars"]]
  custom_hill_climbing_for_optimal(drug_vars, radiation_vars, dropped_columns, vital.logr, dense_matrix)
  vital.logr
}


perfect_separation <- function()
{
 library(arm)
 set.seed(123456)
 # Faking some data where x1 is unrelated to y
 # while x2 perfectly separates y.
 d <- data.frame(y  =  c(0,0,0,0, 0, 1,1,1,1,1),
                x1 = rnorm(10),
                x2 = sort(rnorm(10)))

 #x2 gets a high coefficient of 81.689
 fit <- glm(y ~ x1 + x2, data=d, family="binomial")
 print(summary(fit))

 #x2 gets a coefficient of 1.86 only: that will change if seed is not fixed since samples are drawn from prior
 fit <- bayesglm(y ~ x1 + x2, data=d, family="binomial")
 display(fit)
}



