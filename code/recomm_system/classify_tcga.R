library(arm)
library(logistf)
library(e1071)
library(ggplot2)

process_vital_status <- function(status)
{
  if (status == 'Alive' || status == 'LIVING') 
    return('TRUE')
  return('FALSE')
}

#return whether patient survived more than one year or not
process_days_to_death <- function(days_to_death)
{
  ifelse(((is.na(days_to_death)) || (as.numeric(days_to_death) >= 365)), 'TRUE', 'FALSE')
}


create_bs_by_over_and_undersampling <- function(dense_matrix, response_var)
{
  set.seed(1)
  n_dense_matrix <- nrow(dense_matrix)
  size_each_part <- n_dense_matrix/2
  
  #Extract the majority label
  df <- as.data.frame(table(dense_matrix[, response_var]))
  df <- df[order(-df[, "Freq"]),]
  majority_label <- as.character(df[1, "Var1"])
  minority_label <- as.character(df[2, "Var1"])

  majority_set_query_expn <- paste("majority_set <- subset(dense_matrix, (", response_var , " == '", majority_label , "'))", sep = "")
  majority_set <- eval(parse(text = majority_set_query_expn))
  n_majority <- nrow(majority_set)
  cat(paste("n_majority = ", n_majority, ", size_each_part = ", size_each_part, "\n", sep = ""))
  sample_majority_ind <- sample(1:n_majority, size_each_part, replace = FALSE)
  sample_majority <- majority_set[sample_majority_ind, ]
    
  minority_set_query_expn <- paste("minority_set <- subset(dense_matrix, (", response_var , " == '", minority_label, "'))", sep = "")
  minority_set <- eval(parse(text = minority_set_query_expn))
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
  return(bal_dense_matrix)
}

prepare_data <- function()
{
  file_path <- "/Users/blahiri/healthcare/data/tcga/Raw_Data/v3"
  dense_matrix <- read.csv(paste(file_path, "/", "clinical_all_combined_gbm.csv", sep = ""))
  
  dense_matrix$vital_status <- apply(dense_matrix, 1, function(row)process_vital_status(row["vital_status"])) 
  
  dense_matrix$lived_past_one_year <- apply(dense_matrix, 1, function(row)process_days_to_death(row["days_to_death"]))
  dense_matrix <- dense_matrix[,!(names(dense_matrix) %in% c("bcr_patient_barcode", "days_to_death", "vital_status"))]
  cat(paste("nrow(dense_matrix) = ", nrow(dense_matrix), "\n", sep = ""))

  demog_vars <- c("age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "race")
  case_history_vars <- c("histological_type", "history_of_neoadjuvant_treatment", "initial_pathologic_diagnosis_method", "karnofsky_performance_score", 
                         "person_neoplasm_cancer_status", "prior_glioma")
  drug_vars <- c("Avastin", "BCNU", "CCNU", "CPT.11", "Dexamethasone", "Gliadel.Wafer",  
                           "Other_drug", "Tarceva", "Temozolomide", "VP.16")
  radiation_vars <- c("EXTERNAL.BEAM", "Other_radiation")

  factor_vars <- c(demog_vars, case_history_vars, 
                   #"vital_status",
                   "lived_past_one_year"
                   )
  numeric_vars <- c("age_at_initial_pathologic_diagnosis", "karnofsky_performance_score")
  factor_vars <- factor_vars[!factor_vars %in% numeric_vars]

  #Converting drug and radiation variables to numeric as they are 1-0 only, and we want to avoid the change in variable names
  numeric_vars <- c(numeric_vars, drug_vars, radiation_vars)

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
  data_package <-  prepare_data()
  dense_matrix <- data_package[["dense_matrix"]]
  x <- dense_matrix[,!(names(dense_matrix) %in% c("lived_past_one_year"))]
  y <- dense_matrix[, "lived_past_one_year"]
  train = sample(1:nrow(x), 0.5*nrow(x))

  test = (-train)
  df.train <- create_bs_by_over_and_undersampling(dense_matrix[train, ], "lived_past_one_year")
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))
  str_formula <- "lived_past_one_year ~ "
   for (column in colnames(dense_matrix))
   {
       if (column != 'lived_past_one_year')
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   
   tune.out = tune.rpart(as.formula(str_formula), data = df.train, minsplit = c(5, 10, 15), maxdepth = c(1, 3, 5, 7))
   bestmod <- tune.out$best.model
   
   ypred <- predict(bestmod, newdata = dense_matrix[test, ], type = "class")
   print(table(dense_matrix[test, "lived_past_one_year"], ypred, dnn = list('actual', 'predicted')))

   #tune.out$best.model$variable.importance
   tune.out
}


train_validate_test_rf <- function()
 {
   set.seed(1)
   data_package <-  prepare_data()
   dense_matrix <- data_package[["dense_matrix"]]

   train = sample(1:nrow(dense_matrix), 0.5*nrow(dense_matrix))
   test = (-train)
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(dense_matrix) - length(train)), "\n", sep = ""))

   df.train <- dense_matrix[train, ]
   df.train <- create_bs_by_over_and_undersampling(df.train, "lived_past_one_year")
   x.train <- df.train[,!(names(df.train) %in% c("lived_past_one_year"))]
   y.train <- df.train[, "lived_past_one_year"]

   tune.out = tune.randomForest(x.train, y.train, 
                                ntree = seq(100, 600, 100), mtry = c(1, 2))
   bestmod <- tune.out$best.model
   ypred <- predict(bestmod, newdata = dense_matrix[test, ], type = "class")
   cont_tab <- table(dense_matrix[test, "lived_past_one_year"], ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))
   varImpPlot(bestmod)  
   bestmod
 }


principal_component <- function()
{
  set.seed(1)
  data_package <-  prepare_data()
  dense_matrix <- data_package[["dense_matrix"]]
  cat(paste("nrow(dense_matrix) = ", nrow(dense_matrix), "\n", sep = ""))
  dense_matrix <- na.omit(dense_matrix)
  cat(paste("nrow(dense_matrix) = ", nrow(dense_matrix), "\n", sep = ""))
  lived_past_one_year <- dense_matrix$lived_past_one_year
  dense_matrix <- dense_matrix[,!(names(dense_matrix) %in% c("bcr_patient_barcode", "ethnicity", "gender", "histological_type", 
                                                             "history_of_neoadjuvant_treatment", "initial_pathologic_diagnosis_method", 
                                                             "prior_glioma", "race", "vital_status", "lived_past_one_year"))]
  for (column in colnames(dense_matrix))
  {
    dense_matrix[, column] <- as.numeric(dense_matrix[, column])
  }
  
  #Drop columns with variance 0 as that presents a problem in scaling
  #dense_matrix <- dense_matrix[, apply(dense_matrix, 2, var, na.rm=TRUE) != 0]
  
  pc <- prcomp(dense_matrix, scale = TRUE)
  projected <- as.data.frame(pc$x[, c("PC1", "PC2")])
  projected$lived_past_one_year <- factor(lived_past_one_year == 'TRUE')

  png("/Users/blahiri/healthcare/data/tcga/Raw_Data/v2/figures/patients_first_two_pc.png",  width = 600, height = 480, units = "px")
  projected <- data.frame(projected)
  p <- ggplot(projected, aes(x = PC1, y = PC2)) + geom_point(aes(colour = lived_past_one_year), size = 2) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Projections along first two PCs for patients")
  print(p)
  dev.off()
  
  survivors <- subset(projected, (lived_past_one_year == 'TRUE'))
  non_survivors <- subset(projected, (lived_past_one_year == 'FALSE'))
  cat("Five num for survivors is\n")
  print(fivenum(survivors $PC1))
  cat("Five num for non_survivors is\n")
  print(fivenum(non_survivors $PC1))
  #Five num for survivors is
  # -2.43392690 -0.75897055 -0.04187241  1.31563270  8.69712112
  #Five num for non_survivors is
  # -2.9913999 -1.9376086 -0.8352016 -0.1849189  3.5748086
  #Non-survivors are more on the left side of the plot
  pc
}

compute_odds_ratios <- function(df.train)
{
  for (column in colnames(df.train))
  {
    if (is.factor(df.train[, column]))
    {
     cont_tab <- table(df.train[, column], df.train$lived_past_one_year, dnn = list(column, 'lived_past_one_year'))
     print(cont_tab)
    }
  }
}

check_stat_signif <- function()
{
  data_package <-  prepare_data()
  dense_matrix <- data_package[["dense_matrix"]]

  for (column1 in colnames(dense_matrix))
  {
    for (column2 in colnames(dense_matrix))
    {
      if (is.factor(dense_matrix[, column1]) & is.factor(dense_matrix[, column2]) & column1 != column2)
      {
        M <-  table(dense_matrix[, column1], dense_matrix[, column2])
        Xsq <- chisq.test(M, simulate.p.value = TRUE, B = 9999)

        if (Xsq$p.value < 0.05)
        {
          cat(paste("column1 = ", column1, ", column2 = ", column2, "\n", sep = ""))
          print(M)
          print(Xsq)
        }
      }
    }
   }
}


classify_lr <- function(dense_matrix, response_var)
{
  set.seed(1)
  train = sample(1:nrow(dense_matrix), 0.6*nrow(dense_matrix))
  test = (-train)

  df.train <- dense_matrix[train, ]
  df.train <- create_bs_by_over_and_undersampling(df.train, "lived_past_one_year")

  df.test <- dense_matrix[test, ]
  cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))
  #compute_odds_ratios(df.train)
 
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

   str_formula <- paste(response_var, " ~ ", sep = "")
   for (column in colnames(df.train))
   {
       if (column != response_var)
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   str_formula <- paste(str_formula, " + gender:prior_glioma + histological_type:history_of_neoadjuvant_treatment + 
                                      histological_type:prior_glioma + person_neoplasm_cancer_status:race", sep = "") 
   
   #Problem: With regular logistic regression, only age_at_initial_pathologic_diagnosis and karnofsky_performance_score are statistically significant predictors, 
   #although other predictors (e.g., person_neoplasm_cancer_statusTUMOR FREE) have high values of coefficients. One reason may be scale (?).

   #vital.logr = glm(as.formula(str_formula), family = binomial("logit"), data = df.train)

   #If the input training data is same, the Bayesian LR returns the same coefficients and standard errors even if invoked multiple times. 

   vital.logr = bayesglm(as.formula(str_formula), family = binomial("logit"), prior.scale = 2.5, prior.df = 1, data = df.train, drop.unused.levels = FALSE, x = TRUE)
   display(vital.logr)

   x.train <- df.train[,!(names(df.train) %in% c(response_var))]
   y.train <- df.train[, response_var]

   #Note: Using names(df.train) for df.test as columns have been dropped from df.train only
   x.test <- df.test[, names(x.train)]
   y.test = df.test[, response_var]
   
   ypred <- predict(vital.logr, x.train, type = "response")
   
   #print(invlogit(vital.model$x[1,]%*%vital.model$coefficients))
   #print(ypred[1])

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
  
   file_path <- "/Users/blahiri/healthcare/data/tcga/Raw_Data/v3"
   save(vital.logr, file = paste(file_path, "/vital_logr.rda", sep = ""))
   model_package <- list("dropped_columns" = dropped_columns , "vital.model" = vital.logr)
}


bagging <- function(dense_matrix, response_var)
{
  set.seed(1)
  train = sample(1:nrow(dense_matrix), 0.5*nrow(dense_matrix))
  test = (-train)

  df.train <- dense_matrix[train, ]
  df.train <- create_bs_by_over_and_undersampling(df.train, "lived_past_one_year")

  df.test <- dense_matrix[test, ]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(dense_matrix) - length(train)), "\n", sep = ""))
 
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

   str_formula <- paste(response_var, " ~ ", sep = "")
   for (column in colnames(df.train))
   {
       if (column != response_var)
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)

   B <- 300
   models <- list()  
   for (b in 1:B)
   { 
     bootstrap_sample_indices <- sample(1:nrow(df.train), size = nrow(df.train), replace = TRUE)
     bootstrap_sample <- df.train[bootstrap_sample_indices, ]
     models[[b]] = bayesglm(as.formula(str_formula), family = binomial("logit"), prior.scale = 2.5, prior.df = 1, data = bootstrap_sample, drop.unused.levels = FALSE)
   }
   x.test <- df.test[, names(df.train)]
   y.test = df.test[, response_var]
   ypred_final <- c()
   for (i in 1:nrow(x.test))
   {
     predictions <- c()
     for (b in 1:B)
     {
       predictions[b] <- predict(models[[b]], x.test[i, ], type = "response")
     }
     ypred <-  ifelse(predictions >= 0.5, 'TRUE', 'FALSE')
     df <- as.data.frame(table(ypred))
     df <- df[order(-df[, "Freq"]),]
     majority_label <- as.character(df[1, "ypred"])
     cat(paste("i = ", i, ", majority_label = ", majority_label, "\n", sep = ""))
     ypred_final[i] <- majority_label
   }

   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y.test, ypred_final, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))
}

classify_nb <- function(dense_matrix, response_var)
 {
   set.seed(1)

   train = sample(1:nrow(dense_matrix), 0.5*nrow(dense_matrix))
   test = (-train)
   df.train <- dense_matrix[train, ]
   df.train <- create_bs_by_over_and_undersampling(df.train, "lived_past_one_year")
   df.test <- dense_matrix[test, ]

   x.train <- df.train[,!(names(df.train) %in% c(response_var))]
   y.train <- df.train[, response_var]

   #Note: Using names(df.train) for df.test as columns have been dropped from df.train only
   x.test <- df.test[, names(x.train)]
   y.test = df.test[, response_var]
   
   cat(paste("Size of training data = ", nrow(df.train), ", size of test data = ", nrow(df.test), "\n", sep = ""))

   str_formula <- paste(response_var, " ~ ", sep = "")
   for (column in colnames(df.train))
   {
       if (column != response_var)
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   vital.nb <- naiveBayes(as.formula(str_formula), data = df.train)

   ypred <- predict(vital.nb, x.train, type = "class")
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(y.train, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   ypred = predict(vital.nb, x.test, type = "class")
   cat("Confusion matrix for test data\n")
   cont_tab <-  table(y.test, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   test_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Test FNR = ", FNR, ", test FPR = ", FPR, ", test_error = ", test_error, "\n", sep = ""))
   
   model_package <- list("vital.model" = vital.nb)
 }


classify_logistf <- function(dense_matrix, response_var)
{
  set.seed(1)
  train = sample(1:nrow(dense_matrix), 0.5*nrow(dense_matrix))
  test = (-train)

  df.train <- dense_matrix[train, ]
  df.train <- create_bs_by_over_and_undersampling(df.train, "lived_past_one_year")

  df.test <- dense_matrix[test, ]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(dense_matrix) - length(train)), "\n", sep = ""))
 
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

   str_formula <- paste(response_var, " ~ ", sep = "")
   for (column in colnames(df.train))
   {
       if (column != response_var)
       {
         str_formula <- paste(str_formula, column, " + ", sep = "")
       }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   
   #Problem: With regular logistic regression, only age_at_initial_pathologic_diagnosis and karnofsky_performance_score are statistically significant predictors, 
   #although other predictors (e.g., person_neoplasm_cancer_statusTUMOR FREE) have high values of coefficients. One reason may be scale (?).

   vital.logr = logistf(as.formula(str_formula), data = df.train)

   y.train <- df.train[, response_var]
   ypred <- vital.logr$predict
   ypred <-  ifelse(ypred >= 0.5, 'TRUE', 'FALSE')
   
   cat("Confusion matrix for training data\n")
   cont_tab <-  table(y.train, ypred, dnn = list('actual', 'predicted'))
   print(cont_tab)
   FNR <- cont_tab[2,1]/sum(cont_tab[2,])
   FPR <- cont_tab[1,2]/sum(cont_tab[1,])
   training_error <- (cont_tab[2,1] + cont_tab[1,2])/sum(cont_tab)
   cat(paste("Training FNR = ", FNR, ", training FPR = ", FPR, ", training_error = ", training_error, "\n", sep = ""))
   
   model_package <- list("dropped_columns" = dropped_columns , "vital.model" = vital.logr)
}


evaluate_node <- function(bit_string, treatment_options, n_options, dropped_columns, demog_ch_vars, vital.logr, dense_matrix, response_var)
{
  columns <- setdiff(c(demog_ch_vars, treatment_options), dropped_columns)
  test_row <- data.frame(matrix(ncol = length(columns)))
  colnames(test_row) <- columns
  
  #Some random values set for demo and case history variables

  test_row[1, "age_at_initial_pathologic_diagnosis"] <- 59
  test_row[1, "ethnicity"] <- 'NOT HISPANIC OR LATINO'
  test_row[1, "gender"] <- 'FEMALE'
  test_row[1, "race"] <- 'WHITE'
  test_row[1, "histological_type"] <- 'Untreated primary (de novo) GBM'
  test_row[1, "history_of_neoadjuvant_treatment"] <- 'No'
  test_row[1, "initial_pathologic_diagnosis_method"] <- 'Tumor resection'
  test_row[1, "karnofsky_performance_score"] <- 80
  test_row[1, "person_neoplasm_cancer_status"] <- 'WITH TUMOR'
  test_row[1, "prior_glioma"] <- 'NO'

  for (j in 1:n_options)
   {
     if (bit_string[j])
     {
       test_row[1, treatment_options[j]] = 1
     }
     else
     {
       test_row[1, treatment_options[j]] = 0
     }
   }
  for (column in columns)
  {
    if (column != response_var & is.factor(dense_matrix[, column]))
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

binary_programming_for_optimal <- function(vital.logr)
{ 
  #The objective function is the linear combination of variables returned by logistic regression. For the demographic and case history variables, we will add 
  #constraints based on the inputs obtained from the user. For the intercept/constant term, we will add a pseudo-variable that is always set to 1 by a constraint.

  n_vars <- length(vital.model$coefficients) #includes intercept
  f.obj <- as.numeric(vital.logr$coefficients)

  constraint_for_constant <- c(1, rep(0, n_vars - 1))
  all_constraints <- constraint_for_constant
  all_rhs <- c(1)
  all_dirs <- c("==")

  constraint_for_demog_ch <- c("age_at_initial_pathologic_diagnosis" = 40, "ethnicityHISPANIC OR LATINO" = 0, "genderMALE" = 1, 
                               "histological_typeTreated primary GBM" = 1, "histological_typeGlioblastoma Multiforme (GBM)" = 0,
                               "history_of_neoadjuvant_treatmentNo" = 1, "initial_pathologic_diagnosis_methodExcisional Biopsy" = 1,
                               "initial_pathologic_diagnosis_methodIncisional Biopsy" = 0, "initial_pathologic_diagnosis_methodOther method, specify:" = 0,
                               "initial_pathologic_diagnosis_methodFine needle aspiration biopsy" = 0, "karnofsky_performance_score" = 30,
                               "person_neoplasm_cancer_statusTUMOR FREE" = 0, "prior_gliomaYES" = 1, "raceBLACK OR AFRICAN AMERICAN" = 0, 
                               "raceASIAN" = 0)

  #Create a vector of coefficients for each constraint on demographic and case history variables
  n_constraint_for_demog_ch <- length(constraint_for_demog_ch)
  for (i in 1: n_constraint_for_demog_ch)
  {
    constraint <- rep(0, n_vars) #One pseudo-variable for the constant term 
    constraint[i + 1] <- 1
    all_constraints <- c(all_constraints, constraint)
    all_dirs <- c(all_dirs, "==")
    all_rhs <- c(all_rhs, constraint_for_demog_ch[[i]])
  }

  #Take combination of at most 3 treatment options
  constraint <- rep(0, n_vars)
  constraint[(n_constraint_for_demog_ch + 2):n_vars] <- 1
  all_constraints <- c(all_constraints, constraint)
  all_rhs <- c(all_rhs, 4)
  all_dirs <- c(all_dirs, "<=")

  #Take at most one radiation option
  constraint <- rep(0, n_vars)
  constraint[(n_vars - 1):n_vars] <- 1
  all_constraints <- c(all_constraints, constraint)
  all_rhs <- c(all_rhs, 2)
  all_dirs <- c(all_dirs, "<")

  f.con <- matrix (all_constraints, nrow = n_constraint_for_demog_ch + 3, byrow=TRUE)
  #print(f.con)
  #print(all_dirs)
  #print(all_rhs)
  lp_solve <- lp("max", f.obj, f.con, all_dirs, all_rhs, binary.vec = 17:28)  

  all_vars <- names(vital.model$coefficients)
  print(convert_bit_string_to_text(lp_solve$solution[17:28], all_vars[17:28]))
}

convert_bit_string_to_text <- function(bit_string, treatment_options)
{
  paste("Optimal combination is", paste(treatment_options[which(bit_string == 1)], collapse = ", "), sep = " ")
}


test_optimal_from_bi_prog <- function(drug_vars, radiation_vars, dropped_columns, vital.logr, dense_matrix)
{
  treatment_options <- sort(append(drug_vars, radiation_vars))
  n_options <- length(treatment_options)
  bit_string <- rep(FALSE, length(treatment_options))
  bit_string[which(treatment_options %in% c("Avastin", "Tarceva", "Temozolomide", "EXTERNAL.BEAM"))] <- TRUE
  demog_ch_vars <- c("age_at_initial_pathologic_diagnosis", "ethnicity", "gender", "race", "histological_type", "history_of_neoadjuvant_treatment", 
                     "initial_pathologic_diagnosis_method", "karnofsky_performance_score", "person_neoplasm_cancer_status", "prior_glioma")
  max_prob <- evaluate_node(bit_string, treatment_options, n_options, dropped_columns, demog_ch_vars, vital.logr, dense_matrix, "lived_past_one_year")
  cat(paste("max_prob = ", max_prob, "\n", sep = ""))
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
  current_val <- evaluate_node(bit_string, treatment_options, n_options, dropped_columns, demog_ch_vars, vital.logr, dense_matrix, "lived_past_one_year")
 
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
          this_neighbor_val <- evaluate_node(neighbor_bit_string, treatment_options, n_options, dropped_columns, demog_ch_vars, vital.logr, dense_matrix, "lived_past_one_year")
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
    #cat(paste("score rising to = ", max_score_from_neighbor, ", new current is ", convert_bit_string_to_evidence(bit_string, treatment_options, n_options), "\n\n", sep = ""))
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

  model_package <- classify_lr(dense_matrix, "lived_past_one_year")
  vital.model <- model_package[["vital.model"]]
  dropped_columns <- model_package[["dropped_columns"]]
  lincomb <- model_package[["lincomb"]]
  drug_vars <- data_package[["drug_vars"]]
  radiation_vars <- data_package[["radiation_vars"]]
  
  #custom_hill_climbing_for_optimal(drug_vars, radiation_vars, dropped_columns, vital.model, dense_matrix)
  #binary_programming_for_optimal(vital.model)
  #test_optimal_from_bi_prog(drug_vars, radiation_vars, dropped_columns, vital.model, dense_matrix)
  #print(sort(vital.logr$coefficients, decreasing = TRUE)) #Gives the predictors in decreasing order of coefficients, first few are Temozolomide, initial_pathologic_diagnosis_method, Irinotecan and Procarbazine
  
  vital.model
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



