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


check_stat_signif <- function()
{
  data_package <-  prepare_data()
  dense_matrix <- data_package[["dense_matrix"]]
  drug_vars <- c("Avastin", "BCNU", "CCNU", "CPT.11", "Dexamethasone", "Gliadel.Wafer",  
                           "Other_drug", "Tarceva", "Temozolomide", "VP.16")
  radiation_vars <- c("EXTERNAL.BEAM", "Other_radiation")

  for (column1 in colnames(dense_matrix))
  {
    for (column2 in colnames(dense_matrix))
    {
      if (is.factor(dense_matrix[, column1]) 
          & (is.factor(dense_matrix[, column2]) || column2 %in% c(drug_vars, radiation_vars)) 
          & column1 != column2)
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
   #str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)

   #Interaction terms will be necessary to make sure the variables out of demographic and case history variables do not give rise to a constant.
   #We keep the interaction terms among the demographic-case history variables and the treatment variables only.
   #Once we have the interaction terms (restricted to 2 x 2), we can still use linear program to optimize the objective function.

   str_formula <- paste(str_formula, " histological_type:Temozolomide + histological_type:EXTERNAL.BEAM + 
                                       history_of_neoadjuvant_treatment:Avastin + history_of_neoadjuvant_treatment:EXTERNAL.BEAM + 
                                       initial_pathologic_diagnosis_method:Avastin + initial_pathologic_diagnosis_method:CCNU + 
                                       initial_pathologic_diagnosis_method:CPT.11 + initial_pathologic_diagnosis_method:Other_drug + 
                                       initial_pathologic_diagnosis_method:Temozolomide + initial_pathologic_diagnosis_method:VP.16 + 
                                       initial_pathologic_diagnosis_method:EXTERNAL.BEAM + person_neoplasm_cancer_status:Other_drug", sep = "") 
   
   #Problem: With regular logistic regression, only age_at_initial_pathologic_diagnosis and karnofsky_performance_score are statistically significant predictors, 
   #although other predictors (e.g., person_neoplasm_cancer_statusTUMOR FREE) have high values of coefficients. One reason may be scale (?).

   #vital.logr = glm(as.formula(str_formula), family = binomial("logit"), data = df.train)

   #If the input training data is same, the Bayesian LR returns the same coefficients and standard errors even if invoked multiple times. 

   vital.logr = bayesglm(as.formula(str_formula), family = binomial("logit"), prior.scale = 2.5, prior.df = 1, data = df.train, drop.unused.levels = FALSE, x = TRUE)
   #display(vital.logr)

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


inputs_for_demog_ch <<- c("intercept_coeff" = 1, "age_at_initial_pathologic_diagnosis" = 59, "ethnicityHISPANIC OR LATINO" = 0, "genderMALE" = 0, 
                           "histological_typeTreated primary GBM" = 1, "histological_typeGlioblastoma Multiforme (GBM)" = 0,
                           "history_of_neoadjuvant_treatmentNo" = 1, "initial_pathologic_diagnosis_methodExcisional Biopsy" = 0,
                           "initial_pathologic_diagnosis_methodIncisional Biopsy" = 1, "initial_pathologic_diagnosis_methodOther method, specify" = 0,
                           "initial_pathologic_diagnosis_methodFine needle aspiration biopsy" = 0, "karnofsky_performance_score" = 80,
                           "person_neoplasm_cancer_statusTUMOR FREE" = 0, "prior_gliomaYES" = 0, "raceBLACK OR AFRICAN AMERICAN" = 0, 
                           "raceASIAN" = 0)

#The input is a bit vector that specifies a chromosome, which essentially 
#tells which of the treatment options will be used. The output is the negative of the 
#linear combination generated by logistic regression. The negative is taken because 
#rbga.bin only minimizes the objective function.

evalFunc <- function(bit_string)
{
  
  file_path <- "/Users/blahiri/healthcare/data/tcga/Raw_Data/v3"
  load(paste(file_path, "/vital_logr.rda", sep = ""))
  obj_func <- as.numeric(vital.logr$coefficients[1:16]) %*% as.numeric(inputs_for_demog_ch)

  obj_func <- obj_func + as.numeric(vital.logr$coefficients[17:28]) %*% bit_string

  #Compute the part of the (quadratic) combination arising out of the interaction terms
  sum_of_part <- 0
  interaction_terms <- names(vital.logr$coefficients[29:63])

  drug_vars <- c("Avastin", "BCNU", "CCNU", "CPT.11", "Dexamethasone", "Gliadel.Wafer",  
                           "Other_drug", "Tarceva", "Temozolomide", "VP.16")
  radiation_vars <- c("EXTERNAL.BEAM", "Other_radiation")
  treatment_options <- sort(append(drug_vars, radiation_vars))

  for (interaction_term in interaction_terms)
  {
    variables <- unlist(strsplit(interaction_term, ":"))
    demog_ch_var <- variables[1]
    treatment_option <- variables[2]
    sum_of_part <- sum_of_part + inputs_for_demog_ch[[demog_ch_var]]* bit_string[which(treatment_options == treatment_option)]  
  }
  obj_func <- obj_func + sum_of_part

  n_bits <- length(bit_string)
  if ((sum(bit_string) > 3) || (bit_string[which(treatment_options == "EXTERNAL.BEAM")] + bit_string[which(treatment_options == "Other_radiation")] == 2))
  {
    return(0)
  }
  else
  {
    return(-obj_func)
  }
}

genetic_algorithm_for_optimal <- function(drug_vars, radiation_vars)
{
  library(genalg)
  treatment_options <- sort(append(drug_vars, radiation_vars))
  n_options <- length(treatment_options)

  GAmodel <- rbga.bin(size = n_options, popSize = 200, iters = 7, 
                      #mutationChance = 1/(n_options + 1), 
                      mutationChance = 0.01,
                      elitism = T, evalFunc = evalFunc, verbose = FALSE)
  df <- process_output_of_genetic(GAmodel, treatment_options)
  model_data <- list("GAmodel" = GAmodel, "df" = df)
}

#Rank the chromosomes of the final population by evaluation. Since the GA 
#minimizes the objective function, the lower the evaluation, the higher they should 
#be ranked. Survival probability = sigmoid(-evaluation)

process_output_of_genetic <- function(GAmodel, treatment_options)
{
  library(boot)
  df <- as.data.frame(GAmodel$population)
  df$evaluation <- GAmodel$evaluations
  df$surv_prob <- inv.logit(-df$evaluation)
  df <- df[order(-df[, "surv_prob"]),]
  df <- df[!duplicated(df), ]
  df$combination <- apply(df, 1, function(row) convert_bit_string_to_text(row[paste("V", 1:12, sep = "")], treatment_options)) 
  df <- df[,!(names(df) %in% c("evaluation", paste("V", 1:12, sep = "")))]
  df
}

convert_bit_string_to_text <- function(bit_string, treatment_options)
{
  paste(treatment_options[which(bit_string == 1)], collapse = ", ")
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

  #The optimum is the chromosome for which the evaluation value is minimal
  model_data <- genetic_algorithm_for_optimal(drug_vars, radiation_vars)
}

expt_random_io <- function()
{
  expt_results <- data.frame(matrix(0, ncol = 1 + length(inputs_for_demog_ch)))
  colnames(expt_results) <- c(names(inputs_for_demog_ch), "best_combi")
  n_trials <- 100
  drug_vars <- c("Avastin", "BCNU", "CCNU", "CPT.11", "Dexamethasone", "Gliadel.Wafer",  
                           "Other_drug", "Tarceva", "Temozolomide", "VP.16")
  radiation_vars <- c("EXTERNAL.BEAM", "Other_radiation")

  for (i in 1:n_trials)
  {
    expt_results[i, 1] <- round(runif(1, 10, 89))
    expt_results[i, 2:10] <- sample(c(0,1), 9, replace = TRUE)
    expt_results[i, 11] <- round(runif(1, 20, 100))
    expt_results[i, 12:15] <- sample(c(0,1), 4, replace = TRUE)
    inputs_for_demog_ch <<- c(1, expt_results[i, 1:])
    print(inputs_for_demog_ch)
    model_data <- genetic_algorithm_for_optimal(drug_vars, radiation_vars)
    df <- model_data[["df"]]
    expt_results[i, "best_combi"] <- df[1, "combination"] 
  }
  expt_results
}



