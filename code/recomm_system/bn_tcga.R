construct_bn <- function()
{
  library(bnlearn)
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

  factor_vars <- c(demog_vars, case_history_vars, "vital_status")
  numeric_vars <- c(drug_vars, radiation_vars, "age_at_initial_pathologic_diagnosis", "karnofsky_performance_score")
  factor_vars <- factor_vars[!factor_vars %in% numeric_vars]

  for (column in factor_vars)
  {
    dense_matrix[, column] <- factor(dense_matrix[, column], levels = unique(dense_matrix[, column]))
  }
  for (column in numeric_vars)
  {
    dense_matrix[, column] <- as.numeric(dense_matrix[, column])
  }

  #Create the blacklist
  blacklist <- expand.grid(demog_vars, demog_vars)

  df <- expand.grid(case_history_vars, case_history_vars)
  blacklist <- rbind(blacklist, df)

  df <- expand.grid(drug_vars, drug_vars)
  blacklist <- rbind(blacklist, df)

  df <- expand.grid(radiation_vars, radiation_vars)
  blacklist <- rbind(blacklist, df)

  #There should be no outgoing arrows from vital_status, only incoming arrows to it
  df <- data.frame("Var1" = "vital_status", "Var2" = c(factor_vars, numeric_vars))
  blacklist <- rbind(blacklist, df)

  #There should be no incoming arrows to any of the demographic variables, only outgoing arrows
  df <- expand.grid(c(case_history_vars, drug_vars, radiation_vars), demog_vars)
  blacklist <- rbind(blacklist, df)

  colnames(blacklist) <- c("from", "to")

  #Construct the network structure
  res = hc(dense_matrix , blacklist = blacklist, optimized = FALSE)
  #Fit the parameters of the Bayesian network, conditional on its structure
  fitted = bn.fit(res, dense_matrix)
  list("res" = res, "fitted" = fitted)
}

process_vital_status <- function(status)
{
  if (status == 'Alive' || status == 'LIVING') 
    return('Alive')
  return('Dead')
}

#Turn all drug and radiation-related variables to discrete ones
construct_bn_mostly_discrete <- function()
{
  library(bnlearn)
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

  #Create the blacklist
  blacklist <- expand.grid(demog_vars, demog_vars)

  df <- expand.grid(case_history_vars, case_history_vars)
  blacklist <- rbind(blacklist, df)

  df <- expand.grid(drug_vars, drug_vars)
  blacklist <- rbind(blacklist, df)

  df <- expand.grid(radiation_vars, radiation_vars)
  blacklist <- rbind(blacklist, df)

  #There should be no outgoing arrows from vital_status, only incoming arrows to it
  df <- data.frame("Var1" = "vital_status", "Var2" = c(factor_vars, numeric_vars))
  blacklist <- rbind(blacklist, df)

  #There should be no incoming arrows to any of the demographic variables, only outgoing arrows
  df <- expand.grid(c(case_history_vars, drug_vars, radiation_vars), demog_vars)
  blacklist <- rbind(blacklist, df)

  #There should be no arrows from drug variables to case history variables: other direction is OK
  df <- expand.grid(drug_vars, case_history_vars)
  blacklist <- rbind(blacklist, df)

  #There should be no arrows from radiation variables to case history variables: other direction is OK
  df <- expand.grid(radiation_vars, case_history_vars)
  blacklist <- rbind(blacklist, df)

  colnames(blacklist) <- c("from", "to")

  sink(paste(file_path, "/", "debug_text.txt", sep = ""))
  #Construct the network structure
  res = hc(dense_matrix , blacklist = blacklist, 
             #whitelist = whitelist, 
             debug = FALSE)
  #Fit the parameters of the Bayesian network, conditional on its structure
  fitted = bn.fit(res, dense_matrix, debug = FALSE)
  #plot(res, highlight = c("vital_status", mb(res, "vital_status")))
  
  custom_hill_climbing_for_optimal(drug_vars, radiation_vars, fitted)
  sink()
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

#Node is input as bit_string. Generate the evidence for node, do CP query and return result.
evaluate_node <- function(bit_string, treatment_options, n_options, fitted)
{
  evidence <- convert_bit_string_to_evidence(bit_string, treatment_options, n_options)
  event <- "(vital_status == 'Alive')"
  cat("\n")
  cat(paste("evidence = ", evidence, "\n", sep = ""))
  #cpquery uses logic sampling by default
  cpquery_expn <- paste("cpquery(fitted, ", event, ", ", evidence, ")", sep = "")
  eval(parse(text = cpquery_expn))
}

custom_hill_climbing_for_optimal <- function(drug_vars, radiation_vars, fitted)
{
  treatment_options <- sort(append(drug_vars, radiation_vars))
  n_options <- length(treatment_options)
  bit_string <- rep(FALSE, length(treatment_options))

  #pick an option at random, to start with. A node is an assignment of values to the evidence variables.
  current <- sample(treatment_options, 1)
  bit_string[which(treatment_options == current)] <- TRUE
 
  print(current)
  
  
  while (TRUE)
  {
    current_val <- evaluate_node(bit_string, treatment_options, n_options, fitted)
    cat(paste("current evidence = ", convert_bit_string_to_evidence(bit_string, treatment_options, n_options), ", current_val = ", current_val, "\n", sep = ""))
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
          this_neighbor_val <- evaluate_node(neighbor_bit_string, treatment_options, n_options, fitted)
          cat(paste("this_neighbor_val = ", this_neighbor_val, "\n", sep = ""))
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
    #Setting current to highest scoring neighbor for the next iteration. bit_string represents the current
    #node, except within the foor loop
    bit_string <- highest_scoring_neighbor
    cat(paste("score rising to = ", max_score_from_neighbor, ", new current is ", convert_bit_string_to_evidence(bit_string, treatment_options, n_options), "\n", sep = ""))
    if (max_score_from_neighbor == 1)
    {
      return(highest_scoring_neighbor)
    }
  }
}


hill_climbing_for_optimal <- function(drug_vars, radiation_vars, fitted, dense_matrix)
{
  #Search for the optimal treatment option 
  library(FSelector)
  treatment_options <- append(drug_vars, radiation_vars)

  evaluator <- function(subset) {
    #Conditional probability query with a random subset of treatment_options (and their values chosen at random) being used as the evidence
    evidence <- ""
    #Combine at most one drug and at most one radiation type
    drug_chosen <- FALSE
    radiation_chosen <- FALSE

    for (option in subset)
    {
       if (((option %in% drug_vars) & !drug_chosen) | ((option %in% radiation_vars) & !radiation_chosen))
       { 
         starts_with <- ifelse((evidence == ""), "", " & ")
         value_of_var <- sample(levels(dense_matrix[, option]), 1)
         evidence <- paste(evidence, starts_with, "(", option, " == '", value_of_var, "')", sep = "")
         if (option %in% drug_vars) drug_chosen = TRUE
         if (option %in% radiation_vars) radiation_chosen = TRUE
       }
    }
    event <- "(vital_status == 'Alive')"
    #cpquery uses logic sampling by default
    cpquery_expn <- paste("cpquery(fitted, ", event, ", ", evidence, ")", sep = "")
    cond_prob <- eval(parse(text = cpquery_expn))
    cat(paste("evidence = ", evidence, "\n", sep = ""))
    cat(paste("cond_prob = ", cond_prob, "\n", sep = ""))
    return(cond_prob)
  }
 subset <- hill.climbing.search(treatment_options, evaluator) 
 cat("The most optimal treatment options are\n")
 print(subset)
}

test_cp_values <- function(fitted)
{
  #Values returned by cpquery are different in different runs since it is generated by logic sampling. Values in CPT tables are 
  #MLE estimates, and hence are derived simply from counts.
  prob <-  cpquery(fitted, (vital_status == 'Alive'), (person_neoplasm_cancer_status == 'WITH TUMOR') & (Irinotecan == 'TRUE')) #0.84375 whereas CPT table says 0.83333333 
  cat(paste("prob = ", prob, "\n", sep = ""))

  ss <- subset(dense_matrix, (person_neoplasm_cancer_status == 'WITH TUMOR') & (Irinotecan == 'TRUE'))
  fss <- subset(dense_matrix, (vital_status == 'Alive') & (person_neoplasm_cancer_status == 'WITH TUMOR') & (Irinotecan == 'TRUE'))  #0.833333333333333
  cat(paste("from counts, ", nrow(fss)/nrow(ss), "\n", sep = ""))

  prob <-  cpquery(fitted, (Avastin == 'TRUE'), (initial_pathologic_diagnosis_method == 'Tumor resection') & (OTHER..SPECIFY.IN.NOTES == 'FALSE'))
  cat(paste("prob = ", prob, "\n", sep = "")) #0.071498 whereas CPT table says 0.07185629

  ss <- subset(dense_matrix, (initial_pathologic_diagnosis_method == 'Tumor resection') & (OTHER..SPECIFY.IN.NOTES == 'FALSE'))
  fss <- subset(dense_matrix, (Avastin == 'TRUE') & (initial_pathologic_diagnosis_method == 'Tumor resection') & (OTHER..SPECIFY.IN.NOTES == 'FALSE')) #0.0718562874251497
  cat(paste("from counts, ", nrow(fss)/nrow(ss), "\n", sep = ""))

  prob <-  cpquery(fitted, (EXTERNAL.BEAM == 'FALSE'), (Gliadel.Wafer == 'FALSE'))
  cat(paste("prob = ", prob, "\n", sep = "")) #0.0112845 whereas CPT table says 0.01310044

  prob <- cpquery(fitted, (vital_status == 'Alive'), (age_at_initial_pathologic_diagnosis > 50) & (age_at_initial_pathologic_diagnosis < 60) & (ethnicity == 'NOT HISPANIC OR LATINO'))
  cat(paste("prob = ", prob, "\n", sep = "")) #Returns 0.27122464312547 for age in (50, 60), returns 0 when age is set to a fixed value: example of a continuos parent of a discrete node

  ss <- subset(dense_matrix, (age_at_initial_pathologic_diagnosis > 50) & (age_at_initial_pathologic_diagnosis < 60) & (ethnicity == 'NOT HISPANIC OR LATINO'))
  fss <- subset(dense_matrix, (vital_status == 'Alive') & (age_at_initial_pathologic_diagnosis > 50) & (age_at_initial_pathologic_diagnosis < 60) & (ethnicity == 'NOT HISPANIC OR LATINO')) 
  cat(paste("from counts, ", nrow(fss)/nrow(ss), "\n", sep = "")) #Returns 0.238805970149254 (based on actual count)

  prob <- cpquery(fitted, (vital_status == 'Alive'), (Avastin == 'TRUE'))
  cat(paste("prob = ", prob, "\n", sep = "")) #Returns 0.316728167281673: example of a CP query where event and evidence variables do not have an arc between in the structure 

  prob <- cpquery(fitted, (Avastin == 'TRUE'), (initial_pathologic_diagnosis_method == 'Excisional Biopsy') & (OTHER..SPECIFY.IN.NOTES == 'TRUE')) #Example when there are no values at all in data
  cat(paste("prob = ", prob, "\n", sep = "")) #Returns 0 always
}

#Test the accuracy of the model by randomly performing conditional probability queries, 
#for queries which have a good enough support 
test_bn <- function(fitted)
{
  node_names <- names(fitted)
  combined_table <- data.frame(matrix(ncol = 5))
  colnames(combined_table) <- c("event", "evidence1", "evidence2", "cpquery_result", "actual_cp")
  threshold <- 0.1
  rownumber <- 1

  for (node_name in node_names)
  {
    #fitted[[<node_name>]] is an object of class bn.fit.dnode, or bn.fit.gnode or bn.fit.cgnode
    if (class(fitted[[node_name]]) == 'bn.fit.dnode')
    {
      cptable <- as.data.frame(fitted[[node_name]][["prob"]]) 
      if (ncol(cptable) == 4)
      {
        n_cptable <- nrow(cptable)
        cols_cptable <- colnames(cptable)
        for (i in 1:n_cptable)
        {
          if (is.finite(cptable[i, "Freq"]) & cptable[i, "Freq"] >= threshold)
          {
            event <- paste("(",  cols_cptable[1], " == '", cptable[i, 1], "')", sep = "")
            combined_table[rownumber, "event"] <- event

            evidence1 <- paste("(",  cols_cptable[2], " == '", cptable[i, 2], "')", sep = "")
            combined_table[rownumber, "evidence1"] <- evidence1

            evidence2 <- paste("(",  cols_cptable[3], " == '", cptable[i, 3], "')", sep = "")
            combined_table[rownumber, "evidence2"] <- evidence2

            combined_table[rownumber, "actual_cp"] <- cptable[i, "Freq"]

            cpquery_expn <- paste("cpquery(fitted, ", event, ", ", evidence1, " & ", evidence2, ")", sep = "")
            cond_prob <- eval(parse(text = cpquery_expn))
            combined_table[rownumber, "cpquery_result"] <- cond_prob

            rownumber <- rownumber + 1
          }
        }
      }
    }
  }
  combined_table$percent_error <- 100*(abs(combined_table$cpquery_result - combined_table$actual_cp))/combined_table$actual_cp
  print(combined_table)
}

