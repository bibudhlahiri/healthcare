process_vital_status <- function(status)
{
  if (status == 'Alive' || status == 'LIVING') 
    return('TRUE')
  return('FALSE')
}

print_network <- function(adjacency_matrix)
{
  names <- rownames(adjacency_matrix)
  for (node in names)
  {
    neighbors <- which(adjacency_matrix[node, ] != 0)
    if (length(neighbors) > 0)
    {
      cat(paste("node = ", node, ", neighbors = ", "\n", sep = ""))
      print(names(neighbors))
    }
  }
}


#Perform Belief Propagation for inference. Steps are:
# 1) Moralize the parents in the Bayesian Network. Converting the DAG to an undirected graph
#    is part of moralizing.
# 2) Triangulate the undirected graph
# 3) Create a Junction Tree out of the triangulated graph, by extracting the max cliques from the chordal graph 
#    and extracting their maximum-spanning clique tree.
#    See here: http://www.cs.cmu.edu/~guestrin/Class/10708/recitations/r7/Clique_Trees_2slides.pdf
# 4) Perform Belief Propagation over the Junction Tree in a good message passing order
#    that ensures convergence and accuracy

moralize <- function(bn)
{
  res <- bn[["res"]]
  nodes <- names(res$nodes)
  n_vars <- length(nodes)
  adjacency <- matrix(0, nrow = n_vars, ncol = n_vars)
  rownames(adjacency) <- nodes
  colnames(adjacency) <- nodes

  n_arcs <- nrow(res$arcs)
  for (i in 1:n_arcs)
  {
    from <- as.character(res$arcs[i, "from"])
    to <- as.character(res$arcs[i, "to"])
    adjacency[from, to] <- 1
  }
  #adjacency <- matrix(c(0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), nrow = 5, byrow = TRUE)
  for (i in 1:n_vars)
  {
    #i is the current column index
    for (j in 1:n_vars)
    {
      for (k in 1:n_vars)
      {
        #(j, k) indicate a pair of indices in the adjacency matrix
        if (j != i & k != i & j != k & adjacency[j, i] == 1 & adjacency[k, i] == 1)
        {
           #Both X_j and X_k are parents of X_i. Marry them.
           adjacency[j, k] <- 1
           adjacency[k, j] <- 1
        }
      }
    }
  }
  #Make the graph an undirected one
  for (i in 1:n_vars)
  {
    #i is the current column index
    for (j in 1:n_vars)
    {
      if (adjacency[i, j] == 1 & adjacency[j, i] == 0)
      {
        adjacency[j, i] <- 1
      } 
    }
  }
  adjacency
}

#Given an undirected graph, convert it to a chordal graph: an undirected graph is said to be chordal or triangulated if 
#and only if every cycle of length four or more has an arc between a pair of nonadjacent nodes.
triangulate <- function(adjacency)
{
  set.seed(1)
  if (FALSE)
  {
    adjacency <- matrix(c(0, 1, 0, 0, 1, 
                        1, 0, 1, 0, 0, 
                        0, 1, 0, 1, 0, 
                        0, 0, 1, 0, 1, 
                        1, 0, 0, 1, 0), nrow = 5, byrow = TRUE)
    rownames(adjacency) <- c("a", "b", "c", "d", "e")
    colnames(adjacency) <- c("a", "b", "c", "d", "e")
  }
  nodes <- rownames(adjacency)
  n_vars <- length(nodes)
  #Create a random permutation of the node variables
  permutation <- sample(1:n_vars, n_vars)
  #permutation <- 1:n_vars
  triangulated <- adjacency
  
  for (i in permutation)
  {
    #i is an integer, not a name
    #Take two random neighbors of X_sigma_i and add a link between them in triangulated 
    neighbors_i <- nodes[which(adjacency[i, ] == 1)]
    if (length(neighbors_i) >= 2)
    {
      pair_of_neighbors <- sample(neighbors_i, 2)
      #Connect the pair of neighbors of i in adjacency
      adjacency[pair_of_neighbors[1], pair_of_neighbors[2]] <- 1
      adjacency[pair_of_neighbors[2], pair_of_neighbors[1]] <- 1
      #Connect the pair of neighbors of i in triangulated
      triangulated[pair_of_neighbors[1], pair_of_neighbors[2]] <- 1
      triangulated[pair_of_neighbors[2], pair_of_neighbors[1]] <- 1
    }
    #Remove node X_sigma_i and the edges related to it from adjacency
    adjacency[i, ] <- 0
    adjacency[, i] <- 0
  }
  triangulated
}


max_cliques_and_their_graph <- function(adjacency)
{
  if (FALSE)
  {
    #The wikipedia example graph
    adjacency <- matrix(c(0, 1, 0, 0, 1, 0,  
                        1, 0, 1, 0, 1, 0,
                        0, 1, 0, 1, 0, 0,
                        0, 0, 1, 0, 1, 1,
                        1, 1, 0, 1, 0, 0,
                        0, 0, 0, 1, 0, 0), nrow = 6, byrow = TRUE)
    rownames(adjacency) <- colnames(adjacency) <- as.character(1:6)
  }
  P <- rownames(adjacency) 
  R <- c()
  X <- c()
  cliques <<- list()
  bron_kerbosch(adjacency, R, P, X)
  create_graph_of_cliques(cliques)
}

#The Bron-Kerbosch maximal clique finding algorithm
bron_kerbosch <- function(adjacency, R, P, X)
{
  if (length(P) == 0 & length(X) == 0)
  {
    #Report R as a maximal clique
    cliques <<- append(cliques, list(R))
    return(R)
  }
  for (v in P)
  {
    N_v <- names(which(adjacency[v, ] == 1))
    bron_kerbosch(adjacency, union(R, v), intersect(P, N_v), intersect(X, N_v))
    P <- setdiff(P, v)
    X <- union(X, v)
  }
}

#Create a graph out of all maximal cliques: the cliques will be the nodes, and the edges will 
#have weights equal to the number of common vertices between two cliques
create_graph_of_cliques <- function(cliques)
{
  n_cliques <- length(cliques)
  adjacency <- matrix(0, nrow = n_cliques, ncol = n_cliques)
  for (i in 1:n_cliques)
  {
    for (j in 1:n_cliques)
    {
      if (i != j)
      {
        adjacency[i, j] <- length(intersect(cliques[[i]], cliques[[j]]))
      }
    }
  }
  adjacency
} 

#Implementing Prim’s algorithm to find minimum spanning tree
max_spanning_tree <- function(adjacency)
{
  if (FALSE)
  {
    #Example graph from CLR
    adjacency <- matrix(c(0, 4, 0, 0, 0, 0, 0, 8, 0,  
                        4, 0, 8, 0, 0, 0, 0, 11, 0,
                        0, 8, 0, 7, 0, 4, 0, 0, 2,
                        0, 0, 7, 0, 9, 14, 0, 0, 0,
                        0, 0, 0, 9, 0, 10, 0, 0, 0,
                        0, 0, 4, 14, 10, 0, 2, 0, 0,
                        0, 0, 0, 0, 0, 2, 0, 1, 6,
                        8, 11, 0, 0, 0, 0, 1, 0, 7,
                        0, 0, 2, 0, 0, 0, 6, 7, 0), nrow = 9, byrow = TRUE)
  }

  adjacency <- (-1)*adjacency
  n_vertices <- nrow(adjacency)
  parents <- rep(NA, n_vertices)
   
  #We start growing the tree from node 1. queue, a priority queue, is always kept sorted based on the key column
  queue <- data.frame(vertex = 1: n_vertices, key = c(0, rep(Inf, n_vertices-1)))
  rownames(queue) <- as.character(1: n_vertices)
  #print(queue)
  while (nrow(queue) > 0)
  {
    u <- queue[1, "vertex"]
    queue <- queue[-1, ]
    neighbors_u <- (1: n_vertices)[which(adjacency[u, ] != 0)]
    for (v in neighbors_u)
    {
      if ((as.character(v) %in% rownames(queue)) & (adjacency[u, v] < queue[as.character(v), "key"]))
      {
        parents[v] <- u
        queue[as.character(v), "key"] <- adjacency[u, v]
        queue <- queue[order(queue[, "key"]),]
      } 
    }
  }
  parents
}

clique_as_string <- function(nodes)
{
  paste(nodes, sep = '', collapse = '_')
}

create_junction_tree <- function(bn)
{
  adjacency <- moralize(bn)
  triangulated <- triangulate(adjacency)
  clique_adjacency <- max_cliques_and_their_graph(triangulated)
  parents <- max_spanning_tree(clique_adjacency)
  for (i in 1:length(parents))
  {
    cat(paste("parent of ", clique_as_string(cliques[i]), " is ", clique_as_string(cliques[parents[i]]), "\n", sep = ""))
  }
}

#Perform Gibbs sampling (MCMC) for approximate inference: conditional probability queries given some evidence
gibbs_sampling <- function(bit_string, treatment_options, n_options, demog_ch_vars, fitted, res)
{
  #The set of states for the Markov chain comprises of the non-observed variables and the event variable
  states_markov_chain <- c(treatment_options[which(bit_string == FALSE)], 'vital_status')
  evidence_vars <- treatment_options[which(bit_string == TRUE)]
  n_states_markov_chain <- length(states_markov_chain)
  samples <- data.frame(matrix(ncol = n_states_markov_chain))
  colnames(samples) <- states_markov_chain

  #Set the values of all non-observed variables and the event variable (vital_status) randomly
  samples[1, ] <- sample(c('TRUE', 'FALSE'), ncol(samples), replace = TRUE)
  
  #Sequentially sample for the variables in states_markov_chain: this is where actually drawing from the Markov chain occurs
  i <- 1
  row_number <- 2
  n_trials <- 30000
  #while (TRUE)
  for (k in 1:(n_trials - 1))
  {
    samples[row_number, ] <- samples[row_number - 1, ]
    samples[row_number, i] <- draw_sample_for_one_variable(res, fitted, states_markov_chain, evidence_vars, demog_ch_vars, i, samples[row_number - 1, ])
    i <- ifelse((i == n_states_markov_chain), 1, (i+1))
    row_number <- row_number + 1
  }
  test_convergence(samples)
  burn_in <- 1000
  #How many initial samples to discard to give the mixin time
  samples <- samples[(burn_in + 1):nrow(samples), ]
  #Take every 100-th or so because successive samples are not independent of each other
  step_size <- 100
  samples <- samples[seq(1, nrow(samples), by = step_size), ]
  nrow(subset(samples, (vital_status == 'TRUE')))/nrow(samples)
}

#Convergence diagnostics for the Markov chain that arises in Gibbs sampling
test_convergence <- function(samples)
{
  window_length <- 2000
  n_samples <- nrow(samples)
  window_start <- 1
  window_end <- window_start + window_length - 1 

  while (window_end < n_samples)
  {
    ss <- subset(samples[window_start:window_end, ], (vital_status == 'TRUE'))
    prob_est <- nrow(ss)/window_length
    cat(paste("n_samples = ", n_samples, ", window_length = ", window_length, ", window_start = ", window_start, ", window_end = ", window_end, ", prob_est = ", prob_est, "\n", sep = ""))
    window_start <- window_end + 1
    window_end <- window_start + window_length - 1
  }
}

#Generate a sample for the variable given by var_index, given the assignments to all the 
#other variables at that point in time
draw_sample_for_one_variable <- function(res, fitted, states_markov_chain, evidence_vars, demog_ch_vars, var_index, var_assignments)
{
  target_var <- states_markov_chain[var_index]

  #Find values for P(X = 'TRUE'/Parents(X)) and product of P(Y/Parents(Y)) for all children Y of X. 
  #Also find P(X = 'FALSE'/Parents(X)) and product of P(Y/Parents(Y)) for all children Y of X.
  #These will be computed only upto a constant factor and then normalized.

  parents <- res$nodes[[states_markov_chain[var_index]]][["parents"]]
  children <- setdiff(res$nodes[[states_markov_chain[var_index]]][["nbr"]], parents)
  true_prob <- find_prob_for_one_value(target_var, parents, children, evidence_vars, demog_ch_vars, res, fitted, 'TRUE', var_assignments, states_markov_chain)
  false_prob <- find_prob_for_one_value(target_var, parents, children, evidence_vars, demog_ch_vars, res, fitted, 'FALSE', var_assignments, states_markov_chain)
  sum <- true_prob + false_prob
  true_prob <- true_prob/sum
  false_prob <- false_prob/sum
  rand_val <- runif(1)
  sampled_val <- ifelse(rand_val <= true_prob, 'TRUE', 'FALSE') 
  sampled_val
}

find_prob_for_one_value <- function(target_var, parents, children, evidence_vars, demog_ch_vars, res, fitted, target_var_val, var_assignments, states_markov_chain)
{
  prob <- 1
  df <- as.data.frame(fitted[[target_var]][["prob"]])
  #ss <- paste("(subset(df, ((", target_var, "== '", target_var_val, "')", sep = "")
  ss <- paste("df[df$", target_var, " == '", target_var_val, "'", sep = "")
  if (length(res$nodes[[target_var]][["parents"]]) == 0)
  {
    colnames(df) <- c(target_var, "Freq")
  }
  for (parent in parents)
  {
     if (parent %in% evidence_vars)
     {
       parent_val <- 'TRUE'
     }
     else if (parent %in% names(demog_ch_vars))
     {
       parent_val <- demog_ch_vars[[parent]]
     }
     else
     {
       parent_val <- var_assignments[which(states_markov_chain == parent)]
     }
     #ss <- paste(ss, " & (", parent, " == '", parent_val, "')", sep = "")
     ss <- paste(ss, " & df$", parent, " == '", parent_val, "'", sep = "")
  }
  #ss <- paste(ss, ")))$Freq", sep = "")
  ss <- paste(ss, ", ]$Freq", sep = "")
  exprssn <- parse(text = ss)
  #Metioning the environment for eval explicitly to avoid 'object not found'
  evaluated <- eval(exprssn, df)
  prob <- prob*evaluated
  for (child in children)
  {
    #Find P(Y/Parents(Y)) for each child Y of X
    df <- as.data.frame(fitted[[child]][["prob"]])
    if (child %in% evidence_vars)
    {
      child_val <- 'TRUE'
    }
    #A demographic or case history variable will not be a child of any node outside these two groups,
    #because of our blacklist
    else
    {
     child_val <- var_assignments[which(states_markov_chain == child)]
    }
    ss <- paste("(subset(df, ((", child, " == '", child_val, "')", sep = "")
    parents_of_child <- res$nodes[[child]][["parents"]]
    for (parent_of_child in parents_of_child)
    {
      if (parent_of_child == target_var)
      {
        ss <- paste(ss, " & (", target_var, " == '", target_var_val, "')", sep = "") 
      }
      else if (parent_of_child %in% evidence_vars)
      {
        parent_of_child_val <- 'TRUE'
        ss <- paste(ss, " & (", parent_of_child, " == '", parent_of_child_val, "')", sep = "")
      }
      else if (parent_of_child %in% names(demog_ch_vars))
      {
       parent_of_child_val <- demog_ch_vars[[parent_of_child]]
       ss <- paste(ss, " & (", parent_of_child, " == '", parent_of_child_val, "')", sep = "")
      }
      else
      {
        parent_of_child_val <- var_assignments[which(states_markov_chain == parent_of_child)]
        ss <- paste(ss, " & (", parent_of_child, " == '", parent_of_child_val, "')", sep = "")
      }
    }
    ss <- paste(ss, ")))$Freq", sep = "")
    exprssn <- parse(text = ss)
    evaluated <- eval(exprssn, df)
    prob <- prob*evaluated
  }
  ifelse((length(prob) == 0), 0, prob)
}

#Turn all drug and radiation-related variables to discrete ones
construct_bn_mostly_discrete <- function(method = "ls")
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

  #sink(paste(file_path, "/", "debug_text.txt", sep = ""))
  #Construct the network structure
  res = hc(dense_matrix , blacklist = blacklist, 
             #whitelist = whitelist, 
             debug = FALSE)
  #Fit the parameters of the Bayesian network, conditional on its structure
  fitted = bn.fit(res, dense_matrix, debug = FALSE)
  #plot(res, highlight = c("vital_status", mb(res, "vital_status")))
  
  #test_cp_values(dense_matrix, fitted)
  #custom_hill_climbing_for_optimal(drug_vars, radiation_vars, fitted, res, dense_matrix, method)
  #sink()
  bn <- list("res" = res, "fitted" = fitted)
  create_junction_tree(bn)
  bn
}


convert_bit_string_to_evidence <- function(bit_string, treatment_options, n_options, method = "ls")
{
  if (method == "ls")
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
  }
  else 
  {
    evidence <- "list("
    for (j in 1:n_options)
    {
     if (bit_string[j])
     {
       starts_with <- ifelse((evidence == "list("), "", " , ")
       evidence <- paste(evidence, starts_with, "'", treatment_options[j], "' = 'TRUE'", sep = "")
     }
    }
    evidence <- paste(evidence, ")", sep = "")
  }
  evidence
}

#Node is input as bit_string. Generate the evidence for node, do CP query and return result.
evaluate_node <- function(bit_string, treatment_options, n_options, fitted, method = "ls")
{
  evidence <- convert_bit_string_to_evidence(bit_string, treatment_options, n_options, method)
  event <- "(vital_status == 'TRUE')"
  #cpquery uses logic sampling by default
  cpquery_expn <- paste("cpquery(fitted, event = ", event, ", evidence = ", evidence,
  #Likelihood weighting returns NaN as conditional probability in most cases, since event has a probability mass of NaN out of NaN, 
  #after generating 10,000 samples from the conditional distribution involving the upper closure of the involved nodes.
  #Logic sampling works fine. 
                        ", method = '", method, "'", 
                        ", debug = FALSE)", sep = "")
  cat(paste("cpquery_expn = ", cpquery_expn, "\n", sep = ""))
  cond_prob <- eval(parse(text = cpquery_expn))
  cat(paste("cond_prob = ", cond_prob, "\n", sep = ""))
  cond_prob
}

custom_hill_climbing_for_optimal <- function(drug_vars, radiation_vars, fitted, res, dense_matrix, method = "ls")
{
  treatment_options <- sort(append(drug_vars, radiation_vars))
  n_options <- length(treatment_options)
  bit_string <- rep(FALSE, length(treatment_options))

  #pick an option at random, to start with. A node is an assignment of values to the evidence variables.
  current <- sample(treatment_options, 1)
  bit_string[which(treatment_options == current)] <- TRUE
  current_val <- evaluate_node(bit_string, treatment_options, n_options, fitted, method)
  #Fix some values for the demographic and case history variables, for now
  demog_ch_vars <- list("age_at_initial_pathologic_diagnosis" = 45, "ethnicity" = 'NOT HISPANIC OR LATINO', "gender" = 'FEMALE', "race" = 'WHITE', 
                        "histological_type" = 'Untreated primary (de novo) GBM', "history_of_neoadjuvant_treatment" = 'No', 
                        "initial_pathologic_diagnosis_method" = 'Tumor resection', "karnofsky_performance_score" = 50, 
                         "person_neoplasm_cancer_status" = 'WITH TUMOR', "prior_glioma" = 'NO')
  #current_val <- gibbs_sampling(bit_string, treatment_options, n_options, demog_ch_vars, fitted, res)
 
  while (TRUE)
  {
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
          #this_neighbor_val <- gibbs_sampling(neighbor_bit_string, treatment_options, n_options, demog_ch_vars, fitted, res)
          this_neighbor_val <- evaluate_node(neighbor_bit_string, treatment_options, n_options, fitted, method)
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
    #Setting current to highest scoring neighbor for the next iteration. bit_string represents the current node
    bit_string <- highest_scoring_neighbor
    current_val <- max_score_from_neighbor
    cat(paste("score rising to = ", max_score_from_neighbor, ", new current is ", convert_bit_string_to_evidence(bit_string, treatment_options, n_options), "\n", sep = ""))
    if (max_score_from_neighbor == 1)
    {
      ss_query_expn <- paste("nrow(subset(dense_matrix, ",  convert_bit_string_to_evidence(bit_string, treatment_options, n_options), "))", sep = "")
      n_ss <- eval(parse(text = ss_query_expn))
      cat(paste("ss size = ", n_ss, "\n", sep = ""))

      fss_query_expn <- paste("nrow(subset(dense_matrix, (vital_status == 'TRUE') & ",  convert_bit_string_to_evidence(bit_string, treatment_options, n_options), "))", sep = "")
      n_fss <- eval(parse(text = ss_query_expn))
      cat(paste("fss size = ", n_fss, "\n", sep = ""))

      cat(paste("from counts, ", n_fss/n_ss, "\n", sep = ""))
      return(highest_scoring_neighbor)
    }
  }
}


test_cp_values <- function(dense_matrix, fitted)
{
  #Values returned by cpquery are different in different runs since it is generated by logic sampling. Values in CPT tables are 
  #MLE estimates, and hence are derived simply from counts.
  prob <-  cpquery(fitted, (vital_status == 'TRUE'), (person_neoplasm_cancer_status == 'WITH TUMOR') & (Irinotecan == 'TRUE')) #0.84375 whereas CPT table says 0.83333333 
  cat(paste("prob = ", prob, "\n", sep = ""))

  ss <- subset(dense_matrix, (person_neoplasm_cancer_status == 'WITH TUMOR') & (Irinotecan == 'TRUE'))
  fss <- subset(dense_matrix, (vital_status == 'TRUE') & (person_neoplasm_cancer_status == 'WITH TUMOR') & (Irinotecan == 'TRUE'))  #0.833333333333333
  cat(paste("from counts, ", nrow(fss)/nrow(ss), "\n", sep = ""))

  prob <-  cpquery(fitted, (Avastin == 'TRUE'), (initial_pathologic_diagnosis_method == 'Tumor resection') & (OTHER..SPECIFY.IN.NOTES == 'FALSE'))
  cat(paste("prob = ", prob, "\n", sep = "")) #0.071498 whereas CPT table says 0.07185629

  ss <- subset(dense_matrix, (initial_pathologic_diagnosis_method == 'Tumor resection') & (OTHER..SPECIFY.IN.NOTES == 'FALSE'))
  fss <- subset(dense_matrix, (Avastin == 'TRUE') & (initial_pathologic_diagnosis_method == 'Tumor resection') & (OTHER..SPECIFY.IN.NOTES == 'FALSE')) #0.0718562874251497
  cat(paste("from counts, ", nrow(fss)/nrow(ss), "\n", sep = ""))

  prob <-  cpquery(fitted, (EXTERNAL.BEAM == 'FALSE'), (Gliadel.Wafer == 'FALSE'))
  cat(paste("prob = ", prob, "\n", sep = "")) #0.0112845 whereas CPT table says 0.01310044

  prob <- cpquery(fitted, (vital_status == 'TRUE'), (age_at_initial_pathologic_diagnosis > 50) & (age_at_initial_pathologic_diagnosis < 60) & (ethnicity == 'NOT HISPANIC OR LATINO'))
  cat(paste("prob = ", prob, "\n", sep = "")) #Returns 0.27122464312547 for age in (50, 60), returns 0 when age is set to a fixed value: example of a continuos parent of a discrete node

  ss <- subset(dense_matrix, (age_at_initial_pathologic_diagnosis > 50) & (age_at_initial_pathologic_diagnosis < 60) & (ethnicity == 'NOT HISPANIC OR LATINO'))
  fss <- subset(dense_matrix, (vital_status == 'TRUE') & (age_at_initial_pathologic_diagnosis > 50) & (age_at_initial_pathologic_diagnosis < 60) & (ethnicity == 'NOT HISPANIC OR LATINO')) 
  cat(paste("from counts, ", nrow(fss)/nrow(ss), "\n", sep = "")) #Returns 0.238805970149254 (based on actual count)

  prob <- cpquery(fitted, (vital_status == 'TRUE'), (Avastin == 'TRUE'))
  cat(paste("prob = ", prob, "\n", sep = "")) #Returns 0.316728167281673: example of a CP query where event and evidence variables do not have an arc between in the structure 

  prob <- cpquery(fitted, (Avastin == 'TRUE'), (initial_pathologic_diagnosis_method == 'Excisional Biopsy') & (OTHER..SPECIFY.IN.NOTES == 'TRUE')) #Example when there are no values at all in data
  cat(paste("prob = ", prob, "\n", sep = "")) #Returns 0 always

  ss <- subset(dense_matrix, (External == 'TRUE') & (Irinotecan == 'TRUE') & (OTHER..SPECIFY.IN.NOTES == 'TRUE'))
  fss <- subset(dense_matrix, (vital_status == 'TRUE') & (External == 'TRUE') & (Irinotecan == 'TRUE') & (OTHER..SPECIFY.IN.NOTES == 'TRUE')) 
  cat(paste("from counts, nrow(fss) = ", nrow(fss), ", nrow(ss) = ", nrow(ss), ", ratio = ", nrow(fss)/nrow(ss), "\n", sep = ""))

  #The next three ratios are to check how accurately Gibbs sampling is working
  ss <- subset(dense_matrix, (Gliadel.Wafer == 'TRUE') & (Irinotecan == 'TRUE') & (Tamoxifen == 'TRUE'))
  fss <- subset(dense_matrix, (vital_status == 'TRUE') & (Gliadel.Wafer == 'TRUE') & (Irinotecan == 'TRUE') & (Tamoxifen == 'TRUE')) 
  cat(paste("from counts, nrow(fss) = ", nrow(fss), ", nrow(ss) = ", nrow(ss), ", ratio = ", nrow(fss)/nrow(ss), "\n", sep = ""))
  
  ss <- subset(dense_matrix, (Irinotecan == 'TRUE') & (Tamoxifen == 'TRUE'))
  fss <- subset(dense_matrix, (vital_status == 'TRUE') & (Irinotecan == 'TRUE') & (Tamoxifen == 'TRUE')) 
  cat(paste("from counts, nrow(fss) = ", nrow(fss), ", nrow(ss) = ", nrow(ss), ", ratio = ", nrow(fss)/nrow(ss), "\n", sep = ""))

  ss <- subset(dense_matrix, (Tamoxifen == 'TRUE'))
  fss <- subset(dense_matrix, (vital_status == 'TRUE') & (Tamoxifen == 'TRUE')) 
  cat(paste("from counts, nrow(fss) = ", nrow(fss), ", nrow(ss) = ", nrow(ss), ", ratio = ", nrow(fss)/nrow(ss), "\n", sep = ""))

  ss <- subset(dense_matrix, (Temozolomide == 'TRUE'))
  fss <- subset(dense_matrix, (vital_status == 'TRUE') & (Temozolomide == 'TRUE')) 
  cat(paste("from counts, nrow(fss) = ", nrow(fss), ", nrow(ss) = ", nrow(ss), ", ratio = ", nrow(fss)/nrow(ss), "\n", sep = ""))

  ss <- subset(dense_matrix, (Temozolomide == 'FALSE'))
  fss <- subset(dense_matrix, (vital_status == 'TRUE') & (Temozolomide == 'FALSE')) 
  cat(paste("from counts, nrow(fss) = ", nrow(fss), ", nrow(ss) = ", nrow(ss), ", ratio = ", nrow(fss)/nrow(ss), "\n", sep = ""))
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

