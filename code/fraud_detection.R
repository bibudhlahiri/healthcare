library(RPostgreSQL)
library(ggplot2)
library(plyr)
library(e1071)

process_five_number_summary <- function(fiveNumberSummary)
{
  return(paste("Q1 = ", round(fiveNumberSummary[2],2),
                    ", Median = ", round(fiveNumberSummary[3],2),
                    ", Q3 = ", round(fiveNumberSummary[4],2),
                    sep = ""))
}

prepare_data <- function(con)
{
  statement <- "select a.prcdr_cd, a.clm_id, ip.clm_pmt_amt, a.desynpuf_id, pc.long_desc, pc.short_desc, 1 as count
                from (select *
                      from transformed_claim_prcdr_codes tcpc
                      where tcpc.claim_type = 'inpatient'
                      and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                      and not exists (select 1 from transformed_claim_prcdr_codes tcpc1
                                      where tcpc.clm_id = tcpc1.clm_id
                                      and tcpc.prcdr_cd <> tcpc1.prcdr_cd)) a join inpatient_claims ip on (a.clm_id = ip.clm_id)
                                                                              left outer join procedure_codes pc on (a.prcdr_cd = pc.procedure_code)
                order by a.prcdr_cd"
  res <- dbSendQuery(con, statement)
  proc_exps <- fetch(res, n = -1)
  threshold <- 200
  #Take the procedures for which there are more than or equal claims than a threshold
  #aggdata <- as.data.frame(table(proc_exps$prcdr_cd))
  aggdata <- aggregate(x = proc_exps$count, by = list(proc_exps$prcdr_cd, proc_exps$long_desc, proc_exps$short_desc), FUN = sum, na.rm = TRUE)
  
  colnames(aggdata) <- c("prcdr_cd", "long_desc", "short_desc", "n_claims")
  aggdata <- aggdata[order(-aggdata[,"n_claims"]),]
  aggdata <- subset(aggdata, (n_claims >= threshold))
  print(aggdata)
  list("proc_exps" = proc_exps, "aggdata" = aggdata)
}

plot_histograms <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  data <- prepare_data(con)
  proc_exps <- data[["proc_exps"]]
  aggdata <- data[["aggdata"]]

  for (i in 1:nrow(aggdata))
  {
    this_prcdr_cd <- aggdata[i, "prcdr_cd"]
    exp_this_proc <- subset(proc_exps, (prcdr_cd == this_prcdr_cd))
    filename <- paste("./figures/fraud_detection/inpatient_procedures/", this_prcdr_cd, ".png")
    png(filename,  width = 600, height = 480, units = "px")
    fiveNumberSummary <- fivenum(exp_this_proc$clm_pmt_amt)
    axis_title <- paste("IP claim amounts for ", nrow(exp_this_proc), " claims", sep = "")
    p <- ggplot(exp_this_proc, aes(x = clm_pmt_amt)) + geom_histogram(aes(y = ..density..)) + geom_density() + 
         labs(x = paste(axis_title, process_five_number_summary(fiveNumberSummary), sep = "\n")) + ylab("Fraction of claims") 
    print(p)
    dev.off()
  }
  dbDisconnect(con)
}


ip_claim_amts_by_procedure <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  data <- prepare_data(con)
  proc_exps <- data[["proc_exps"]]
  aggdata <- data[["aggdata"]]

  #For the selected procedures, get the other details of the beneficiaries who underwent these procedures
  statement <- "select desynpuf_id, extract(year from age(to_date('2008-01-01', 'YYYY-MM-DD'), bene_birth_dt)) age,
                bene_sex_ident_cd, sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr, sp_copd, sp_depressn, 
                sp_diabetes, sp_ischmcht, sp_osteoprs, sp_ra_oa, sp_strketia
                from beneficiary_summary_2008"
  res <- dbSendQuery(con, statement)
  bene_details <- fetch(res, n = -1)
  #One novelty detection model for each procedure
  for (i in 1:nrow(aggdata))
  {
    this_prcdr_cd <- aggdata[i, "prcdr_cd"]
    exp_this_proc <- subset(proc_exps, (prcdr_cd == this_prcdr_cd))
    data_for_novelty <- merge(x = exp_this_proc, y = bene_details, all.x =  TRUE)
    cat(paste("this_prcdr_cd = ", this_prcdr_cd, ", nrow(data_for_novelty) = ", nrow(data_for_novelty), "\n", sep = ""))
    data_for_novelty <- data_for_novelty[,!(names(data_for_novelty) %in% c("prcdr_cd", "clm_id", 
                         "desynpuf_id", "long_desc", "short_desc", "count"))]
    #data_for_novelty <- data.frame(data_for_novelty[, c("clm_pmt_amt")])
    for (column in colnames(data_for_novelty))
    {    
      if (column != 'clm_pmt_amt' & column != 'age')
      {
        data_for_novelty[, column] <- as.numeric(data_for_novelty[, column] == '1')
      }
    }
    print(data_for_novelty[1:20, ])
    clf <- svm(~., data = data_for_novelty, type = "one-classification", nu = 0.1, kernel = "radial", gamma = 0.1) 
    p <- predict(clf, data_for_novelty)
    print(data_for_novelty$clm_pmt_amt)
    print(p)
  }
  dbDisconnect(con)
}

#Get the frequency distribution of number of procedures performed as a categorical distribution: one for each provider and one 
#for all providers together. Check how they differ.
providers_and_procedures <- function()
{
  library(entropy)
  #Use KL.empirical(y1, y2) from entropy library where y1 and y2 are bin counts.
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- "select pc.procedure_code, pc.long_desc, count(tcpc.*) total_freq
                from transformed_claim_prcdr_codes tcpc, procedure_codes pc
                where tcpc.claim_type = 'inpatient'
                and tcpc.prcdr_cd = pc.procedure_code
                group by pc.procedure_code, pc.long_desc
                order by pc.procedure_code"
  res <- dbSendQuery(con, statement)
  all_providers <- fetch(res, n = -1)
  #cat(paste("nrow(all_providers) = ", nrow(all_providers), "\n", sep = ""))
  #print(all_providers[1:5, ])

  statement <- "select ip.prvdr_num, pc.procedure_code, pc.long_desc, count(tcpc.*) provider_freq
                from transformed_claim_prcdr_codes tcpc, inpatient_claims ip, procedure_codes pc
                where tcpc.claim_type = 'inpatient'
                and tcpc.clm_id = ip.clm_id
                and tcpc.prcdr_cd = pc.procedure_code
                group by ip.prvdr_num, pc.procedure_code, pc.long_desc
                order by pc.procedure_code"
  res <- dbSendQuery(con, statement)
  specific_providers <- fetch(res, n = -1)

  unique_providers <- unique(specific_providers$prvdr_num)
  for (provider in unique_providers)
  {
    
    distn_this_prov <- subset(specific_providers, (prvdr_num == provider))
    #Make sure that all providers has one row for each possible procedure
    distn_this_prov <- merge(x = all_providers, y = distn_this_prov, all.x = TRUE)
    distn_this_prov <- distn_this_prov[,!(names(distn_this_prov) %in% c("total_freq", "prvdr_num"))]
    distn_this_prov[is.na(distn_this_prov)] <- 0 
    distn_this_prov <- distn_this_prov[order(distn_this_prov[,"procedure_code"]),]
    #cat(paste("nrow(distn_this_prov) = ", nrow(distn_this_prov), "\n", sep = ""))
    #print(distn_this_prov[1:5, ])
    KLdiv <- KL.empirical(all_providers$total_freq, distn_this_prov$provider_freq)
    cat(paste("provider = ", provider, ", KLdiv = ", KLdiv, "\n", sep = ""))
  }
  dbDisconnect(con)
}

prepare_data_all_together <- function()
{
 con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- "select ip.desynpuf_id, extract(year from age(to_date('2008-01-01', 'YYYY-MM-DD'), bene_birth_dt)) age,
                bene_sex_ident_cd, sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr, sp_copd, sp_depressn, 
                sp_diabetes, sp_ischmcht, sp_osteoprs, sp_ra_oa, sp_strketia, ip.prvdr_num, ip.clm_pmt_amt, a.prcdr_cd
                from (select *
                      from transformed_claim_prcdr_codes tcpc
                      where tcpc.claim_type = 'inpatient'
                      and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                      and not exists (select 1 from transformed_claim_prcdr_codes tcpc1
                                      where tcpc.clm_id = tcpc1.clm_id
                                      and tcpc.prcdr_cd <> tcpc1.prcdr_cd)) a, inpatient_claims ip, beneficiary_summary_2008 b, procedure_codes pc
                where a.clm_id = ip.clm_id
                and b.desynpuf_id = ip.desynpuf_id
                and a.prcdr_cd = pc.procedure_code
                order by a.prcdr_cd"
  res <- dbSendQuery(con, statement)
  all_data <- fetch(res, n = -1)
  dbDisconnect(con)
  all_data
}

novelty_all_data <- function()
{
  all_data <- prepare_data_all_together()
  for (column in colnames(all_data))
  {
    if (column != 'age' & column != 'clm_pmt_amt')
    {
      all_data[, column] <- as.factor(all_data[, column])
    }
  }
  clf <- svm(~., data = all_data, type = "one-classification", nu = 0.1, kernel = "radial", gamma = 0.1) 
  p <- predict(clf, all_data)
}

convert_factors_to_numeric <- function(all_data)
{
  binary_columns <- c("bene_sex_ident_cd", "sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr", "sp_copd", "sp_depressn", 
                      "sp_diabetes", "sp_ischmcht", "sp_osteoprs", "sp_ra_oa", "sp_strketia")
  for (column in binary_columns)
  {
    all_data[, column] <- as.numeric(all_data[, column] == '1')
  }
 
  providers <- unique(all_data$prvdr_num)
  n_providers <- length(providers)
  cat(paste("n_providers = ", n_providers, "\n", sep = ""))
  for (i in 1:(n_providers - 1))
  {
    colname <- paste("prov_", providers[i], sep = "")
    all_data[, colname] <- as.numeric(all_data$prvdr_num == providers[i])
  }

  procedures <- unique(all_data$prcdr_cd)
  n_procedures <- length(procedures)
  cat(paste("n_procedures = ", n_procedures, "\n", sep = ""))
  for (i in 1:(n_procedures - 1))
  {
    colname <- paste("proc_", procedures[i], sep = "")
    all_data[, colname] <- as.numeric(all_data$prcdr_cd == procedures[i])
  }

  all_data <- all_data[,!(names(all_data) %in% c("prvdr_num", "prcdr_cd"))]
  
}

diagnoses <<- data.frame()

fill_diagnoses <- function(desynpuf_id, dgns_cd)
{
  #cat(paste("desynpuf_id = ", desynpuf_id, ", dgns_cd = ", dgns_cd, "\n", sep = ""))
  diagnoses[desynpuf_id, dgns_cd] <<- 1
  diagnoses[desynpuf_id, "desynpuf_id"] <<- desynpuf_id
}

add_diagnoses_data <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- "select distinct tcdc.desynpuf_id, tcdc.dgns_cd
                from (select *
                      from transformed_claim_prcdr_codes tcpc
                      where tcpc.claim_type = 'inpatient'
                      and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                      and not exists (select 1 from transformed_claim_prcdr_codes tcpc1
		                      where tcpc.clm_id = tcpc1.clm_id
		                      and tcpc.prcdr_cd <> tcpc1.prcdr_cd)) a, transformed_claim_diagnosis_codes tcdc
                where a.desynpuf_id = tcdc.desynpuf_id
                and tcdc.clm_thru_year = to_char(a.clm_thru_dt, 'YYYY')
                order by tcdc.desynpuf_id"
  res <- dbSendQuery(con, statement)
  diagnoses_data <- fetch(res, n = -1)
  beneficiaries <- unique(diagnoses_data$desynpuf_id)
  n_beneficiaries <- length(beneficiaries)
  dgns_codes <- unique(diagnoses_data$dgns_cd)
  n_dgns_codes <- length(dgns_codes)
  cat(paste("n_beneficiaries = ", n_beneficiaries, ", n_dgns_codes = ", n_dgns_codes, "\n", sep = ""))

  dbDisconnect(con)
  diagnoses <- aggregate(diagnoses_data$dgns_cd, diagnoses_data['desynpuf_id'], paste, collapse = ', ')
  colnames(diagnoses) <- c("desynpuf_id", "diagnosed_features")
  write.csv(diagnoses, "/Users/blahiri/healthcare/documents/fraud_detection/diagnoses.csv")
}

pca_all_data <- function()
{
  #all_data <- prepare_data_all_together()
  #all_data <- convert_factors_to_numeric(all_data)
  #write.csv(all_data, "/Users/blahiri/healthcare/documents/fraud_detection/processed_all_data.csv")
  all_data <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/processed_all_data.csv")
  all_data <- all_data[,!(names(all_data) %in% c("X"))]
  cat(paste("nrow(all_data) = ", nrow(all_data), ", ncol(all_data) = ", ncol(all_data), "\n", sep = ""))
  #5672 rows, 2116 columns
  #print(all_data[1:5, ])
  pc <- prcomp(all_data, scale = TRUE)
  projected <- pc$x[, c("PC1", "PC2")]

  #if (FALSE)
  #{
   k <- 2
   cl <- kmeans(projected, k, nstart = 20)

   png("./figures/fraud_detection/inpatient_procedures/cluster_of_claims_first_two_pc.png",  width = 600, height = 480, units = "px")
   plot(projected, col = cl$cluster)
   points(cl$centers, col = 1:k, pch = 8, cex = 2)
   dev.off()
   cat("Sizes of the clusters are\n")
   print(cl$size)
   #3825 and 1847
  #}

  png("./figures/fraud_detection/inpatient_procedures/claims_first_two_pc.png",  width = 600, height = 480, units = "px")
  projected <- data.frame(projected)
  p <- ggplot(projected, aes(x = PC1, y = PC2)) + geom_point(size = 2) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Projections along first two PCs for claims")
  print(p)
  dev.off()
  
  #print(sort(pc$rotation[, "PC1"], decreasing = TRUE))
  #sp_chf, sp_chrnkidn, sp_copd, sp_diabetes, sp_ischmcht, sp_alzhdmta, sp_depressn, sp_strketia, sp_osteoprs, sp_cncr, sp_ra_oa, 
  #proc_3995, proc_3895, age, proc_4525, proc_5593, proc_9390, proc_3893, prov_0500JD 
  cl
}

#Transform back the dummy covariates to categorical variables before printing
transform_dummy_to_categ <- function(data)
{
 columns <- colnames(data)
 for (i in 1:nrow(data))
  {
    procedures <- c()
    providers <- c()
    for (column in columns)
    {
     if (substring(column, 1, 5) == 'prov_' & data[i, column] == 1)
     {
      providers <- append(providers, substring(column, 6, nchar(column)))
     }
     if (substring(column, 1, 5) == 'proc_' & data[i, column] == 1)
     {
      procedures <- append(procedures, substring(column, 6, nchar(column)))
     }
    }
    data[i, "procedures"] <- procedures
    data[i, "providers"] <- providers
  }
  for (column in columns)
    {
     if (substring(column, 1, 5) == 'prov_')
     {
       data <- data[,!(names(data) %in% c(column))]
     }
     if (substring(column, 1, 5) == 'proc_')
     {
       data <- data[,!(names(data) %in% c(column))]
     }
    }
  data
} 

cluster_all_data <- function(k = 2)
{
  #The initial cluster centers are chosen randomly, so fix it to repeat results.
  set.seed(1)
  #all_data <- prepare_data_all_together()
  #all_data <- convert_factors_to_numeric(all_data)
  #write.csv(all_data, "/Users/blahiri/healthcare/documents/fraud_detection/processed_all_data.csv")
  all_data <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/processed_all_data.csv")
  all_data <- all_data[,!(names(all_data) %in% c("X"))]
  scaled_data <- scale(all_data[,!(names(all_data) %in% c("desynpuf_id"))])

  #After scaling the data, for k = 2, betweenss/totss = 0.001 (clusters of size 8 and 5664!)

  cl <- kmeans(scaled_data, centers = k, nstart = 20)
  
  cat("Sizes of the clusters are\n")
  print(cl$size)
  cat(paste("betweenss/totss = ", cl$betweenss/cl$totss, "\n", sep = ""))

  cat("The ones in the small cluster are\n")
  
  small_cluster <- all_data[which(cl$cluster == 1), ]
  #Check which providers and procedures are set to 1. Procedures are mostly colon-related, 
  #partial or complete removal of organs.
  
  small_cluster <- transform_dummy_to_categ(small_cluster)
  print(small_cluster)
  cl
}

hcluster_all_data <- function()
{
  all_data <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/processed_all_data.csv")
  all_data <- all_data[,!(names(all_data) %in% c("X"))]
  scaled_data <- scale(all_data[,!(names(all_data) %in% c("desynpuf_id"))])
  hc.out = hclust(dist(scaled_data))
  hc.clusters = cutree(hc.out, 2)
}

nci60 <- function()
{
  library(ISLR)
  nci.labs = NCI60$labs
  nci.data = NCI60$data
  sd.data = scale(nci.data)
  par(mfrow = c(1,3))
  data.dist = dist(sd.data)
  plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage")
  plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage")
  plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single Linkage")

  hc.out = hclust(dist(sd.data))
  hc.clusters = cutree(hc.out, 4)
  table(hc.clusters, nci.labs)

  par(mfrow = c(1,1))
  plot(hc.out, labels = nci.labs)
  abline(h = 139, col = "red")

  hc.out
  
  set.seed(2)
  km.out = kmeans(sd.data, 4, nstart = 20)
  km.clusters = km.out$cluster
  table(km.clusters, hc.clusters)
}

cluster_mixed_vars <- function()
{
  library(ClustOfVar)
  data(decathlon)
  #choice of the number of clusters
  tree <- hclustvar(X.quanti=decathlon[,1:10])
  stab <- stability(tree,B=60)
  #a random set of variables is chosen as the initial cluster centers, nstart=10 times
  part1 <- kmeansvar(X.quanti=decathlon[,1:10],init=5,nstart=10)
  summary(part1)
  #the partition from the hierarchical clustering is chosen as initial partition
  part_init<-cutreevar(tree,5)$cluster
  part2<-kmeansvar(X.quanti=decathlon[,1:10],init=part_init,matsim=TRUE)
  summary(part2)
  part2$sim
}

pca_mixed_data <- function()
{
  library(PCAmixdata)
  all_data <- prepare_data_all_together()
  X.quanti <- all_data[, c("age", "clm_pmt_amt")]
  X.quali <- all_data[, setdiff(colnames(all_data), c("desynpuf_id", "age", "clm_pmt_amt"))]
  pc <- PCAmix(X.quanti,X.quali, ndim = 4)
}

compute_feature_distance <- function(diagnoses, patient_1, patient_2)
{
  print(colnames(diagnoses))
  d1 <- subset(diagnoses, (diagnoses$desynpuf_id == patient_1))
  d2 <- subset(diagnoses, (diagnoses$desynpuf_id == patient_2))
  i1 <- 1; i2 <- 1
  f1 <- as.character(d1$diagnosed_features)
  f1 <- unlist(strsplit(f1, ", "))
  
  f2 <- as.character(d2$diagnosed_features)
  f2 <- unlist(strsplit(f2, ", "))
  n1 <- length(f1)
  n2 <- length(f2)
  cat(paste("n1 = ", n1, ", n2 = ", n2, "\n", sep = ""))

  distance <- length(setdiff(f1, f2)) + length(setdiff(f2, f1))
}

#Find the distance with the k-th nearest neighbor for each point. List the ones for which this distance is highest.
compute_knn_distances <- function()
{
  all_data <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/processed_all_data.csv")
  #Keep only binary variables
  patient_ids <- all_data$desynpuf_id
  all_data <- all_data[,!(names(all_data) %in% c("X", "desynpuf_id", "age", "clm_pmt_amt"))]
  data.dist = dist(all_data, method = "manhattan")
  #attributes(data.dist)
  n <- attr(data.dist, "Size")
  dist_to_kNN <- data.frame()
  diagnoses <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/diagnoses.csv")
  diagnoses <- diagnoses[,!(names(diagnoses) %in% c("X"))]
  for (i in 1:n)
  {
    #Get the distances to all other points
    distances <- c()
    for (j in 1:n)
    {
      if (j > i)
      {
        #The original formula is meant to give correct result for i > j
        dist <- data.dist[n*(i-1) - i*(i-1)/2 + j-i]
      }
      else if (j < i)
      {
        dist <- data.dist[n*(j-1) - j*(j-1)/2 + i-j]
      }
      dist <- dist + compute_feature_distance(diagnoses, patient_ids[i], patient_ids[j])
      distances <- append(distances, dist)
    }
    distances <- sort(distances)
    dist_to_kNN[i, "id"] <- i
    for (k in 1:20)
    {
      column <- paste("distance_", k, sep = "")
      dist_to_kNN[i, column] <- distances[k]
    }
  }
  write.csv(dist_to_kNN, "/Users/blahiri/healthcare/documents/fraud_detection/dist_to_kNN.csv")
  dist_to_kNN
}

outliers_by_knn <- function(n_potential_outliers = 100)
{
  dist_to_kNN <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/dist_to_kNN.csv")
  dist_to_kNN <- dist_to_kNN[,!(names(dist_to_kNN) %in% c("X"))]
  potential_outliers <- data.frame(matrix(nrow = n_potential_outliers, ncol = 0))
  for (k in 1:20)
  {
    column <- paste("distance_", k, sep = "")
    dist_to_kNN <- dist_to_kNN[order(-dist_to_kNN[, column]),]
    potential_outliers[, column] <- dist_to_kNN[1:n_potential_outliers, "id"]
  }
  write.csv(potential_outliers, "/Users/blahiri/healthcare/documents/fraud_detection/potential_outliers.csv")
  outliers <- intersect(potential_outliers$distance_1, potential_outliers$distance_2)
  for (k in 3:20)
  {
    column <- paste("distance_", k, sep = "")
    outliers <- intersect(outliers, potential_outliers[, column])
  }
  outliers <- sort(outliers)
  all_data <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/processed_all_data.csv")
  #Keep only binary variables as Manhattan distance was computed based on those only
  all_data <- all_data[,!(names(all_data) %in% c("X", "age", "clm_pmt_amt"))]
  outlier_data <- all_data[outliers, ]
  outlier_data <- transform_dummy_to_categ(outlier_data)

  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF") 
  statement <- "select * from procedure_codes"
  res <- dbSendQuery(con, statement)
  pc_codes <- fetch(res, n = -1)
  dbDisconnect(con)
  outlier_data <- merge(x = outlier_data, y = pc_codes, by.x = "procedures", by.y = "procedure_code", all.x =  TRUE)
  #3 instances of each of hemicolectomy and sigmoidectomy. These are relatively rare procedures. In the entire dataset with 5,672 instanes, there are 
  #7 instances of sigmoidectomy and 14 instances of hemicolectomy.
  outlier_data <- outlier_data[,!(names(outlier_data) %in% c("procedures", "providers", "short_desc"))]
}

test_add_diagnoses_data <- function()
{

}
