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
  statement <- "select extract(year from age(to_date('2008-01-01', 'YYYY-MM-DD'), bene_birth_dt)) age,
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
    cat(paste("colname = ", colname, "\n", sep = ""))
    all_data[, colname] <- as.numeric(all_data$prvdr_num == providers[i])
  }

  procedures <- unique(all_data$prcdr_cd)
  n_procedures <- length(procedures)
  cat(paste("n_procedures = ", n_procedures, "\n", sep = ""))
  for (i in 1:(n_procedures - 1))
  {
    colname <- paste("proc_", procedures[i], sep = "")
    cat(paste("colname = ", colname, "\n", sep = ""))
    all_data[, colname] <- as.numeric(all_data$prcdr_cd == procedures[i])
  }

  all_data <- all_data[,!(names(all_data) %in% c("prvdr_num", "prcdr_cd"))]
  
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

  if (FALSE)
  {
   k <- 2
   (cl <- kmeans(projected, k))

   png("./figures/fraud_detection/inpatient_procedures/cluster_of_claims_first_two_pc.png",  width = 600, height = 480, units = "px")
   plot(projected, col = cl$cluster)
   points(cl$centers, col = 1:k, pch = 8, cex = 2)
   dev.off()
   cat("Clustering done\n")
  }

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
  pc
}

cluster_all_data <- function(k = 2)
{
  #The initial cluster centers are chosen randomly, so fix it to repeat results.
  set.seed(1)
  all_data <- read.csv("/Users/blahiri/healthcare/documents/fraud_detection/processed_all_data.csv")
  all_data <- all_data[,!(names(all_data) %in% c("X"))]
  scaled_data <- scale(all_data)
  #Before scaling the data, for k = 2, betweenss/totss = 0.546
  #For k = 3, betweenss/totss = 0.7732983
  #For k = 4, betweenss/totss = 0.86

  #After scaling the data, for k = 2, betweenss/totss = 0.001 (clusters of size 8 and 5664!)
  #For k = 3, betweenss/totss = 0.002

  cl <- kmeans(scaled_data, centers = k, nstart = 20)
  #What happens to the ones which are in the smaller cluster?
  #print(all_data[which(cl$cluster == 1), "clm_pmt_amt"])
  #print((all_data[which(cl$cluster == 1), ])[1, ])
  cat("Sizes of the clusters are\n")
  print(cl$size)
  cat(paste("betweenss/totss = ", cl$betweenss/cl$totss, "\n", sep = ""))
  cl
}
