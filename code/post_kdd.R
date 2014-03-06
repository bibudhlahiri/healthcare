library(RPostgreSQL)
library(ggplot2)
library(plyr)

condition_groups <- function()
{
  df_cac <- read.csv("/Users/blahiri/healthcare/documents/prepared_data_post_feature_selection.csv")
  df_cac <- df_cac[,!(names(df_cac) %in% c("desynpuf_id", "bene_sex_ident_cd", "X", "dev_esrds" 
                    , "cost_year1", "age_year2", "change_type"
                   ))]
  df_cac <- t(df_cac)
  n <- 10
  print(df_cac[1:n, 1:n])
  s <- svd(df_cac)
  D <- diag(s$d)
  k <- 20
  #Retrive a low-rank approximation of the original matrix using first k singular values 
  compressed <- s$u[, 1:k] %*% D[1:k, 1:k] %*% t(s$v[, 1:k])
  print(compressed[1:n, 1:n])
  #Application: recommendation, etc?
  s
}

prepare_data <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- "select sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr, 
                sp_copd, sp_depressn, sp_diabetes,  
                sp_ischmcht, sp_osteoprs, sp_ra_oa, sp_strketia
                from beneficiary_summary_2008"
  res <- dbSendQuery(con, statement)
  chronic_conds <- fetch(res, n = -1)
  for (column in colnames(chronic_conds))
  {
    chronic_conds[, column]  <- as.numeric(chronic_conds[, column] == '1')
  }
  dbDisconnect(con)
  chronic_conds
}

cluster_of_benefs <- function()
{
  chronic_conds <- prepare_data()
  #The rotation matrix is a matrix whose columns contain the eigenvectors.
  #x is the the centred and scaled data multiplied by the rotation matrix.
  pc <- prcomp(chronic_conds, scale = TRUE)
  projected <- pc$x[, c("PC1", "PC2")]

  k <- 10
  (cl <- kmeans(projected, k))

  png("./figures/cluster_of_benefs.png",  width = 600, height = 480, units = "px")
  plot(projected, col = cl$cluster)
  points(cl$centers, col = 1:k, pch = 8, cex = 2)
  dev.off()

  png("./figures/chronic_conds_first_two_pc.png",  width = 1200, height = 960, units = "px")
  projected <- data.frame(projected)
  p <- ggplot(projected, aes(x = PC1, y = PC2)) + geom_point(size = 2) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Projections along first two PCs for chronic conditions")
  print(p)
  dev.off()
  
  #png("./figures/chronic_conds_biplot.png",  width = 1200, height = 960, units = "px")
  #biplot(pc, col = c("blue", "red"))
  #dev.off()
  fiveNumberSummary <- fivenum(projected$PC1)

  #With projection along the first PC, there is a huge spike at -1.722861
  filename <- paste("./figures/chronic_conds_first_pc_distn.png", sep = "");
  png(filename,  width = 600, height = 480, units = "px");
  p <- qplot(PC1, data = projected, geom = "histogram") 
  print(fiveNumberSummary)  
  print(p)
  dev.off()

  #With projection along the second PC, there is a huge spike at -0.2188852
  fiveNumberSummary <- fivenum(projected$PC2)
  filename <- paste("./figures/chronic_conds_second_pc_distn.png", sep = "");
  png(filename,  width = 600, height = 480, units = "px");
  p <- qplot(PC2, data = projected, geom = "histogram") 
  print(fiveNumberSummary)  
  print(p)
  dev.off()

  pc
}


my_pca <- function()
{
  chronic_conds <- prepare_data()
  scaled_chronic_conds <- scale(chronic_conds)
  my.cov <- cov(scaled_chronic_conds)
  #print(my.cov)
  my.eigen <- eigen(my.cov)
  
  eigenvalues <- sort(my.eigen$values, decreasing = TRUE)
  cat("The eigenvalues of the covariance matrix in descending order are\n")
  print(eigenvalues)
  sum_eigenvalues <- sum(my.eigen$values)
  cat(paste("The sum of the eigenvalues is = ", sum_eigenvalues, "\n", sep = ""))

  proportions <- eigenvalues/sum_eigenvalues
  cat("The proportions of variance explained by the eigenvalues are\n")
  print(proportions)
  cat("The cumulative proportions of variance explained by the eigenvalues are\n")
  print(cumsum(proportions))

  n_cols <- ncol(scaled_chronic_conds)
  sum_of_variance <- 0
  for (i in 1:n_cols)
  {
    sum_of_variance <- sum_of_variance + var(scaled_chronic_conds[,i])
  }
  cat(paste("sum_of_variance = ", sum_of_variance, "\n", sep = "")) #OK, sum_of_variance matches sum of the eigenvalues, as it should
  loadings <- my.eigen$vectors

  print((scaled_chronic_conds%*%loadings)[1:10, ])
  print(loadings)
}

find_common_drugs <- function(patient_1_id, patient_2_id, con)
{
  statement <- paste("select a.patient_1_id, a.patient_2_id, count(distinct a.substancename) as n_common_drugs
                      from (select pde1.desynpuf_id patient_1_id, pde2.desynpuf_id patient_2_id, nc.substancename
                            from prescription_drug_events pde1, ndc_codes nc, prescription_drug_events pde2
                            where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
                            and pde2.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
                            and nc.substancename is not null
                            and pde1.desynpuf_id = '", patient_1_id, "' ",  
                            "and pde2.desynpuf_id = '", patient_2_id, "') a ", 
                      " group by a.patient_1_id, a.patient_2_id", sep = "")
  res <- dbSendQuery(con, statement)
  common_drugs <- fetch(res, n = -1)
  ifelse (nrow(common_drugs) > 0,  as.numeric(common_drugs$n_common_drugs), 0)
}


effectiveness_of_drug <- function(limit = 1000)
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  threshold <- 5
  
  statement <- paste("select b.patient_1_id, b.patient_2_id, b.n_matching_conditions
                      from (select a.patient_1_id, a.patient_2_id, 
                                  (a.match_alzhdmta + a.match_chf + a.match_chrnkidn + a.match_cncr + a.match_copd + 
                                   a.match_depressn + a.match_diabetes + a.match_ischmcht + a.match_osteoprs + a.match_ra_oa + 
                                   a.match_sp_strketia) as n_matching_conditions
               	            from (select b1.desynpuf_id patient_1_id, b2.desynpuf_id patient_2_id, 
		                  case when (b1.sp_alzhdmta = '1' and b2.sp_alzhdmta = '1') then 1 else 0 end as match_alzhdmta,
          		          case when (b1.sp_chf = '1' and b2.sp_chf = '1') then 1 else 0 end as match_chf,
		                  case when (b1.sp_chrnkidn = '1' and b2.sp_chrnkidn = '1') then 1 else 0 end as match_chrnkidn,
		                  case when (b1.sp_cncr = '1' and b2.sp_cncr = '1') then 1 else 0 end as match_cncr,
		                  case when (b1.sp_copd = '1' and b2.sp_copd = '1') then 1 else 0 end as match_copd,
		                  case when (b1.sp_depressn = '1' and b2.sp_depressn = '1') then 1 else 0 end as match_depressn,
		                  case when (b1.sp_diabetes = '1' and b2.sp_diabetes = '1') then 1 else 0 end as match_diabetes,
		                  case when (b1.sp_ischmcht = '1' and b2.sp_ischmcht = '1') then 1 else 0 end as match_ischmcht,
		                  case when (b1.sp_osteoprs = '1' and b2.sp_osteoprs = '1') then 1 else 0 end as match_osteoprs,
		                  case when (b1.sp_ra_oa = '1' and b2.sp_ra_oa = '1') then 1 else 0 end as match_ra_oa,
		                  case when (b1.sp_strketia = '1' and b2.sp_strketia = '1') then 1 else 0 end as match_sp_strketia
		                  from (select * from beneficiary_summary_2008 limit ", limit, ") b1, (select * from beneficiary_summary_2008 limit ", limit, ") b2
		                  where b1.desynpuf_id <> b2.desynpuf_id) a) b
                            where b.n_matching_conditions > ", threshold, 
                       " order by b.patient_1_id, b.n_matching_conditions desc", sep = "")
  res <- dbSendQuery(con, statement)
  similar_patients <- fetch(res, n = -1)
  
  similar_patients$n_common_drugs <- apply(similar_patients, 1, 
                                           function(row)find_common_drugs(as.character(row["patient_1_id"]), 
                                                                          as.character(row["patient_2_id"]), 
                                                                          con))
  similar_patients <- similar_patients[order(-similar_patients[,"n_common_drugs"]),] 

  dbDisconnect(con)
  similar_patients
} 
