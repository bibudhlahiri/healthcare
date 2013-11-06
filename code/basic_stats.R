library(ggplot2)
library(plyr)
#library(reshape)
library(reshape2)
library(RPostgreSQL)
require(scales)
library(gridExtra)
library(ada)
library(rpart)
require(hash)



los_distn <- function()
{
  ip_claims <- read.csv("../data/DE-SynPUF/DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv")
  ip_claims$los <- as.numeric(as.Date(as.character(ip_claims$NCH_BENE_DSCHRG_DT), "%Y%m%d") -  as.Date(as.character(ip_claims$CLM_ADMSN_DT), "%Y%m%d")) + 1
  filename <- paste("./los_distn.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")
  p <- qplot(los, data = ip_claims, geom = "histogram")    
  print(p)
  aux <- dev.off()
  #1  3  5  8 36
  one_week_or_more <- subset(ip_claims, (los >= 7))
  #22735
  cat(paste("On ", nrow(one_week_or_more), " claims patients stayed one week or longer\n", sep = ""))
  print(fivenum(ip_claims$los))
  #13 days
  cat(paste("90-th percentile is ", quantile(ip_claims$los, c(.9)), "\n", sep = "")) 
}

claim_amt_distn <- function()
{
  claims <- read.csv("../data/DE-SynPUF/DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.csv")
  filename <- paste("./outpatient_claim_amt_distn.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")
  p <- qplot(CLM_PMT_AMT, data = claims, geom = "histogram")    
  print(p)
  aux <- dev.off()
  #For inpatient claims: -8000  4000  7000 11000 57000: Median claim is 7000 dollars
  ##For outpatient claims: -100   40   80  200 3300: Median claim is $80
  print(fivenum(claims$CLM_PMT_AMT))
}



los_vs_cost <- function()
{
  ip_claims <- read.csv("../data/DE-SynPUF/DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv")
  #print(ip_claims[1:5, c("NCH_BENE_DSCHRG_DT", "CLM_ADMSN_DT")])
  ip_claims$los <- as.numeric(as.Date(as.character(ip_claims$NCH_BENE_DSCHRG_DT), "%Y%m%d") -  as.Date(as.character(ip_claims$CLM_ADMSN_DT), "%Y%m%d")) + 1

  png(file = "./los_vs_cost.png", width = 800, height = 600)
  p <- ggplot(ip_claims, aes(x = los, y = CLM_PMT_AMT)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  aux <- dev.off()

  #Coeff for los is 585.5: the increase in amount for each day of additional stay
  fit <- lm(CLM_PMT_AMT ~ los, data = ip_claims)
  summary(fit)
}

n_claims_per_patient <- function()
{
  ip_claims <- read.csv("../data/DE-SynPUF/DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv")
  aggdata <- aggregate(x = ip_claims$CLM_ID, by = list(ip_claims$DESYNPUF_ID), FUN = "length")
  colnames(aggdata) <- c("patient_id", "n_claims")
  aggdata <- aggdata[order(-aggdata[,"n_claims"]),]
  print(aggdata[1:20, ])

  filename <- paste("./n_claims_per_patient.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")
  p <- qplot(n_claims, data = aggdata, geom = "histogram")    
  print(p)
  aux <- dev.off()
  #1  1  1  2 14: Median no. of claims per patient is 1. Also, 75% people have 2 or less claims.
  print(fivenum(aggdata$n_claims))
  cat(paste(nrow(aggdata), " unique patients, ", nrow(ip_claims), " claims", "\n", sep = ""))

  #How is number of claims related to total claim amount
  total_claim_amounts <- aggregate(x = ip_claims$CLM_PMT_AMT, by = list(ip_claims$DESYNPUF_ID), FUN = "sum")
  cat(paste(nrow(total_claim_amounts), " unique patients",  "\n", sep = ""))
  colnames(total_claim_amounts) <- c("patient_id", "total_amount")

  data_for_plots <- merge(x = aggdata, y = total_claim_amounts, 
                            all.x =  TRUE)
  print(data_for_plots[1:5, ])
  png(file = "./n_claims_vs_total_amounts.png", width = 800, height = 600)
  p <- ggplot(data_for_plots, aes(x = n_claims, y = total_amount)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  aux <- dev.off()
}

#Is there any pattern across the different days of the week, when it comes to releasing patients?
discharge_days <- function()
{
  ip_claims <- read.csv("../data/DE-SynPUF/DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv")
  ip_claims$discharge_day_of_week <- weekdays(as.Date(as.character(ip_claims$NCH_BENE_DSCHRG_DT), "%Y%m%d"))
  png(file = "./discharge_days.png", width = 800, height = 600)

  aggdata <- aggregate(x = ip_claims$CLM_ID, by = list(ip_claims$discharge_day_of_week), FUN = "length")
  colnames(aggdata) <- c("discharge_day_of_week", "frequency")
  print(aggdata)

  #p <- ggplot(ip_claims, aes(factor(discharge_day_of_week))) + geom_bar()
  p <- ggplot(aggdata, aes(x = discharge_day_of_week, y = frequency)) + geom_bar() + 
        theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  aux <- dev.off()
}

#Claims by diagnosis codes
claims_by_dianosis_codes <- function()
{
  #ADMTNG_ICD9_DGNS_CD is the diagnosis made at the time of admission: the symptom the patient reported on admission
  ip_claims <- read.csv("../data/DE-SynPUF/DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv")
  png(file = "./claims_by_dianosis_codes.png", width = 800, height = 600)

  aggdata <- aggregate(x = ip_claims$CLM_ID, by = list(ip_claims$ADMTNG_ICD9_DGNS_CD), FUN = "length")
  colnames(aggdata) <- c("dianosis_code", "frequency")
  n_claims <- nrow(ip_claims)
  aggdata$relative_frequency <- aggdata$frequency/n_claims
  aggdata <- aggdata[order(-aggdata[,"frequency"]),]
  aggdata$dianosis_code <- factor(aggdata$dianosis_code, 
                              levels = aggdata$dianosis_code,
                              ordered = TRUE)
  #2286
  cat(paste(nrow(aggdata), " unique diagnosis codes\n", sep = ""))
  #The top 10 diagnosis codes are:
  #786.05 SHORTNESS OF BREATH: 4.1% of all claims
  #786.50 UNSPECIFIED CHEST PAIN
  #486 PNEUMONIA ORGANISM UNSPECIFIED
  #428.0 CONGESTIVE HEART FAILURE UNSPECIFIED
  #780.2 SYNCOPE AND COLLAPSE
  #V57.89 CARE INVOLVING OTHER SPECIFIED REHABILITATION PROCEDURE
  #780.97 ALTERED MENTAL STATUS
  #780.79 OTHER MALAISE AND FATIGUE
  #789.00 ABDOMINAL PAIN UNSPECIFIED SITE
  #491.21 OBSTRUCTIVE CHRONIC BRONCHITIS WITH (ACUTE) EXACERBATION
  
  aggdata <- aggdata[1:10, ]

  print(aggdata)

  p <- ggplot(aggdata, aes(x = dianosis_code, y = relative_frequency)) + geom_bar() + 
        theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  aux <- dev.off()
}

process_five_number_summary <- function(fiveNumberSummary)
{
  return(paste("Q1 = ", round(fiveNumberSummary[2],2),
                    ", Median = ", round(fiveNumberSummary[3],2),
                    ", Q3 = ", round(fiveNumberSummary[4],2),
                    sep = ""))
}


#How many times were the expenses in 2009 over the expenses in 2008? 
inc_exp_pats <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF");
  statement <- paste("select cbpy2.DESYNPUF_ID, cbpy1.total_claim_amt cost_2008, cbpy2.total_claim_amt cost_2009, 
                      (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) times_increase
                      from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2
                      where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
                      and cbpy1.year = '2008'
                      and cbpy2.year = '2009'
                      and cbpy1.total_claim_amt > 0
                      and (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) > 1
                      order by times_increase desc", sep = "")
  res <- dbSendQuery(con, statement);
  df_inc_exp_pats <- fetch(res, n = -1)
  fiveNumberSummary <- fivenum(df_inc_exp_pats$times_increase)
  print(fiveNumberSummary) #1.026316   1.500000   2.200000   3.714286 157.142857
  cat(paste("90-th percentile is ", quantile(df_inc_exp_pats$times_increase, c(.9)), "\n", sep = "")) #For the people in top 90th percentile, cost increased 6.6675 times or more

  #Truncate at 10 for visualization
  df_inc_exp_pats <- subset(df_inc_exp_pats, (times_increase <= 10))
  filename <- paste("./inc_exp_pats.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")
  p <- ggplot(df_inc_exp_pats, aes(x=times_increase))
  #By default Gaussian kernel for KDE
  p <- p + geom_histogram(aes(y = ..density..)) + geom_density() + 
        labs(x = paste("Times increase", process_five_number_summary(fiveNumberSummary), sep = "\n")) 
  print(p)
  aux <- dev.off()
  dbDisconnect(con)
}

#Get all p(y/x) values for all categorical predictors in a data set and a categorical response y
list_posteriors <- function(df, response_var_name)
{
  columns <- colnames(df)
  for (column in columns)
  {
    if (column != response_var_name & is.factor(df[,column]))
    {
      cat(paste("\ncolumn = ", column, sep = ""))
      M <- table(df[, column], df[, response_var_name])
      print(M)
      D <- M
      for (i in 1:nrow(M))
      {
       D[i,1] <- M[i,1]/(M[i,1] + M[i,2])
       D[i,2] <- M[i,2]/(M[i,1] + M[i,2])
      }
      print(D)
    }
  }
}

get_data_for_change_type <- function(year1, year2)
{
   con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
   statement <- paste("select b1.MEDREIMB_IP cost_year1, 
                       b1.bene_sex_ident_cd, 
                       extract(year from age(to_date('", year2, "-01-01', 'YYYY-MM-DD'), b1.bene_birth_dt)) age_year2, 
                       case when (b1.bene_esrd_ind = '2' and b2.bene_esrd_ind = '1') then 'yes' else 'no' end as dev_esrds,
                       case when (b1.sp_alzhdmta = '2' and b2.sp_alzhdmta = '1') then 'yes' else 'no' end as dev_alzhdmta,
                       case when (b1.sp_chf = '2' and b2.sp_chf = '1') then 'yes' else 'no' end as dev_chf,
                       case when (b1.sp_chrnkidn = '2' and b2.sp_chrnkidn = '1') then 'yes' else 'no' end as dev_chrnkidn,
                       case when (b1.sp_cncr = '2' and b2.sp_cncr = '1') then 'yes' else 'no' end as dev_cncr,
                       case when (b1.sp_copd = '2' and b2.sp_copd = '1') then 'yes' else 'no' end as dev_copd,
		       case when (b1.sp_depressn = '2' and b2.sp_depressn = '1') then 'yes' else 'no' end as dev_depressn,
		       case when (b1.sp_diabetes = '2' and b2.sp_diabetes = '1') then 'yes' else 'no' end as dev_diabetes,
		       case when (b1.sp_ischmcht = '2' and b2.sp_ischmcht = '1') then 'yes' else 'no' end as dev_ischmcht,
		       case when (b1.sp_osteoprs = '2' and b2.sp_osteoprs = '1') then 'yes' else 'no' end as dev_osteoprs,
		       case when (b1.sp_ra_oa = '2' and b2.sp_ra_oa = '1') then 'yes' else 'no' end as dev_ra_oa,
		       case when (b1.sp_strketia = '2' and b2.sp_strketia = '1') then 'yes' else 'no' end as dev_strketia,
                       (select count(distinct dgns_cd) from transformed_claim_diagnosis_codes tcdc
		        where tcdc.DESYNPUF_ID = b1.DESYNPUF_ID
		        and to_char(tcdc.clm_thru_dt, 'YYYY') = '", year1, "') as n_dgns_year1, 
                       case when b2.MEDREIMB_IP > b1.MEDREIMB_IP then 'increased'
                            else 'did_not_increase'
                       end as change_type
                       from beneficiary_summary_", year1, " b1, beneficiary_summary_", year2, " b2 ",
                       "where b1.DESYNPUF_ID = b2.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement);
  df_cac <- fetch(res, n = -1)
  
  columns <- colnames(df_cac)
  for (column in columns)
  {
    if (substr(column, 1, 4) != 'cost' & substr(column, 1, 3) != 'age' & substr(column, 1, 7) != 'n_dgns_')
    {
      df_cac[, column] <- as.factor(df_cac[, column])
    }
  }
  dbDisconnect(con)
  return(df_cac)
}

visualization_for_report <- function(df)
{
  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_alzhdmta.png"
  png(file = filename, width = 800, height = 600)
  p <- ggplot(df, aes(dev_alzhdmta, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed alzheimer") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_chf.png"
  png(file = filename, width = 800, height = 600)
  p <- ggplot(df, aes(dev_chf, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed chronic heart failure") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_chrnkidn.png"
  png(file = filename, width = 800, height = 600)
  p <- ggplot(df, aes(dev_chrnkidn, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed chronic kidney condition") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_cncr.png"
  png(file = filename, width = 800, height = 600)
  p <- ggplot(df, aes(dev_cncr, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed cancer") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_copd.png"
  png(file = filename, width = 800, height = 600)
  p <- ggplot(df, aes(dev_copd, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed Chronic Obstructive Pulmonary Disease (COPD)") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_depressn.png"
  png(file = filename, width = 800, height = 600)
  p <- ggplot(df, aes(dev_depressn, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed depression") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_diabetes.png"
  png(file = filename, width = 800, height = 600)
  p <- ggplot(df, aes(dev_diabetes, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed diabetes") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_ischmcht.png"
  png(file = filename, width = 800, height = 600)
  p <- ggplot(df, aes(dev_ischmcht, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed Ischemic Heart Disease") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_osteoprs.png"
  png(file = filename, width = 800, height = 600)
  p <- ggplot(df, aes(dev_osteoprs, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed Osteoporosis") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_ra_oa.png"
  png(file = filename, width = 800, height = 600)
  p <- ggplot(df, aes(dev_ra_oa, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed rheumatoid arthritis and osteoarthritis") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  filename  <- "/Users/blahiri/healthcare/documents/visualizations/dev_strketia.png"
  png(file = filename, width = 800, height = 600) 
  p <- ggplot(df, aes(dev_strketia, fill = change_type)) + geom_bar(position = "dodge") + labs(x = "Developed Stroke/transient Ischemic Attack") + 
           labs(y = "Frequency") + 
           theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
           theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off()

  increased_grp <- subset(df, (change_type == 'increased'))
  did_not_increase_grp <- subset(df, (change_type == 'did_not_increase'))
  print(fivenum(increased_grp$n_dgns_year1))
  print(fivenum(did_not_increase_grp$n_dgns_year1))

  p1 <- ggplot(increased_grp, aes(x = n_dgns_year1)) + geom_histogram(aes(y = ..density..)) + geom_density() + 
        labs(x = "#Diagnoses from 2008 for positive group") + ylab("#Patients")

  p2 <- ggplot(did_not_increase_grp, aes(x = n_dgns_year1)) + geom_histogram(aes(y = ..density..)) + geom_density() +
        labs(x = "#Diagnoses from 2008 for negative group") + ylab("#Patients")

  gp1 <- ggplot_gtable(ggplot_build(p1))
  gp2 <- ggplot_gtable(ggplot_build(p2))

  frame_grob <- grid.arrange(gp1, gp2, ncol = 1)
  grob <- grid.grab()

  image_file <- "/Users/blahiri/healthcare/documents/visualizations/n_dgns_year1.png"
  png(image_file, width = 600, height = 600)
  grid.newpage()
  grid.draw(grob)
  dev.off()


  print(fivenum(increased_grp$age_year2))
  print(fivenum(did_not_increase_grp$age_year2))

  p1 <- ggplot(increased_grp, aes(x = age_year2)) + geom_histogram(aes(y = ..density..)) + geom_density() + 
        labs(x = "Age at start of 2009 for positive group") + ylab("#Patients")

  p2 <- ggplot(did_not_increase_grp, aes(x = age_year2)) + geom_histogram(aes(y = ..density..)) + geom_density() +
        labs(x = "Age at start of 2009 for negative group") + ylab("#Patients")

  gp1 <- ggplot_gtable(ggplot_build(p1))
  gp2 <- ggplot_gtable(ggplot_build(p2))

  frame_grob <- grid.arrange(gp1, gp2, ncol = 1)
  grob <- grid.grab()

  image_file <- "/Users/blahiri/healthcare/documents/visualizations/age_year2.png"
  png(image_file, width = 600, height = 600)
  grid.newpage()
  grid.draw(grob)
  dev.off()

  overall_error <- c(0.3068, 0.2426, 0.3149, 0.292)
  FPR <- c(0.318, 0.2279, 0.323, 0.302)
  FNR <- c(0.233, 0.3336, 0.2637, 0.229)
  algorithms <- c("Naive Bayes", "Logistic regression", "Decision tree", "Random Forest")
  

  error_df <- data.frame(algorithms = algorithms, overall_error = overall_error, FPR = FPR, FNR = FNR)
  molten_data <- melt(error_df, id = c("algorithms"))
  colnames(molten_data) <- c("algorithms", "error_type", "error_value")


  png(file = "/Users/blahiri/healthcare/documents/visualizations/algos_and_errors.png", width = 800, height = 600)
  p <- ggplot(molten_data, aes(x = error_type, y = error_value, fill = algorithms)) + geom_bar(position="dodge") + 
       labs(x = "Error type") + labs(y = "Error value") + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  aux <- dev.off()
}


#Can we predict whether expense will increase or not between 2008 and 2009?
claim_amount_change_type <- function()
{
  library(randomForest)
  library(foreach)
  library(doMC)
  registerDoMC(8)

  df_cac <- get_data_for_change_type('2008', '2009')
  visualization_for_report(df_cac)

  if (FALSE)
  {

    bal_df_cac <- create_balanced_sample_for_brf(df_cac) 

    #On original data, did_not_increase had error rate of 0.27%, increased had error rate 90.8%, overall error rate 12.8%.
    cac.rf <- foreach(ntree=rep(63, 8), .combine = combine, .packages = 'randomForest') %dopar% 
         randomForest(bal_df_cac[,!(names(bal_df_cac) %in% c("change_type"))], bal_df_cac[,"change_type"], ntree = ntree)

   
    if (FALSE)
    {
     df_cac$predicted_change_type <- predict(cac.rf, newdata = df_cac, type = "response")
     print(table(df_cac[,"change_type"], df_cac[, "predicted_change_type"], dnn = list('actual', 'predicted')))
     return(cac.rf)
    }

    #How well does model based on 2008 and 2009 perform on data based on 2009 and 2010?
    #Without n_dgns_year1 as a predictor, overall error = 22.37%, FPR = 20.96%, FNR = 36.22%.
    #With n_dgns_year1 as a predictor, overall error = 26.97%, FPR = 26.66%, FNR = 29.9%.

    df_cac_test <- get_data_for_change_type('2009', '2010')
    df_cac_test$predicted_change_type <- predict(cac.rf, newdata = df_cac_test, type = "response")
    print(table(df_cac_test[,"change_type"], df_cac_test[, "predicted_change_type"], dnn = list('actual', 'predicted')))
    return(cac.rf)
  }

  
  #pred <- naive_bayes_for_change_type(df_cac)
  #return(pred)
  #logistic_regression_for_change_type(df_cac)
  #decision_tree_for_change_type(df_cac)
  #cac.rf <- balanced_random_forest_for_change_type(df_cac)
  #return(cac.rf)
  #cross_validation_rf(df_cac, 0.6, 0.2, 5, 2100)
 }

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
  
  d <- rbind(d1, d2)
  d <- d[order(d[,"desynpuf_id"]),]
  interesting_patients <- unique(d$desynpuf_id)
  n_beneficiaries <- length(interesting_patients)
  cat(paste("n_beneficiaries = ", n_beneficiaries, "\n", sep = ""))
  hash_bene <<- hash(interesting_patients, 1:n_beneficiaries)

  statement <- paste("select b1.DESYNPUF_ID,  
                             case when b2.MEDREIMB_IP > b1.MEDREIMB_IP then 1 else 0
                             end as change_type
                      from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                      where b1.DESYNPUF_ID = b2.DESYNPUF_ID 
                      order by b1.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement)
  all_patients <- fetch(res, n = -1)
  interesting_patients <- data.frame(desynpuf_id = interesting_patients)
  interesting_patients <- merge(x = interesting_patients, y = all_patients, all.x = TRUE, by.x = "desynpuf_id", by.y = "desynpuf_id")
  cat(paste("nrow(interesting_patients) = ", nrow(interesting_patients), "\n", sep = ""))

  d$bene_num <- apply(d, 1, function(row)lookup_bene_num(row["desynpuf_id"]))
  d$feature_num <- apply(d, 1, function(row)lookup_feature_num(row["feature"]))
  print(d[1:20, ])
  sparse_mat <- sparseMatrix(i = d$bene_num, j = d$feature_num, x = 1, dimnames=list(1:n_beneficiaries,1:n_features))
  cat(paste("nrow(sparse_mat) = ", nrow(sparse_mat), ", ncol(sparse_mat) = ", ncol(sparse_mat), "\n", sep = ""))

  cvob1 = cv.glmnet(sparse_mat, interesting_patients$change_type, family="binomial", type.measure = "class", nfolds = 10)
  index_min_xval_error <- which.min(cvob1$cvm)
  cat(paste("Min xval error = ", min(cvob1$cvm), " occurs at lambda = ", cvob1$lambda[index_min_xval_error], " for ", 
              cvob1$nzero[index_min_xval_error], " covariates\n", sep = ""))
  #feature_numbers <- unlist(predict(cvob1, newx = sparse_mat, s = "lambda.min", type = "nonzero"))
  #imp_predictors <- data.frame(feature_numbers)
  #What are the features that have nonzero coefficients for the lambda which minimizes the cross-validation error?
  #imp_predictors$dgns_codes <- apply(imp_predictors, 1, function(row)lookup_dgns_code(as.character(row["dgns_numbers"])))
  return(cvob1)
   
  dbDisconnect(con)
}



 #Given a dataset, name of response variable, shares of validation and test set, run k-fold cross validation on the training + validation set 
 #and report cross-validation error and test error. The training should be done on a balanced sample of the training subset. 
 #trg_frac and val_frac tell what fraction of total data should be used for training and validation, e.g., trg_frac = 0.6 and val_frac = 0.2
 #mean 60% should be used for training, 20% for validation and rest 20% for testing.
 cross_validation_rf <- function(df, trg_frac, val_frac, k, max_trees)
 {
   library(foreach)
   library(doMC)
   registerDoMC(8)

   cat(paste("max_trees = ", max_trees, "\n", sep = ""))
   xval_set <- df[1:ceiling((trg_frac + val_frac)*nrow(df)), ]
   cat(paste("nrow(xval_set) = ", nrow(xval_set), "\n", sep = ""))
   test_set <- df[(ceiling((trg_frac + val_frac)*nrow(df)) + 1):nrow(df), ]
   cat(paste("nrow(test_set) = ", nrow(test_set), "\n", sep = ""))

   errors <- data.frame()
  
   xval_set$fold_id <- round(runif(nrow(xval_set), 1, k))
   #We do the split only once (so there is no randomness from splitting as the complexity parameter is varied), 
   #and repeat the experiment (measuring cross-validation error and test error) by varying ntree
   i <- 1
   for (complx in seq(8, max_trees, 512))
   {
     for (m in 1:k)
     {
       trg_set_this_fold <- subset(xval_set, (fold_id != m))
       val_set_this_fold <- subset(xval_set, (fold_id == m))
 
       bal_df_cac <- create_balanced_sample_for_brf(trg_set_this_fold)
   
       cac.rf <- foreach(ntree = rep(complx/8, 8), .combine = combine, .packages = 'randomForest') %dopar% 
                       randomForest(bal_df_cac[,!(names(bal_df_cac) %in% c("change_type"))], bal_df_cac[,"change_type"], ntree = ntree)
       val_set_this_fold$predicted_change_type <- predict(cac.rf, newdata = val_set_this_fold, type = "response")
       M <- table(val_set_this_fold[,"change_type"], val_set_this_fold[, "predicted_change_type"], dnn = list('actual', 'predicted'))
       FPR <- M[1,2]/(M[1,1] + M[1,2])
       FNR <- M[2,1]/(M[2,1] + M[2,2])
       overall_error <- (M[1,2] + M[2,1])/sum(M)
       cat(paste("complx = ", complx, ", m = ", m, ", FPR = ", FPR, ", FNR = ", FNR, ", overall_error = ", overall_error, "\n", sep = ""))
       errors[i, "complx"] <- complx
       errors[i, "fold_id"] <- m
       errors[i, "FPR"] <- FPR
       errors[i, "FNR"] <- FNR
       errors[i, "overall_error"] <- overall_error
       i <- i + 1
     }
   }
   #print(errors)
   write.csv(errors, file = "../documents/cv_results/with_dgns_codes/errors.csv")
  }

  analyze_cv_error <- function()
  {
     library(sciplot)
     errors <- read.csv("../documents/cv_results/with_dgns_codes/errors.csv") 
     #mean_errors <- aggregate(x = errors$overall_error, by = list(errors$complx), FUN = "mean")
     #se_errors <- aggregate(x = errors$overall_error, by = list(errors$complx), FUN = "mean")
     #print(xval_errors_for_plot)
     
     filename <- paste("./figures/cv_results/with_dgns_codes/overall_error.png", sep = "")
     png(filename,  width = 600, height = 480, units = "px")
     lineplot.CI(x.factor = complx, response = overall_error, data = errors, xlab = "Number of decision trees", ylab = "Overall classification error")
     dev.off()

     filename <- paste("./figures/cv_results/with_dgns_codes/FPR.png", sep = "")
     png(filename,  width = 600, height = 480, units = "px")
     lineplot.CI(x.factor = complx, response = FPR, data = errors, xlab = "Number of decision trees", ylab = "FPR")
     dev.off()

     filename <- paste("./figures/cv_results/with_dgns_codes/FNR.png", sep = "")
     png(filename,  width = 600, height = 480, units = "px")
     lineplot.CI(x.factor = complx, response = FNR, data = errors, xlab = "Number of decision trees", ylab = "FNR")
     dev.off()
  }

  
 naive_bayes_for_change_type <- function(df_cac)
 {
   #With naive Bayes over all data (114,538), did_not_increase had an error rate of 8.7%, and increased had an error rate of 61.5%. Overall error rate is 16%. 
   #With cost_2008 and age_2009 taken off, did_not_increase had an error rate of 5%, and increased had an error rate of 74.5%. Overall error rate is 14.74%.
   #On a balanced sample (15000 from each class), did_not_increase had an error rate of 31.8%, and increased had an error rate of 23.5%. Overall error rate is 27.68%. 
   #Likelihood values (p(x/y)) are similar in balanced and original dataset, however p(y/x) values are very different, because balancing makes the distribution of y very
   #different between the original and the balanced dataset. 
   #After adjusting the class posteriors in the original dataset based on the model built upon the balanced sample, did_not_increase had an error rate of 31.8%, 
   #and increased had an error rate of 23.3%. Overall error rate is 30.68%. 

   #98609 did_not_increase, 15929 increased in original data
   #Take a balanced sample
   library(sampling)
   st = strata(df_cac, stratanames = c("change_type"), size = c(15000, 15000), method = "srswor")
   bal_df_cac <- getdata(df_cac, st)
   bal_df_cac <- bal_df_cac[,!(names(bal_df_cac) %in% c("ID_unit", "Prob", "Stratum"))]

   if (FALSE)
   {
     cat("Calling list_posteriors for df_cac\n")
     list_posteriors(df_cac, "change_type")

     cat("Calling list_posteriors for bal_df_cac\n")
     list_posteriors(bal_df_cac, "change_type")
   }
   
   library(e1071)
   #For continuous predictors, the first column is the means, the second column is the standard deviations.
   classifier <- naiveBayes(bal_df_cac[,!(names(bal_df_cac) %in% c("change_type"))], bal_df_cac[,"change_type"]) 
   #type = "raw" gives the class posteriors
   pred <- predict(classifier, df_cac[,!(names(df_cac) %in% c("change_type"))], type = "raw")
   class_prior_true <- table(df_cac[,"change_type"])
   class_prior_true <- class_prior_true/sum(class_prior_true)
   class_prior_balanced <- table(bal_df_cac[,"change_type"])
   class_prior_balanced <- class_prior_balanced/sum(class_prior_balanced)
   adj_factors <- class_prior_true/class_prior_balanced
   #Multiply class posteriors based on the balanced sample by the adjustment factors for the corresponding class
   n <- nrow(pred)
   sweep(pred, MARGIN = 2, adj_factors, `*`)
   df_cac[, "predicted_change_type"] <- ifelse(pred[, "did_not_increase"] > pred[, "increased"], 'did_not_increase', 'increased')
   print(table(df_cac[,"change_type"], df_cac[, "predicted_change_type"], dnn = list('actual', 'predicted')))
   return(pred)
 }

 logistic_regression_for_change_type <- function(df_cac)
 {
   #With logistic regression over all data (114,538), did_not_increase had an error rate of 2.1%, and increased had an error rate of 85.7%. Overall error rate is 13.7%.
   #Using original data but a 6:1 weight ratio for increased vs did_not_increase, did_not_increase had an error rate of 22.79%, and increased had an error rate of 33.36%. 
   #Overall error rate is 24.26%.

   weights <- ifelse(df_cac$change_type == 'increased', 6, 1)
   cac.logr <- glm(change_type ~ cost_2008 + bene_sex_ident_cd + age_2009 +
                                 dev_alzhdmta + dev_chf + dev_chrnkidn +
                                 dev_cncr + dev_copd + dev_depressn +
		                 dev_diabetes +
		                 dev_ischmcht +
		                 dev_osteoprs +
		                 dev_ra_oa +
		                 dev_strketia,  
                 family = binomial("logit"), data = df_cac, weights = weights
                 )
  df_cac$predicted_prob_increase <- predict(cac.logr, newdata = df_cac, type = "response")
  #print(contrasts(df_cac$change_type))
  df_cac$predicted_change_type <- ifelse(df_cac$predicted_prob_increase >= 0.5, 'increased', 'did_not_increase')
  #print(summary(cac.logr))
  print(table(df_cac[,"change_type"], df_cac[, "predicted_change_type"], dnn = list('actual', 'predicted')))
 }


 decision_tree_for_change_type <- function(df_cac)
 {
   #Using original data but a 6:1 weight ratio for increased vs did_not_increase, did_not_increase had an error rate of 32.3%, and increased had an error rate of 26.37%. 
   #Overall error rate is 31.49%.
   weights <- ifelse(df_cac$change_type == 'increased', 6, 1)
   cac.rpart <- rpart(change_type ~ cost_2008 + bene_sex_ident_cd + age_2009 +
                                    dev_alzhdmta + dev_chf + dev_chrnkidn +
                                    dev_cncr + dev_copd + dev_depressn +
		                    dev_diabetes +
		                    dev_ischmcht +
		                    dev_osteoprs +
		                    dev_ra_oa +
		                    dev_strketia,  
                      data = df_cac, weights = weights)
    pred <- predict(cac.rpart, newdata = df_cac, type = "prob")
  df_cac$predicted_prob_increase <- (predict(cac.rpart, newdata = df_cac, type = "prob"))[, "increased"]
  df_cac$predicted_change_type <- ifelse(df_cac$predicted_prob_increase >= 0.5, 'increased', 'did_not_increase')
  print(table(df_cac[,"change_type"], df_cac[, "predicted_change_type"], dnn = list('actual', 'predicted')))
 }

  
 create_balanced_sample_for_brf <- function(df_cac)
 {
   minority_set <- subset(df_cac, (change_type =='increased'))
   n_minority <- nrow(minority_set)
   bs_minority_ind <- sample(1:n_minority, n_minority, replace = TRUE)
   bootstrap_minority <- minority_set[bs_minority_ind, ]
   
   majority_set <- subset(df_cac, (change_type =='did_not_increase'))
   n_majority <- nrow(minority_set)
   sample_majority_ind <- sample(1:n_majority, n_minority, replace = TRUE)
   sample_majority <- majority_set[sample_majority_ind, ]

   bal_df_cac <- rbind(bootstrap_minority, sample_majority)
   return(bal_df_cac)
 }

 #Custom random forest implementation to deal with class imbalance in data
 balanced_random_forest_for_change_type <- function(df_cac)
 {
   #Register the 8 cores for parallel building of decision trees
   library(foreach)
   library(doMC)
   registerDoMC(8)

   #Took a boostrap sample from the minority class and a sample with replacement of the same size from the majority class. 
   #On the balanced sample itself, did_not_increase had an error rate of 29%, and increased had an error rate of 22%,  overall error rate is 25.67%.
   #Fitting the model based on sample data back on the original data, did_not_increase had an error rate of 30%, and increased had an error rate of 22.9%,  
   #overall error rate is 29.2%.

   bal_df_cac <- create_balanced_sample_for_brf(df_cac)
   
   cac.rf <- foreach(ntree=rep(63, 8), .combine = combine, .packages = 'randomForest') %dopar% 
         randomForest(bal_df_cac[,!(names(bal_df_cac) %in% c("change_type"))], bal_df_cac[,"change_type"], ntree = ntree)
   
   #bal_df_cac$predicted_change_type <- cac.rf$predicted
   #print(table(bal_df_cac[,"change_type"], bal_df_cac[, "predicted_change_type"], dnn = list('actual', 'predicted')))

   df_cac$predicted_change_type <- predict(cac.rf, newdata = df_cac, type = "response")
   print(table(df_cac[,"change_type"], df_cac[, "predicted_change_type"], dnn = list('actual', 'predicted')))
   return(cac.rf)
 }


#With logistic regression: binary outcome
claim_amount_change_lr <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")

  #Assuming if they had a chronic condition in 2009, they had it in 2008, too.
  statement <- paste("select bene_sex_ident_cd,
       bene_race_cd, bene_esrd_ind,
       sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr,
       sp_copd, sp_depressn, sp_diabetes, sp_ischmcht,
       sp_osteoprs, sp_ra_oa, sp_strketia, a.cost_2008, 
       case when a.times_increase > 1 then 'increased'
            when a.times_increase < 1 then 'decreased'
       end change_type
       from (select cbpy2.DESYNPUF_ID, cbpy1.total_claim_amt cost_2008, cbpy2.total_claim_amt cost_2009, 
            (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) times_increase
            from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2
            where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
            and cbpy1.year = '2008'
            and cbpy2.year = '2009'
            and cbpy1.total_claim_amt > 0
            and cbpy2.total_claim_amt <> cbpy1.total_claim_amt
            order by times_increase desc) a, beneficiary_summary_2009 b 
      where a.DESYNPUF_ID = b.DESYNPUF_ID
      order by a.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement);
  df_cac <- fetch(res, n = -1)
  
  columns <- colnames(df_cac)
  for (column in columns)
  {
    if (column != 'cost_2008')
    {
      df_cac[, column] <- as.factor(df_cac[, column])
    }
  }
  dbDisconnect(con)

  cac.logr <- glm(change_type ~ bene_sex_ident_cd + 
                          bene_race_cd + bene_esrd_ind + 
                          sp_alzhdmta  + sp_chf  + sp_chrnkidn  + sp_cncr + 
                          sp_copd + sp_depressn + sp_diabetes  + sp_ischmcht + 
                          sp_osteoprs + sp_ra_oa + sp_strketia +  cost_2008,  
                 family = binomial("logit"), data = df_cac);
  df_cac$predicted_prob_increase <- predict(cac.logr, newdata = df_cac, type = "response")
  df_cac$predicted_change_type <- ifelse(df_cac$predicted_prob_increase >= 0.5, 'increased', 'decreased')
  overall_classification_accuracy <- (sum(df_cac$predicted_change_type == df_cac$change_type))/nrow(df_cac);
  #Logistic regression correctly classifies training data with accuracy 75.7%. bene_esrd_ind, sp_chrnkidn, sp_copd, sp_ra_oa,
  #sp_strketia and cost_2008 are statistically significant indicators.
  cat(paste("overall_classification_accuracy on training set = ", overall_classification_accuracy, "\n", sep = ""));
  summary(cac.logr)
}

#How well can we predict the total claim amount in 2009?
predict_claim_amount_2009 <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")

  statement <- paste("select ip_2008.DESYNPUF_ID, bene_sex_ident_cd,
                      bene_race_cd, bene_esrd_ind,
                      sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr,
                      sp_copd, sp_depressn, sp_diabetes, sp_ischmcht,
                      sp_osteoprs, sp_ra_oa, sp_strketia, cbpy1.total_claim_amt cost_2008, cbpy2.total_claim_amt cost_2009, 
                      sum(ip_2008.NCH_BENE_DSCHRG_DT - ip_2008.CLM_ADMSN_DT + 1) los_2008, 
                      sum(ip_2009.NCH_BENE_DSCHRG_DT - ip_2009.CLM_ADMSN_DT + 1) los_2009
                      from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2, beneficiary_summary_2009 b, inpatient_claims ip_2008, inpatient_claims ip_2009
                      where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
                      and cbpy2.DESYNPUF_ID = b.DESYNPUF_ID
                      and cbpy1.year = '2008'
                      and cbpy2.year = '2009'
                      and cbpy1.total_claim_amt > 0
                      and cbpy2.total_claim_amt > 0
                      and cbpy1.DESYNPUF_ID = ip_2008.DESYNPUF_ID
                      and to_char(ip_2008.CLM_ADMSN_DT, 'YYYY') = '2008'
                      and ip_2008.DESYNPUF_ID = ip_2009.DESYNPUF_ID
                      and to_char(ip_2009.CLM_ADMSN_DT, 'YYYY') = '2009'
                      group by ip_2008.DESYNPUF_ID, bene_sex_ident_cd,
                      bene_race_cd, bene_esrd_ind,
                      sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr,
                      sp_copd, sp_depressn, sp_diabetes, sp_ischmcht,
                      sp_osteoprs, sp_ra_oa, sp_strketia, cost_2008, cost_2009
                      order by ip_2008.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement);
  df_cac <- fetch(res, n = -1)
  
  columns <- colnames(df_cac)
  for (column in columns)
  {
    if (column != 'cost_2008' & column != 'cost_2009' & column != 'los_2008' & column != 'los_2009')
    {
      df_cac[, column] <- as.factor(df_cac[, column])
    }
  }
  dbDisconnect(con)

  cac.lm <- lm(cost_2009 ~ bene_sex_ident_cd +
                          bene_race_cd + bene_esrd_ind +
                          sp_alzhdmta + sp_chf + sp_chrnkidn + sp_cncr +
                          sp_copd + sp_depressn + sp_diabetes + sp_ischmcht +
                          sp_osteoprs + sp_ra_oa + sp_strketia + cost_2008 + los_2008 + los_2009,  
                data = df_cac);
  df_cac$predicted_cost_2009 <- predict(cac.lm, newdata = df_cac, type = "response", interval = "none")
  df_cac$rel_error <- abs(df_cac$predicted_cost_2009 - df_cac$cost_2009)/df_cac$cost_2009  
  print(df_cac[1:5, c("cost_2009", "predicted_cost_2009", "rel_error")])
  #Adding los_2008 and los_2009 changes mean relative error from 1.4 to 1.16 on the training data.
  #Both los_2008 and los_2009 are statistically significant predictors.
  cat(paste("Mean relative error is ", mean(df_cac$rel_error), "\n", sep = ""))
  summary(cac.lm)
}

#Can we predict by how many times cost increased between 2008 and 2009?
predict_increase_proportion <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")

  statement <- paste("select b1.DESYNPUF_ID, 
                       case when (b1.sp_alzhdmta = '2' and b2.sp_alzhdmta = '1') then 'yes' else 'no' end as dev_alzhdmta,
                       case when (b1.sp_chf = '2' and b2.sp_chf = '1') then 'yes' else 'no' end as dev_chf,
                       case when (b1.sp_chrnkidn = '2' and b2.sp_chrnkidn = '1') then 'yes' else 'no' end as dev_chrnkidn,
                       case when (b1.sp_cncr = '2' and b2.sp_cncr = '1') then 'yes' else 'no' end as dev_cncr,
                       case when (b1.sp_copd = '2' and b2.sp_copd = '1') then 'yes' else 'no' end as dev_copd,
		       case when (b1.sp_depressn = '2' and b2.sp_depressn = '1') then 'yes' else 'no' end as dev_depressn,
		       case when (b1.sp_diabetes = '2' and b2.sp_diabetes = '1') then 'yes' else 'no' end as dev_diabetes,
		       case when (b1.sp_ischmcht = '2' and b2.sp_ischmcht = '1') then 'yes' else 'no' end as dev_ischmcht,
		       case when (b1.sp_osteoprs = '2' and b2.sp_osteoprs = '1') then 'yes' else 'no' end as dev_osteoprs,
		       case when (b1.sp_ra_oa = '2' and b2.sp_ra_oa = '1') then 'yes' else 'no' end as dev_ra_oa,
		       case when (b1.sp_strketia = '2' and b2.sp_strketia = '1') then 'yes' else 'no' end as dev_strketia,
                       (cast(b2.MEDREIMB_IP as real)/b1.MEDREIMB_IP) times_increase
                     from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                     where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                     and b1.MEDREIMB_IP > 0
                     and b2.MEDREIMB_IP  > b1.MEDREIMB_IP
                     --and (cast(b2.MEDREIMB_IP as real)/b1.MEDREIMB_IP) > 1.5
                     order by times_increase", sep = "")
  res <- dbSendQuery(con, statement);
  df_cac <- fetch(res, n = -1)
  
  columns <- colnames(df_cac)
  for (column in columns)
  {
    if (column != 'DESYNPUF_ID' & column != 'times_increase')
    {
      df_cac[, column] <- as.factor(df_cac[, column])
    }
  }
  dbDisconnect(con)
  print(fivenum(df_cac$times_increase)) #1.001250   1.500000   2.200000   3.666667 263.809524
  #pairs(~ dev_alzhdmta + dev_chf + dev_chrnkidn + dev_cncr + dev_copd + dev_depressn + dev_diabetes + dev_ischmcht + 
  #                              dev_osteoprs + dev_ra_oa + dev_strketia, data = df_cac, main = "Simple Scatterplot Matrix")

  if (FALSE)
  {

  cac.lm <- lm(times_increase ~ dev_alzhdmta + dev_chf + dev_chrnkidn + dev_cncr + dev_copd + dev_depressn + dev_diabetes + dev_ischmcht + 
                                dev_osteoprs + dev_ra_oa + dev_strketia,  
                data = df_cac);
  df_cac$predicted_times_increase <- predict(cac.lm, newdata = df_cac, type = "response", interval = "none")
  df_cac$rel_error <- abs(df_cac$predicted_times_increase - df_cac$times_increase)/df_cac$times_increase  
  print(df_cac[1:5, c("times_increase", "predicted_times_increase", "rel_error")])
  cat(paste("Mean relative error is ", mean(df_cac$rel_error), "\n", sep = ""))
  print(summary(cac.lm))
  }
  #return(df_cac)
}

#This function checks, before applying a classification algorithm, how the features which are categorical variables related to 
#the response variable (which is always categorical)
perform_chi_square <- function(df, response_var_name, alpha)
{
  relative_risk_results <- data.frame()
  chi_square_results <- data.frame()
  odds_ratio_results <- data.frame()

  i <- 1
  j <- 1
  k <- 1

  columns <- colnames(df)
  for (column in columns)
  {
    if (column != response_var_name & is.factor(df[,column]))
    {
      M <- table(df[, column], df[, response_var_name])
      print(M)
      if (nrow(M) > 2)
      {
       Xsq <- chisq.test(M)
       if (Xsq$p.value < alpha)
       {
        chi_square_results[i, "factor"] <- column
        chi_square_results[i, "chi_square"] <- Xsq$p.value
        i <- i + 1
       }
       relative_risk <- (M[2,1]*((M[1,1] + M[1,2])/(M[2,1] + M[2,2])))/M[1,1]
       if (relative_risk > 1.05)
       {
        relative_risk_results[j, "factor"] <- column
        relative_risk_results[j, "relative_risk"] <- relative_risk
        j <- j + 1
       }
       odds_ratio <- (M[2,1]*M[1,2])/(M[2,2]*M[1,1])
       if (odds_ratio > 1.1)
       {
        odds_ratio_results[k, "factor"] <- column
        odds_ratio_results[k, "odds_ratio"] <- odds_ratio
        k <- k + 1
       }
      }
    }
  }
  print(chi_square_results)
  print(relative_risk_results)
  print(odds_ratio_results)
  imp_factors <- intersect(chi_square_results$factor, relative_risk_results$factor)
  imp_factors <- intersect(imp_factors, odds_ratio_results$factor)
  cat("The important factors from all three are\n")
  print(imp_factors) #dev_chrnkidn and dev_copd
  for (column in imp_factors)
  {
    cat(paste("Table for important factor ", column, "\n", sep = ""))
    M <- table(df[, column], df[, response_var_name])
    print(M)
  }
}

#Can we predict whether cost increase was low, moderate or high?
predict_increase_type <- function()
{
  library(randomForest)
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")

  statement <- paste("select b1.DESYNPUF_ID,
                      b1.bene_sex_ident_cd, 
                       extract(year from age(to_date('2009-01-01', 'YYYY-MM-DD'), b1.bene_birth_dt)) age_2009, 
                       case when (b1.sp_alzhdmta = '2' and b2.sp_alzhdmta = '1') then 'yes' else 'no' end as dev_alzhdmta,
                       case when (b1.sp_chf = '2' and b2.sp_chf = '1') then 'yes' else 'no' end as dev_chf,
                       case when (b1.sp_chrnkidn = '2' and b2.sp_chrnkidn = '1') then 'yes' else 'no' end as dev_chrnkidn,
                       case when (b1.sp_cncr = '2' and b2.sp_cncr = '1') then 'yes' else 'no' end as dev_cncr,
                       case when (b1.sp_copd = '2' and b2.sp_copd = '1') then 'yes' else 'no' end as dev_copd,
		       case when (b1.sp_depressn = '2' and b2.sp_depressn = '1') then 'yes' else 'no' end as dev_depressn,
		       case when (b1.sp_diabetes = '2' and b2.sp_diabetes = '1') then 'yes' else 'no' end as dev_diabetes,
		       case when (b1.sp_ischmcht = '2' and b2.sp_ischmcht = '1') then 'yes' else 'no' end as dev_ischmcht,
		       case when (b1.sp_osteoprs = '2' and b2.sp_osteoprs = '1') then 'yes' else 'no' end as dev_osteoprs,
		       case when (b1.sp_ra_oa = '2' and b2.sp_ra_oa = '1') then 'yes' else 'no' end as dev_ra_oa,
		       case when (b1.sp_strketia = '2' and b2.sp_strketia = '1') then 'yes' else 'no' end as dev_strketia,
                       (select count(*)
                       from prescription_drug_events pde
                       where pde.DESYNPUF_ID = b1.DESYNPUF_ID
                       and to_char(pde.srvc_dt, 'YYYY') = '2009') as n_pde_2009,
                       (select COALESCE(sum(tot_rx_cst_amt), 0)
                       from prescription_drug_events pde1
                       where pde1.DESYNPUF_ID = b1.DESYNPUF_ID
                       and to_char(pde1.srvc_dt, 'YYYY') = '2009') as cost_pde_2009, 
                       --(select COALESCE(sum(clm_pmt_amt), 0)
			--from outpatient_claims oc
			--where oc.desynpuf_id = b1.desynpuf_id
			--and to_char(oc.clm_thru_dt, 'YYYY') = '2009') as cost_opv_2009, 
                       (cast(b2.MEDREIMB_IP as real)/b1.MEDREIMB_IP) times_increase,
                       case when (cast(b2.MEDREIMB_IP as real)/b1.MEDREIMB_IP) < 2.2 then 'low'
                            else 'high' 
                       end as increase_type
                     from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
                     where b1.DESYNPUF_ID = b2.DESYNPUF_ID
                     and b1.MEDREIMB_IP > 0
                     and b2.MEDREIMB_IP  > b1.MEDREIMB_IP
                     order by times_increase", sep = "")
  res <- dbSendQuery(con, statement);
  df_cac <- fetch(res, n = -1)

  columns <- colnames(df_cac)
  for (column in columns)
  {
    if (column != 'desynpuf_id' & column != 'times_increase' & column != 'age_2009' & column != 'n_pde_2009')
    {
      df_cac[, column] <- as.factor(df_cac[, column])
    }
  }
  dbDisconnect(con)

  #perform_chi_square(df_cac, "increase_type", 0.05)
  rsb <- regularized_boost(df_cac)
  return(rsb)
 }



  cost_increase_distn <- function(df_cac)
  {
    #Only dev_chrnkidn and dev_copd are significant (at the 5% level) as indicated by chi-square test. However, random forest shows most important variables are
    #age_2009 and bene_sex_ident_cd.

    #Distribution of times_increase between 2008 and 2009
    fns_ti <- fivenum(df_cac$times_increase)

    df_cac_subset <- subset(df_cac, (times_increase <= 5))
    #print(fivenum(df_cac_subset$times_increase))

    filename <- paste("./figures/times_increase_distn.png", sep = "")
    png(filename,  width = 600, height = 480, units = "px")
 
    p <- ggplot(df_cac_subset, aes(x = times_increase)) + geom_histogram(aes(y = ..density..)) + geom_density() + 
        labs(x = paste("Cost increase between 2008 and 2009", process_five_number_summary(fns_ti), sep = "\n")) + ylab("#Patients")
    print(p)
    aux <- dev.off()
  }



random_forest <- function(df_cac)
{
    #500 decision trees, No. of variables tried at each split: 3

    #With < 1.5 as low, >= 1.5 to < 2.2 as moderate, and > 2.2 as high, high has a class error of only 1.7% but low has a class error of 100% and 
    #moderate has a class error of 97%. Most low and modearte points get mapped to high.

    #With < 2.2 as low, and >= 2.2 as high, overall error rate = 48%, error rate for high: 58%, error rate for low: 38%. With ntree reduced from 500 to 100,
    #the error rates remain in the same range. With no. of variables tried at each split increased from 3 to 5, things remain about same.
    #Adding sex and age reduces error for high to 53% but increases error for low to 47%. So, more lows are getting mapped to high after adding sex and age.
    #'high's get classfied as 'low' more often => bar set by algorithm is too high? 

    #With < 3.67 as low, and >= 3.67 as high, overall error rate = 25%, almost all points get classified as 'low'.

    #With < 5.0 as low, and >= 5.0 as high, overall error rate = 17%, almost all points get classified as 'low'.  

    cac.rf <- randomForest(df_cac[,!(names(df_cac) %in% c("desynpuf_id", "times_increase", "increase_type"))], df_cac[,"increase_type"], 
                         #ntree = 100, mtry = 5, 
                         prox = TRUE)
    #cac.rf$proximity is an N x N matrix. After a tree is grown, put all of the data, both training and oob, down the tree. If cases k and n are in the same terminal     
    #node, increase their proximity by one. At the end, normalize the proximities by dividing by the number of trees.
    #cac.rf$votes tells, for each case, the proportion of votes gone to each class. Can be useful in computing CPV.
    #What do we do if one variable is important for singling out a class, but other classes can be predicted more accurately with that variable removed? 
    
    #Added all chronic conditions for 2009 but that worsened things: overall error rate = 50%, error rate for high: 50%, error rate for low: 50%. No better than random guess.

     #The ones that get predicted wrongly, are they too close to the boundary of 2.2?
     df_cac$predicted <-  cac.rf$predicted
     df_cac$classification_result <- (df_cac$increase_type == df_cac$predicted)
     #print(df_cac[, c("increase_type", "predicted", "classification_result")])
     df_cac_wrongly_classified <- subset(df_cac, (classification_result == FALSE))
     print(fivenum(df_cac_wrongly_classified$times_increase)) #1.001250   1.607205   2.495330   4.010667 183.333333: pretty much all over
}


naive_bayes <- function(df_cac)
{
    #With Naive Bayes, high has a classification error of 47%, low has a classification error of 47%, overall error rate is 46%. Naive Bayes has a lower FNR compared to 
    #random forest at the cost of a higher FPR.
    library(e1071)
    classifier <- naiveBayes(df_cac[,!(names(df_cac) %in% c("desynpuf_id", "times_increase", "increase_type"))], df_cac[,"increase_type"]) 
    df_cac$predicted_increase_type <- predict(classifier, df_cac[,!(names(df_cac) %in% c("desynpuf_id", "times_increase", "increase_type"))])
    df_cac[1:5, c("increase_type", "predicted_increase_type")]
    table(df_cac[,"increase_type"], df_cac[, "predicted_increase_type"], dnn = list('actual', 'predicted'))
}


 scatterplots <- function(df_cac)
 {
    png(file = "./figures/age_2009_vs_times_increase.png", width = 800, height = 600)
    p <- ggplot(df_cac, aes(x = age_2009, y = times_increase)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
    print(p)
    aux <- dev.off()

    png(file = "./figures/n_pde_2009_vs_times_increase.png", width = 800, height = 600)
    p <- ggplot(df_cac, aes(x = n_pde_2009, y = times_increase)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
    print(p)
    aux <- dev.off()

    df_cac$cost_pde_2009 <- as.numeric(df_cac$cost_pde_2009)
    png(file = "./figures/cost_pde_2009_vs_times_increase.png", width = 800, height = 600)
    p <- ggplot(df_cac, aes(x = cost_pde_2009, y = times_increase)) + geom_point(shape=1) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
    print(p)
    aux <- dev.off()

    df_cac$cost_opv_2009 <- as.numeric(df_cac$cost_opv_2009)
    png(file = "./figures/cost_opv_2009_vs_times_increase.png", width = 800, height = 600)
    p <- ggplot(df_cac, aes(x = cost_opv_2009, y = times_increase)) + geom_point(shape=1) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
    print(p)
    aux <- dev.off()

    cat(paste("Correlation coeff between age_2009 and times_increase is = ", cor(df_cac$age_2009, df_cac$times_increase), "\n", sep = "")) #0.0123
    cat(paste("Correlation coeff between n_pde_2009 and times_increase is = ", cor(df_cac$n_pde_2009, df_cac$times_increase), "\n", sep = "")) #-0.006
    cat(paste("Correlation coeff between cost_pde_2009 and times_increase is = ", cor(df_cac$cost_pde_2009, df_cac$times_increase), "\n", sep = "")) #-0.0165 
    cat(paste("Correlation coeff between cost_opv_2009 and times_increase is = ", cor(df_cac$cost_opv_2009, df_cac$times_increase), "\n", sep = "")) #-0.00073 

    #Distribution of n_pde_2009
    fns_pde <- fivenum(df_cac$n_pde_2009)

    #df_cac_subset <- subset(df_cac, (times_increase <= 5))
    #print(fivenum(df_cac_subset$times_increase))

    filename <- paste("./figures/n_pde_2009_distn.png", sep = "")
    png(filename,  width = 600, height = 480, units = "px")
 
    p <- ggplot(df_cac, aes(x = n_pde_2009)) + geom_histogram(aes(y = ..density..)) + geom_density() + 
        labs(x = paste("Number of PDEs in 2009", process_five_number_summary(fns_pde), sep = "\n")) + ylab("#Patients")
    print(p)
    aux <- dev.off()

    #Zoom in on the <=10 section of n_pde_2009

    df_cac_subset <- subset(df_cac, (n_pde_2009 <= 10))
    fns_pde_subset <- fivenum(df_cac_subset$n_pde_2009)

    filename <- paste("./figures/n_pde_2009_subset_distn.png", sep = "")
    png(filename,  width = 600, height = 480, units = "px")
 
    p <- ggplot(df_cac_subset, aes(x = n_pde_2009)) + geom_histogram(aes(y = ..density..)) + geom_density() + 
        labs(x = paste("Number of PDEs in 2009 (truncate to 5)", process_five_number_summary(fns_pde_subset), sep = "\n")) + ylab("#Patients")
    print(p)
    aux <- dev.off()
 }

 adaboost <- function(df_cac)
 {
   #Try adaboost with decision trees
  n <- dim(df_cac)[1] #2399
  trind <- sample(1:n,floor(.8*n),FALSE)
  teind <- setdiff(1:n,trind)
  cat(paste("n = ", n, ", length(trind) = ", length(trind), ", length(teind) = ", length(teind), "\n", sep = ""))

  #With 500 iterations, training set error reduces from 0.45 to 0.3. However, test set error fluctuates at around 50% and does not improve. So, adaboost is probably overfitting. 
  #In final prediction (gdis$confusion) over the training data, high has an error rate of 29.88%, low has an error rate of 32.45%. Cross-validation can tell how well the model will
  #generalize but in order to avoid overfitting, we need to apply regularization, early stopping, pruning or Bayesian priors.

  #After adding cost_pde_2009, with 500 iterations, training set error reduces from 0.35 to 0.1. However, test set error fluctuates at around 50% and does not improve. So, adaboost is overfitting again. 
  #In final prediction (gdis$confusion) over the training data, high has an error rate of 10.2%, low has an error rate of 9.5%. 

  #After adding cost_opv_2009, with 500 iterations, training set error reduces from 0.45 to 0.2. However, test set error fluctuates at around 50% and does not improve.  
  #In final prediction (gdis$confusion) over the training data, high has an error rate of 20%, low has an error rate of 22.8%. Adding cost_opv_2009 worsened things after adding cost_pde_2009.

  #With decision stumps, training set error reduces from 0.48 to 0.43. However, test set error fluctuates at around 48% and does not improve much.  
  #In final prediction (gdis$confusion) over the training data, high has an error rate of 39.73%, low has an error rate of 49.25% - worse than when we were using deeper decision trees.

  #With 4-split trees, training set error reduces from 0.46 to 0.42. However, test set error reduces from 0.48 to 0.46. This generalizes better than the other two versions of adaboost.  
  #In final prediction (gdis$confusion) over the training data, high has an error rate of 39.46%, low has an error rate of 45.34%.

  #With bag.frac = 1.0, a very different pattern emerges. The training error to stabilizes to about 0.42 and the test error stabilizes to about 0.48 within 100 iterations. In other trials, 
  #training error was decreasing all the way till 500 iterations. high has an error rate of 48.21%, low has an error rate of 35.33%.

  #With nu = 0.5 and bag.frac = 1.0, the training error to stabilizes to about 0.44 and the test error stabilizes to about 0.47 within 20 iterations. high has an error rate of 48.33%, 
  #low has an error rate of 39.41%.
  
  
  #The "strength" of the "weak" learners: If you use very simple weak learners, such as decision stumps (1-level decision trees), then the algorithms are much less prone to overfitting. 
  #Whenever I've tried using more complicated weak learners (such as decision trees or even hyperplanes) I've found that overfitting occurs much more rapidly

  #The noise level in the data: AdaBoost is particularly prone to overfitting on noisy datasets. In this setting the regularised forms (RegBoost, AdaBoostReg, LPBoost, QPBoost) are preferable

  #The dimensionality of the data: We know that in general, we experience overfitting more in high dimensional spaces ("the curse of dimensionality"), and AdaBoost can also suffer in that respect, 
  #as it is simply a linear combination of classifiers which themselves suffer from the problem. Whether it is as prone as other classifiers is hard to determine.

  #Train the classifier.
  stump = rpart.control(cp = -1, maxdepth = 1, minsplit = 0)
  four = rpart.control(cp = -1, maxdepth = 2, minsplit = 0)
  
  #The loss function is the default exponential function. 
  gdis <- ada(x = df_cac[trind,!(names(df_cac) %in% c("desynpuf_id", "times_increase", "increase_type"))], y = df_cac[trind,"increase_type"], iter = 500, type = "discrete"
              #, bag.frac = 1.0,
              #nu = 0.5
              #control = stump)
              #control = four
             )
  #Apply the learnt model on the test data.
  gdis = addtest(x = gdis, test.x = df_cac[teind,!(names(df_cac) %in% c("desynpuf_id", "times_increase", "increase_type"))], test.y = df_cac[teind,"increase_type"])

  filename <- paste("./figures/bias_variance_discrete_adaboost.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")
  plot(gdis, kappa = FALSE, test = TRUE)
  dev.off()
  #pairs(gdis, df_cac[trind,!(names(df_cac) %in% c("desynpuf_id", "times_increase", "increase_type"))], maxvar = 2)
  return(gdis)
 }


 regularized_boost <- function(df_cac)
 {
  n <- dim(df_cac)[1] #2399
  trind <- sample(1:n,floor(.8*n),FALSE)
  teind <- setdiff(1:n,trind)
  cat(paste("n = ", n, ", length(trind) = ", length(trind), ", length(teind) = ", length(teind), "\n", sep = ""))
  source("rsb.R")

  lambda=seq(0,1,length=29)  
  gelasso <- rsb(df_cac[trind,!(names(df_cac) %in% c("desynpuf_id", "times_increase", "increase_type"))], 
                 df_cac[trind,"increase_type"], iter=50, penalty="elasso", 
                  test.response=df_cac[teind,"increase_type"], 
                  test.x=df_cac[teind,!(names(df_cac) %in% c("desynpuf_id", "times_increase", "increase_type"))], lambda=lambda,  k=1, bag=FALSE)
  
  ##Compute training estimator
  ntr<-which.min(gelasso$err[,1])

  ##Compute OOB estimator with burnin of 15
  noob<-which.min(gelasso$err[-c(1:15),3])+15

  ## Give corresponding testing errors
  gelasso$err[c(ntr,noob),3]

  ##Plot error rates
  layout(1:2)
  matplot(gelasso$err,type="l",lwd=2,xlab=paste("1...",gelasso$iter),ylab="Error Rates")
  return(gelasso)
 }






#What caused increase of amount claimed from 2008 to 2009?
causes_of_increase_in_expense <- function()
{
  library(randomForest)
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF");
  statement <- paste("select ip_2008.DESYNPUF_ID, sum(ip_2008.NCH_BENE_DSCHRG_DT - ip_2008.CLM_ADMSN_DT + 1) los_2008, 
                      sum(ip_2009.NCH_BENE_DSCHRG_DT - ip_2009.CLM_ADMSN_DT + 1) los_2009
                      from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2, inpatient_claims ip_2008, inpatient_claims ip_2009
                      where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
                      and cbpy1.year = '2008'
                      and cbpy2.year = '2009'
                      and cbpy2.total_claim_amt > cbpy1.total_claim_amt 
                      and cbpy1.total_claim_amt > 0
                      and cbpy1.DESYNPUF_ID = ip_2008.DESYNPUF_ID
                      and to_char(ip_2008.CLM_ADMSN_DT, 'YYYY') = '2008'
                      and ip_2008.DESYNPUF_ID = ip_2009.DESYNPUF_ID
                      and to_char(ip_2009.CLM_ADMSN_DT, 'YYYY') = '2009'
                      group by ip_2008.DESYNPUF_ID
                      order by ip_2008.DESYNPUF_ID", sep = "")
  res <- dbSendQuery(con, statement);
  df_los_two_years <- fetch(res, n = -1)
  fns_2008 <- fivenum(df_los_two_years$los_2008)
  fns_2009 <- fivenum(df_los_two_years$los_2009)
  n_risers <- nrow(df_los_two_years)

  #Truncate at LoS = 100 for visualization
  df_los_two_years <- subset(df_los_two_years, ((los_2008 <= 100) & (los_2009 <= 100))) 

  p1 <- ggplot(df_los_two_years, aes(x = los_2008)) + geom_histogram(aes(y = ..density..)) + geom_density() + 
        labs(x = paste("Total LoS 2008", process_five_number_summary(fns_2008), sep = "\n")) + ylab("#Patients")

  p2 <- ggplot(df_los_two_years, aes(x = los_2009)) + geom_histogram(aes(y = ..density..)) + geom_density() +
        labs(x = paste("Total LoS 2009", process_five_number_summary(fns_2009), sep = "\n")) + ylab("#Patients")

  gp1 <- ggplot_gtable(ggplot_build(p1))
  gp2 <- ggplot_gtable(ggplot_build(p2))

  #grid.arrange arranges ggplot2, lattice, and grobs (grid objects) on a page. 
  #Returns a frame grob.
  frame_grob <- grid.arrange(gp1, gp2, ncol = 1
                             #, heights = rep(3, 2), widths = rep(3,2)
                            )
  grob <- grid.grab()

  image_file <- "los_2008_2009.png"
  png(image_file, width = 600, height = 600)
  grid.newpage()
  #grid.draw produces graphical output from a graphical object
  grid.draw(grob)
  dev.off()

  statement <- paste("SELECT a.attname chronic_condition
                      FROM pg_class c, pg_attribute a
                      WHERE c.relname = 'beneficiary_summary_2008'
                      AND a.attnum > 0
                      AND a.attrelid = c.oid
                      and a.attname like 'sp_%'
                      and a.attname <> 'sp_state_code'
                      order by a.attname", sep = "")
  res <- dbSendQuery(con, statement);
  df_chronic_conds <- fetch(res, n = -1)
  chronic_conditions <- df_chronic_conds$chronic_condition
  developer_stats <- data.frame();
  row <- 1

  for (chronic_condition in chronic_conditions)
  {
     statement <- paste("select count(*) n_developers
                         from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2, beneficiary_summary_2008 bs1, beneficiary_summary_2009 bs2
                         where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
                         and cbpy1.year = '2008'
                         and cbpy2.year = '2009'
                         and cbpy1.total_claim_amt > 0
                         and cbpy2.total_claim_amt > cbpy1.total_claim_amt
                         and bs1.", chronic_condition, " = '2'
                         and bs2.", chronic_condition, " = '1'
                         and cbpy1.DESYNPUF_ID = bs1.DESYNPUF_ID
                         and cbpy2.DESYNPUF_ID = bs2.DESYNPUF_ID", sep = "")
      developer_stats[row, "chronic_condition"] <- chronic_condition
      developer_stats[row, "n_developers"] <- as.numeric(dbGetQuery(con, statement))
      row <- row + 1
  }
  developer_stats$f_risers <- developer_stats$n_developers/n_risers
  

  fullnames <- data.frame(chronic_condition = chronic_conditions, 
                          cc_name = c("Alzheimer", "Heart Failure", "Kidney", "Cancer", 
                                      "Pulmonary", "Depression", "Diabetes", "Ischemic Heart", 
                                      "Osteoporosis", "Arthritis", "Stroke"))
  developer_stats <- merge(x = developer_stats, y = fullnames, 
                            all.x =  TRUE)
  developer_stats <- developer_stats[order(-developer_stats[,"n_developers"]),]
  print(developer_stats)
  developer_stats$cc_name <- factor(developer_stats$cc_name, 
                              levels = developer_stats$cc_name,
                              ordered = TRUE)

  print(developer_stats)

  png("developer_stats.png",  width = 600, height = 480, units = "px")
  p <- ggplot(developer_stats, aes(x = cc_name, y = f_risers)) + geom_bar() + 
        theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  aux <- dev.off()

  dbDisconnect(con)
}

increased_vs_decreased <- function()
{
   con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
   statement <- paste("SELECT a.attname chronic_condition
                      FROM pg_class c, pg_attribute a
                      WHERE c.relname = 'beneficiary_summary_2008'
                      AND a.attnum > 0
                      AND a.attrelid = c.oid
                      and a.attname like 'sp_%'
                      and a.attname <> 'sp_state_code'
                      order by a.attname", sep = "")
  res <- dbSendQuery(con, statement);
  df_chronic_conds <- fetch(res, n = -1)
  chronic_conditions <- df_chronic_conds$chronic_condition
  increased_stats <- data.frame();

  for (chronic_condition in chronic_conditions)
  {
    statement <- paste("select '", chronic_condition, "' as condition, 
                        case when ", chronic_condition, " = '1' then 'yes'
                             when ", chronic_condition, " = '2' then 'no' 
                        end as status,
                        count(distinct bs.DESYNPUF_ID) freq
                        from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2, beneficiary_summary_2009 bs
                        where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
                        and cbpy1.year = '2008'
                        and cbpy2.year = '2009'
                        and cbpy1.total_claim_amt > 0
                        and (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) > 1
                        and cbpy1.DESYNPUF_ID = bs.DESYNPUF_ID
                        group by status 
                        order by status desc", sep = "")
    res <- dbSendQuery(con, statement);
    df_chronic_status <- fetch(res, n = -1)
    increased_stats <- rbind(increased_stats, df_chronic_status)
  }
  print(increased_stats)

  decreased_stats <- data.frame();

  for (chronic_condition in chronic_conditions)
  {
    statement <- paste("select '", chronic_condition, "' as condition, 
                        case when ", chronic_condition, " = '1' then 'yes'
                             when ", chronic_condition, " = '2' then 'no' 
                        end as status,
                        count(distinct bs.DESYNPUF_ID) freq
                        from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2, beneficiary_summary_2009 bs
                        where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
                        and cbpy1.year = '2008'
                        and cbpy2.year = '2009'
                        and cbpy1.total_claim_amt > 0
                        and (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) < 1
                        and cbpy1.DESYNPUF_ID = bs.DESYNPUF_ID
                        group by status 
                        order by status desc", sep = "")
    res <- dbSendQuery(con, statement);
    df_chronic_status <- fetch(res, n = -1)
    decreased_stats <- rbind(decreased_stats, df_chronic_status)
  }
  print(decreased_stats)


  p1 <- ggplot(increased_stats, aes(x = condition, y = freq, fill = status)) + geom_bar() + labs(x = "Chronic condition (as in 2009)") +
        scale_x_discrete(limits = increased_stats$condition) +
        scale_fill_manual(values = c( "#FF6666", "#00CCCC")) +  
        theme(axis.text.x = element_text(colour = 'blue', size = 12, angle = 90, face = 'bold')) +
        theme(axis.text.y = element_text(colour = 'blue', size = 12, face = 'bold')) +
        theme(axis.title = element_text(colour = 'red', size = 12, face = 'bold')) +
        ylab("#patients with increased cost") + ggtitle("Chronic conditions for patients with increased cost")

  p2 <- ggplot(decreased_stats, aes(x = condition, y = freq, fill = status)) + geom_bar() + labs(x = "Chronic condition (as in 2009)") +
        scale_x_discrete(limits = decreased_stats$condition) +
        scale_fill_manual(values = c( "#FF6666", "#00CCCC")) +  
        theme(axis.text.x = element_text(colour = 'blue', size = 12, angle = 90, face = 'bold')) +
        theme(axis.text.y = element_text(colour = 'blue', size = 12, face = 'bold')) +
        theme(axis.title = element_text(colour = 'red', size = 12, face = 'bold')) +
        ylab("#patients with decreased cost") + ggtitle("Chronic conditions for patients with decreased cost")

  gp1 <- ggplot_gtable(ggplot_build(p1))
  gp2 <- ggplot_gtable(ggplot_build(p2))

  #grid.arrange arranges ggplot2, lattice, and grobs (grid objects) on a page. 
  #Returns a frame grob.
  frame_grob <- grid.arrange(gp1, gp2, ncol = 1
                             #, heights = rep(3, 2), widths = rep(3,2)
                            )
  grob <- grid.grab()

  image_file <- "./figures/chronic_conditions_increased_decreased.png"
  png(image_file, width = 700, height = 700)
  grid.newpage()
  #grid.draw produces graphical output from a graphical object
  grid.draw(grob)
  dev.off()

  dbDisconnect(con)
}

