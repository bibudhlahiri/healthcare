library(ggplot2)
library(plyr)
#library(reshape)
library(reshape2)
library(RPostgreSQL)
require(scales);
library(gridExtra);



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

#Can we predict whether expense will increase, decrease or remain same between 2008 and 2009?
claim_amount_change_rf <- function()
{
  library(randomForest)
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
            else 'same'
       end change_type
       from (select cbpy2.DESYNPUF_ID, cbpy1.total_claim_amt cost_2008, cbpy2.total_claim_amt cost_2009, 
            (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) times_increase
            from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2
            where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
            and cbpy1.year = '2008'
            and cbpy2.year = '2009'
            and cbpy1.total_claim_amt > 0
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

  #Using the beneficiary_summary as on 2008, 71% of the 'increased' get classified as 'increased', 78% of the 'decreased' get classified as 'decreased'. 
  #Using the beneficiary_summary as on 2009, 73.5% of the 'increased' get classified as 'increased', 77% of the 'decreased' get classified as 'decreased'.
  cac.rf <- randomForest(df_cac[,!(names(df_cac) %in% c("change_type"))], df_cac[,"change_type"], prox = TRUE)
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

#Can we predict whether cost increase was low, moderate or high?
predict_increase_type <- function()
{
  library(randomForest)
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
                       (cast(b2.MEDREIMB_IP as real)/b1.MEDREIMB_IP) times_increase,
                       case when (cast(b2.MEDREIMB_IP as real)/b1.MEDREIMB_IP) < 1.5 then 'low'
                            when ((cast(b2.MEDREIMB_IP as real)/b1.MEDREIMB_IP) >= 1.5 and (cast(b2.MEDREIMB_IP as real)/b1.MEDREIMB_IP) < 2.2) then 'moderate'
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
    if (column != 'desynpuf_id' & column != 'times_increase')
    {
      df_cac[, column] <- as.factor(df_cac[, column])
    }
  }
  dbDisconnect(con)

  #With < 1.5 as low, >= 1.5 to < 2.2 as moderate, and > 2.2 as high, high has a class error of only 1.7% but low has a class error of 100% and 
  #moderate has a class error of 97%. Most low and modearte points get mapped to high.
  cac.rf <- randomForest(df_cac[,!(names(df_cac) %in% c("desynpuf_id", "times_increase", "increase_type"))], df_cac[,"increase_type"], prox = TRUE)
  return(cac.rf)
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

