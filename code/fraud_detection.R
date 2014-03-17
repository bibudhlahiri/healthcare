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
    data_for_novelty <- data_for_novelty[,!(names(data_for_novelty) %in% c("prcdr_cd", "clm_id", "desynpuf_id", "long_desc", "short_desc", "count"))]
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
