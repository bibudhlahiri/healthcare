library(RPostgreSQL)
library(ggplot2)
library(plyr)

process_five_number_summary <- function(fiveNumberSummary)
{
  return(paste("Q1 = ", round(fiveNumberSummary[2],2),
                    ", Median = ", round(fiveNumberSummary[3],2),
                    ", Q3 = ", round(fiveNumberSummary[4],2),
                    sep = ""))
}


ip_claim_amts_by_procedure <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
  statement <- "select a.prcdr_cd, a.clm_id, ip.clm_pmt_amt
                from (select *
                      from transformed_claim_prcdr_codes tcpc
                      where tcpc.claim_type = 'inpatient'
                      and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
                      and not exists (select 1 from transformed_claim_prcdr_codes tcpc1
                                      where tcpc.clm_id = tcpc1.clm_id
                                      and tcpc.prcdr_cd <> tcpc1.prcdr_cd)) a, inpatient_claims ip
                where a.clm_id = ip.clm_id
                order by a.prcdr_cd"
  res <- dbSendQuery(con, statement)
  proc_exps <- fetch(res, n = -1)
  threshold <- 200
  #Take the procedures for which there are more than or equal claims than a threshold
  #aggdata <- aggregate(x = proc_exps$clm_id, by = list(proc_exps$prcdr_cd), FUN = length, na.rm = TRUE)
  aggdata <- as.data.frame(table(proc_exps$prcdr_cd))
  
  colnames(aggdata) <- c("prcdr_cd", "n_claims")
  aggdata <- aggdata[order(-aggdata[,"n_claims"]),]
  aggdata <- subset(aggdata, (n_claims >= threshold))
  print(aggdata)
  for (i in 1:nrow(aggdata))
  {
    this_prcdr_cd <- aggdata[i, "prcdr_cd"]
    exp_this_proc <- subset(proc_exps, (prcdr_cd == this_prcdr_cd))
    filename <- paste("./figures/fraud_detection/inpatient_procedures/", this_prcdr_cd, ".png")
    png(filename,  width = 600, height = 480, units = "px")
    #p <- qplot(clm_pmt_amt, data = exp_this_proc, geom="histogram")
    fiveNumberSummary <- fivenum(exp_this_proc$clm_pmt_amt)
    axis_title <- paste("IP claim amounts for ", nrow(exp_this_proc), " claims", sep = "")
    p <- ggplot(exp_this_proc, aes(x = clm_pmt_amt)) + geom_histogram(aes(y = ..density..)) + geom_density() + 
         labs(x = paste(axis_title, process_five_number_summary(fiveNumberSummary), sep = "\n")) + ylab("Fraction of claims") 
    print(p)
    dev.off()
  }
  dbDisconnect(con)
  aggdata
}
