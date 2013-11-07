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

