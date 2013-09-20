select *
from inpatient_claims
limit 1

select count(*)
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 7

--"V5789 (CARE INVOLVING OTHER SPECIFIED REHABILITATION PROCEDURE)", "78605 (SHORTNESS OF BREATH)", "486 (PNEUMONIA ORGANISM UNSPECIFIED)", 
--"4280 (CONGESTIVE HEART FAILURE UNSPECIFIED)", "78650 (UNSPECIFIED CHEST PAIN)"
select ADMTNG_ICD9_DGNS_CD, count(*)
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 7
group by ADMTNG_ICD9_DGNS_CD
order by count(*) desc

--90th percentile is 13 days
--"V5789 (CARE INVOLVING OTHER SPECIFIED REHABILITATION PROCEDURE)", "78605 (SHORTNESS OF BREATH)", "486 (PNEUMONIA ORGANISM UNSPECIFIED)", 
--"51881" (ACUTE RESPIRATORY FAILURE)", "0389" (UNSPECIFIED SEPTICEMIA)"
select ADMTNG_ICD9_DGNS_CD, count(*)
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
group by ADMTNG_ICD9_DGNS_CD
order by count(*) desc

--Are the possible values of ICD9_DGNS_CD_1 and ICD9_DGNS_CD_2 distinct, and so on? No.
select ip1.DESYNPUF_ID, ip1.ICD9_DGNS_CD_1, ip2.DESYNPUF_ID, ip2.ICD9_DGNS_CD_2
from inpatient_claims ip1, inpatient_claims ip2
where ip1.ICD9_DGNS_CD_1 = ip2.ICD9_DGNS_CD_2
order by ip1.DESYNPUF_ID, ip1.ICD9_DGNS_CD_1

--Can ICD9_DGNS_CD_1 and ICD9_DGNS_CD_2 have same value for same patient and same claim? No. Good.
select ip1.DESYNPUF_ID, ip1.ICD9_DGNS_CD_1, ip2.ICD9_DGNS_CD_2
from inpatient_claims ip1, inpatient_claims ip2
where ip1.ICD9_DGNS_CD_1 = ip2.ICD9_DGNS_CD_2
and ip1.DESYNPUF_ID = ip2.DESYNPUF_ID
and ip1.CLM_ID = ip2.CLM_ID
order by ip1.DESYNPUF_ID, ip1.ICD9_DGNS_CD_1

--2740 possible values
select distinct(ICD9_DGNS_CD_1)
from inpatient_claims
order by ICD9_DGNS_CD_1

--What tests did the long stayers undergo? 
--401.9 (UNSPECIFIED ESSENTIAL HYPERTENSION), "5990" (URINARY TRACT INFECTION SITE NOT SPECIFIED), 
--"5849" (ACUTE KIDNEY FAILURE, UNSPECIFIED), "25000" (DIABETES MELLITUS), "4280" (CONGESTIVE HEART FAILURE UNSPECIFIED),
--"V5789 (CARE INVOLVING OTHER SPECIFIED REHABILITATION PROCEDURE), "42731" (ATRIAL FIBRILLATION), "2724" (HYPERLIPIDEMIA),
--486 (PNEUMONIA ORGANISM UNSPECIFIED), "41401" (CORONARY ATHEROSCLEROSIS OF NATIVE CORONARY ARTERY)
select DGNS_CD, count(distinct CLM_ID)
from (
select DESYNPUF_ID, CLM_ID, ICD9_DGNS_CD_1 as DGNS_CD
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
and ICD9_DGNS_CD_1 is not null
union
select DESYNPUF_ID, CLM_ID, ICD9_DGNS_CD_2 as DGNS_CD
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
and ICD9_DGNS_CD_2 is not null
union
select DESYNPUF_ID, CLM_ID, ICD9_DGNS_CD_3 as DGNS_CD
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
and ICD9_DGNS_CD_3 is not null
union
select DESYNPUF_ID, CLM_ID, ICD9_DGNS_CD_4 as DGNS_CD
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
and ICD9_DGNS_CD_4 is not null
union
select DESYNPUF_ID, CLM_ID, ICD9_DGNS_CD_5 as DGNS_CD
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
and ICD9_DGNS_CD_5 is not null
union
select DESYNPUF_ID, CLM_ID, ICD9_DGNS_CD_6 as DGNS_CD
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
and ICD9_DGNS_CD_6 is not null
union
select DESYNPUF_ID, CLM_ID, ICD9_DGNS_CD_7 as DGNS_CD
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
and ICD9_DGNS_CD_7 is not null
union
select DESYNPUF_ID, CLM_ID, ICD9_DGNS_CD_8 as DGNS_CD
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
and ICD9_DGNS_CD_8 is not null
union
select DESYNPUF_ID, CLM_ID, ICD9_DGNS_CD_9 as DGNS_CD
from inpatient_claims
where (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
and ICD9_DGNS_CD_9 is not null) a
group by a.DGNS_CD
order by count(distinct CLM_ID) desc
limit 10

create table transformed_claim_diagnosis_codes
as select * from (
select DESYNPUF_ID, CLM_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT, ICD9_DGNS_CD_1 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_1 is not null
union
select DESYNPUF_ID, CLM_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT, ICD9_DGNS_CD_2 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_2 is not null
union
select DESYNPUF_ID, CLM_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT, ICD9_DGNS_CD_3 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_3 is not null
union
select DESYNPUF_ID, CLM_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT, ICD9_DGNS_CD_4 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_4 is not null
union
select DESYNPUF_ID, CLM_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT, ICD9_DGNS_CD_5 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_5 is not null
union
select DESYNPUF_ID, CLM_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT, ICD9_DGNS_CD_6 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_6 is not null
union
select DESYNPUF_ID, CLM_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT, ICD9_DGNS_CD_7 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_7 is not null
union
select DESYNPUF_ID, CLM_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT, ICD9_DGNS_CD_8 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_8 is not null
union
select DESYNPUF_ID, CLM_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT, ICD9_DGNS_CD_9 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_9 is not null) a

--What 2-combination of tests do the long-stayers undergo?
--(4019:HYPERTENSION, V5789: CARE INVOLVING OTHER SPECIFIED REHABILITATION PROCEDURE)
--(25000:DIABETES MELLITUS, 4019:HYPERTENSION)
--(2724:HYPERLIPIDEMIA, 4019:HYPERTENSION)
--(4019:HYPERTENSION, 5990:URINARY TRACT INFECTION)
--(4019:HYPERTENSION, 42731:ATRIAL FIBRILLATION)
select a.code1, a.code2, count(distinct a.CLM_ID)
from (select tcdc1.DESYNPUF_ID, tcdc1.CLM_ID, tcdc1.DGNS_CD code1, tcdc2.DGNS_CD code2
      from transformed_claim_diagnosis_codes tcdc1, transformed_claim_diagnosis_codes tcdc2
      where tcdc1.DESYNPUF_ID = tcdc2.DESYNPUF_ID
      and tcdc1.CLM_ID = tcdc2.CLM_ID
      and (tcdc1.NCH_BENE_DSCHRG_DT - tcdc1.CLM_ADMSN_DT + 1) >= 13
      and tcdc1.DGNS_CD < tcdc2.DGNS_CD
      order by tcdc1.DESYNPUF_ID, tcdc1.CLM_ID, tcdc1.DGNS_CD, tcdc2.DGNS_CD) a
group by a.code1, a.code2s
order by count(distinct a.CLM_ID) desc

--What 3-combination of tests do the long-stayers undergo? Trying to guess what conditions they were treated for...
--(4019:HYPERTENSION, 5990:URINARY TRACT INFECTION, V5789: CARE INVOLVING OTHER SPECIFIED REHABILITATION PROCEDURE)
--(2724:HYPERLIPIDEMIA, 4019:HYPERTENSION, V5789: CARE INVOLVING OTHER SPECIFIED REHABILITATION PROCEDURE)
--(4019:HYPERTENSION, 7812: ABNORMALITY OF GAIT, V5789: CARE INVOLVING OTHER SPECIFIED REHABILITATION PROCEDURE)
--(25000:DIABETES MELLITUS, 2724:HYPERLIPIDEMIA, 4019:HYPERTENSION)
--(25000:DIABETES MELLITUS, 4019:HYPERTENSION, V5789: CARE INVOLVING OTHER SPECIFIED REHABILITATION PROCEDURE)
--(2724: HYPERLIPIDEMIA, 4019:HYPERTENSION, 41401: CORONARY ATHEROSCLEROSIS)
--(0389: UNSPECIFIED SEPTICEMIA, 486: PNEUMONIA ORGANISM UNSPECIFIED, 5849: ACUTE KIDNEY FAILURE)
select a.code1, a.code2, a.code3, count(distinct a.CLM_ID)
from (select tcdc1.DESYNPUF_ID, tcdc1.CLM_ID, tcdc1.DGNS_CD code1, tcdc2.DGNS_CD code2, tcdc3.DGNS_CD code3
      from transformed_claim_diagnosis_codes tcdc1, transformed_claim_diagnosis_codes tcdc2, transformed_claim_diagnosis_codes tcdc3
      where tcdc1.DESYNPUF_ID = tcdc2.DESYNPUF_ID
      and tcdc1.CLM_ID = tcdc2.CLM_ID
      and tcdc2.DESYNPUF_ID = tcdc3.DESYNPUF_ID
      and tcdc2.CLM_ID = tcdc3.CLM_ID
      and (tcdc1.NCH_BENE_DSCHRG_DT - tcdc1.CLM_ADMSN_DT + 1) >= 13
      and tcdc1.DGNS_CD < tcdc2.DGNS_CD
      and tcdc2.DGNS_CD < tcdc3.DGNS_CD
      order by tcdc1.DESYNPUF_ID, tcdc1.CLM_ID, tcdc1.DGNS_CD, tcdc2.DGNS_CD, tcdc3.DGNS_CD) a
group by a.code1, a.code2, a.code3
order by count(distinct a.CLM_ID) desc

select *
from transformed_claim_diagnosis_codes
where DESYNPUF_ID = '00013D2EFD8E45D1'
and CLM_ID = '196661176988405'





--For people with multiple claims, are the claims for diff hospital admissions or same?
select DESYNPUF_ID, count(distinct CLM_ID)
from inpatient_claims
group by DESYNPUF_ID
order by count(distinct CLM_ID) desc
limit 10

--Hospital stays can be overlapping! How often?
select DESYNPUF_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT
from inpatient_claims 
where DESYNPUF_ID in ('CD57573A5B77AAFE', 'FC02104A9F15D57C', '7D0B2F944DE86994', 'D7279B2238D4AF28', '3A9D94388C426924')
order by DESYNPUF_ID, CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT

select ip1.DESYNPUF_ID, ip1.CLM_ID, ip1.CLM_ADMSN_DT, ip2.CLM_ID, ip2.CLM_ADMSN_DT, ip1.NCH_BENE_DSCHRG_DT, ip2.NCH_BENE_DSCHRG_DT
from inpatient_claims ip1, inpatient_claims ip2
where ip1.DESYNPUF_ID = ip2.DESYNPUF_ID
and ip2.CLM_ADMSN_DT > ip1.CLM_ADMSN_DT 
and ip2.CLM_ADMSN_DT < ip1.NCH_BENE_DSCHRG_DT
order by ip1.DESYNPUF_ID, ip1.CLM_ADMSN_DT

select count(*)
from beneficiary_summary

select *
from beneficiary_summary
limit 1

select sp_cncr, count(*)
from beneficiary_summary
group by sp_cncr

--More women (55%) than men (45%) in long-stayers
select BENE_SEX_IDENT_CD, count(distinct ic.DESYNPUF_ID)
from inpatient_claims ic, beneficiary_summary bs
where ic.DESYNPUF_ID = bs.DESYNPUF_ID
and (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
group by BENE_SEX_IDENT_CD

--But 55% female in overall population.
select BENE_SEX_IDENT_CD, count(*)
from beneficiary_summary bs
group by BENE_SEX_IDENT_CD

--5523 (58%) had chronic heart failure, 3989 (42%) did not.
select SP_CHF, count(distinct ic.DESYNPUF_ID)
from inpatient_claims ic, beneficiary_summary bs
where ic.DESYNPUF_ID = bs.DESYNPUF_ID
and (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
group by SP_CHF

--101752 (30%) had chronic heart failure, 241892 (70%) did not.
select SP_CHF, count(*)
from beneficiary_summary bs
group by SP_CHF

--5008 (50%) had chronic kidney disease, 4932 (50%) did not.
select SP_CHRNKIDN, count(distinct ic.DESYNPUF_ID)
from inpatient_claims ic, beneficiary_summary bs
where ic.DESYNPUF_ID = bs.DESYNPUF_ID
and (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
group by SP_CHRNKIDN

--58023 (17%) had chronic kidney disease, 285621 (83%) did not.
select SP_CHRNKIDN, count(*)
from beneficiary_summary bs
group by SP_CHRNKIDN

--1959 (25%) had cancer, 5917 (75%) did not.
select SP_CNCR, count(distinct ic.DESYNPUF_ID)
from inpatient_claims ic, beneficiary_summary bs
where ic.DESYNPUF_ID = bs.DESYNPUF_ID
and (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
group by SP_CNCR

--22484 (6.5%) had cancer, 321160 (93.5%) did not
select SP_CNCR, count(*)
from beneficiary_summary bs
group by SP_CNCR

--5637 (60%) had diabetes, 3742 (40%) did not
select SP_DIABETES, count(distinct ic.DESYNPUF_ID)
from inpatient_claims ic, beneficiary_summary bs
where ic.DESYNPUF_ID = bs.DESYNPUF_ID
and (NCH_BENE_DSCHRG_DT - CLM_ADMSN_DT + 1) >= 13
group by SP_DIABETES

--124747 (36%) had diabetes, 218897 (64%) did not
select SP_DIABETES, count(*)
from beneficiary_summary bs
group by SP_DIABETES


--Patients whose cost of claims increased the most between 2008 and 2009
select DESYNPUF_ID, to_char(clm_from_dt, 'YYYY'), sum(clm_pmt_amt) 
from inpatient_claims
group by DESYNPUF_ID, to_char(clm_from_dt, 'YYYY')
order by sum(clm_pmt_amt) desc


create table claims_by_patient_year as
select DESYNPUF_ID, to_char(clm_from_dt, 'YYYY') as year, sum(clm_pmt_amt) total_claim_amt
from inpatient_claims
group by DESYNPUF_ID, to_char(clm_from_dt, 'YYYY')
order by DESYNPUF_ID, to_char(clm_from_dt, 'YYYY')

select *
from claims_by_patient_year
limit 1

select DESYNPUF_ID, count(*)
from claims_by_patient_year
group by DESYNPUF_ID
having count(*) > 3

select *
from claims_by_patient_year
where DESYNPUF_ID = '0248E8733BFBF500'

select count(*)
from claims_by_patient_year
where year = '2008'
and total_claim_amt > 0


select *
from inpatient_claims
where DESYNPUF_ID = 'B010E908FD8DED6A'
and to_char(clm_from_dt, 'YYYY') = '2008'

--15861 patients
select count(distinct DESYNPUF_ID)
from inpatient_claims
where to_char(clm_from_dt, 'YYYY') = '2008'

--15864 patients
select count(distinct DESYNPUF_ID)
from inpatient_claims
where to_char(clm_admsn_dt, 'YYYY') = '2008'

--2404 patients (out of 15861, 15%) showed an increase from 2008 to 2009
select cbpy2.DESYNPUF_ID, cbpy1.total_claim_amt cost_2008, cbpy2.total_claim_amt cost_2009, 
(cbpy2.total_claim_amt - cbpy1.total_claim_amt) increase
from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2s
where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
and cbpy1.year = '2008'
and cbpy2.year = '2009'
and cbpy2.total_claim_amt > cbpy1.total_claim_amt 
and cbpy1.total_claim_amt > 0
order by (cbpy2.total_claim_amt - cbpy1.total_claim_amt) desc

--The total increase in amount is $31,493,330.00
select sum(a.increase) from 
(select cbpy2.DESYNPUF_ID, cbpy1.total_claim_amt cost_2008, cbpy2.total_claim_amt cost_2009, 
(cbpy2.total_claim_amt - cbpy1.total_claim_amt) increase
from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2
where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
and cbpy1.year = '2008'
and cbpy2.year = '2009'
and cbpy2.total_claim_amt > cbpy1.total_claim_amt 
and cbpy1.total_claim_amt > 0) a

--Total 5,035 patients (32%) carried over from 2008 to 2009, out of 15861 in 2008.
--Cost increased for 2290 (45%) of them, decreased for 2542 (50%) of them, remained same for 203 (4%) of them.
select a.DESYNPUF_ID, a.cost_2008, a.cost_2009, a.times_increase,
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
      order by times_increase desc) a
--where a.times_increase = 1

--Other info for people who carried over from 2008 to 2009
select bene_sex_ident_cd,
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
      order by times_increase desc) a, beneficiary_summary b
where a.DESYNPUF_ID = b.DESYNPUF_ID
order by a.DESYNPUF_ID


select *
from beneficiary_summary
where DESYNPUF_ID = '0016D2185D29BC11'

select DESYNPUF_ID, count(*)
from beneficiary_summary
group by DESYNPUF_ID
order by count(*) desc


--2290 patients out of showed an increase from 2008 to 2009. The median is 2.2 times.
select cbpy2.DESYNPUF_ID, cbpy1.total_claim_amt cost_2008, cbpy2.total_claim_amt cost_2009, 
(cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) times_increase
from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2
where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
and cbpy1.year = '2008'
and cbpy2.year = '2009'
and cbpy1.total_claim_amt > 0
and (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) > 1
--and (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) <= 50
order by times_increase desc

--What did they do in 2009 that they did not do in 2008?
--4019:HYPERTENSION, 2724: HYPERLIPIDEMIA, 25000: DIABETES MELLITUS, 
--4280: CONGESTIVE HEART FAILURE UNSPECIFIED, 41401: CORONARY ATHEROSCLEROSIS OF NATIVE CORONARY ARTERY
select tcdc_2009.dgns_cd, count(distinct tcdc_2009.DESYNPUF_ID)
from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2, transformed_claim_diagnosis_codes tcdc_2009
where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
and cbpy1.year = '2008'
and cbpy2.year = '2009'
and cbpy2.total_claim_amt > cbpy1.total_claim_amt 
and cbpy1.total_claim_amt > 0
and cbpy1.DESYNPUF_ID = tcdc_2009.DESYNPUF_ID
and to_char(tcdc_2009.clm_admsn_dt, 'YYYY') = '2009'
and not exists (select 1 from transformed_claim_diagnosis_codes tcdc_2008
                where tcdc_2008.DESYNPUF_ID = tcdc_2009.DESYNPUF_ID
                and tcdc_2008.dgns_cd = tcdc_2009.dgns_cd
                and to_char(tcdc_2008.clm_admsn_dt, 'YYYY') = '2008')
group by tcdc_2009.dgns_cd
order by count(distinct tcdc_2009.DESYNPUF_ID) desc


--What was the total number of days spent in hospital by these 2290 patients in 2008 vs in 2009?
select ip_2008.DESYNPUF_ID, sum(ip_2008.NCH_BENE_DSCHRG_DT - ip_2008.CLM_ADMSN_DT + 1) los_2008, 
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
having sum(ip_2008.NCH_BENE_DSCHRG_DT - ip_2008.CLM_ADMSN_DT + 1) <= 100 
and sum(ip_2009.NCH_BENE_DSCHRG_DT - ip_2009.CLM_ADMSN_DT + 1) <= 100
order by ip_2008.DESYNPUF_ID

--1355 of the 2290 (59%) had a longer length of stay in 2009.
select ip_2008.DESYNPUF_ID, sum(ip_2008.NCH_BENE_DSCHRG_DT - ip_2008.CLM_ADMSN_DT + 1) los_2008, 
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
having sum(ip_2009.NCH_BENE_DSCHRG_DT - ip_2009.CLM_ADMSN_DT + 1) > sum(ip_2008.NCH_BENE_DSCHRG_DT - ip_2008.CLM_ADMSN_DT + 1)
order by ip_2008.DESYNPUF_ID

--The median increase in LoS is 8 days from 2008 to 2009 for these patients.
select a.DESYNPUF_ID, a.los_2008, a.los_2009, (a.los_2009 - a.los_2008) increase_in_los
from (select ip_2008.DESYNPUF_ID, sum(ip_2008.NCH_BENE_DSCHRG_DT - ip_2008.CLM_ADMSN_DT + 1) los_2008, 
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
order by ip_2008.DESYNPUF_ID) a
where a.los_2009 > a.los_2008
order by (a.los_2009 - a.los_2008) desc




--18831
select count(distinct DESYNPUF_ID)
from inpatient_claims
where to_char(clm_from_dt, 'YYYY') = '2009'

--The total increase in amount is $13,221,020.00
select sum(a.increase) from 
(select cbpy2.DESYNPUF_ID, cbpy1.total_claim_amt cost_2009, cbpy2.total_claim_amt cost_2010, 
(cbpy2.total_claim_amt - cbpy1.total_claim_amt) increase
from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2
where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
and cbpy1.year = '2009'
and cbpy2.year = '2010'
and cbpy2.total_claim_amt > cbpy1.total_claim_amt 
and cbpy1.total_claim_amt > 0) a

--1116 patients out of 18831 (5.9%) showed an increase from 2009 to 2010. The median here is 2.2, too!
select cbpy2.DESYNPUF_ID, cbpy1.total_claim_amt cost_2009, cbpy2.total_claim_amt cost_2010, 
(cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) times_increase
from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2
where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
and cbpy1.year = '2009'
and cbpy2.year = '2010'
and cbpy1.total_claim_amt > 0
and (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) > 1
order by times_increase desc


SELECT a.attname chronic_condition
FROM pg_class c, pg_attribute a
WHERE c.relname = 'beneficiary_summary_2008'
AND a.attnum > 0
AND a.attrelid = c.oid
and a.attname like 'sp_%'
and a.attname <> 'sp_state_code'
order by a.attname


--Did they develop chronic conditions in 2009 that they did not have in 2008?
--Out of 2290, 585 (25% of 2290) did not have sp_alzhdmta in 2008 but developed in 2009
select cbpy2.DESYNPUF_ID, cbpy1.total_claim_amt cost_2008, cbpy2.total_claim_amt cost_2009, 
       bs1.sp_alzhdmta, bs2.sp_alzhdmta
from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2, beneficiary_summary_2008 bs1, beneficiary_summary_2009 bs2
where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
and cbpy1.year = '2008'
and cbpy2.year = '2009'
and cbpy1.total_claim_amt > 0
/*and (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) > 1*/
 and cbpy2.total_claim_amt > cbpy1.total_claim_amt
/*and bs1.sp_alzhdmta = '2'
and bs2.sp_alzhdmta = '1'*/
and cbpy1.DESYNPUF_ID = bs1.DESYNPUF_ID
and cbpy2.DESYNPUF_ID = bs2.DESYNPUF_ID
order by cbpy2.DESYNPUF_ID

--Out of 2290, 353 (15% of 2290) did not have sp_chf in 2008 but developed in 2009
select count(*)
from claims_by_patient_year cbpy1, claims_by_patient_year cbpy2, beneficiary_summary_2008 bs1, beneficiary_summary_2009 bs2
where cbpy1.DESYNPUF_ID = cbpy2.DESYNPUF_ID
and cbpy1.year = '2008'
and cbpy2.year = '2009'
and cbpy1.total_claim_amt > 0
and (cast(cbpy2.total_claim_amt as real)/cbpy1.total_claim_amt) > 1
and bs1.sp_chf = '2'
and bs2.sp_chf = '1'
and cbpy1.DESYNPUF_ID = bs1.DESYNPUF_ID
and cbpy2.DESYNPUF_ID = bs2.DESYNPUF_ID




