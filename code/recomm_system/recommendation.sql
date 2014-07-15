--52% of those who developed alzhdmta got cured
select a.cured_alzhdmta, count(*)
from (select b1.desynpuf_id, case when (b1.sp_alzhdmta = '1' and b2.sp_alzhdmta = '2') then 1 else 0 end as cured_alzhdmta 
      from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
      where b1.desynpuf_id = b2.desynpuf_id 
      and b1.sp_alzhdmta = '1') a
group by a.cured_alzhdmta

--42% of those who developed chrnkidn got cured
select a.cured_chrnkidn, count(*)
from (select b1.desynpuf_id, case when (b1.sp_chrnkidn = '1' and b2.sp_chrnkidn = '2') then 1 else 0 end as cured_chrnkidn
      from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
      where b1.desynpuf_id = b2.desynpuf_id
      and b1.sp_chrnkidn = '1') a
group by a.cured_chrnkidn

--35% got cured
select a.cured_chf, count(*)
from (select b1.desynpuf_id, case when (b1.sp_chf = '1' and b2.sp_chf = '2') then 1 else 0 end as cured_chf
      from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
      where b1.desynpuf_id = b2.desynpuf_id and b1.sp_chf = '1') a
group by a.cured_chf

--46.7% got cured
select a.cured_cncr, count(*)
from (select b1.desynpuf_id, case when (b1.sp_cncr = '1' and b2.sp_cncr = '2') then 1 else 0 end as cured_cncr
      from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
      where b1.desynpuf_id = b2.desynpuf_id and b1.sp_cncr = '1') a
group by a.cured_cncr


--Cured alzhdmta 
select nc.substancename
from prescription_drug_events pde1, ndc_codes nc
where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
and nc.substancename is not null
and pde1.desynpuf_id = '00131C35661B2926' 
and to_char(pde1.srvc_dt, 'YYYY') = '2008'

select b1.desynpuf_id, nc.substancename
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, prescription_drug_events pde1, ndc_codes nc
where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
and b1.desynpuf_id = b2.desynpuf_id
and nc.substancename is not null
and b1.sp_alzhdmta = '1' and b2.sp_alzhdmta = '2'
and pde1.desynpuf_id = b1.desynpuf_id
and to_char(pde1.srvc_dt, 'YYYY') = '2008'
order by b1.desynpuf_id

--Lisinopril is used to treat high blood pressure. It is used in combination with other medications to treat heart failure.
--Simvastatinis used to reduce the amount of fatty substances such as low-density lipoprotein (LDL) cholesterol ('bad cholesterol') and triglycerides in the blood 
select nc.substancename, count(distinct b1.desynpuf_id)
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, prescription_drug_events pde1, ndc_codes nc
where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
and b1.desynpuf_id = b2.desynpuf_id
and nc.substancename is not null
and b1.sp_chrnkidn = '1' and b2.sp_chrnkidn = '2'
and pde1.desynpuf_id = b1.desynpuf_id
and to_char(pde1.srvc_dt, 'YYYY') = '2008'
group by nc.substancename
order by count(distinct b1.desynpuf_id) desc

--"METFORMIN HYDROCHLORIDE"
select nc.substancename, count(distinct b1.desynpuf_id)
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, prescription_drug_events pde1, ndc_codes nc
where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
and b1.desynpuf_id = b2.desynpuf_id
and nc.substancename is not null
and b1.sp_chf = '1' and b2.sp_chf = '2'
and pde1.desynpuf_id = b1.desynpuf_id
and to_char(pde1.srvc_dt, 'YYYY') = '2008'
group by nc.substancename
order by count(distinct b1.desynpuf_id) desc

--"LEVOTHYROXINE SODIUM" (thyroid hormone) helps to maintain brain function, utilization of food, and body temperature.
--Lisinopril is used to treat high blood pressure. It is used in combination with other medications to treat heart failure.
--"LOVASTATIN" for is used for lowering cholesterol (hypolipidemic agent) in those with hypercholesterolemia to reduce risk of cardiovascular disease.
--"ACETAMINOPHEN; HYDROCODONE BITARTRATE" is used for the relief of moderate to moderately severe pain
select nc.substancename, count(distinct b1.desynpuf_id)
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, prescription_drug_events pde1, ndc_codes nc
where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
and b1.desynpuf_id = b2.desynpuf_id
and nc.substancename is not null
and b1.sp_chf = '1' and b2.sp_chf = '2'
and b1.sp_alzhdmta = '2' and b1.sp_chrnkidn = '2'
and b1.sp_cncr = '2' and b1.sp_copd = '2'
and b1.sp_depressn = '2' and b1.sp_diabetes = '2'
and b1.sp_ischmcht = '2' and b1.sp_osteoprs = '2'
and b1.sp_ra_oa = '2' and b1.sp_strketia = '2'
and pde1.desynpuf_id = b1.desynpuf_id
and to_char(pde1.srvc_dt, 'YYYY') = '2008'
group by nc.substancename
order by count(distinct b1.desynpuf_id) desc

--"00B7C860FC746768" was recovered from sp_chf and was not diagnosd with any other chronic condition in 2008. Person took 
--Risperidone which is mainly used to treat schizophrenia (including adolescent schizophrenia), schizoaffective disorder, 
--the mixed and manic states associated with bipolar disorder, and irritability in people with autism.
select nc.substancename, b1.desynpuf_id
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, prescription_drug_events pde1, ndc_codes nc
where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
and b1.desynpuf_id = b2.desynpuf_id
and nc.substancename is not null
and b1.sp_chf = '1' and b2.sp_chf = '2'
and b1.sp_alzhdmta = '2' and b1.sp_chrnkidn = '2'
and b1.sp_cncr = '2' and b1.sp_copd = '2'
and b1.sp_depressn = '2' and b1.sp_diabetes = '2'
and b1.sp_ischmcht = '2' and b1.sp_osteoprs = '2'
and b1.sp_ra_oa = '2' and b1.sp_strketia = '2'
and pde1.desynpuf_id = b1.desynpuf_id
and to_char(pde1.srvc_dt, 'YYYY') = '2008'
order by b1.desynpuf_id

--00B7C860FC746768 was diagnosed with some other conditions, like 
--"Unspecified acquired hypothyroidism" and "Pure hypercholesterolemia"
select *
from transformed_claim_diagnosis_codes, diagnosis_codes
where desynpuf_id = '00B7C860FC746768'
and clm_thru_year = '2008'
and diagnosis_code = dgns_cd

--No procedure was performed on the person!
select *
from transformed_claim_prcdr_codes, procedure_codes
where desynpuf_id = '00B7C860FC746768'
and clm_thru_year = '2008'
and procedure_code = prcdr_cd

select sp_chf
from beneficiary_summary_2010
where desynpuf_id = '00B7C860FC746768'

select *
from diagnosis_codes
where long_desc like '%sperm%'

select age(tcdc.clm_thru_dt, b.bene_birth_dt)
from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
where dgns_cd = '6060'
and clm_thru_year = '2008'
and b.desynpuf_id = tcdc.desynpuf_id

--(4019, 9904); (4019, 8154); (4019, 3893), (25000, 9904)
--401.9	UNSPECIFIED ESSENTIAL HYPERTENSION, 9904: Packed cell transfusion, 
--8154: Total knee replacement, 3893: Venous Catheterization,
--25000: DIABETES MELLITUS, "3995": Hemodialysis, 2724: HYPERLIPIDEMIA,
--4516: Esophagogastroduodenoscopy with closed biopsy
select tcdc.dgns_cd, tcpc.prcdr_cd, count(distinct tcdc.desynpuf_id)
from transformed_claim_diagnosis_codes tcdc, transformed_claim_prcdr_codes tcpc
where tcdc.clm_thru_year = '2008'
and tcpc.clm_thru_year = '2008'
and tcdc.desynpuf_id = tcpc.desynpuf_id
and not exists (select 1 from transformed_claim_diagnosis_codes tcdc1 where tcdc1.dgns_cd = tcpc.prcdr_cd)
group by tcdc.dgns_cd, tcpc.prcdr_cd
order by count(distinct tcdc.desynpuf_id) desc





select tcpc.prcdr_cd, tcdc.dgns_cd, count(distinct tcdc.desynpuf_id)
from transformed_claim_diagnosis_codes tcdc, transformed_claim_prcdr_codes tcpc
where tcdc.clm_thru_year = '2008'
and tcpc.clm_thru_year = '2008'
and tcdc.desynpuf_id = tcpc.desynpuf_id
and not exists (select 1 from transformed_claim_diagnosis_codes tcdc1 where tcdc1.dgns_cd = tcpc.prcdr_cd)
group by tcpc.prcdr_cd, tcdc.dgns_cd
order by count(distinct tcdc.desynpuf_id) desc

select sp_alzhdmta, avg(extract(year from age(to_date('2008-01-01', 'YYYY-MM-DD'), b.bene_birth_dt)))
from beneficiary_summary_2008 b
group by sp_alzhdmta

select sp_chf, avg(extract(year from age(to_date('2008-01-01', 'YYYY-MM-DD'), b.bene_birth_dt)))
from beneficiary_summary_2008 b
group by sp_chf

select sp_chrnkidn, avg(extract(year from age(to_date('2008-01-01', 'YYYY-MM-DD'), b.bene_birth_dt)))
from beneficiary_summary_2008 b
group by sp_chrnkidn

select bene_sex_ident_cd, sp_alzhdmta, count(*)
from beneficiary_summary_2008 b
group by bene_sex_ident_cd, sp_alzhdmta
order by bene_sex_ident_cd, sp_alzhdmta

select bene_sex_ident_cd, sp_chrnkidn, count(*)
from beneficiary_summary_2008 b
group by bene_sex_ident_cd, sp_chrnkidn
order by bene_sex_ident_cd, sp_chrnkidn

--Top 5 are 4019, 25000, 2724, 42731, V5869
select tcdc.dgns_cd, count(distinct tcdc.desynpuf_id)
from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
where tcdc.clm_thru_year = '2008'
and b.desynpuf_id = tcdc.desynpuf_id
and b.sp_alzhdmta = '1'
group by tcdc.dgns_cd
order by count(distinct tcdc.desynpuf_id) desc

--Top 5 are 4019, 25000, 2724, 42731, 4280
select tcdc.dgns_cd, count(distinct tcdc.desynpuf_id)
from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
where tcdc.clm_thru_year = '2008'
and b.desynpuf_id = tcdc.desynpuf_id
and b.sp_chrnkidn = '1'
group by tcdc.dgns_cd
order by count(distinct tcdc.desynpuf_id) desc

--10,641 distinct diagnosed conditions
select count(distinct tcdc.dgns_cd)
from transformed_claim_diagnosis_codes tcdc
where tcdc.clm_thru_year = '2008'

--2,914 distinct procedures
select count(distinct tcpc.prcdr_cd)
from transformed_claim_prcdr_codes tcpc
where tcpc.clm_thru_year = '2008'

--What is the number of distinct conditions people were diagnosed with for different procedures?
--The median number of distinct conditions for a procedure is 77. Max is 4562.
select tcpc.prcdr_cd, count(distinct tcdc.dgns_cd)
from transformed_claim_diagnosis_codes tcdc, transformed_claim_prcdr_codes tcpc
where tcdc.clm_thru_year = '2008'
and tcpc.clm_thru_year = '2008'
and tcdc.desynpuf_id = tcpc.desynpuf_id
group by tcpc.prcdr_cd
order by count(distinct tcdc.dgns_cd)
limit 1457

select count(*) from beneficiary_summary_2008

--1,806 distinct substances
select count(distinct nc.substancename)
from beneficiary_summary_2008 b1, prescription_drug_events pde1, ndc_codes nc
where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
and nc.substancename is not null
and pde1.desynpuf_id = b1.desynpuf_id
and to_char(pde1.srvc_dt, 'YYYY') = '2008'

select distinct b1.desynpuf_id, nc.substancename
from beneficiary_summary_2008 b1, prescription_drug_events pde1, ndc_codes nc
where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
and nc.substancename is not null
and pde1.desynpuf_id = b1.desynpuf_id
and to_char(pde1.srvc_dt, 'YYYY') = '2008'
order by b1.desynpuf_id

select b1.desynpuf_id, b1.sp_alzhdmta as sp_alzhdmta_2008, b1.sp_chf as sp_chf_2008, 
b1.sp_chrnkidn as sp_chrnkidn_2008, b1.sp_cncr as sp_cncr_2008, b1.sp_copd as sp_copd_2008, b1.sp_depressn as sp_depressn_2008, 
b1.sp_diabetes as sp_diabetes_2008, b1.sp_ischmcht as sp_ischmcht_2008, b1.sp_osteoprs as sp_osteoprs_2008, b1.sp_ra_oa as sp_ra_oa_2008, b1.sp_strketia as sp_strketia_2008,
b2.sp_alzhdmta as sp_alzhdmta_2009, b2.sp_chf as sp_chf_2009, 
b2.sp_chrnkidn as sp_chrnkidn_2009, b2.sp_cncr as sp_cncr_2009, b2.sp_copd as sp_copd_2009, b2.sp_depressn as sp_depressn_2009, 
b2.sp_diabetes as sp_diabetes_2009, b2.sp_ischmcht as sp_ischmcht_2009, b2.sp_osteoprs as sp_osteoprs_2009, b2.sp_ra_oa as sp_ra_oa_2009, b2.sp_strketia as sp_strketia_2009
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
where b1.desynpuf_id = b2.desynpuf_id

--Total sum of counts is 385772, sum of top 50 rows is 118294,
--sum of top 100 rows is 155688
--sum of top 200 rows is 199783
--sum of top 300 rows is 225858 (58.5% of 385772)
select sum(a.count)
from (
select tcdc.dgns_cd, count(distinct tcdc.desynpuf_id)
from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
where tcdc.clm_thru_year = '2008'
and b.desynpuf_id = tcdc.desynpuf_id
and b.sp_alzhdmta = '1'
group by tcdc.dgns_cd
order by count(distinct tcdc.desynpuf_id) desc
limit 300) a

--Total 947391 diagnoses in 2008 for the people registered in 2008
--sum of top 1000 diagnoses is 738291 (77.8%)
--sum of top 2000 diagnoses is 830511 (87.66%)
select sum(a.count)
from (
select tcdc.dgns_cd, count(*)
from transformed_claim_diagnosis_codes tcdc
where tcdc.clm_thru_year = '2008'
and exists (select 1 from beneficiary_summary_2008 b where b.desynpuf_id = tcdc.desynpuf_id)
group by tcdc.dgns_cd
order by count(*) desc
limit 2000) a

--830511 rows
select * from transformed_claim_diagnosis_codes tcdc1 where tcdc1.clm_thru_year = '2008'
and tcdc1.dgns_cd in (select tcdc.dgns_cd
from transformed_claim_diagnosis_codes tcdc
where tcdc.clm_thru_year = '2008'
and exists (select 1 from beneficiary_summary_2008 b where b.desynpuf_id = tcdc.desynpuf_id)
group by tcdc.dgns_cd
order by count(*) desc
limit 2000)

--947391 rows 
select * from transformed_claim_diagnosis_codes tcdc1 where tcdc1.clm_thru_year = '2008'


select tcdc.dgns_cd, nc.substancename, count(distinct tcdc.desynpuf_id)
from transformed_claim_diagnosis_codes tcdc, prescription_drug_events pde1, ndc_codes nc
where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
and nc.substancename is not null
and pde1.desynpuf_id = tcdc.desynpuf_id
and to_char(pde1.srvc_dt, 'YYYY') = '2008'
and tcdc.clm_thru_year = '2008'
group by tcdc.dgns_cd, nc.substancename
order by count(distinct tcdc.desynpuf_id) desc

--Top 5 are 4019 ("Unspecified essential hypertension"), 25000 ("Diabetes mellitus"), 2724, 4011 ("Benign essential hypertension"), V5869
select tcdc.dgns_cd, dc.long_desc, count(distinct tcdc.desynpuf_id)
from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, diagnosis_codes dc
where tcdc.clm_thru_year = '2008'
and b1.desynpuf_id = tcdc.desynpuf_id
and b1.desynpuf_id = b2.desynpuf_id 
and tcdc.dgns_cd = dc.diagnosis_code
group by tcdc.dgns_cd, dc.long_desc
order by count(distinct tcdc.desynpuf_id) desc
limit 5

--Top 5 are 9904 ("Transfusion of packed cells"), 8154 ("Total knee replacement"), 3893 ("Venous catheterization, not elsewhere classified"), 
--3995 ("Hemodialysis"), 4516 ("Esophagogastroduodenoscopy [EGD] with closed biopsy")
select tcpc.prcdr_cd, pc.long_desc, count(distinct tcpc.desynpuf_id)
from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, procedure_codes pc
where tcpc.clm_thru_year = '2008'
and b1.desynpuf_id = tcpc.desynpuf_id
and b1.desynpuf_id = b2.desynpuf_id 
and tcpc.prcdr_cd = pc.procedure_code
and not exists (select 1 from transformed_claim_diagnosis_codes tcdc1 where tcdc1.dgns_cd = tcpc.prcdr_cd)
group by tcpc.prcdr_cd, pc.long_desc
order by count(distinct tcpc.desynpuf_id) desc
limit 5

--Top 5 are "LOVASTATIN" (to reduce risk of cardiovascular disease), "GEMFIBROZIL" (used to lower lipid levels), "SULFASALAZINE" (to treat rheumatoid arthritis), 
--"LOSARTAN POTASSIUM" (to treat high blood pressure), "VALSARTAN" (reduces blood pressure)
select nc.hipaa_ndc_labeler_product_code, nc.substancename, count(distinct b1.desynpuf_id)
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2, prescription_drug_events pde1, ndc_codes nc
where pde1.hipaa_ndc_labeler_product_code = nc.hipaa_ndc_labeler_product_code
and b1.desynpuf_id = b2.desynpuf_id
and nc.substancename is not null
and pde1.desynpuf_id = b1.desynpuf_id
and to_char(pde1.srvc_dt, 'YYYY') = '2008'
group by nc.hipaa_ndc_labeler_product_code, nc.substancename
order by count(distinct b1.desynpuf_id) desc
limit 5

--114,538
select count(distinct b1.desynpuf_id)
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
where b1.desynpuf_id = b2.desynpuf_id

--28,359
select count(distinct b1.desynpuf_id)
from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
where tcdc.clm_thru_year = '2008'
and b1.desynpuf_id = tcdc.desynpuf_id
and b1.desynpuf_id = b2.desynpuf_id 
and tcdc.dgns_cd = '4019'

--86,179
select count(distinct b1.desynpuf_id)
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
where b1.desynpuf_id = b2.desynpuf_id 
and not exists (select 1 from transformed_claim_diagnosis_codes tcdc where tcdc.clm_thru_year = '2008'
and b1.desynpuf_id = tcdc.desynpuf_id
and tcdc.dgns_cd = '4019')

--32,652
select count(distinct b1.desynpuf_id)
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
where b1.desynpuf_id = b2.desynpuf_id 
and b1.sp_chf = '1'

--11,595 (35% of 32,652)
select count(distinct b1.desynpuf_id)
from beneficiary_summary_2008 b1, beneficiary_summary_2009 b2
where b1.desynpuf_id = b2.desynpuf_id 
and b1.sp_chf = '1'
and b2.sp_chf = '2'


