drop table if exists transformed_claim_diagnosis_codes;
create table transformed_claim_diagnosis_codes
as select * from (
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, ICD9_DGNS_CD_1 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_1 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, ICD9_DGNS_CD_2 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_2 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, ICD9_DGNS_CD_3 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_3 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, ICD9_DGNS_CD_4 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_4 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, ICD9_DGNS_CD_5 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_5 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, ICD9_DGNS_CD_6 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_6 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, ICD9_DGNS_CD_7 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_7 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, ICD9_DGNS_CD_8 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_8 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, ICD9_DGNS_CD_9 as DGNS_CD
from inpatient_claims
where ICD9_DGNS_CD_9 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, ICD9_DGNS_CD_1 as DGNS_CD
from outpatient_claims
where ICD9_DGNS_CD_1 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, ICD9_DGNS_CD_2 as DGNS_CD
from outpatient_claims
where ICD9_DGNS_CD_2 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, ICD9_DGNS_CD_3 as DGNS_CD
from outpatient_claims
where ICD9_DGNS_CD_3 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, ICD9_DGNS_CD_4 as DGNS_CD
from outpatient_claims
where ICD9_DGNS_CD_4 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, ICD9_DGNS_CD_5 as DGNS_CD
from outpatient_claims
where ICD9_DGNS_CD_5 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, ICD9_DGNS_CD_6 as DGNS_CD
from outpatient_claims
where ICD9_DGNS_CD_6 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, ICD9_DGNS_CD_7 as DGNS_CD
from outpatient_claims
where ICD9_DGNS_CD_7 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, ICD9_DGNS_CD_8 as DGNS_CD
from outpatient_claims
where ICD9_DGNS_CD_8 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, ICD9_DGNS_CD_9 as DGNS_CD
from outpatient_claims
where ICD9_DGNS_CD_9 is not null) a

select count(*) from transformed_claim_diagnosis_codes