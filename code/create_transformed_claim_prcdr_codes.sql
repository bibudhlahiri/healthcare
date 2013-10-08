drop table if exists transformed_claim_prcdr_codes;
create table transformed_claim_prcdr_codes
as select * from (
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, icd9_prcdr_cd_1 as prcdr_cd
from inpatient_claims
where icd9_prcdr_cd_1 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, icd9_prcdr_cd_2 as prcdr_cd
from inpatient_claims
where icd9_prcdr_cd_2 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, icd9_prcdr_cd_3 as prcdr_cd
from inpatient_claims
where icd9_prcdr_cd_3 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, icd9_prcdr_cd_4 as prcdr_cd
from inpatient_claims
where icd9_prcdr_cd_4 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, icd9_prcdr_cd_5 as prcdr_cd
from inpatient_claims
where icd9_prcdr_cd_5 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, icd9_prcdr_cd_6 as prcdr_cd
from inpatient_claims
where icd9_prcdr_cd_6 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, icd9_prcdr_cd_1 as prcdr_cd
from outpatient_claims
where icd9_prcdr_cd_1 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, icd9_prcdr_cd_2 as prcdr_cd
from outpatient_claims
where icd9_prcdr_cd_2 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, icd9_prcdr_cd_3 as prcdr_cd
from outpatient_claims
where icd9_prcdr_cd_3 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, icd9_prcdr_cd_4 as prcdr_cd
from outpatient_claims
where icd9_prcdr_cd_4 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, icd9_prcdr_cd_5 as prcdr_cd
from outpatient_claims
where icd9_prcdr_cd_5 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, icd9_prcdr_cd_6 as prcdr_cd
from outpatient_claims
where icd9_prcdr_cd_6 is not null
) a

select count(*) from transformed_claim_prcdr_codes;