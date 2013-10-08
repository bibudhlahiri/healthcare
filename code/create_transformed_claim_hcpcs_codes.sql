drop table if exists transformed_claim_hcpcs_codes;
create table transformed_claim_hcpcs_codes
as select * from (
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_1 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_1 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_2 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_2 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_3 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_3 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_4 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_4 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_5 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_5 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_6 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_6 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_7 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_7 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_8 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_8 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_9 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_9 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_10 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_10 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_11 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_11 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_12 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_12 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_13 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_13 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_14 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_14 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_15 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_15 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_16 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_16 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_17 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_17 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_18 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_18 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_19 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_19 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_20 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_20 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_21 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_21 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_22 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_22 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_23 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_23 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_24 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_24 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_25 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_25 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_26 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_26 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_27 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_27 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_28 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_28 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_29 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_29 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_30 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_30 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_31 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_31 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_32 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_32 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_33 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_33 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_34 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_34 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_35 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_35 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_36 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_36 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_37 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_37 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_38 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_38 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_39 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_39 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_40 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_40 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_41 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_41 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_42 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_42 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_43 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_43 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_44 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_44 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'inpatient' as claim_type, hcpcs_cd_45 as hcpcs_cd
from inpatient_claims
where hcpcs_cd_45 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_1 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_1 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_2 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_2 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_3 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_3 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_4 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_4 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_5 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_5 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_6 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_6 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_7 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_7 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_8 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_8 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_9 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_9 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_10 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_10 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_11 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_11 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_12 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_12 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_13 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_13 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_14 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_14 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_15 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_15 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_16 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_16 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_17 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_17 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_18 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_18 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_19 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_19 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_20 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_20 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_21 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_21 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_22 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_22 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_23 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_23 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_24 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_24 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_25 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_25 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_26 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_26 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_27 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_27 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_28 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_28 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_29 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_29 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_30 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_30 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_31 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_31 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_32 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_32 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_33 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_33 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_34 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_34 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_35 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_35 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_36 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_36 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_37 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_37 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_38 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_38 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_39 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_39 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_40 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_40 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_41 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_41 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_42 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_42 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_43 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_43 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_44 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_44 is not null
union
select DESYNPUF_ID, CLM_ID, clm_from_dt, clm_thru_dt, 'outpatient' as claim_type, hcpcs_cd_45 as hcpcs_cd
from outpatient_claims
where hcpcs_cd_45 is not null
) a


