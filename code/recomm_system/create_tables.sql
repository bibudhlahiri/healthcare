drop table if exists health_recomm.transformed_claim_diagnosis_codes;

CREATE TABLE health_recomm.transformed_claim_diagnosis_codes
(
  desynpuf_id character varying(100),
  clm_id bigint,
  clm_from_dt date,
  clm_thru_dt date,
  claim_type text,
  dgns_cd character varying(100),
  clm_thru_year character varying(10)
);

copy health_recomm.transformed_claim_diagnosis_codes
from '/Users/blahiri/healthcare/documents/recommendation_system/transformed_claim_diagnosis_codes.csv' 
WITH CSV HEADER DELIMITER ',';

drop table if exists health_recomm.transformed_claim_prcdr_codes;

CREATE TABLE health_recomm.transformed_claim_prcdr_codes
(
  desynpuf_id character varying(100),
  clm_id bigint,
  clm_from_dt date,
  clm_thru_dt date,
  claim_type text,
  prcdr_cd character varying(100),
  clm_thru_year character varying(10)
);

copy health_recomm.transformed_claim_prcdr_codes
from '/Users/blahiri/healthcare/documents/recommendation_system/transformed_claim_prcdr_codes.csv' 
WITH CSV HEADER DELIMITER ',';

drop table if exists health_recomm.beneficiary_summary_2008_2009;

create table health_recomm.beneficiary_summary_2008_2009
(
  desynpuf_id character varying(100),
  chron_alzhdmta_2008 character varying(10),
  chron_chf_2008 character varying(10),
  chron_chrnkidn_2008 character varying(10),
  chron_cncr_2008 character varying(10),
  chron_copd_2008 character varying(10),
  chron_depressn_2008 character varying(10),
  chron_diabetes_2008 character varying(10),
  chron_ischmcht_2008 character varying(10),
  chron_osteoprs_2008 character varying(10),
  chron_ra_oa_2008 character varying(10),
  chron_strketia_2008 character varying(10),
  chron_alzhdmta_2009 character varying(10),
  chron_chf_2009 character varying(10),
  chron_chrnkidn_2009 character varying(10),
  chron_cncr_2009 character varying(10),
  chron_copd_2009 character varying(10),
  chron_depressn_2009 character varying(10),
  chron_diabetes_2009 character varying(10),
  chron_ischmcht_2009 character varying(10),
  chron_osteoprs_2009 character varying(10),
  chron_ra_oa_2009 character varying(10),
  chron_strketia_2009 character varying(10)
);

copy health_recomm.beneficiary_summary_2008_2009
from '/Users/blahiri/healthcare/documents/recommendation_system/beneficiary_summary_2008_2009.csv' 
WITH CSV HEADER DELIMITER ',';

drop table if exists health_recomm.prescribed_drugs;
create table health_recomm.prescribed_drugs
(
  desynpuf_id character varying(100),
  substancename character varying(3000)
);
copy health_recomm.prescribed_drugs
from '/Users/blahiri/healthcare/documents/recommendation_system/prescribed_drugs.csv' 
WITH CSV HEADER DELIMITER ',';

--272,880
select count(*) from health_recomm.prescribed_drugs;

--1,792
select count(distinct substancename) from health_recomm.prescribed_drugs;
