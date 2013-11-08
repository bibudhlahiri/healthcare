drop table inpatient_claims;

create table inpatient_claims
(
 DESYNPUF_ID varchar(100),
 CLM_ID bigint,
 SEGMENT integer,
 CLM_FROM_DT date,
 CLM_THRU_DT date,
 PRVDR_NUM varchar(100),
 CLM_PMT_AMT numeric,
 NCH_PRMRY_PYR_CLM_PD_AMT numeric,
 AT_PHYSN_NPI bigint,
 OP_PHYSN_NPI bigint,
 OT_PHYSN_NPI bigint,
 CLM_ADMSN_DT date,
 ADMTNG_ICD9_DGNS_CD varchar(100),
 CLM_PASS_THRU_PER_DIEM_AMT numeric,
 NCH_BENE_IP_DDCTBL_AMT numeric,
 NCH_BENE_PTA_COINSRNC_LBLTY_AM numeric,
 NCH_BENE_BLOOD_DDCTBL_LBLTY_AM numeric,
 CLM_UTLZTN_DAY_CNT integer,
 NCH_BENE_DSCHRG_DT date,
 CLM_DRG_CD varchar(100),
 ICD9_DGNS_CD_1 varchar(100),
 ICD9_DGNS_CD_2 varchar(100),
 ICD9_DGNS_CD_3 varchar(100),
 ICD9_DGNS_CD_4 varchar(100),
 ICD9_DGNS_CD_5 varchar(100),
 ICD9_DGNS_CD_6 varchar(100),
 ICD9_DGNS_CD_7 varchar(100),
 ICD9_DGNS_CD_8 varchar(100),
 ICD9_DGNS_CD_9 varchar(100),
 ICD9_DGNS_CD_10 varchar(100),
 ICD9_PRCDR_CD_1 varchar(100),
 ICD9_PRCDR_CD_2 varchar(100),
 ICD9_PRCDR_CD_3 varchar(100),
 ICD9_PRCDR_CD_4 varchar(100),
 ICD9_PRCDR_CD_5 varchar(100),
 ICD9_PRCDR_CD_6 varchar(100),
 HCPCS_CD_1 varchar(100),
 HCPCS_CD_2 varchar(100),
 HCPCS_CD_3 varchar(100),
 HCPCS_CD_4 varchar(100),
 HCPCS_CD_5 varchar(100),
 HCPCS_CD_6 varchar(100),
 HCPCS_CD_7 varchar(100),
 HCPCS_CD_8 varchar(100),
 HCPCS_CD_9 varchar(100),
 HCPCS_CD_10 varchar(100),
 HCPCS_CD_11 varchar(100),
 HCPCS_CD_12 varchar(100),
 HCPCS_CD_13 varchar(100),
 HCPCS_CD_14 varchar(100),
 HCPCS_CD_15 varchar(100),
 HCPCS_CD_16 varchar(100),
 HCPCS_CD_17 varchar(100),
 HCPCS_CD_18 varchar(100),
 HCPCS_CD_19 varchar(100),
 HCPCS_CD_20 varchar(100),
 HCPCS_CD_21 varchar(100),
 HCPCS_CD_22 varchar(100),
 HCPCS_CD_23 varchar(100),
 HCPCS_CD_24 varchar(100),
 HCPCS_CD_25 varchar(100),
 HCPCS_CD_26 varchar(100),
 HCPCS_CD_27 varchar(100),
 HCPCS_CD_28 varchar(100),
 HCPCS_CD_29 varchar(100),
 HCPCS_CD_30 varchar(100),
 HCPCS_CD_31 varchar(100),
 HCPCS_CD_32 varchar(100),
 HCPCS_CD_33 varchar(100),
 HCPCS_CD_34 varchar(100),
 HCPCS_CD_35 varchar(100),
 HCPCS_CD_36 varchar(100),
 HCPCS_CD_37 varchar(100),
 HCPCS_CD_38 varchar(100),
 HCPCS_CD_39 varchar(100),
 HCPCS_CD_40 varchar(100),
 HCPCS_CD_41 varchar(100),
 HCPCS_CD_42 varchar(100),
 HCPCS_CD_43 varchar(100),
 HCPCS_CD_44 varchar(100),
 HCPCS_CD_45 varchar(100)
);

copy inpatient_claims
from '/Users/blahiri/healthcare/data/DE-SynPUF/DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv' 
WITH CSV HEADER DELIMITER ',';

CREATE INDEX idx_inpatient_claims_desynpuf_id ON inpatient_claims (desynpuf_id);

-------------------------------------------------
drop table beneficiary_summary;
create table beneficiary_summary_2008
(
 DESYNPUF_ID varchar(100),
 BENE_BIRTH_DT date,
 BENE_DEATH_DT date,
 BENE_SEX_IDENT_CD varchar(10),
 BENE_RACE_CD varchar(10),
 BENE_ESRD_IND varchar(10),
 SP_STATE_CODE integer,
 BENE_COUNTY_CD integer,
 BENE_HI_CVRAGE_TOT_MONS integer,
 BENE_SMI_CVRAGE_TOT_MONS integer,
 BENE_HMO_CVRAGE_TOT_MONS integer,
 PLAN_CVRG_MOS_NUM integer,
 SP_ALZHDMTA varchar(10),
 SP_CHF varchar(10),
 SP_CHRNKIDN varchar(10),
 SP_CNCR varchar(10),
 SP_COPD varchar(10),
 SP_DEPRESSN varchar(10),
 SP_DIABETES varchar(10),
 SP_ISCHMCHT varchar(10),
 SP_OSTEOPRS varchar(10),
 SP_RA_OA varchar(10),
 SP_STRKETIA varchar(10),
 MEDREIMB_IP numeric,
 BENRES_IP numeric,
 PPPYMT_IP numeric,
 MEDREIMB_OP numeric,
 BENRES_OP numeric,
 PPPYMT_OP numeric,
 MEDREIMB_CAR numeric,
 BENRES_CAR numeric,
 PPPYMT_CAR numeric
);

copy beneficiary_summary_2008
from '/Users/blahiri/healthcare/data/DE-SynPUF/DE1_0_2008_Beneficiary_Summary_File_Sample_1.csv' 
WITH CSV HEADER DELIMITER ',';


CREATE INDEX idx_beneficiary_summary_2008_desynpuf_id ON beneficiary_summary_2008 (desynpuf_id);


select count(*) from beneficiary_summary_2008;
alter table beneficiary_summary_2008
add column total_expense numeric;

update beneficiary_summary_2008 b1
set total_expense = MEDREIMB_IP + BENRES_IP + PPPYMT_IP + 
                    MEDREIMB_OP + BENRES_OP + PPPYMT_OP + 
                    MEDREIMB_CAR + BENRES_CAR + PPPYMT_CAR + 
                    (select COALESCE(sum(tot_rx_cst_amt), 0) as cost_pde_2008
                    from prescription_drug_events pde
                    where b1.DESYNPUF_ID = pde.DESYNPUF_ID
                    and to_char(pde.srvc_dt, 'YYYY') = '2008')

select MEDREIMB_IP ,
 BENRES_IP ,
 PPPYMT_IP ,
 MEDREIMB_OP ,
 BENRES_OP ,
 PPPYMT_OP ,
 MEDREIMB_CAR ,
 BENRES_CAR ,
 PPPYMT_CAR, 
 (select COALESCE(sum(tot_rx_cst_amt), 0) as cost_pde_2008
                    from prescription_drug_events pde
                    where b1.DESYNPUF_ID = pde.DESYNPUF_ID
                    and to_char(pde.srvc_dt, 'YYYY') = '2008'),
 total_expense
from beneficiary_summary_2008 b1
limit 10
                    
-------------------------------------------------

drop table if exists beneficiary_summary_2009;
create table beneficiary_summary_2009
(
 DESYNPUF_ID varchar(100),
 BENE_BIRTH_DT date,
 BENE_DEATH_DT date,
 BENE_SEX_IDENT_CD varchar(10),
 BENE_RACE_CD varchar(10),
 BENE_ESRD_IND varchar(10),
 SP_STATE_CODE integer,
 BENE_COUNTY_CD integer,
 BENE_HI_CVRAGE_TOT_MONS integer,
 BENE_SMI_CVRAGE_TOT_MONS integer,
 BENE_HMO_CVRAGE_TOT_MONS integer,
 PLAN_CVRG_MOS_NUM integer,
 SP_ALZHDMTA varchar(10),
 SP_CHF varchar(10),
 SP_CHRNKIDN varchar(10),
 SP_CNCR varchar(10),
 SP_COPD varchar(10),
 SP_DEPRESSN varchar(10),
 SP_DIABETES varchar(10),
 SP_ISCHMCHT varchar(10),
 SP_OSTEOPRS varchar(10),
 SP_RA_OA varchar(10),
 SP_STRKETIA varchar(10),
 MEDREIMB_IP numeric,
 BENRES_IP numeric,
 PPPYMT_IP numeric,
 MEDREIMB_OP numeric,
 BENRES_OP numeric,
 PPPYMT_OP numeric,
 MEDREIMB_CAR numeric,
 BENRES_CAR numeric,
 PPPYMT_CAR numeric
);

copy beneficiary_summary_2009
from '/Users/blahiri/healthcare/data/DE-SynPUF/DE1_0_2009_Beneficiary_Summary_File_Sample_1.csv' 
WITH CSV HEADER DELIMITER ',';

CREATE INDEX idx_beneficiary_summary_2009_desynpuf_id ON beneficiary_summary_2009 (desynpuf_id);

select count(*) from beneficiary_summary_2009;

alter table beneficiary_summary_2009
add column total_expense numeric;

update beneficiary_summary_2009 b2
set total_expense = MEDREIMB_IP + BENRES_IP + PPPYMT_IP + 
                    MEDREIMB_OP + BENRES_OP + PPPYMT_OP + 
                    MEDREIMB_CAR + BENRES_CAR + PPPYMT_CAR + 
                    (select COALESCE(sum(tot_rx_cst_amt), 0) as cost_pde_2009
                    from prescription_drug_events pde
                    where b2.DESYNPUF_ID = pde.DESYNPUF_ID
                    and to_char(pde.srvc_dt, 'YYYY') = '2009')

select MEDREIMB_IP ,
 BENRES_IP ,
 PPPYMT_IP ,
 MEDREIMB_OP ,
 BENRES_OP ,
 PPPYMT_OP ,
 MEDREIMB_CAR ,
 BENRES_CAR ,
 PPPYMT_CAR, 
 (select COALESCE(sum(tot_rx_cst_amt), 0) as cost_pde_2009
                    from prescription_drug_events pde
                    where b2.DESYNPUF_ID = pde.DESYNPUF_ID
                    and to_char(pde.srvc_dt, 'YYYY') = '2009'),
 total_expense
from beneficiary_summary_2009 b2
limit 10

---------------------------------------------------
drop table if exists beneficiary_summary_2010;
create table beneficiary_summary_2010
(
 DESYNPUF_ID varchar(100),
 BENE_BIRTH_DT date,
 BENE_DEATH_DT date,
 BENE_SEX_IDENT_CD varchar(10),
 BENE_RACE_CD varchar(10),
 BENE_ESRD_IND varchar(10),
 SP_STATE_CODE integer,
 BENE_COUNTY_CD integer,
 BENE_HI_CVRAGE_TOT_MONS integer,
 BENE_SMI_CVRAGE_TOT_MONS integer,
 BENE_HMO_CVRAGE_TOT_MONS integer,
 PLAN_CVRG_MOS_NUM integer,
 SP_ALZHDMTA varchar(10),
 SP_CHF varchar(10),
 SP_CHRNKIDN varchar(10),
 SP_CNCR varchar(10),
 SP_COPD varchar(10),
 SP_DEPRESSN varchar(10),
 SP_DIABETES varchar(10),
 SP_ISCHMCHT varchar(10),
 SP_OSTEOPRS varchar(10),
 SP_RA_OA varchar(10),
 SP_STRKETIA varchar(10),
 MEDREIMB_IP numeric,
 BENRES_IP numeric,
 PPPYMT_IP numeric,
 MEDREIMB_OP numeric,
 BENRES_OP numeric,
 PPPYMT_OP numeric,
 MEDREIMB_CAR numeric,
 BENRES_CAR numeric,
 PPPYMT_CAR numeric
);

copy beneficiary_summary_2010
from '/Users/blahiri/healthcare/data/DE-SynPUF/DE1_0_2010_Beneficiary_Summary_File_Sample_1.csv' 
WITH CSV HEADER DELIMITER ',';

CREATE INDEX idx_beneficiary_summary_2010_desynpuf_id ON beneficiary_summary_2010 (desynpuf_id);

select count(*) from beneficiary_summary_2010;



drop table if exists prescription_drug_events;
create table prescription_drug_events
(
 DESYNPUF_ID varchar(100),
 PDE_ID bigint,
 SRVC_DT date,
 PROD_SRVC_ID varchar(100),
 QTY_DSPNSD_NUM numeric,
 DAYS_SUPLY_NUM numeric,
 PTNT_PAY_AMT numeric, 
 TOT_RX_CST_AMT numeric
);

copy prescription_drug_events
from '/Users/blahiri/healthcare/data/DE-SynPUF/DE1_0_2008_to_2010_Prescription_Drug_Events_Sample_1.csv' 
WITH CSV HEADER DELIMITER ',';

select count(*) from prescription_drug_events;

CREATE INDEX idx_prescription_drug_events_desynpuf_id ON prescription_drug_events (desynpuf_id);

alter table prescription_drug_events
add column hipaa_ndc_labeler_product_code varchar(100);

--Took 3231 secs!
update prescription_drug_events
set hipaa_ndc_labeler_product_code = substring(prod_srvc_id from 1 for 5) || '-' || substring(prod_srvc_id from 6 for 4);

create index idx_prescription_drug_events_hipaa_ndc_labeler_product_code on prescription_drug_events (hipaa_ndc_labeler_product_code);

alter table prescription_drug_events
add column srvc_year varchar(100);

update prescription_drug_events
set srvc_year = to_char(srvc_dt, 'YYYY')

---------------------------------------------
drop table if exists outpatient_claims;
create table outpatient_claims
(
 DESYNPUF_ID varchar(100),
 CLM_ID bigint,
 SEGMENT integer,
 CLM_FROM_DT date,
 CLM_THRU_DT date,
 PRVDR_NUM varchar(100),
 CLM_PMT_AMT numeric,
 NCH_PRMRY_PYR_CLM_PD_AMT numeric,
 AT_PHYSN_NPI bigint,
 OP_PHYSN_NPI bigint,
 OT_PHYSN_NPI bigint,
 NCH_BENE_BLOOD_DDCTBL_LBLTY_AM numeric,
 ICD9_DGNS_CD_1 varchar(100),
 ICD9_DGNS_CD_2 varchar(100),
 ICD9_DGNS_CD_3 varchar(100),
 ICD9_DGNS_CD_4 varchar(100),
 ICD9_DGNS_CD_5 varchar(100),
 ICD9_DGNS_CD_6 varchar(100),
 ICD9_DGNS_CD_7 varchar(100),
 ICD9_DGNS_CD_8 varchar(100),
 ICD9_DGNS_CD_9 varchar(100),
 ICD9_DGNS_CD_10 varchar(100),
 ICD9_PRCDR_CD_1 varchar(100),
 ICD9_PRCDR_CD_2 varchar(100),
 ICD9_PRCDR_CD_3 varchar(100),
 ICD9_PRCDR_CD_4 varchar(100),
 ICD9_PRCDR_CD_5 varchar(100),
 ICD9_PRCDR_CD_6 varchar(100),
 NCH_BENE_PTB_DDCTBL_AMT numeric,
 NCH_BENE_PTB_COINSRNC_AMT numeric,
 ADMTNG_ICD9_DGNS_CD varchar(100),
 HCPCS_CD_1 varchar(100),
 HCPCS_CD_2 varchar(100),
 HCPCS_CD_3 varchar(100),
 HCPCS_CD_4 varchar(100),
 HCPCS_CD_5 varchar(100),
 HCPCS_CD_6 varchar(100),
 HCPCS_CD_7 varchar(100),
 HCPCS_CD_8 varchar(100),
 HCPCS_CD_9 varchar(100),
 HCPCS_CD_10 varchar(100),
 HCPCS_CD_11 varchar(100),
 HCPCS_CD_12 varchar(100),
 HCPCS_CD_13 varchar(100),
 HCPCS_CD_14 varchar(100),
 HCPCS_CD_15 varchar(100),
 HCPCS_CD_16 varchar(100),
 HCPCS_CD_17 varchar(100),
 HCPCS_CD_18 varchar(100),
 HCPCS_CD_19 varchar(100),
 HCPCS_CD_20 varchar(100),
 HCPCS_CD_21 varchar(100),
 HCPCS_CD_22 varchar(100),
 HCPCS_CD_23 varchar(100),
 HCPCS_CD_24 varchar(100),
 HCPCS_CD_25 varchar(100),
 HCPCS_CD_26 varchar(100),
 HCPCS_CD_27 varchar(100),
 HCPCS_CD_28 varchar(100),
 HCPCS_CD_29 varchar(100),
 HCPCS_CD_30 varchar(100),
 HCPCS_CD_31 varchar(100),
 HCPCS_CD_32 varchar(100),
 HCPCS_CD_33 varchar(100),
 HCPCS_CD_34 varchar(100),
 HCPCS_CD_35 varchar(100),
 HCPCS_CD_36 varchar(100),
 HCPCS_CD_37 varchar(100),
 HCPCS_CD_38 varchar(100),
 HCPCS_CD_39 varchar(100),
 HCPCS_CD_40 varchar(100),
 HCPCS_CD_41 varchar(100),
 HCPCS_CD_42 varchar(100),
 HCPCS_CD_43 varchar(100),
 HCPCS_CD_44 varchar(100),
 HCPCS_CD_45 varchar(100)
);

copy outpatient_claims
from '/Users/blahiri/healthcare/data/DE-SynPUF/DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.csv' 
WITH CSV HEADER DELIMITER ',';

select count(*) from outpatient_claims;

CREATE INDEX idx_outpatient_claims_desynpuf_id ON outpatient_claims (desynpuf_id);
---------------------------------------------
drop table if exists diagnosis_codes;
create table diagnosis_codes
(
 diagnosis_code varchar(10),
 long_desc varchar(500),
 short_desc varchar(100)
);
copy diagnosis_codes
from '/Users/blahiri/healthcare/data/DE-SynPUF/CMS31_DESC_LONG_SHORT_DX.csv' 
WITH CSV HEADER DELIMITER ',' ENCODING 'ISO_8859_5';

select * from diagnosis_codes limit 1;
--------------------------------
drop table if exists procedure_codes;
create table procedure_codes
(
 procedure_code varchar(10),
 long_desc varchar(500),
 short_desc varchar(100)
);
copy procedure_codes
from '/Users/blahiri/healthcare/data/DE-SynPUF/CMS31_DESC_LONG_SHORT_SG.csv' 
WITH CSV HEADER DELIMITER ',' ENCODING 'ISO_8859_5';

select * from procedure_codes limit 1
 
 





