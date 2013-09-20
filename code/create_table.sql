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

select count(*) from beneficiary_summary_2008;


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

select count(*) from beneficiary_summary_2009;


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

select count(*) from beneficiary_summary_2010;
 
 





