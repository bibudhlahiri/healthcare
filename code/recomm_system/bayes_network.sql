select tcdc.dgns_cd, count(b.desynpuf_id)
from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
where b.desynpuf_id = tcdc.desynpuf_id
and tcdc.clm_thru_year = '2008'
group by tcdc.dgns_cd
order by count(b.desynpuf_id) desc

--10,641
select distinct tcdc.dgns_cd
from transformed_claim_diagnosis_codes tcdc, beneficiary_summary_2008 b
where b.desynpuf_id = tcdc.desynpuf_id
and tcdc.clm_thru_year = '2008'
order by tcdc.dgns_cd

--116,352
select count(*) from beneficiary_summary_2008

select desynpuf_id, sp_alzhdmta, sp_chf, sp_chrnkidn, sp_cncr, sp_copd, sp_depressn, 
                sp_diabetes, sp_ischmcht, sp_osteoprs, sp_ra_oa, sp_strketia
from beneficiary_summary_2008
order by desynpuf_id

--5.2 secs
select b.desynpuf_id, (select count(*) from transformed_claim_diagnosis_codes tcdc
                       where b.desynpuf_id = tcdc.desynpuf_id and tcdc.clm_thru_year = '2008'
                       and tcdc.dgns_cd = '4019') count_4019, 
                      (select count(*) from transformed_claim_diagnosis_codes tcdc1
                       where b.desynpuf_id = tcdc1.desynpuf_id and tcdc1.clm_thru_year = '2008'
                       and tcdc1.dgns_cd = '25000') count_25000
from beneficiary_summary_2008 b
order by b.desynpuf_id

explain
select a.desynpuf_id, case when a.count_4019 > 0 then 1 else 0 end as diag_4019
from (select b.desynpuf_id, (select count(*) from transformed_claim_diagnosis_codes tcdc
                       where b.desynpuf_id = tcdc.desynpuf_id and tcdc.clm_thru_year = '2008'
                       and tcdc.dgns_cd = '4019') count_4019
      from beneficiary_summary_2008 b
      order by b.desynpuf_id) a

--88 ms
select b.desynpuf_id, (select count(*) from transformed_claim_diagnosis_codes tcdc
                       where b.desynpuf_id = tcdc.desynpuf_id and tcdc.clm_thru_year = '2008'
                       and tcdc.dgns_cd = '4019') count_4019, 
                      (select count(*) from transformed_claim_diagnosis_codes tcdc1
                       where b.desynpuf_id = tcdc1.desynpuf_id and tcdc1.clm_thru_year = '2008'
                       and tcdc1.dgns_cd = '25000') count_25000
from (select * from beneficiary_summary_2008 limit 1000) b
order by b.desynpuf_id

--2,914
select distinct tcpc.prcdr_cd
from transformed_claim_prcdr_codes tcpc, beneficiary_summary_2008 b
where tcpc.desynpuf_id = b.desynpuf_id
and to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
order by tcpc.prcdr_cd

--1.2 secs
explain
select a.desynpuf_id, case when a.count_0013 > 0 then 1 else 0 end as proc_0013
from (select b.desynpuf_id, (select count(*) from transformed_claim_prcdr_codes tcpc
                       where b.desynpuf_id = tcpc.desynpuf_id and tcpc.clm_thru_year = '2008'
                       and tcpc.prcdr_cd = '0013') count_0013
      from beneficiary_summary_2008 b
      order by b.desynpuf_id) a


alter table transformed_claim_prcdr_codes
add column clm_thru_year character varying(10);


update transformed_claim_prcdr_codes
set clm_thru_year = to_char(clm_thru_dt, 'YYYY')

select * 
from transformed_claim_prcdr_codes
limit 5


CREATE INDEX idx_transformed_claim_prcdr_codes_clm_thru_year
  ON transformed_claim_prcdr_codes
  USING btree
  (clm_thru_year);

CREATE INDEX idx_transformed_claim_prcdr_codes_desynpuf_id
  ON transformed_claim_prcdr_codes
  USING btree
  (desynpuf_id);

select * from transformed_claim_prcdr_codes tcpc where to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'


select * from transformed_claim_prcdr_codes tcpc where to_char(tcpc.clm_thru_dt, 'YYYY') = '2008'
and not exists (select 1 from transformed_claim_diagnosis_codes where dgns_cd = prcdr_cd)

explain
select distinct tcpc.* from transformed_claim_prcdr_codes tcpc 
left outer join transformed_claim_diagnosis_codes tcdc on (tcpc.prcdr_cd = tcdc.dgns_cd and tcpc.clm_thru_year = '2008')
where tcdc.dgns_cd is null

--39,712 without any filter on count
--10,032 with a.count = 0
explain
select a.desynpuf_id, a.clm_id, a.clm_from_dt, a.clm_thru_dt, a.claim_type, a.prcdr_cd, a.clm_thru_year
from  (select tcpc.*, (select count(*) from transformed_claim_diagnosis_codes tcdc where tcpc.prcdr_cd = tcdc.dgns_cd)
from transformed_claim_prcdr_codes tcpc 
where tcpc.clm_thru_year = '2008') a
where a.count = 0


CREATE INDEX idx_transformed_claim_diagnosis_codes_dgns_cd
  ON transformed_claim_diagnosis_codes
  USING btree
  (dgns_cd);
