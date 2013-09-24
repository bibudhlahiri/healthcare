--66773 claims from sample 1 only, there would be a total of 1332822 IP claims from all 20 samples
select count(*) from inpatient_claims

--116352 for 2008 and sample 1. There would be a total of 6,873,274 beneficiary records across 3 years.
select count(*) from beneficiary_summary_2008

--Mean clm_pmt_amt (per patient) for 2008, 2009 and 2010 are 16249, 13000 and 11324 respectively. Medians are 10000, 9000 and 8000 respectively.
select avg(a.total) from 
(select desynpuf_id, sum(clm_pmt_amt) as total
 from inpatient_claims
 where to_char(clm_from_dt, 'YYYY') = '2010'
 group by desynpuf_id) a


select desynpuf_id, sum(clm_pmt_amt) as total
 from inpatient_claims
 where to_char(clm_from_dt, 'YYYY') = '2010'
 group by desynpuf_id
 order by sum(clm_pmt_amt)
 limit 5857

--Mean clm_pmt_amt (per patient) for 2008, 2009 and 2010 are 9312, 9702 and 9775 respectively.
select avg(clm_pmt_amt)
 from inpatient_claims
 where to_char(clm_from_dt, 'YYYY') = '2010'


select clm_pmt_amt, CLM_UTLZTN_DAY_CNT, CLM_PASS_THRU_PER_DIEM_AMT, clm_pmt_amt + CLM_UTLZTN_DAY_CNT*CLM_PASS_THRU_PER_DIEM_AMT
from inpatient_claims
where CLM_PASS_THRU_PER_DIEM_AMT > 0
limit 5

--5.58 days: close to median LoS
select avg(CLM_UTLZTN_DAY_CNT)
from inpatient_claims

--18973, which is 28.4% of all rows in inpatient_claims
select count(*)
from inpatient_claims
where CLM_PASS_THRU_PER_DIEM_AMT > 0

--102 
select avg(CLM_PASS_THRU_PER_DIEM_AMT)
from inpatient_claims
where CLM_PASS_THRU_PER_DIEM_AMT > 0

--Seems like MEDREIMB_IP equals sum of all values of column 6 for a patient
select b.desynpuf_id, b.MEDREIMB_IP, clm_pmt_amt, CLM_UTLZTN_DAY_CNT, CLM_PASS_THRU_PER_DIEM_AMT, clm_pmt_amt + CLM_UTLZTN_DAY_CNT*CLM_PASS_THRU_PER_DIEM_AMT
from beneficiary_summary_2008 b, inpatient_claims ip
where b.desynpuf_id = ip.desynpuf_id
and to_char(clm_from_dt, 'YYYY') = '2008'
order by b.desynpuf_id, ip.clm_id

--15861 rows
select a.desynpuf_id, b.MEDREIMB_IP, a.calculated_total
from (select ip.desynpuf_id, sum(clm_pmt_amt + CLM_UTLZTN_DAY_CNT*CLM_PASS_THRU_PER_DIEM_AMT) calculated_total
      from inpatient_claims ip
      where to_char(clm_from_dt, 'YYYY') = '2008'
      group by ip.desynpuf_id) a, beneficiary_summary_2008 b
where b.desynpuf_id = a.desynpuf_id
order by a.desynpuf_id

--528 out of 15861 rows do not match: 3%, one example: "006D1BD234E5C844" has MEDREIMB_IP = 6000.00, but calculated_total = 13000.00
select a.desynpuf_id, b.MEDREIMB_IP, a.calculated_total
from (select ip.desynpuf_id, sum(clm_pmt_amt + CLM_UTLZTN_DAY_CNT*CLM_PASS_THRU_PER_DIEM_AMT) calculated_total
      from inpatient_claims ip
      where to_char(clm_from_dt, 'YYYY') = '2008'
      group by ip.desynpuf_id) a, beneficiary_summary_2008 b
where b.desynpuf_id = a.desynpuf_id
and b.MEDREIMB_IP <> a.calculated_total
order by a.desynpuf_id

select clm_pmt_amt, CLM_UTLZTN_DAY_CNT, CLM_PASS_THRU_PER_DIEM_AMT, clm_pmt_amt + CLM_UTLZTN_DAY_CNT*CLM_PASS_THRU_PER_DIEM_AMT
from inpatient_claims
where desynpuf_id = '006D1BD234E5C844'

