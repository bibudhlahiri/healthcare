--5,552,421
select count(*) from prescription_drug_events;

--99,538
select count(distinct DESYNPUF_ID) from prescription_drug_events;

--37,780
select count(distinct ip.DESYNPUF_ID)
from inpatient_claims ip


--36,480 out of 37,780 (96.5%)
select count(distinct ip.DESYNPUF_ID)
from inpatient_claims ip, prescription_drug_events pde
where ip.DESYNPUF_ID = pde.DESYNPUF_ID


--Median number of PDEs is 44 per patient.
select DESYNPUF_ID, count(*) 
from prescription_drug_events
group by DESYNPUF_ID
order by count(*)
limit 49769

--Median number of PDEs is 48 per patient for patients who had inpatient claims.
select ip.DESYNPUF_ID, count(distinct pde_id)
from inpatient_claims ip, prescription_drug_events pde
where ip.DESYNPUF_ID = pde.DESYNPUF_ID
group by ip.DESYNPUF_ID
order by count(distinct pde_id)
limit 18240

--Median total amount for PDEs is $1730.
select DESYNPUF_ID, sum(tot_rx_cst_amt)
from prescription_drug_events
group by DESYNPUF_ID
order by sum(tot_rx_cst_amt)
limit 49769

--Median total amount for PDEs is $2840 (64% higher than $1730) per patient for patients who had inpatient claims.
select ip.DESYNPUF_ID, sum(tot_rx_cst_amt)
from inpatient_claims ip, prescription_drug_events pde
where ip.DESYNPUF_ID = pde.DESYNPUF_ID
group by ip.DESYNPUF_ID
order by sum(tot_rx_cst_amt)
limit 18240

--PDEs for a patient between two consecutive hospital stays
select ip1.DESYNPUF_ID, ip1.nch_bene_dschrg_dt, pde.srvc_dt, ip2.clm_admsn_dt
from inpatient_claims ip1, inpatient_claims ip2, prescription_drug_events pde
where ip1.DESYNPUF_ID = ip2.DESYNPUF_ID
and ip2.clm_admsn_dt > ip1.nch_bene_dschrg_dt
and ip1.DESYNPUF_ID = pde.DESYNPUF_ID
and pde.srvc_dt >= ip1.nch_bene_dschrg_dt
and pde.srvc_dt < ip2.clm_admsn_dt
and not exists (select 1 from inpatient_claims ip3 
                where ip3.DESYNPUF_ID = ip1.DESYNPUF_ID
                and ip3.clm_admsn_dt > ip1.nch_bene_dschrg_dt
                and ip3.clm_admsn_dt < ip2.clm_admsn_dt)
order by ip1.DESYNPUF_ID, ip1.nch_bene_dschrg_dt, pde.srvc_dts

--Median number of PDEs between consecutive hospital stays is 7.
select ip1.DESYNPUF_ID, ip1.clm_id, ip2.clm_id, count(distinct pde_id)
from inpatient_claims ip1, inpatient_claims ip2, prescription_drug_events pde
where ip1.DESYNPUF_ID = ip2.DESYNPUF_ID
and ip2.clm_admsn_dt > ip1.nch_bene_dschrg_dt
and ip1.DESYNPUF_ID = pde.DESYNPUF_ID
and pde.srvc_dt >= ip1.nch_bene_dschrg_dt
and pde.srvc_dt < ip2.clm_admsn_dt
and not exists (select 1 from inpatient_claims ip3 
                where ip3.DESYNPUF_ID = ip1.DESYNPUF_ID
                and ip3.clm_admsn_dt > ip1.nch_bene_dschrg_dt
                and ip3.clm_admsn_dt < ip2.clm_admsn_dt)
group by ip1.DESYNPUF_ID, ip1.clm_id, ip2.clm_id
order by count(distinct pde_id)
limit 9657















