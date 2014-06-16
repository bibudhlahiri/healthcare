drop table if exists provider_charge_inpatient;

create table provider_charge_inpatient(
  drg_def STRING,
  provider_id INT,
  provider_name STRING,
  provider_street_address STRING,
  provider_city STRING,
  provider_state STRING,
  provider_zip INT,
  hrr STRING,
  total_discharges INT,
  avg_covered_charges DOUBLE,
  avg_total_payment DOUBLE,
  avg_medicare_payment DOUBLE
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ',' ESCAPED BY '\\';

load data local inpath '/home/impadmin/bibudh/healthcare/data/cloudera_challenge/Inpatient_Data_2012_CSV/Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv' into table provider_charge_inpatient;

drop table if exists provider_charge_outpatient;

create table provider_charge_outpatient(
  apc_def STRING,
  provider_id INT,
  provider_name STRING,
  provider_street_address STRING,
  provider_city STRING,
  provider_state STRING,
  provider_zip INT,
  hrr STRING,
  outpatient_services INT,
  avg_est_sub_charge DOUBLE,
  avg_total_payment DOUBLE
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ',' ESCAPED BY '\\';

load data local inpath '/home/impadmin/bibudh/healthcare/data/cloudera_challenge/Outpatient_Data_2012_CSV/Medicare_Provider_Charge_Outpatient_APC30_CY2012.csv' into table provider_charge_outpatient;

drop table if exists prov_proc_claim_diff;
--Each combination of procedure and provider will occur exactly once in the following table
create table prov_proc_claim_diff as
select *
from (select drg_def as procedure_def, provider_id, (avg_covered_charges - avg_total_payment) as claim_diff
      from provider_charge_inpatient
      union all
      select apc_def as procedure_def, provider_id, (avg_est_sub_charge - avg_total_payment) as claim_diff
      from provider_charge_outpatient) all_procs;


drop table if exists highest_claim_diff;
create table highest_claim_diff as
select procedure_def, max(claim_diff) as highest_clm_diff
from prov_proc_claim_diff
group by procedure_def;

drop table if exists providers_with_highest_clm_diff;
create table providers_with_highest_clm_diff as
select ppc.procedure_def, ppc.provider_id as prov_with_highest_clm_diff, ha.highest_charge
from prov_proc_claim_diff ppcd join highest_claim_diff hcd on (ppcd.procedure_def = hcd.procedure_def and ppcd.claim_diff = hcd.highest_clm_diff);

drop table if exists providers_with_max_highest;
create table providers_with_max_highest as
select a.prov_with_highest_clm_diff as provider, count(distinct a.procedure_def) tops_in_procedures
from providers_with_highest_clm_diff a
group by a.prov_with_highest_clm_diff;

insert overwrite local directory '/home/impadmin/bibudh/healthcare/data/cloudera_challenge/providers_with_most_claim_diff' 
select a.provider 
from (select * 
      from providers_with_max_highest 
      order by tops_in_procedures desc 
      limit 3) a;
