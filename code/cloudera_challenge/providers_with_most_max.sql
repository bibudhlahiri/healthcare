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

drop table if exists prov_proc_charge;
--Each combination of procedure and provider will occur exactly once in the following table
create table prov_proc_charge as
select *
from (select drg_def as procedure_def, provider_id, avg_covered_charges as avg_charge
      from provider_charge_inpatient
      union all
      select apc_def as procedure_def, provider_id, avg_est_sub_charge as avg_charge
      from provider_charge_outpatient) all_procs;


drop table if exists highest_avg;
create table highest_avg as
select procedure_def, max(avg_charge) as highest_charge
from prov_proc_charge
group by procedure_def;

drop table if exists providers_with_highest_avg;
create table providers_with_highest_avg as
select ppc.procedure_def, ppc.provider_id as most_exp_prov, ha.highest_charge
from prov_proc_charge ppc join highest_avg ha on (ppc.procedure_def = ha.procedure_def and ppc.avg_charge = ha.highest_charge);

drop table if exists providers_with_max_highest;
create table providers_with_max_highest as
select a.most_exp_prov as provider, count(distinct a.procedure_def) tops_in_procedures
from providers_with_highest_avg a
group by a.most_exp_prov;

insert overwrite local directory '/home/impadmin/bibudh/healthcare/data/cloudera_challenge/providers_with_most_max' 
select a.provider 
from (select * 
      from providers_with_max_highest 
      order by tops_in_procedures desc 
      limit 3) a;
--hive -e 'select * from some_table' > /home/yourfile.csv
