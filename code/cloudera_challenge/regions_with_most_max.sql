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
from (select drg_def as procedure_def, provider_id, hrr, avg_covered_charges as avg_charge
      from provider_charge_inpatient
      union all
      select apc_def as procedure_def, provider_id, hrr, avg_est_sub_charge as avg_charge
      from provider_charge_outpatient) all_procs;

drop table if exists region_proc_charge;
create table region_proc_charge as
select hrr, procedure_def, avg(avg_charge) as mean_charge
from prov_proc_charge
group by hrr, procedure_def;

drop table if exists region_max_charge;
create table region_max_charge as 
select hrr, max(mean_charge) as max_charge
group by hrr; 

drop table if exists regions_with_highest_avg;
create table regions_with_highest_avg as
select rpc.procedure_def, rpc.hrr as most_exp_region, rmc.max_charge
from region_proc_charge rpc join region_max_charge rmc on (rpc.hrr = rmc.hrr and rpc.mean_charge = rmc.max_charge);

drop table if exists regions_with_most_max;
create table regions_with_most_max as
select a.most_exp_region as region, count(distinct a.procedure_def) tops_in_procedures
from regions_with_highest_avg a
group by a.most_exp_region;

insert overwrite local directory '/home/impadmin/bibudh/healthcare/data/cloudera_challenge/regions_with_most_max' 
select a.region
from (select * 
      from regions_with_most_max 
      order by tops_in_procedures desc 
      limit 3) a;
