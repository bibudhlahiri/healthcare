set yarn.resourcemanager.address = xyz;
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

load data local inpath '/home/impadmin/bibudh1/Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv' into table provider_charge_inpatient;

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

load data local inpath '/home/impadmin/bibudh1/Medicare_Provider_Charge_Outpatient_APC30_CY2011_v2.csv' into table provider_charge_outpatient;

drop table if exists prov_proc_charge;
--Each combination of procedure and provider will occur exactly once in the following table
create table prov_proc_charge as
select *
from (select drg_def as procedure_def, provider_id, avg_covered_charges as avg_charge
      from provider_charge_inpatient
      union all
      select apc_def as procedure_def, provider_id, avg_est_sub_charge as avg_charge
      from provider_charge_outpatient) all_procs;

drop table if exists relative_variance;
create table relative_variance(
  procedure_def STRING,
  mean_submitted_charge DOUBLE,
  variance_of_submitted_charge DOUBLE,
  rel_var DOUBLE
);
insert into table relative_variance
select a.procedure_def, a.mean_submitted_charge, a.variance_of_submitted_charge, a.variance_of_submitted_charge/a.mean_submitted_charge
from (
      select procedure_def, avg(avg_charge) as mean_submitted_charge, var_pop(avg_charge) as variance_of_submitted_charge
      from prov_proc_charge
      group by procedure_def) a;

insert overwrite local directory '/home/impadmin/bibudh1/highest_rel_var' 
select a.procedure_def 
from (select * 
      from relative_variance 
      order by rel_var desc 
      limit 3) a;
