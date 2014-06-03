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
FIELDS TERMINATED BY ',';

load data local inpath '/home/impadmin/bibudh/healthcare/data/cloudera_challenge/Inpatient_Data_2012_CSV/Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv' into table provider_charge_inpatient;

drop table if exists drg_relative_variance;
create table drg_relative_variance(
  drg_def STRING,
  mean_covered_charge DOUBLE,
  variance_of_covered_charge DOUBLE,
  relative_variance DOUBLE
);
insert into table drg_relative_variance
select a.drg_def, a.mean_covered_charge, a.variance_of_covered_charge, a.variance_of_covered_charge/a.mean_covered_charge
from (
      select drg_def, avg(avg_covered_charges) as mean_covered_charge, var_pop(avg_covered_charges) as variance_of_covered_charge
      from provider_charge_inpatient
      group by drg_def) a;


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
FIELDS TERMINATED BY ',';

load data local inpath '/home/impadmin/bibudh/healthcare/data/cloudera_challenge/Outpatient_Data_2012_CSV/Medicare_Provider_Charge_Outpatient_APC30_CY2012.csv' into table provider_charge_outpatient;

drop table if exists apc_relative_variance;
create table apc_relative_variance(
  apc_def STRING,
  mean_submitted_charge DOUBLE,
  variance_of_submitted_charge DOUBLE,
  relative_variance DOUBLE
);
insert into table apc_relative_variance
select a.apc_def, a.mean_submitted_charge, a.variance_of_submitted_charge, a.variance_of_submitted_charge/a.mean_submitted_charge
from (
      select apc_def, avg(avg_est_sub_charge) as mean_submitted_charge, var_pop(avg_est_sub_charge) as variance_of_submitted_charge
      from provider_charge_outpatient
      group by apc_def) a;


drop table if exists in_out_patient_combi;
create table in_out_patient_combi(
  procedure_def STRING,
  relative_variance DOUBLE
);
insert into table in_out_patient_combi
select *
from (select drg_def as procedure_def, relative_variance
      from drg_relative_variance
      union all
      select apc_def as procedure_def, relative_variance
      from apc_relative_variance) all_procs
order by relative_variance desc
limit 3;

insert overwrite local directory '/home/impadmin/bibudh/healthcare/data/cloudera_challenge/highest_rel_var' select * from in_out_patient_combi;



   


  
