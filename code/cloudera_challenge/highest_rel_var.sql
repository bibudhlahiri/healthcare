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

drop table if exists drg_mean_charges;
create table drg_mean_charges(
  drg_def STRING,
  mean_covered_charge DOUBLE
);
insert into table drg_mean_charges
select drg_def, avg(avg_covered_charges)
from provider_charge_inpatient
group by drg_def;

drop table if exists drg_variance_of_charges;
create table drg_variance_of_charges(
  drg_def STRING,
  variance_of_covered_charge DOUBLE
);
insert into table drg_variance_of_charges
select drg_def, var_pop(avg_covered_charges)
from provider_charge_inpatient
group by drg_def;

drop table if exists drg_relative_variance;
create table drg_relative_variance(
  drg_def STRING,
  relative_variance DOUBLE
);
insert into table drg_relative_variance
select m.drg_def, v.variance_of_covered_charge/m.mean_covered_charge as relative_variance
from drg_mean_charges m join drg_variance_of_charges v on (m.drg_def = v.drg_def);


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

drop table if exists apc_mean_charges;
create table apc_mean_charges(
  apc_def STRING,
  mean_submitted_charge DOUBLE
);
insert into table apc_mean_charges
select apc_def, avg(avg_est_sub_charge)
from provider_charge_outpatient
group by apc_def;

drop table if exists apc_variance_of_charges;
create table apc_variance_of_charges(
  apc_def STRING,
  variance_of_submitted_charge DOUBLE
);
insert into table apc_variance_of_charges
select apc_def, var_pop(avg_est_sub_charge)
from provider_charge_outpatient
group by apc_def;

drop table if exists apc_relative_variance;
create table apc_relative_variance(
  apc_def STRING,
  relative_variance DOUBLE
);
insert into table apc_relative_variance
select m.apc_def, v.variance_of_submitted_charge/m.mean_submitted_charge as relative_variance
from apc_mean_charges m join apc_variance_of_charges v on (m.apc_def = v.apc_def);

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



   


  
