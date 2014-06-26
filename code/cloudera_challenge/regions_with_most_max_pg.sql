create or replace function regions_with_most_max() returns integer as $$

begin
drop table if exists provider_charge_inpatient;

--File header got loaded as a row
create table provider_charge_inpatient(
  drg_def varchar(200),
  provider_id integer,
  provider_name varchar(200),
  provider_street_address varchar(200),
  provider_city varchar(200),
  provider_state varchar(200),
  provider_zip integer,
  hrr varchar(200),
  total_discharges integer,
  avg_covered_charges real,
  avg_total_payment real,
  avg_medicare_payment real
);

copy provider_charge_inpatient
from '/Users/blahiri/healthcare/data/cloudera_challenge/Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv' 
WITH CSV HEADER DELIMITER ',';

drop table if exists provider_charge_outpatient;

--File header got loaded as a row
create table provider_charge_outpatient(
  apc_def varchar(200),
  provider_id integer,
  provider_name varchar(200),
  provider_street_address varchar(200),
  provider_city varchar(200),
  provider_state varchar(200),
  provider_zip integer,
  hrr varchar(200),
  outpatient_services integer,
  avg_est_sub_charge real,
  avg_total_payment real
);

copy provider_charge_outpatient
from '/Users/blahiri/healthcare/data/cloudera_challenge/Medicare_Provider_Charge_Outpatient_APC30_CY2011_v2.csv' 
WITH CSV HEADER DELIMITER ',';

drop table if exists prov_proc_charge;
--Each combination of procedure and provider will occur exactly once in the following table
create table prov_proc_charge as
select *
from (select drg_def as procedure_def, provider_id, hrr, avg_covered_charges as avg_charge
      from provider_charge_inpatient
      union all
      select apc_def as procedure_def, provider_id, hrr, avg_est_sub_charge as avg_charge
      from provider_charge_outpatient) all_procs;

--Average charges for a given region and a given procedure: this is OK
drop table if exists region_proc_charge;
create table region_proc_charge as
select hrr, procedure_def, avg(avg_charge) as mean_charge
from prov_proc_charge
group by hrr, procedure_def;

drop table if exists regions_with_highest_avg;
create table regions_with_highest_avg as
select a.hrr as most_exp_region, a.procedure_def, a.max_charge
from (select hrr, procedure_def, mean_charge, max(mean_charge) over (partition by procedure_def) as max_charge
      from region_proc_charge) a
where a.mean_charge = a.max_charge;

drop table if exists regions_with_most_max;
create table regions_with_most_max as
select a.most_exp_region as region, count(distinct a.procedure_def) tops_in_procedures
from regions_with_highest_avg a
group by a.most_exp_region;

return 1;
end;
$$ LANGUAGE plpgsql;
