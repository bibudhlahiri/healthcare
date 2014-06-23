set yarn.resourcemanager.address = xyz;
set hive.enforce.bucketing = true;
set hive.enforce.sorting = true;
set hive.use.tez.natively=true;
set hive.enable.mrr=true;

drop table if exists combined_sample;

create table combined_sample as
select /*+ MAPJOIN(ap) */ p.patient_id, p.age_group, p.gender, p.income_range, pp.proc_date, pp.procedure_id, 
       case when ap.patient_id is null then 0 else 1 end as is_anomalous
from patients TABLESAMPLE(bucket 3 OUT OF 1000 ON patient_id) p inner join patient_procedure TABLESAMPLE(bucket 3 OUT OF 3000 ON patient_id) pp on (p.patient_id = pp.patient_id)
left outer join anomalous_patients ap on (p.patient_id = ap.patient_id);
