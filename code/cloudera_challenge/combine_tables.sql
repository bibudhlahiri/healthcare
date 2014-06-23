--Do the following in the hive shell:
set yarn.resourcemanager.address = xyz;
--add jar /home/impadmin/hive-0.10.0/lib/hivexmlserde-1.0.5.1.jar;
--Do the following outside shell: rm /home/impadmin/kaggle_shopper/metastore_db/*.lck
--Tried job tracker at http://impetus-1084.impetus.co.in:9001/
--One set of logs under /mnt/cdhtmp/data/mapred/local/userlogs/job_201406191054_0009, but permission denied
--Found logs under /tmp/impadmin
set hive.enforce.bucketing = true;
set hive.enforce.sorting = true;
set hive.use.tez.natively=true;
set hive.enable.mrr=true;

--drop table if exists combined;
--Singleton reduces are almost always bad. Query is using 99 mappers and 27 reducers.

create table combined as
select /*+ MAPJOIN(ap) */ p.patient_id, p.age_group, p.gender, p.income_range, pp.proc_date, pp.procedure_id, 
       case when ap.patient_id is null then 0 else 1 end as is_anomalous
from patients p join patient_procedure pp on (p.patient_id = pp.patient_id)
left outer join anomalous_patients ap on (p.patient_id = ap.patient_id);

--Sampling statements
--hive -e "set yarn.resourcemanager.address = xyz; create table patient_sample as select * from patients order by rand() limit 100000"
--hive -e "set yarn.resourcemanager.address = xyz; select * from patients TABLESAMPLE(BUCKET 3 OUT OF 1000 ON patient_id)"
--hive -e "set yarn.resourcemanager.address = xyz; select * from patients TABLESAMPLE(0.1 PERCENT) s"
