--tr '\036\037' '\n,' < PCDR1101.ADT > PCDR1101.csv
drop table if exists patient_procedure_1101;

create table patient_procedure_1101(
  proc_date STRING,
  patient_id STRING,
  procedure_id STRING,
  column4 STRING,
  column5 STRING
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ',';

load data local inpath '/home/impadmin/bibudh1/PCDR1101.csv' into table patient_procedure_1101;
