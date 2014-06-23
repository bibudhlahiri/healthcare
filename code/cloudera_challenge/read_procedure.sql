--tr '\036\037' '\n,' < PCDR1101.ADT > PCDR1101.csv
--SQL_SELECT_LIMIT is in ./mysql-connector-java-5.0.8/src/com/mysql/jdbc/PreparedStatement.java and ./mysql-connector-java-5.0.8/src/com/mysql/jdbc/Connection.java.
set yarn.resourcemanager.address = xyz;
set hive.enforce.bucketing = true;

drop table if exists patient_procedure;

create table patient_procedure(
  proc_date STRING,
  patient_id STRING,
  procedure_id STRING,
  column4 STRING,
  column5 STRING
)
CLUSTERED BY (patient_id) INTO 16 BUCKETS --Same as DISTRIBUTE BY patient_id SORT BY patient_id
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ',';

--Total 300,000,003 rows got loaded from 12 files
load data local inpath '/home/impadmin/bibudh1/PCDR1101.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1102.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1103.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1104.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1105.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1106.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1107.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1108.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1109.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1110.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1111.csv' into table patient_procedure;
load data local inpath '/home/impadmin/bibudh1/PCDR1112.csv' into table patient_procedure;
