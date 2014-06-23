drop table if exists anomalous_patients;

create table anomalous_patients(
  patient_id INT
)
ROW FORMAT DELIMITED;

load data local inpath '/home/impadmin/bibudh1/REVIEW.TXT' into table anomalous_patients;

