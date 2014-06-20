drop table if exists patients;

create table patients
(
  patient_id bigint,
  age_group varchar(100),
  gender  varchar(2),
  income_range varchar(100)
);

copy patients
from '/Users/blahiri/healthcare/data/cloudera_challenge/patients.csv' 
WITH CSV HEADER DELIMITER ',';

--CREATE INDEX idx_patients_patient_id ON patients (patient_id);

--select count(*) from patients;
