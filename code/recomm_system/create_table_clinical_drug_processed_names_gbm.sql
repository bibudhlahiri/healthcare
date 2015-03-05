drop table if exists clinical_drug_processed_names_gbm;

create table clinical_drug_processed_names_gbm
(
 drug_name varchar(200),
 bcr_patient_barcode varchar(100),
 bcr_drug_barcode varchar(100),
 bcr_drug_uuid varchar(1000),
 clinical_trail_drug_classification varchar(1000),
 date_of_form_completion varchar(1000),
 days_to_drug_therapy_end varchar(100),
 days_to_drug_therapy_start varchar(100),
 measure_of_response varchar(100),
 number_cycles varchar(100),
 prescribed_dose varchar(100),
 prescribed_dose_units varchar(100),
 regimen_indication varchar(100),
 regimen_indication_notes varchar(3000),
 regimen_number varchar(1000),
 route_of_administration varchar(2000),
 therapy_ongoing varchar(4000),
 therapy_type varchar(1000),
 therapy_type_notes varchar(1000),
 total_dose varchar(1000),
 total_dose_units varchar(1000),
 tx_on_clinical_trial varchar(1000),
 processed_name varchar(1000)
);

copy clinical_drug_processed_names_gbm
from '/Users/blahiri/healthcare/data/tcga/Raw_Data/clinical_drug_processed_names_gbm.csv' 
WITH CSV HEADER DELIMITER ',';

--1427 rows
select count(*) from clinical_drug_processed_names_gbm;