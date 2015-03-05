drop table if exists clinical_radiation_gbm;

create table clinical_radiation_gbm
(
 bcr_patient_barcode varchar(100),
 bcr_radiation_barcode varchar(100),
 anatomic_treatment_site varchar(1000),
 bcr_radiation_uuid varchar(1000),
 course_number varchar(1000),
 date_of_form_completion varchar(100),
 days_to_radiation_therapy_end varchar(100),
 days_to_radiation_therapy_start varchar(100),
 measure_of_response varchar(100),
 numfractions varchar(100),
 radiation_dosage varchar(100),
 radiation_treatment_ongoing varchar(100),
 radiation_type varchar(3000),
 radiation_type_notes varchar(1000),
 regimen_indication varchar(2000),
 regimen_indication_notes varchar(4000),
 units varchar(1000)
);

copy clinical_radiation_gbm
from '/Users/blahiri/healthcare/data/tcga/Raw_Data/clinical_radiation_gbm.csv' 
WITH CSV HEADER DELIMITER ',';

--519 rows
select count(*) from clinical_radiation_gbm;