select *
from clinical_drug_processed_names_gbm
limit 1

--1427
select count(*)
from clinical_drug_processed_names_gbm

--511 (36%)
select count(*)
from clinical_drug_processed_names_gbm
where prescribed_dose != '[Not Available]'

--1164
select count(*)
from clinical_drug_processed_names_gbm
where prescribed_dose_units != '[Not Available]'

select prescribed_dose, prescribed_dose_units
from clinical_drug_processed_names_gbm
where prescribed_dose = '[Not Available]' 
and prescribed_dose_units != '[Not Available]' 

--1040: total_dose has better data quality than prescribed_dose
select count(*)
from clinical_drug_processed_names_gbm
where total_dose != '[Not Available]'

--1159
select count(*)
from clinical_drug_processed_names_gbm
where total_dose_units != '[Not Available]'

select total_dose, total_dose_units
from clinical_drug_processed_names_gbm
where total_dose = '[Not Available]' 
and total_dose_units != '[Not Available]' 

--Are there instances of same patient, same drug, multiple episodes? 220 instances.
select bcr_patient_barcode, processed_name, count(distinct days_to_drug_therapy_start)
from clinical_drug_processed_names_gbm
where processed_name != 'Other'
group by bcr_patient_barcode, processed_name
having count(distinct days_to_drug_therapy_start) > 1
order by count(distinct days_to_drug_therapy_start) desc

select *
from clinical_drug_processed_names_gbm
where bcr_patient_barcode = 'TCGA-12-0670'
and processed_name = 'Celebrex'
order by days_to_drug_therapy_start

--14 different values
select distinct(total_dose_units)
from clinical_drug_processed_names_gbm

--623 (43%) are mg
select prescribed_dose_units, count(*)
from clinical_drug_processed_names_gbm
--where prescribed_dose != '[Not Available]' 
group by prescribed_dose_units
order by count(*) desc

--689 out of 1427 (48%) are mg
select total_dose_units, count(*)
from clinical_drug_processed_names_gbm
group by total_dose_units
order by count(*) desc

select (to_number(a.days_to_drug_therapy_end, '9999999') - to_number(a.days_to_drug_therapy_start, '9999999') + 1) as duration
from (select prescribed_dose, prescribed_dose_units, total_dose, total_dose_units, days_to_drug_therapy_start, days_to_drug_therapy_end
	from clinical_drug_processed_names_gbm
	where prescribed_dose != '[Not Available]' 
	and prescribed_dose_units != '[Not Available]'
	and total_dose != '[Not Available]'
	and total_dose_units != '[Not Available]'
	and days_to_drug_therapy_start not in ('[Not Available]', 'Completed', ' ')
	and days_to_drug_therapy_end not in ('[Not Available]', 'Completed', ' ')) a

select days_to_drug_therapy_start, count(*)
from clinical_drug_processed_names_gbm
group by days_to_drug_therapy_start
order by count(*) desc

--Median dose is 140 mg, for doses which are in mg
select to_number(prescribed_dose, '9999999')
from clinical_drug_processed_names_gbm
where prescribed_dose != '[Not Available]' 
and prescribed_dose_units = 'mg'
order by to_number(prescribed_dose, '9999999')
limit 163

select to_number(total_dose, '9999999')
from clinical_drug_processed_names_gbm
where total_dose != '[Not Available]' and trim(both ' ' from total_dose) != ''
and total_dose_units = 'mg'
order by to_number(total_dose, '9999999')
limit 309

--1191 (83%)
select count(*)
from clinical_drug_processed_names_gbm
where days_to_drug_therapy_start != '[Not Available]'
and days_to_drug_therapy_end != '[Not Available]'

select to_number(days_to_drug_therapy_start, '9999999'), 
to_number(days_to_drug_therapy_end, '9999999')
from clinical_drug_processed_names_gbm
where days_to_drug_therapy_start not in ('[Not Available]', 'Completed', ' ')
and days_to_drug_therapy_end not in ('[Not Available]', 'Completed', ' ')

select to_number(days_to_drug_therapy_start, '9999999'), 
to_number(days_to_drug_therapy_end, '9999999')
from clinical_drug_processed_names_gbm
limit 1

select to_number(a.days_to_drug_therapy_start, '9999999')
from (select days_to_drug_therapy_start
from clinical_drug_processed_names_gbm
where days_to_drug_therapy_start not in ('[Not Available]', 'Completed', ' ')
limit 2) a

--Check the number of distinct units for each drug: 
--"Temozolomide" has 8, "Other" has 9, "Procarbazine" has 5
select processed_name, count(distinct total_dose_units)
from clinical_drug_processed_names_gbm
group by processed_name
order by count(distinct total_dose_units) desc

--mg occurs 307 times, mg/m2 132 times
select total_dose_units, count(*)
from clinical_drug_processed_names_gbm
where processed_name = 'Temozolomide'
group by total_dose_units
order by count(*) desc

--"mg/m2" occurs 7 times, mg 6 times
select total_dose_units, count(*)
from clinical_drug_processed_names_gbm
where processed_name = 'Procarbazine'
group by total_dose_units
order by count(*) desc


select prescribed_dose, prescribed_dose_units, total_dose, total_dose_units, days_to_drug_therapy_start, days_to_drug_therapy_end
	from clinical_drug_processed_names_gbm
	where prescribed_dose != '[Not Available]' 
	and prescribed_dose_units != '[Not Available]'
	and total_dose != '[Not Available]'
	and total_dose_units != '[Not Available]'
	and processed_name = 'Temozolomide'


