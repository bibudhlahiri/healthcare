select bcr_patient_barcode, count(*)
from clinical_drug_processed_names_gbm
group by bcr_patient_barcode
order by count(*) desc

--Most common 5 drugs are "Temozolomide", "Other", "Avastin", "Dexamethasone", "CCNU"
select processed_name, count(*)
from clinical_drug_processed_names_gbm
group by processed_name
order by count(*) desc

--Most common 5 drugs are (in terms of no. of patients who had them) are "Temozolomide", "Other", "Avastin", "Dexamethasone", "CCNU"
select processed_name, count(distinct bcr_patient_barcode)
from clinical_drug_processed_names_gbm
group by processed_name
order by count(distinct bcr_patient_barcode) desc

--What are the most common sequences to drugs given to the same patient?
--Top 5 are ("Temozolomide", "Temozolomide"), ("Temozolomide", "Other"), ("Temozolomide", "Avastin"), ("Other", "Temozolomide"), ("Temozolomide", "CPT 11")
select a.drug1, a.drug2, count(distinct a.bcr_patient_barcode)
from (select cdpng1.bcr_patient_barcode, cdpng1.processed_name drug1, cdpng2.processed_name drug2
      from clinical_drug_processed_names_gbm cdpng1, clinical_drug_processed_names_gbm cdpng2
      where cdpng1.bcr_patient_barcode = cdpng2.bcr_patient_barcode
      and cdpng1.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
      and cdpng1.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
      and cdpng2.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
      and cdpng2.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
      and to_number(cdpng2.days_to_drug_therapy_start, '9999999') > to_number(cdpng1.days_to_drug_therapy_end, '9999999')) a
group by a.drug1, a.drug2
order by count(distinct a.bcr_patient_barcode) desc

select 
--days_to_drug_therapy_start
to_number(days_to_drug_therapy_start, '9999999')
from clinical_drug_processed_names_gbm
where days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')

--What are the most common sequences to drugs given to the same patient where the names of the two drugs do not match?
--Top 5 are ("Temozolomide", "Avastin"), ("Temozolomide", "Other"), ("Other", "Temozolomide"), ("Temozolomide", "CPT 11"), ("Temozolomide", "CCNU")
select a.drug1, a.drug2, count(distinct a.bcr_patient_barcode)
from (select cdpng1.bcr_patient_barcode, cdpng1.processed_name drug1, cdpng2.processed_name drug2
      from clinical_drug_processed_names_gbm cdpng1, clinical_drug_processed_names_gbm cdpng2
      where cdpng1.bcr_patient_barcode = cdpng2.bcr_patient_barcode
      and cdpng1.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
      and cdpng1.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
      and cdpng2.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
      and cdpng2.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
      and to_number(cdpng2.days_to_drug_therapy_start, '9999999') > to_number(cdpng1.days_to_drug_therapy_end, '9999999')
      and cdpng1.processed_name != cdpng2.processed_name) a
group by a.drug1, a.drug2
order by count(distinct a.bcr_patient_barcode) desc

--Most common radiation types are "EXTERNAL BEAM" (402) and "OTHER: SPECIFY IN NOTES" (50)
select radiation_type, count(distinct bcr_patient_barcode)
from clinical_radiation_gbm
group by radiation_type
order by count(distinct bcr_patient_barcode) desc

--What are the most common combinations of drugs and radiation types given to the same patient, where the drug and radiation are given in overlapping times?
--Top 5 are ("Temozolomide", "EXTERNAL BEAM", 231), ("Other", "EXTERNAL BEAM", 40), ("Dexamethasone", "EXTERNAL BEAM", 16), 
--("Avastin", "OTHER: SPECIFY IN NOTES", 10), ("Temozolomide", "OTHER: SPECIFY IN NOTES", 10)
select a.processed_name, a.radiation_type, count(distinct a.bcr_patient_barcode)
from (select cdpng.bcr_patient_barcode, cdpng.processed_name, cdpng.days_to_drug_therapy_start, cdpng.days_to_drug_therapy_end, crg.radiation_type, 
       crg.days_to_radiation_therapy_start, crg.days_to_radiation_therapy_end
      from clinical_drug_processed_names_gbm cdpng, clinical_radiation_gbm crg
	where cdpng.bcr_patient_barcode = crg.bcr_patient_barcode
	and cdpng.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
	and cdpng.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
	and crg.days_to_radiation_therapy_start not in ('[Not Available]', '[Completed]')
	and crg.days_to_radiation_therapy_end not in ('[Not Available]', '[Completed]')
	and ((to_number(crg.days_to_radiation_therapy_start, '9999999') between to_number(cdpng.days_to_drug_therapy_start, '9999999') and to_number(cdpng.days_to_drug_therapy_end, '9999999'))
	     or (to_number(cdpng.days_to_drug_therapy_start, '9999999') between to_number(crg.days_to_radiation_therapy_start, '9999999') and to_number(crg.days_to_radiation_therapy_end, '9999999')))) a
group by a.processed_name, a.radiation_type
order by count(distinct a.bcr_patient_barcode) desc

--Are there instances where one patient is given two different drugs at the same time?
select cdpng1.bcr_patient_barcode, cdpng1.processed_name drug1, cdpng1.days_to_drug_therapy_start, cdpng1.days_to_drug_therapy_end,
       cdpng2.processed_name drug2, cdpng2.days_to_drug_therapy_start, cdpng2.days_to_drug_therapy_end
from clinical_drug_processed_names_gbm cdpng1, clinical_drug_processed_names_gbm cdpng2
where cdpng1.bcr_patient_barcode = cdpng2.bcr_patient_barcode
and cdpng1.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
and cdpng1.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
and cdpng2.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
and cdpng2.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
and to_number(cdpng2.days_to_drug_therapy_start, '9999999') between to_number(cdpng1.days_to_drug_therapy_start, '9999999') and to_number(cdpng1.days_to_drug_therapy_end, '9999999')
and cdpng1.processed_name != cdpng2.processed_name

--What are the most common pairs of drugs given at the same time?
--Top 5 are ("Other", "Temozolomide"), ("Temozolomide", "Other"), ("Avastin", "Temozolomide"), ("Temozolomide", "Avastin"), ("CPT 11", "Avastin")
select a.drug1, a.drug2, count(distinct a.bcr_patient_barcode)
from (select cdpng1.bcr_patient_barcode, cdpng1.processed_name drug1, cdpng1.days_to_drug_therapy_start, cdpng1.days_to_drug_therapy_end,
       cdpng2.processed_name drug2, cdpng2.days_to_drug_therapy_start, cdpng2.days_to_drug_therapy_end
	from clinical_drug_processed_names_gbm cdpng1, clinical_drug_processed_names_gbm cdpng2
	where cdpng1.bcr_patient_barcode = cdpng2.bcr_patient_barcode
	and cdpng1.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
	and cdpng1.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
	and cdpng2.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
	and cdpng2.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
	and to_number(cdpng2.days_to_drug_therapy_start, '9999999') between to_number(cdpng1.days_to_drug_therapy_start, '9999999') and to_number(cdpng1.days_to_drug_therapy_end, '9999999')
	and cdpng1.processed_name != cdpng2.processed_name) a 
group by a.drug1, a.drug2
order by count(distinct a.bcr_patient_barcode) desc


select *
from clinical_drug_processed_names_gbm 
where bcr_patient_barcode = 'TCGA-12-3644'