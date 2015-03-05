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

--Most common 5 drugs are (in terms of no. of patients who had them and lived past one year) are "Temozolomide", "Other", "Avastin", "CCNU", "Dexamethasone"
select processed_name, count(distinct cpg.bcr_patient_barcode)
from clinical_drug_processed_names_gbm cdpng, clinical_patient_gbm cpg
where cdpng.bcr_patient_barcode = cpg.bcr_patient_barcode
and cpg.lived_past_one_year = 'TRUE'
group by processed_name
order by count(distinct cpg.bcr_patient_barcode) desc
limit 5

--For each of the most common 5 drugs given to patients who survived more than a year, list the five most common drugs also given to those patients
select a.drug1, a.drug2, count(*)
from (select cdpng1.bcr_patient_barcode, cdpng1.processed_name drug1, cdpng2.processed_name drug2
      from clinical_drug_processed_names_gbm cdpng1, clinical_drug_processed_names_gbm cdpng2
      where cdpng1.bcr_patient_barcode = cdpng2.bcr_patient_barcode
      and cdpng1.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
      and cdpng1.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
      and cdpng2.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
      and cdpng2.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
      and to_number(cdpng2.days_to_drug_therapy_start, '9999999') > to_number(cdpng1.days_to_drug_therapy_end, '9999999')
      and cdpng1.processed_name in (select processed_name
				     from clinical_drug_processed_names_gbm cdpng, clinical_patient_gbm cpg
				     where cdpng.bcr_patient_barcode = cpg.bcr_patient_barcode
				     and cpg.lived_past_one_year = 'TRUE'
				     group by processed_name
				     order by count(distinct cpg.bcr_patient_barcode) desc
				     limit 5)) a
group by a.drug1, a.drug2
order by a.drug1 asc, count(*) desc

--Most common 5 drugs are (in terms of no. of patients who had them and did not live past one year) are "Temozolomide", "Other", ""Dexamethasone"", "CCNU", "Avastin"
select processed_name, count(distinct cpg.bcr_patient_barcode)
from clinical_drug_processed_names_gbm cdpng, clinical_patient_gbm cpg
where cdpng.bcr_patient_barcode = cpg.bcr_patient_barcode
and cpg.lived_past_one_year = 'FALSE'
group by processed_name
order by count(distinct cpg.bcr_patient_barcode) desc

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

--What are the most common sequences of drugs given to the same patient where the names of the two drugs do not match?
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


--What are the most common combinations of drugs and radiation types given to the same patient, where the drug and radiation are given in overlapping times, and the patient survived more than a year?
--Top 5 are ("Temozolomide", "EXTERNAL BEAM", 178), ("Other", "EXTERNAL BEAM", 31), ("Dexamethasone", "EXTERNAL BEAM", 11), 
--("Avastin", "OTHER: SPECIFY IN NOTES", 10), ("Temozolomide", "OTHER: SPECIFY IN NOTES", 8)
select a.processed_name, a.radiation_type, count(distinct a.bcr_patient_barcode)
from (select cdpng.bcr_patient_barcode, cdpng.processed_name, cdpng.days_to_drug_therapy_start, cdpng.days_to_drug_therapy_end, crg.radiation_type, 
       crg.days_to_radiation_therapy_start, crg.days_to_radiation_therapy_end
      from clinical_drug_processed_names_gbm cdpng, clinical_radiation_gbm crg, clinical_patient_gbm cpg
	where cdpng.bcr_patient_barcode = crg.bcr_patient_barcode
	and crg.bcr_patient_barcode = cpg.bcr_patient_barcode
	and cdpng.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
	and cdpng.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
	and crg.days_to_radiation_therapy_start not in ('[Not Available]', '[Completed]')
	and crg.days_to_radiation_therapy_end not in ('[Not Available]', '[Completed]')
	and ((to_number(crg.days_to_radiation_therapy_start, '9999999') between to_number(cdpng.days_to_drug_therapy_start, '9999999') and to_number(cdpng.days_to_drug_therapy_end, '9999999'))
	     or (to_number(cdpng.days_to_drug_therapy_start, '9999999') between to_number(crg.days_to_radiation_therapy_start, '9999999') and to_number(crg.days_to_radiation_therapy_end, '9999999')))
	and cpg.lived_past_one_year = 'TRUE') a
group by a.processed_name, a.radiation_type
order by count(distinct a.bcr_patient_barcode) desc


--What are the most common combinations of drugs and radiation types given to the same patient, where the drug and radiation are given in overlapping times, and the patient died within a year?
--Top 5 are ("Temozolomide", "EXTERNAL BEAM", 53), ("Other", "EXTERNAL BEAM", 9), ("Dexamethasone", "EXTERNAL BEAM", 5), 
--("Avastin", "EXTERNAL BEAM", 3), ("Temozolomide", "OTHER: SPECIFY IN NOTES", 2)
select a.processed_name, a.radiation_type, count(distinct a.bcr_patient_barcode)
from (select cdpng.bcr_patient_barcode, cdpng.processed_name, cdpng.days_to_drug_therapy_start, cdpng.days_to_drug_therapy_end, crg.radiation_type, 
       crg.days_to_radiation_therapy_start, crg.days_to_radiation_therapy_end
      from clinical_drug_processed_names_gbm cdpng, clinical_radiation_gbm crg, clinical_patient_gbm cpg
	where cdpng.bcr_patient_barcode = crg.bcr_patient_barcode
	and crg.bcr_patient_barcode = cpg.bcr_patient_barcode
	and cdpng.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
	and cdpng.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
	and crg.days_to_radiation_therapy_start not in ('[Not Available]', '[Completed]')
	and crg.days_to_radiation_therapy_end not in ('[Not Available]', '[Completed]')
	and ((to_number(crg.days_to_radiation_therapy_start, '9999999') between to_number(cdpng.days_to_drug_therapy_start, '9999999') and to_number(cdpng.days_to_drug_therapy_end, '9999999'))
	     or (to_number(cdpng.days_to_drug_therapy_start, '9999999') between to_number(crg.days_to_radiation_therapy_start, '9999999') and to_number(crg.days_to_radiation_therapy_end, '9999999')))
	and cpg.lived_past_one_year = 'FALSE') a
group by a.processed_name, a.radiation_type
order by count(distinct a.bcr_patient_barcode) desc

--Are there instances where one patient is given two different drugs at the same time?
--Quite often: query fetches 815 rows.
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

--Are there instances where one patient is given three different drugs at the same time?
--Yes, 681 rows found
select a.drug1, a.drug2, cdpng3.processed_name as drug3, a.overlap_start, a.overlap_end, cdpng3.days_to_drug_therapy_start, cdpng3.days_to_drug_therapy_end
from (select cdpng1.bcr_patient_barcode, cdpng1.processed_name drug1, cdpng2.processed_name drug2, cdpng1.days_to_drug_therapy_start, 
      cdpng2.days_to_drug_therapy_start as overlap_start, cdpng1.days_to_drug_therapy_end as overlap_end,
        cdpng2.days_to_drug_therapy_end
	from clinical_drug_processed_names_gbm cdpng1, clinical_drug_processed_names_gbm cdpng2
	where cdpng1.bcr_patient_barcode = cdpng2.bcr_patient_barcode
	and cdpng1.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
	and cdpng1.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
	and cdpng2.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
	and cdpng2.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
	and to_number(cdpng2.days_to_drug_therapy_start, '9999999') between to_number(cdpng1.days_to_drug_therapy_start, '9999999') and to_number(cdpng1.days_to_drug_therapy_end, '9999999')
	and cdpng1.processed_name != cdpng2.processed_name) a, clinical_drug_processed_names_gbm cdpng3
where a.bcr_patient_barcode = cdpng3.bcr_patient_barcode
and cdpng3.processed_name != a.drug1
and cdpng3.processed_name != a.drug2
and cdpng3.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
and cdpng3.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
and to_number(cdpng3.days_to_drug_therapy_start, '9999999') between to_number(a.overlap_start, '9999999') and to_number(a.overlap_end, '9999999')

--What are the most common 3-tuples of drugs given at the same time?

select '{b.drug1, b.drug2, b.drug3}', count(*)
from (select a.drug1, a.drug2, cdpng3.processed_name as drug3, a.overlap_start, a.overlap_end, cdpng3.days_to_drug_therapy_start, cdpng3.days_to_drug_therapy_end
	from (select cdpng1.bcr_patient_barcode, cdpng1.processed_name drug1, cdpng2.processed_name drug2, cdpng1.days_to_drug_therapy_start, 
	      cdpng2.days_to_drug_therapy_start as overlap_start, cdpng1.days_to_drug_therapy_end as overlap_end,
		cdpng2.days_to_drug_therapy_end
		from clinical_drug_processed_names_gbm cdpng1, clinical_drug_processed_names_gbm cdpng2
		where cdpng1.bcr_patient_barcode = cdpng2.bcr_patient_barcode
		and cdpng1.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
		and cdpng1.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
		and cdpng2.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
		and cdpng2.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
		and to_number(cdpng2.days_to_drug_therapy_start, '9999999') between to_number(cdpng1.days_to_drug_therapy_start, '9999999') and to_number(cdpng1.days_to_drug_therapy_end, '9999999')
		and cdpng1.processed_name != cdpng2.processed_name) a, clinical_drug_processed_names_gbm cdpng3
	where a.bcr_patient_barcode = cdpng3.bcr_patient_barcode
	and cdpng3.processed_name != a.drug1
	and cdpng3.processed_name != a.drug2
	and cdpng3.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
	and cdpng3.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
	and to_number(cdpng3.days_to_drug_therapy_start, '9999999') between to_number(a.overlap_start, '9999999') and to_number(a.overlap_end, '9999999')) b
group by '{b.drug1, b.drug2, b.drug3}'
order by count(*) desc

--What are the most common pairs of drugs given at the same time?
--Top 5 are ("Other", "Temozolomide"), ("Avastin", "Temozolomide"), ("Avastin", "CPT 11"), ("Avastin", "Irinotecan"), ("CCNU", "Procarbazine")
select c.lower_name, c.higher_name, sum(c.count)
from (select case when b.drug1 <= b.drug2 then b.drug1 else b.drug2 end as lower_name,
       case when b.drug1 > b.drug2 then b.drug1 else b.drug2 end as higher_name,
       b.count
       from (select a.drug1, a.drug2, count(distinct a.bcr_patient_barcode)
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
             order by count(distinct a.bcr_patient_barcode) desc) b) c
group by c.lower_name, c.higher_name
order by sum(c.count) desc


--What are the most common pairs of drugs given at the same time to patients who survived more than a year?
--Top 5 are ("Other", "Temozolomide"), ("Avastin", "Temozolomide"), ("Avastin", "Irinotecan"), ("Avastin", "CPT 11"), ("Dexamethasone", "Other")
select c.lower_name, c.higher_name, sum(c.count)
from (select case when b.drug1 <= b.drug2 then b.drug1 else b.drug2 end as lower_name,
       case when b.drug1 > b.drug2 then b.drug1 else b.drug2 end as higher_name,
       b.count
       from (select a.drug1, a.drug2, count(distinct a.bcr_patient_barcode)
	     from (select cdpng1.bcr_patient_barcode, cdpng1.processed_name drug1, cdpng1.days_to_drug_therapy_start, cdpng1.days_to_drug_therapy_end,
		    cdpng2.processed_name drug2, cdpng2.days_to_drug_therapy_start, cdpng2.days_to_drug_therapy_end
		   from clinical_drug_processed_names_gbm cdpng1, clinical_drug_processed_names_gbm cdpng2, clinical_patient_gbm cpg
		   where cdpng1.bcr_patient_barcode = cdpng2.bcr_patient_barcode
		   and cdpng2.bcr_patient_barcode = cpg.bcr_patient_barcode
		   and cdpng1.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
		   and cdpng1.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
		   and cdpng2.days_to_drug_therapy_start not in ('[Not Available]', '[Completed]')
		   and cdpng2.days_to_drug_therapy_end not in ('[Not Available]', '[Completed]')
		   and to_number(cdpng2.days_to_drug_therapy_start, '9999999') between to_number(cdpng1.days_to_drug_therapy_start, '9999999') and to_number(cdpng1.days_to_drug_therapy_end, '9999999')
		   and cdpng1.processed_name != cdpng2.processed_name
		   and cpg.lived_past_one_year = 'TRUE') a 
             group by a.drug1, a.drug2
             order by count(distinct a.bcr_patient_barcode) desc) b) c
group by c.lower_name, c.higher_name
order by sum(c.count) desc

--Are there instances where one patient is given two different radiation types at the same time? 
--Very rarely: only 10 instances found in the data.
select crg1.bcr_patient_barcode, crg1.radiation_type radiation_type1, crg1.days_to_radiation_therapy_start, crg1.days_to_radiation_therapy_end,
       crg2.radiation_type radiation_type2, crg2.days_to_radiation_therapy_start, crg2.days_to_radiation_therapy_end
from clinical_radiation_gbm crg1, clinical_radiation_gbm crg2
where crg1.bcr_patient_barcode = crg2.bcr_patient_barcode
and crg1.days_to_radiation_therapy_start not in ('[Not Available]', '[Completed]')
and crg1.days_to_radiation_therapy_end not in ('[Not Available]', '[Completed]')
and crg2.days_to_radiation_therapy_start not in ('[Not Available]', '[Completed]')
and crg2.days_to_radiation_therapy_end not in ('[Not Available]', '[Completed]')
and to_number(crg2.days_to_radiation_therapy_start, '9999999') between to_number(crg1.days_to_radiation_therapy_start, '9999999') and to_number(crg1.days_to_radiation_therapy_end, '9999999')
and crg1.radiation_type != crg2.radiation_type


select *
from clinical_drug_processed_names_gbm 
where bcr_patient_barcode = 'TCGA-12-3644'

--47 TRUE (41.2%), 67 FALSE (58.77%)
select lived_past_one_year, count(*)
from clinical_patient_gbm
where age_at_initial_pathologic_diagnosis >= 70.5
group by lived_past_one_year

--307 TRUE (66.45%), 155 FALSE (33.55%)
select lived_past_one_year, count(*)
from clinical_patient_gbm
where age_at_initial_pathologic_diagnosis < 70.5
group by lived_past_one_year

--25 TRUE (69.4%) TRUE, 11 (30.5%) FALSE
select lived_past_one_year, count(distinct cpg.bcr_patient_barcode)
from clinical_patient_gbm cpg, clinical_drug_processed_names_gbm cdpng 
where cpg.bcr_patient_barcode = cdpng.bcr_patient_barcode
and cdpng.processed_name = 'Gliadel Wafer'
group by lived_past_one_year

--329 TRUE (60.9%) TRUE, 211 (39.07%) FALSE
select lived_past_one_year, count(distinct cpg.bcr_patient_barcode)
from clinical_patient_gbm cpg where not exists (select 1 from clinical_drug_processed_names_gbm cdpng 
						where cpg.bcr_patient_barcode = cdpng.bcr_patient_barcode
						and cdpng.processed_name = 'Gliadel Wafer')
group by lived_past_one_year

--109 (76.76%) TRUE, 23.23% FALSE
select lived_past_one_year, count(distinct cpg.bcr_patient_barcode)
from clinical_patient_gbm cpg, clinical_drug_processed_names_gbm cdpng 
where cpg.bcr_patient_barcode = cdpng.bcr_patient_barcode
and cdpng.processed_name = 'Other'
group by lived_past_one_year

--245 TRUE (56.45%) TRUE, 189 (43.54%) FALSE
select lived_past_one_year, count(distinct cpg.bcr_patient_barcode)
from clinical_patient_gbm cpg where not exists (select 1 from clinical_drug_processed_names_gbm cdpng 
						where cpg.bcr_patient_barcode = cdpng.bcr_patient_barcode
						and cdpng.processed_name = 'Other')
group by lived_past_one_year


--256 TRUE (75.96%), 81 (24.03%) FALSE
select lived_past_one_year, count(distinct cpg.bcr_patient_barcode)
from clinical_patient_gbm cpg, clinical_drug_processed_names_gbm cdpng 
where cpg.bcr_patient_barcode = cdpng.bcr_patient_barcode
and cdpng.processed_name = 'Temozolomide'
group by lived_past_one_year

--98 TRUE (41%), 141 (59%) FALSE
select lived_past_one_year, count(distinct cpg.bcr_patient_barcode)
from clinical_patient_gbm cpg where not exists (select 1 from clinical_drug_processed_names_gbm cdpng 
						where cpg.bcr_patient_barcode = cdpng.bcr_patient_barcode
						and cdpng.processed_name = 'Temozolomide')
group by lived_past_one_year

--43 (86%) TRUE, 7 (14%) FALSE
select lived_past_one_year, count(distinct cpg.bcr_patient_barcode)
from clinical_patient_gbm cpg, clinical_radiation_gbm crg
where cpg.bcr_patient_barcode = crg.bcr_patient_barcode
and crg.radiation_type = 'OTHER: SPECIFY IN NOTES'
group by lived_past_one_year

--311 (59%) TRUE, 215 (41%) FALSE
select lived_past_one_year, count(distinct cpg.bcr_patient_barcode)
from clinical_patient_gbm cpg where not exists (select 1 from clinical_radiation_gbm crg 
						where cpg.bcr_patient_barcode = crg.bcr_patient_barcode
						and crg.radiation_type = 'OTHER: SPECIFY IN NOTES')
group by lived_past_one_year

select distinct(radiation_type) 
from clinical_radiation_gbm