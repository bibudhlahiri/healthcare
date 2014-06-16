--The entire XML file should be a single line. Do by tr '\n\r' ' ' < PNTSDUMP.XML > processed_patients.xml 
drop table if exists patients;

create table patients(
  patient_id INT,
  age_group STRING,
  gender STRING,
  income_range STRING
)
ROW FORMAT SERDE 'com.ibm.spss.hive.serde2.xml.XmlSerDe'
WITH SERDEPROPERTIES (
"column.xpath.patient_id" = "/rows/field[@name=\"id\"]/text()",
"column.xpath.age_group" = "/rows/field[@name=\"age\"]/text()",
"column.xpath.gender" = "/rows/field[@name=\"gndr\"]/text()",
"column.xpath.income_range" = "/rows/field[@name=\"inc\"]/text()"
)
STORED AS
INPUTFORMAT 'com.ibm.spss.hive.serde2.xml.XmlInputFormat'
OUTPUTFORMAT 'org.apache.hadoop.hive.ql.io.IgnoreKeyTextOutputFormat'
TBLPROPERTIES (
"xmlinput.start" = "<rows>",
"xmlinput.end" = "</rows>"
);

load data local inpath '/home/impadmin/bibudh1/pat_sample.xml' into table patients;
