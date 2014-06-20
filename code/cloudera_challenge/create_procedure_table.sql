drop table if exists patient_procedure;

create table patient_procedure
(
  proc_date varchar(100),
  patient_id bigint,
  procedure_id  varchar(100),
  column4 varchar(10),
  column5 varchar(10)
);

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1101.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1102.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1103.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1104.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1105.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1106.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1107.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1108.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1109.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1110.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1111.csv' 
WITH CSV HEADER DELIMITER ',';

copy patient_procedure
from '/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR1112.csv' 
WITH CSV HEADER DELIMITER ',';

CREATE INDEX idx_patient_procedure_patient_id ON patient_procedure (patient_id);

select count(1) from patient_procedure;
