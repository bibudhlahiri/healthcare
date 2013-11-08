drop table if exists ndc_codes;

create table ndc_codes
(
 PRODUCTID varchar(200),
 PRODUCTNDC varchar(100),
 PRODUCTTYPENAME varchar(100),
 PROPRIETARYNAME varchar(1000),
 PROPRIETARYNAMESUFFIX varchar(1000),
 NONPROPRIETARYNAME varchar(1000),
 DOSAGEFORMNAME varchar(100),
 ROUTENAME varchar(100),
 STARTMARKETINGDATE date,
 ENDMARKETINGDATE date,
 MARKETINGCATEGORYNAME varchar(100),
 APPLICATIONNUMBER varchar(100),
 LABELERNAME varchar(100),
 SUBSTANCENAME varchar(3000),
 ACTIVE_NUMERATOR_STRENGTH varchar(1000),
 ACTIVE_INGRED_UNIT varchar(2000),
 PHARM_CLASSES varchar(4000),
 DEASCHEDULE varchar(1000)
);

copy ndc_codes
from '/Users/blahiri/healthcare/data/DE-SynPUF/product.csv' 
WITH CSV HEADER DELIMITER ',';

CREATE INDEX idx_ndc_codes_productid ON ndc_codes (substring(productid from 6 for 4));


CREATE INDEX idx_ndc_codes_productid ON ndc_codes (substring(productid from 6 for 4));

alter table ndc_codes
add column hipaa_ndc_labeler_product_code varchar(100);

update ndc_codes
set hipaa_ndc_labeler_product_code = lpad(substring(productid from 1 for position('_' in productid)-1), 10, '0');

create index idx_ndc_codes_hipaa_ndc_labeler_product_code on ndc_codes(hipaa_ndc_labeler_product_code);

