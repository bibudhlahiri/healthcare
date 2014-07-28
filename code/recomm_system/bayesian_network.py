import graphlab as gl
import time
from time import gmtime, strftime

chronic_conditions = ["sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr", "sp_copd", "sp_depressn", 
                          "sp_diabetes", "sp_ischmcht", "sp_osteoprs", "sp_ra_oa", "sp_strketia"]



def build_dense_matrix():
 file_path = "/Users/blahiri/healthcare/documents/recommendation_system/"
 beneficiaries = gl.SFrame.read_csv(file_path + "beneficiary_summary_2008_2009.csv")
 print beneficiaries.num_rows()
 columns = beneficiaries.column_names()
 for column in columns:
   if column != 'desynpuf_id':
     beneficiaries[column] = (beneficiaries[column] == 1)
 dense_matrix = beneficiaries

 tcdc = gl.SFrame.read_csv(file_path + "transformed_claim_diagnosis_codes.csv")
 diagnosis_codes = tcdc['dgns_cd'].unique()  
 diagnosis_codes = diagnosis_codes.head(5)
 
 diag_conds_for_benefs = gl.SFrame(None)
 loopc = 0

 for diagnosis_code in diagnosis_codes:
   loopc = loopc + 1
   tcdc_this_diag = tcdc[tcdc['dgns_cd'].apply(lambda x: x == diagnosis_code)]
   patient_ids = tcdc_this_diag['desynpuf_id'].unique()
   column = 'diag_' + diagnosis_code
   benefs_this_cond = gl.SFrame({'patient_id': patient_ids, 
                                 column: [1]* patient_ids.size()})
   
   benef_tcdc = beneficiaries.join(benefs_this_cond, on = {'desynpuf_id':'patient_id'}, how = 'left')
   benef_tcdc[column] = benef_tcdc[column].apply(lambda x:0 if x is None else x, skip_undefined = False)

   diag_conds_for_benefs.add_column(benef_tcdc[column], column)
   if loopc % 20 == 0:
     print "for diagnosis_code, loopc = " + str(loopc) + ", time = " + strftime("%Y-%m-%d %H:%M:%S", gmtime()) 

 dense_matrix.add_columns(diag_conds_for_benefs, diag_conds_for_benefs.column_names())
 print dense_matrix.column_names()
 return 

build_dense_matrix()
