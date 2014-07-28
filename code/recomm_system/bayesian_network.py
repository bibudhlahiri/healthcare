import graphlab as gl

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
 print beneficiaries.head(2)
 return 

build_dense_matrix()
