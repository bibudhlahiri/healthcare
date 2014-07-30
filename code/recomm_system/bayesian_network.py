import graphlab 
from graphlab import SGraph, Vertex, Edge, SFrame, SArray
import time
from time import gmtime, strftime


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
 #diagnosis_codes = diagnosis_codes.head(5)
 
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

 
def build_data_graph():
  file_path = "/Users/blahiri/healthcare/documents/recommendation_system/"
  beneficiaries = SFrame.read_csv(file_path + "beneficiary_summary_2008_2009.csv")
  bene_packed = beneficiaries.pack_columns(column_prefix = 'chron_', dtype = dict, new_column_name = 'chronic_conditions', remove_prefix = False)
  
  #x is a row of bene_packed in the following lambda. We insert the desynpuf_id into the (key, value) tuple, convert the tuple to a list by calling list(), 
  #and the outer [] makes sure we emit a list of lists.
  bene_chrons = bene_packed.flat_map(["chronic_condition_name", "chronic_condition_value", "desynpuf_id"], 
                                     lambda x:[list(k + (x['desynpuf_id'], )) for k in x['chronic_conditions'].iteritems()])
 

  bene_chrons = bene_chrons[bene_chrons['chronic_condition_value'] == 1]
  del bene_chrons['chronic_condition_value']
  bene_chrons.rename({'chronic_condition_name': 'chronic_condition'})

  g = SGraph()
  g = g.add_edges(bene_chrons, src_field = 'desynpuf_id', dst_field = 'chronic_condition')
  print g.summary()
 
  #Take out the distinct IDs of patients with chronic conditions to avoid repetition in query
  bene_with_chrons = SFrame(None)
  bene_with_chrons.add_column(bene_chrons['desynpuf_id'].unique(), 'desynpuf_id')
  
  #Add edges to the graph indicating which patient had which diagnosed condition
  tcdc = SFrame.read_csv(file_path + "transformed_claim_diagnosis_codes.csv")
  #Take diagnosed conditions for only those patients who had some chronic condition in 2008 or 2009. It is possible that 
  #such a patient had no diagnosed condition, however.
  bene_chrons_tcdc = bene_with_chrons.join(tcdc)
  
  cols_to_drop = ['clm_id', 'clm_from_dt', 'clm_thru_dt', 'claim_type', 'clm_thru_year']
  for column in cols_to_drop:
     del bene_chrons_tcdc[column]
  g = g.add_edges(bene_chrons_tcdc, src_field = 'desynpuf_id', dst_field = 'dgns_cd')
  print g.summary()

  
  #Add edges to the graph indicating which patient had which procedure
  tcpc = SFrame.read_csv(file_path + "transformed_claim_prcdr_codes.csv", column_type_hints = {'prcdr_cd' : str})
  #Take procedures for only those patients who had some chronic condition in 2008 or 2009. It is possible that 
  #such a patient had no procedure, however.
  bene_chrons_tcpc = bene_with_chrons.join(tcpc)
  
  cols_to_drop = ['clm_id', 'clm_from_dt', 'clm_thru_dt', 'claim_type', 'clm_thru_year']
  for column in cols_to_drop:
     del bene_chrons_tcpc[column]
  g = g.add_edges(bene_chrons_tcpc, src_field = 'desynpuf_id', dst_field = 'prcdr_cd')
  print g.summary()

  #Add edges to the graph indicating which patient had which medicine
  pde = SFrame.read_csv(file_path + "prescribed_drugs.csv")
  #Take medicines for only those patients who had some chronic condition in 2008 or 2009. It is possible that 
  #such a patient had no medicine, however.
  bene_chrons_pde = bene_with_chrons.join(pde)
  
  g = g.add_edges(bene_chrons_pde, src_field = 'desynpuf_id', dst_field = 'substancename')
  print g.summary()
  
  return g
  
#build_dense_matrix()
#execfile('bayesian_network.py')
#g = build_data_graph()
#g.get_edges(src_ids = ['0076A3B03FA644E9'])
