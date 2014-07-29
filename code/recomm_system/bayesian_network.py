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
  print bene_packed.head(5)
  #x is a row of bene_packed in the following lambda. We insert the desynpuf_id into the (key, value) tuple, convert the tuple to a list by calling list(), 
  #and the outer [] makes sure we emit a list of lists.
  bene_chrons = bene_packed.flat_map(["chronic_condition_name", "chronic_condition_value", "desynpuf_id"], lambda x:[list(k + (x['desynpuf_id'], )) for k in x['chronic_conditions'].iteritems()])

  print bene_chrons.head(10)
  
  '''
  columns = beneficiaries.column_names()
  #g = SGraph()
  #g = g.add_vertices(beneficiaries, vid_field = 'desynpuf_id')
  #print g.get_vertex_fields()

  
  #Create a node out of every patient and every chronic condition
  patients = SFrame(None)
  patients.add_column(beneficiaries['desynpuf_id'].unique(), 'desynpuf_id')
  g = SGraph()
  #Add the patients as the initial vertices with the chronic conditions as attributes
  g = g.add_vertices(patients, vid_field = 'desynpuf_id')
  print g.summary()
  
  
  #Add the chronic conditions as vertices
  chronic_conditions = SFrame(None)
  cc = SArray(filter(lambda x: x <> 'desynpuf_id', columns), dtype = str)
  chronic_conditions.add_column(cc, 'chron_cond_name')
  g = g.add_vertices(chronic_conditions, vid_field = 'chron_cond_name')
  print g.summary()
  
  
  #Add edges to the graph indicating which patient had which diagnosed condition
  tcdc = SFrame.read_csv(file_path + "transformed_claim_diagnosis_codes.csv")
  diagnosis_codes = SFrame(None)
  diagnosis_codes.add_column(tcdc['dgns_cd'].unique(), 'dgns_cd')
  g = g.add_vertices(diagnosis_codes, vid_field = 'dgns_cd')
  print g.get_vertices()
  '''
  return
  
#build_dense_matrix()
build_data_graph()
