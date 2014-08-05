import graphlab 
from graphlab import SGraph, Vertex, Edge, SFrame, SArray, load_sgraph
import time
from time import gmtime, strftime
import random
import graphlab.aggregate as agg


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
  bene_chrons['relation'] = 'had_chronic'
  g = g.add_edges(bene_chrons, src_field = 'desynpuf_id', dst_field = 'chronic_condition')
  print g.summary()
 
  #Take out the distinct IDs of patients with chronic conditions to avoid repetition in query
  bene_with_chrons = SFrame(None)
  bene_with_chrons.add_column(bene_chrons['desynpuf_id'].unique(), 'desynpuf_id')
  
  #Add edges to the graph indicating which patient had which diagnosed condition
  tcdc = SFrame.read_csv(file_path + "transformed_claim_diagnosis_codes.csv")
  cols_to_drop = ['clm_id', 'clm_from_dt', 'clm_thru_dt', 'claim_type', 'clm_thru_year']
  for column in cols_to_drop:
     del tcdc[column]
  #Same patient can be diagnosed with same condition multiple times a year, so take distinct
  tcdc = tcdc.unique()
  #Take diagnosed conditions for only those patients who had some chronic condition in 2008 or 2009. It is possible that 
  #such a patient had no diagnosed condition, however.
  bene_chrons_tcdc = bene_with_chrons.join(tcdc)
  
  bene_chrons_tcdc['relation'] = 'diagnosed_with'
  g = g.add_edges(bene_chrons_tcdc, src_field = 'desynpuf_id', dst_field = 'dgns_cd')
  print g.summary()

  
  #Add edges to the graph indicating which patient had which procedure
  tcpc = SFrame.read_csv(file_path + "transformed_claim_prcdr_codes.csv", column_type_hints = {'prcdr_cd' : str})
  cols_to_drop = ['clm_id', 'clm_from_dt', 'clm_thru_dt', 'claim_type', 'clm_thru_year']
  for column in cols_to_drop:
     del tcpc[column]
  tcpc = tcpc.unique()
  #Take procedures for only those patients who had some chronic condition in 2008 or 2009. It is possible that 
  #such a patient had no procedure, however.
  bene_chrons_tcpc = bene_with_chrons.join(tcpc)
  bene_chrons_tcpc['relation'] = 'underwent'
  g = g.add_edges(bene_chrons_tcpc, src_field = 'desynpuf_id', dst_field = 'prcdr_cd')
  print g.summary()

  #Add edges to the graph indicating which patient had which medicine
  pde = SFrame.read_csv(file_path + "prescribed_drugs.csv")
  pde = pde.unique()
  #Take medicines for only those patients who had some chronic condition in 2008 or 2009. It is possible that 
  #such a patient had no medicine, however.
  bene_chrons_pde = bene_with_chrons.join(pde)
  bene_chrons_pde['relation'] = 'had_drug'
  g = g.add_edges(bene_chrons_pde, src_field = 'desynpuf_id', dst_field = 'substancename')
  print g.summary()
   
  return g
 
def create_initial_bayesian_network():
  '''
  Start from a randomly generated Bayesian network where there is no edge between the variables of the same type.
  First, create a blacklist. 
  '''
  g = load_sgraph('data_graph')
  edges = g.get_edges()
  features = edges[['__dst_id', 'relation']].unique()
  features.rename({'__dst_id': 'feature_id', 'relation': 'feature_type'})
  
  '''
  chronic_conditions = features[features['relation'] == 'had_chronic']
  diagnosis_codes = features[features['relation'] == 'diagnosed_with']
  procedure_codes = features[features['relation'] == 'underwent']
  drugs = features[features['relation'] == 'had_drug']
  '''
  bn = SGraph()
  
  '''
  sampling_fraction = 5/float(features.num_rows())
  #Connect each feature randomly to five other features, not of the same type
  bn_edges = features.flat_map(["src_feature_id", "src_feature_type", "dst_feature_id", "dst_feature_type"], 
                               lambda x: [[x['feature_id'], x['feature_type'], y['feature_id'], y['feature_type']] for y in features.sample(sampling_fraction)])
  bn_edges.head(20)
  '''
  bn = bn.add_vertices(features, vid_field = 'feature_id')
  n_features = features.num_rows()
  edges_data_graph = g.get_edges()
  n_patients = edges_data_graph['__src_id'].unique().size()
 
  random.seed(1234)
  for i in range(20):
    src = features['feature_id'][random.randint(0, n_features-1)]
    dst = 'E8498'
    #dst = features['feature_id'][random.randint(0, n_features-1)]
    bn = bn.add_edges(Edge(src, dst))
    print "Added edge between " + src + " and " + dst

  bic = get_bic_score(g, bn, n_patients)
  return g
 

def get_bic_score(data_graph, bn, n_patients):
  return log_likelihood_score(data_graph, bn, n_patients)

def log_likelihood_score(data_graph, bn, n_patients):
  
  features = bn.get_vertices()
  edges = bn.get_edges()
  n_features = features.num_rows()
  
  for i in range(n_features):
    #X_i = features[i]['__id']
    X_i = 'E8498'
    #Get the parents of X_i from the Bayesian Network
    parents_X_i = edges[edges['__dst_id'] == X_i]['__src_id']
    print "Number of parents is " + str(len(parents_X_i))
    #Generate all the bit strings of length (p+1) where p is the number of parents of X_i
    num_parents = parents_X_i.size()
    all_possible_configs = generate_bit_strings([], "", 1 + num_parents, 1 + num_parents)
    #Crawl the list of bit strings. Compute and populate the N_{ijk} values   
    n_all_possible_configs = len(all_possible_configs)
    #print all_possible_configs
    for m in range(n_all_possible_configs):
       N_i_j_k(data_graph, bn, X_i, all_possible_configs[m][0], parents_X_i, all_possible_configs[m][1:], n_patients)
        
  return 0
    

#Compute N_{ijk}: for how many patients feature X_i takes value x_{ik} and the set of its parents take their j-th configuration w_{ij}
def N_i_j_k(data_graph, bn, X_i, x_ik, parents_X_i, w_ij, n_patients):

  edges_data_graph = g.get_edges()
  print parents_X_i
  if x_ik == '1':
    #How many patients had this feature
    #N_i_j_k = (edges_data_graph[edges_data_graph['__dst_id'] ==  X_i]).num_rows()
    #print "X_i = " + X_i + ", x_ik = " + x_ik + ", N_i_j_k = " + str(N_i_j_k) + '\n'
    selected_patients = (edges_data_graph[edges_data_graph['__dst_id'] ==  X_i])['__src_id']
  else:
    #How many patients did not have this feature.
    #N_i_j_k = n_patients - (edges_data_graph[edges_data_graph['__dst_id'] ==  X_i]).num_rows()
    edges_data_graph['has_feature'] = (edges_data_graph['__dst_id'] == X_i)
    df_has_feature = edges_data_graph.groupby(key_columns = '__src_id', operations = {'has_feature': agg.SUM('has_feature')})
    selected_patients = (df_has_feature[df_has_feature['has_feature'] == 0])['__src_id']
    #N_i_j_k = patients_with_X_i_0.num_rows()
    #print "X_i = " + X_i + ", x_ik = " + x_ik + ", N_i_j_k = " + str(N_i_j_k) + '\n'

  parent_number = 0
  for parent in parents_X_i:
    print "parent_number = " + str(parent_number) + ", parent = " + parent + ", w_ij[parent_number] = " + str(w_ij[parent_number]) + '\n'
    if w_ij[parent_number] == '1':
      patients_with_parent_feature = (edges_data_graph[edges_data_graph['__dst_id'] == parent])['__src_id']
    else:
      edges_data_graph['has_feature'] = (edges_data_graph['__dst_id'] == parent)
      df_has_feature = edges_data_graph.groupby(key_columns = '__src_id', operations = {'has_feature': agg.SUM('has_feature')})
      patients_with_parent_feature = (df_has_feature[df_has_feature['has_feature'] == 0])['__src_id']

    selected_patients = intersect(selected_patients, patients_with_parent_feature)
    parent_number = parent_number + 1
 
  ret_value = len(selected_patients)
  print "ret_value = " + str(ret_value)
  return ret_value

def intersect(a, b):
     return list(set(a) & set(b))

def generate_bit_strings(all_possible_configs, string, n, n_orig):
  if n == 0:
    all_possible_configs.append(string)
  else:
    generate_bit_strings(all_possible_configs, string + "0", n - 1, n_orig)
    generate_bit_strings(all_possible_configs, string + "1", n - 1, n_orig)
  if n == n_orig:
    return all_possible_configs
  return 
 
#build_dense_matrix()
#execfile('bayesian_network.py')
#g = build_data_graph()
#g.get_edges(src_ids = ['0076A3B03FA644E9'])

#generate_bit_strings("", 4)
#print all_possible_configs
