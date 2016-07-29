#Simple Python program: no Spark component 
from __future__ import division
import pandas as pd
import numpy as np
import glob

b = {'start': [x/10 for x in range(10)], 'end': [x/10 for x in range(1, 11)], 'points_in_range':0, 'positive_points_in_range': 0}
buckets = pd.DataFrame(b)

#Input data row will have probabilities of being anomalous and true labels   
def calibrate(true_label, predicted_prob):
   global buckets
   #Buckets do not include their right boundaries, except the last one
   if (predicted_prob == 1.0):
     matching_row_idx = len(buckets) - 1
   else:
     matching_row_idx = [i for i in range(len(buckets)) if ((predicted_prob >= buckets.ix[i, 'start']) & (predicted_prob < buckets.ix[i, 'end']))][0]
   buckets.ix[matching_row_idx, 'points_in_range'] = buckets.ix[matching_row_idx, 'points_in_range'] + 1
   if (true_label == 1):
      buckets.ix[matching_row_idx, 'positive_points_in_range'] = buckets.ix[matching_row_idx, 'positive_points_in_range'] + 1
   return
   
def process_probabilities():
   path = r'C:\Users\blahiri\healthcare\data\cloudera_challenge\labelsAndPreds'                     
   all_files = glob.glob(os.path.join(path, "part-*"))     
   labelsAndPreds = pd.concat(pd.read_csv(f, index_col = None, names = ['label', 'predicted_label', 'predicted_prob']) for f in all_files)
   print("Lines read = " + str(len(labelsAndPreds)))
   map(lambda true_label, predicted_prob:calibrate(true_label, predicted_prob), labelsAndPreds['label'], labelsAndPreds['predicted_prob'])
   buckets['fraction'] = buckets['positive_points_in_range']/buckets['points_in_range']
   print(buckets)
   return
  
process_probabilities()