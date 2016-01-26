#from pyspark import SparkConf, SparkContext
#conf = (SparkConf().setMaster("local[*]").setAppName("AnalyzeAnomaly").set("spark.executor.memory", "5g"))
#sc = SparkContext(conf = conf)
from __future__ import division
from pyspark.mllib.regression import LabeledPoint
from pyspark.mllib.tree import DecisionTree, DecisionTreeModel

sc.addPyFile('python/pyspark/pyspark_csv.py')
import pyspark_csv as pycsv

import sys, traceback
    
    
def create_labeled_point(row, features, categorical_features, dict_cat_features, procedure_features):
   #Map each categorical feature to a numeric value. Return a LabeledPoint with is_anomalous as label, and everything else (in numeric values) put in a list
   features_mapped_to_num = []
   for feature in categorical_features:
      distinct_values = (dict_cat_features[feature])[2]
      feature_value = ((row.asDict())[feature]).encode('ascii','ignore')
      features_mapped_to_num.append(distinct_values.index(feature_value))
     
   for feature in procedure_features:
      feature_value = (row.asDict())[feature]
      features_mapped_to_num.append(feature_value)
    
   is_anomalous = ((row.asDict())["is_anomalous"])
   return LabeledPoint(float(is_anomalous), [float(x) for x in features_mapped_to_num])
  
def train_validate_test_rpart():
  try:
    plaintext_rdd = sc.textFile("file:///Users/blahiri/healthcare/data/cloudera_challenge/pat_proc_larger.csv") #69.2 MB
    pat_proc = pycsv.csvToDataFrame(sqlContext, plaintext_rdd, sep = ",")
    
    anom = pat_proc.filter(pat_proc.is_anomalous == 1)
    benign = pat_proc.filter(pat_proc.is_anomalous == 0)
    n_benign = benign.count()
    print("anom.count() = " + str(anom.count()) + ", benign.count() = " + str(benign.count())) #anom.count() = 49542, benign.count() = 197406
    
    sample_from_benign = benign.sample(False, 50000/n_benign)
    pat_proc = anom.unionAll(sample_from_benign)
    print("pat_proc.count() = " + str(pat_proc.count())) #99,227
    
    all_columns = pat_proc.columns
    features = [x for x in all_columns if (x not in ["patient_id", "is_anomalous"])]
    categorical_features = ["age_group", "gender", "income_range"] #We are listing these 3 as categorical features only as the procedure features have 0-1 values anyway 
    procedure_features = [x for x in features if (x not in categorical_features)]
    
    #Construct the map categoricalFeaturesInfo, which specifies which features are categorical and how many categorical values each of those features can take.
    
    #Create a dictionary where the key-value pairs are as follows: key is the name of the categorical feature, and value is a list with the following entries:
    #1) an id of the feature that is incremented sequentially, 2) no. of distinct values of the feature, 3) a list of the distinct values of the feature.
    cat_feature_number = 0
    dict_cat_features = {}
    
    for feature in categorical_features:
       agvalues = pat_proc.select(pat_proc[feature].cast("string").alias("feature")).distinct().collect()
       distinct_values = map(lambda row: row.asDict().values()[0], agvalues)
       distinct_values = sorted(map(lambda unicode_val: unicode_val.encode('ascii','ignore'), distinct_values))
       dict_cat_features[feature] = [cat_feature_number, len(distinct_values), distinct_values]
       cat_feature_number += 1
       
    pat_proc = pat_proc.rdd
    (train, test) = pat_proc.randomSplit([0.5, 0.5])
    print("train.count() = " + str(train.count()) + ", test.count() = " + str(test.count()))
    training_data = train.map(lambda x: create_labeled_point(x, features, categorical_features, dict_cat_features, procedure_features))
    print("training_data.count() = " + str(training_data.count()))
    
    #Populate the actual categoricalFeaturesInfo dictionary
    cat_features_info = dict([(value[0], value[1]) for (key, value) in dict_cat_features.iteritems()])
    procedure_features_info = dict([(feature_id, 2) for feature_id in range(3, 2 + len(procedure_features))])
    cat_features_info = dict(cat_features_info.items() + procedure_features_info.items())
    model = DecisionTree.trainClassifier(training_data, numClasses = 2, categoricalFeaturesInfo = cat_features_info, impurity = 'gini', maxDepth = 5, maxBins = 32)
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return pat_proc 
   

pat_proc = train_validate_test_rpart()
#pat_proc.take(5)
