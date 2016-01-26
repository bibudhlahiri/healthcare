#from pyspark import SparkConf, SparkContext
#conf = (SparkConf().setMaster("local[*]").setAppName("AnalyzeAnomaly").set("spark.executor.memory", "5g"))
#sc = SparkContext(conf = conf)
from __future__ import division
from pyspark.mllib.linalg import Vectors
from pyspark.ml.feature import VectorAssembler
from pyspark.ml.feature import RFormula
from pyspark.mllib.regression import LabeledPoint

sc.addPyFile('python/pyspark/pyspark_csv.py')
import pyspark_csv as pycsv

import sys, traceback
    
    
def create_labeled_point(row, categorical_features, dict_cat_features):
   #Map each categorical feature to a numeric value. Return a LabeledPoint with is_anomalous as label, and everything else (in numeric values) put in a list
   features_mapped_to_num = []
   for feature in categorical_features:
     #print(type(row))
     #print(row.asDict())
     distinct_values = (dict_cat_features[feature])[2]
     feature_value = ((row.asDict())[feature]).encode('ascii','ignore')
     features_mapped_to_num.append(distinct_values.index(feature_value))
     #print("feature_value = " + feature_value + ", converted to numeric = ", str(distinct_values.index(feature_value)))
   is_anomalous = ((row.asDict())["is_anomalous"])
   return LabeledPoint(float(is_anomalous), [float(x) for x in features_mapped_to_num])
  
def train_validate_test_rpart():
  try:
    #sc.setMaster("local[*]").setAppName("AnalyzeAnomaly").set("spark.executor.memory", "5g")
    plaintext_rdd = sc.textFile("file:///Users/blahiri/healthcare/data/cloudera_challenge/pat_proc_larger.csv") #69.2 MB
    pat_proc = pycsv.csvToDataFrame(sqlContext, plaintext_rdd, sep = ",")
    #print type(pat_proc) #<class 'pyspark.sql.dataframe.DataFrame'>
    #print pat_proc.count() #246948, excludes header
    print pat_proc.take(1) #prints first data row
    
    anom = pat_proc.filter(pat_proc.is_anomalous == 1)
    benign = pat_proc.filter(pat_proc.is_anomalous == 0)
    n_benign = benign.count()
    print("anom.count() = " + str(anom.count()) + ", benign.count() = " + str(benign.count())) #anom.count() = 49542, benign.count() = 197406
    
    sample_from_benign = benign.sample(False, 50000/n_benign)
    print("sample_from_benign.count() = " + str(sample_from_benign.count())) #49,998
    pat_proc = anom.unionAll(sample_from_benign)
    print("pat_proc.count() = " + str(pat_proc.count())) #99,227
    
    all_columns = pat_proc.columns
    features = [x for x in all_columns if (x not in ["patient_id", "is_anomalous"])]
    categorical_features = ["age_group", "gender", "income_range"] #We are listing these 3 as categorical features only as the procedure features have 0-1 values anyway 
    
    str_formula = "is_anomalous ~ " + " + ".join(features)
    print(str_formula)
    
    #Construct the map categoricalFeaturesInfo, which specifies which features are categorical and how many categorical values each of those features can take.
    
    #Create a dictionary where the key-value pairs are as follows: key is the name of the categorical feature, and value is a list with the following entries:
    #1) an id of the feature that is incremented sequentially, 2) no. of distinct values of the feature, 3) a list of the distinct values of the feature.
    cat_feature_number = 0
    dict_cat_features = {}
    
    for feature in categorical_features:
       agvalues = pat_proc.select(pat_proc[feature].cast("string").alias("feature")).distinct().collect()
       distinct_values = map(lambda row: row.asDict().values()[0], agvalues)
       distinct_values = sorted(map(lambda unicode_val: unicode_val.encode('ascii','ignore'), distinct_values))
       print(distinct_values)
       print("cat_feature_number = " + str(cat_feature_number) + ", len(distinct_values) = " + str(len(distinct_values)))
       dict_cat_features[feature] = [cat_feature_number, len(distinct_values), distinct_values]
       cat_feature_number += 1
       
    pat_proc = pat_proc.rdd
    (train, test) = pat_proc.randomSplit([0.5, 0.5])
    print("train.count() = " + str(train.count()) + ", test.count() = " + str(test.count()))
    #training_data = train.foreach(lambda x: create_labeled_point(x, categorical_features, dict_cat_features))
    training_data = train.map(lambda x: create_labeled_point(x, categorical_features, dict_cat_features))
    print("training_data.count() = " + str(training_data.count()))
    #model = DecisionTree.trainClassifier(train, numClasses = 2, categoricalFeaturesInfo={}, impurity = 'gini', maxDepth = 5, maxBins = 32)
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return pat_proc 
   

pat_proc = train_validate_test_rpart()
#pat_proc.take(5)
