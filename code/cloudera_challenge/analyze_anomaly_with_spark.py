#from pyspark import SparkConf, SparkContext
#conf = (SparkConf().setMaster("local[*]").setAppName("AnalyzeAnomaly").set("spark.executor.memory", "5g"))
#sc = SparkContext(conf = conf)
from __future__ import division

sc.addPyFile('python/pyspark/pyspark_csv.py')
import pyspark_csv as pycsv

import sys, traceback

def train_validate_test_rpart():
  try:
    #sc.setMaster("local[*]").setAppName("AnalyzeAnomaly").set("spark.executor.memory", "5g")
    plaintext_rdd = sc.textFile("file:///Users/blahiri/healthcare/data/cloudera_challenge/pat_proc_larger.csv") #69.2 MB
    pat_proc = pycsv.csvToDataFrame(sqlContext, plaintext_rdd, sep = ",")
    #print type(pat_proc) #<class 'pyspark.sql.dataframe.DataFrame'>
    #print pat_proc.count() #246948, excludes header
    #print pat_proc.take(1) #prints first data row
    
    anom = pat_proc.filter(pat_proc.is_anomalous == '1')
    benign = pat_proc.filter(pat_proc.is_anomalous == '0')
    n_benign = benign.count()
    #print("anom.count() = " + str(anom.count()) + ", benign.count() = " + str(benign.count())) #anom.count() = 49542, benign.count() = 197406
    
    sample_from_benign = benign.sample(False, 50000/n_benign)
    print("sample_from_benign.count() = " + str(sample_from_benign.count())) #49,998
    pat_proc = anom.unionAll(sample_from_benign)
    print("pat_proc.count() = " + str(pat_proc.count())) #99,227
    
    all_columns = pat_proc.columns
    features = [x for x in all_columns if (x not in ["patient_id", "is_anomalous"])]
    pat_proc = pat_proc.rdd
    (train, test) = pat_proc.randomSplit([0.5, 0.5])
    #model = DecisionTree.trainClassifier(train, numClasses = 2, categoricalFeaturesInfo={}, impurity = 'gini', maxDepth = 5, maxBins = 32)
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return pat_proc 
   

pat_proc = train_validate_test_rpart()
#pat_proc.take(5)
