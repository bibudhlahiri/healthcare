from pyspark import SparkContext


sc.addPyFile('python/pyspark/pyspark_csv.py')
import pyspark_csv as pycsv

import sys, traceback


def train_validate_test_rpart():
  try:
    
    plaintext_rdd = sc.textFile("file:///Users/blahiri/healthcare/data/cloudera_challenge/pat_proc_larger.csv")
    pat_proc = pycsv.csvToDataFrame(sqlContext, plaintext_rdd, sep = ",")
    print type(pat_proc) #<class 'pyspark.sql.dataframe.DataFrame'>
    print pat_proc.count() #246948, excludes header
    print pat_proc.take(1) #prints first data row
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return pat_proc 
   

pat_proc = train_validate_test_rpart()
#pat_proc.take(5)
