from __future__ import print_function
from pyspark import SparkContext
from pyspark.sql import SQLContext
from pyspark.sql import Row
import sys, traceback 

home_folder = "/Users/blahiri"
#home_folder = "/home/impadmin/bibudh"
    
if __name__ == "__main__":
 sc = SparkContext(appName = "AnalyzeAnomaly")
 sc.addPyFile('python/pyspark/pyspark_csv.py')
 import pyspark_csv as pycsv
 sqlContext = SQLContext(sc)
 
    
 def map_categorical_to_numeric(row, age_groups, genders, income_groups):
   #Map each categorical feature to a numeric value. 
   patient_id = ((row.asDict())["patient_id"]).encode('ascii','ignore')
   age_group = ((row.asDict())["age_group"]).encode('ascii','ignore')
   age_group = age_groups.index(age_group)
   
   gender = ((row.asDict())["gender"]).encode('ascii','ignore')
   gender = genders.index(gender)
   
   income_group = ((row.asDict())["income_group"]).encode('ascii','ignore')
   income_group = income_groups.index(income_group)
   
   return Row(patient_id = patient_id, age_group = age_group, gender = gender, income_group = income_group)
    
 def prepare_data():
  try:
    patients_df = sqlContext.read.format("com.databricks.spark.csv").option("header", "false").load("file://" + home_folder + "/healthcare/data/cloudera_challenge/patients.csv")
    oldColumns = patients_df.schema.names
    newColumns = ["patient_id", "age_group", "gender", "income_group"]
    patients_df = reduce(lambda data, idx: data.withColumnRenamed(oldColumns[idx], newColumns[idx]), xrange(len(oldColumns)), patients_df)
      
    procedures_rdd = sc.textFile("file://" + home_folder + "/healthcare/data/cloudera_challenge/PCDR2011/*.csv") #Even with this, and csvToDataFrame() commented, script took 1 sec to run, 
    #so sc.textFile() is actually lazy
    
    reviews_df = sc.textFile("file://" + home_folder + "/healthcare/data/cloudera_challenge/REVIEW.TXT")
    
    #Prepare data in LIBSVM format. Use lists created out of these hard-coded values for now and look up the index of a value in the list
    
    age_groups = [" <65", " 65-74", " 75-84", " 85+"]
    genders = [" M", " F"]
    income_groups = [" <16000", " 16000-23999", " 24000-31999", " 32000-47999", " 48000+"]  
    
    patients_df = patients_df.sample(False, 0.001)
    print(patients_df.take(5))
    patients_processed = patients_df.map(lambda x: map_categorical_to_numeric(x, age_groups, genders, income_groups)).collect()
    print(patients_processed[:5])
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return 
   

sparse_data = prepare_data()