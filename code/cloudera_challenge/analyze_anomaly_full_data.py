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
 
 def get_values_of_categorical_vars(patients_rdd):
    age_groups = patients_rdd.map(lambda x:x.split(",")).map(lambda x: x[1]).distinct() #3.8 min
    print("age_groups.count() = " + str(age_groups.count())) #2 s
    str_age_groups = age_groups.reduce(lambda x, y: x + "; " + y) #0.6 s
    #age_group.foreach(print)
    
    genders = patients_rdd.map(lambda x:x.split(",")).map(lambda x: x[2]).distinct()
    str_genders = genders.reduce(lambda x, y: x + "; " + y) #3.4 min: distinct() is not evaluated until this count() or reduce(), i.e., the actions, are called. distinct() is a transformation.
    
    income_groups = patients_rdd.map(lambda x:x.split(",")).map(lambda x: x[3]).distinct()
    str_income_groups = income_groups.reduce(lambda x, y: x + "; " + y)
    print("str_age_groups = " + str_age_groups + ", str_genders = " + str_genders + ", str_income_groups = " + str_income_groups)
    #str_age_groups =  85+;  65-74;  75-84;  <65, str_genders =  M;  F, str_income_groups =  <16000;  48000+;  32000-47999;  24000-31999;  16000-23999
    return   
    
 def map_categorical_to_numeric(row, age_groups, genders, income_groups):
   #Map each categorical feature to a numeric value. 
   age_group = ((row.asDict())["age_group"]).encode('ascii','ignore')
   print("age_group before conv = " + age_group)
   age_group = age_groups.index(age_group)
   print("age_group after conv = " + str(age_group))
   
   gender = ((row.asDict())["gender"]).encode('ascii','ignore')
   print("gender before conv = " + gender)
   gender = genders.index(gender)
   print("gender after conv = " + str(gender))
   
   income_group = ((row.asDict())["income_group"]).encode('ascii','ignore')
   print("income_group before conv = " + income_group)
   income_group = income_groups.index(income_group)
   print("income_group after conv = " + str(income_group))
   
   return
    
 def prepare_data():
  try:
    patients_df = sqlContext.read.format("com.databricks.spark.csv").option("header", "false").load("file://" + home_folder + "/healthcare/data/cloudera_challenge/patients.csv")
    oldColumns = patients_df.schema.names
    newColumns = ["patient_id", "age_group", "gender", "income_group"]
    patients_df = reduce(lambda data, idx: data.withColumnRenamed(oldColumns[idx], newColumns[idx]), xrange(len(oldColumns)), patients_df)
    
    #age_groups = patients_df.select(patients_df["age_group"].cast("string")).distinct().collect()
    
    procedures_rdd = sc.textFile("file://" + home_folder + "/healthcare/data/cloudera_challenge/PCDR2011/*.csv") #Even with this, and csvToDataFrame() commented, script took 1 sec to run, 
    #so sc.textFile() is actually lazy
    
    reviews_rdd = sc.textFile("file://" + home_folder + "/healthcare/data/cloudera_challenge/REVIEW.TXT")
    
    #Prepare data in LIBSVM format
    
    #Idea: Use lists created out of these hard-coded values for now and look up the index of a value in the list
    
    age_groups = [" <65", " 65-74", " 75-84", " 85+"]
    genders = [" M", " F"]
    income_groups = [" <16000", " 16000-23999", " 24000-31999", " 32000-47999", " 48000+"]  
    patients_processed = patients_df.map(lambda x: map_categorical_to_numeric(x, age_groups, genders, income_groups)).collect()
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return 
   

sparse_data = prepare_data()