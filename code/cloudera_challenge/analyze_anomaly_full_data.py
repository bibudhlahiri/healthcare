from __future__ import print_function
from pyspark import SparkContext
from pyspark.sql import SQLContext
from pyspark.sql import Row
from pyspark.sql.functions import lit
from pyspark.sql.functions import UserDefinedFunction
from pyspark.sql.types import IntegerType
import sys, traceback 

home_folder = "/Users/blahiri"
#home_folder = "/home/impadmin/bibudh"
    
if __name__ == "__main__":
 sc = SparkContext(appName = "AnalyzeAnomaly")
 sc.addPyFile('python/pyspark/pyspark_csv.py')
 import pyspark_csv as pycsv
 sqlContext = SQLContext(sc)
   
 def convert_to_libsvm_format(row, age_groups, genders, income_groups):
   #Map each categorical feature to a numeric value. 
   
   age_group = ((row.asDict())["age_group"]).encode('ascii','ignore')
   age_group = age_groups.index(age_group)
   
   gender = ((row.asDict())["gender"]).encode('ascii','ignore')
   gender = genders.index(gender)
   
   income_group = ((row.asDict())["income_group"]).encode('ascii','ignore')
   income_group = income_groups.index(income_group) 
   
   is_anomalous = (row.asDict())["is_anomalous"]
   procedures = ((row.asDict())["procedures"]).encode('ascii','ignore')
    
   return str(is_anomalous) + " age_group:" + str(age_group) + " gender:" + str(gender) + " income_group:" + str(income_group) + " " + procedures
     
 def prepare_data():
  try:
    patients_df = sqlContext.read.format("com.databricks.spark.csv").option("header", "false").load("file://" + home_folder + "/healthcare/data/cloudera_challenge/sampled_patients.csv")
    oldColumns = patients_df.schema.names
    newColumns = ["patient_id", "age_group", "gender", "income_group"]
    patients_df = reduce(lambda data, idx: data.withColumnRenamed(oldColumns[idx], newColumns[idx]), xrange(len(oldColumns)), patients_df)
        
    reviews_df = sqlContext.read.format("com.databricks.spark.csv").option("header", "false").load("file://" + home_folder + "/healthcare/data/cloudera_challenge/reviews.csv")
    reviews_df = reviews_df.withColumnRenamed('C0', 'patient_id1').withColumn("is_anomalous", lit(1))
    
    #Prepare data in LIBSVM format. Use lists created out of these hard-coded values for now and look up the index of a value in the list    
    age_groups = [" <65", " 65-74", " 75-84", " 85+"]
    genders = [" M", " F"]
    income_groups = [" <16000", " 16000-23999", " 24000-31999", " 32000-47999", " 48000+"]  
    
    patients_df = patients_df.join(reviews_df, patients_df.patient_id == reviews_df.patient_id1, 'left_outer').drop('patient_id1')
    
    #Replace None values by 0 in is_anomalous
    name = 'is_anomalous'
    udf = UserDefinedFunction(lambda x: 0 if x is None else 1, IntegerType())
    patients_df = patients_df.select(*[udf(column).alias(name) if column == name else column for column in patients_df.columns]) 
            
    procedures_df = sqlContext.read.format("com.databricks.spark.csv").option("header", "false").load("file://" + home_folder + "/healthcare/data/cloudera_challenge/sampled_procedures.csv")
    oldColumns = procedures_df.schema.names
    newColumns = ["date", "patient_id2", "proc_code", "junk1", "junk2"]
    procedures_df = reduce(lambda data, idx: data.withColumnRenamed(oldColumns[idx], newColumns[idx]), xrange(len(oldColumns)), procedures_df)
    procedures_df = procedures_df.drop('date').drop('junk1').drop('junk2')
    
    #Combine all procedures for a patient to a single row and add ":1" after each
    procedures_rdd = procedures_df.rdd.combineByKey(lambda x: x + ":1", #Create a Combiner, i.e., create a one-element list
                                                    lambda x, value: x + " " + value + ":1",  #What to do when a combiner is given a new value
                                                    lambda x, y: x + " " + y) #How to merge two combiners
    procedures_df = procedures_rdd.toDF(['patient_id2', 'procedures'])    
    patients_df = patients_df.join(procedures_df, patients_df.patient_id == procedures_df.patient_id2, 'left_outer').drop('patient_id2')
    patients_df = patients_df.map(lambda x: convert_to_libsvm_format(x, age_groups, genders, income_groups))
    print(patients_df.take(5))
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return 
   

sparse_data = prepare_data()