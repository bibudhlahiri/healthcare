from __future__ import print_function
from pyspark import SparkContext
from pyspark.sql import SQLContext
import sys, traceback 

home_folder = "/Users/blahiri"
#home_folder = "/home/impadmin/bibudh"
    
if __name__ == "__main__":
 sc = SparkContext(appName = "AnalyzeAnomaly")
 sc.addPyFile('python/pyspark/pyspark_csv.py')
 import pyspark_csv as pycsv
 sqlContext = SQLContext(sc)
 
 def prepare_data():
  try:
    patients_rdd = sc.textFile("file://" + home_folder + "/healthcare/data/cloudera_challenge/patients.csv") #~100 M patients, 2.8 GB, this statement runs in 1 sec
    #pat_proc = pycsv.csvToDataFrame(sqlContext, plaintext_rdd, sep = ",") #This statement took several hours, launched 89 tasks and then the worker went dead
    
    procedures_rdd = sc.textFile("file://" + home_folder + "/healthcare/data/cloudera_challenge/PCDR2011/*.csv") #Even with this, and csvToDataFrame() commented, script took 1 sec to run, 
    #so sc.textFile() is actually lazy
    #print("patients_rdd.count() = " + str(patients_rdd.count()) + ", procedures_rdd.count() = " + str(procedures_rdd.count())) #This brought back the 89 tasks
    
    #Prepare data in LIBSVM format
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
    #Idea: Use lists created out of these hard-coded values for now and look up the index of a value in the list
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
   
   

sparse_data = prepare_data()