from __future__ import print_function
from __future__ import division
from pyspark import SparkContext
from pyspark.sql import SQLContext
from pyspark.sql import Row
from pyspark.sql.functions import lit
from pyspark.sql.functions import UserDefinedFunction
from pyspark.sql.types import IntegerType
from pyspark.ml.classification import LogisticRegression
from pyspark.mllib.classification import LogisticRegressionWithSGD
from pyspark.mllib.util import MLUtils
from time import time
import sys, traceback 

home_folder = "/Users/blahiri"
#home_folder = "/home/impadmin/bibudh"
    
if __name__ == "__main__":
 sc = SparkContext(appName = "AnalyzeAnomaly")
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
   #Names of features cannot be strings: should be all numbers  
   return str(is_anomalous) + " 1:" + str(age_group) + " 2:" + str(gender) + " 3:" + str(income_group) + " " + procedures
     
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
    
    #Map all procedure codes to numbers between 4 and n + 3, where n is the number of distinct procedures. We start from 4 because the first 3 are reserved for demographic features.
    procedure_codes = procedures_df.select(procedures_df.proc_code).distinct().collect()
    procedure_codes = sorted(map(lambda row: row.asDict().values()[0], procedure_codes)) #Extract the actual procedure codes from list of Row objects. We sort the codes because in the final 
    #LibSVM format data, the numeric codes for procedure codes for a patient should remain numerically sorted.
    procedure_codes = map(lambda unicode_val: unicode_val.encode('ascii'), procedure_codes) #Remove the 'u' from u'286'
    procedure_code_table = {key:val for (key, val) in zip(procedure_codes, range(4, 4 + len(procedure_codes)))} #The "stop" element is not included in python range() function
    
    #Combine all procedures for a patient to a single row and add ":1" after each. Look up the number for the procedure code from procedure_code_table.
    procedures_df = procedures_df.orderBy("patient_id2", "proc_code") #Ordering a given patient's data by procedure code so that the final numeric codes are in ascending order in the LibSVM file
    procedures_rdd = procedures_df.rdd.combineByKey(lambda x: str(procedure_code_table[x.encode('ascii')]) + ":1", #Create a Combiner, i.e., create a one-element list
                                                    lambda x, value: x + " " + str(procedure_code_table[value.encode('ascii')]) + ":1",  #What to do when a combiner is given a new value
                                                    lambda x, y: x + " " + y) #How to merge two combiners
    procedures_df = procedures_rdd.toDF(['patient_id2', 'procedures'])    
    patients_df = patients_df.join(procedures_df, patients_df.patient_id == procedures_df.patient_id2, 'left_outer').drop('patient_id2')
    patients_df = patients_df.map(lambda x: convert_to_libsvm_format(x, age_groups, genders, income_groups)) 
    patients_df.saveAsTextFile('file://' + home_folder + '/healthcare/data/cloudera_challenge/pat_proc_libsvm_format')
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return 
 
#Train the model on a set comprising of the 50K anomalous and a sample of 50K from the remaining data. Next, apply the model on the data that remains after taking off this 100K. 
def anom_with_lr():
  try:
    pat_proc = sqlContext.read.format("libsvm").load('file://' + home_folder + '/healthcare/data/cloudera_challenge/pat_proc_libsvm_format/part-*') #This gives a DataFrame
    #pat_proc = MLUtils.loadLibSVMFile(sc, 'file://' + home_folder + '/healthcare/data/cloudera_challenge/pat_proc_libsvm_format/part-*')
    #.collect() #loadLibSVMFile() returns list of LabeledPoint objects if collect() is called,
    #otherwise returns PipelinedRDD
    #print("pat_proc.count() = " + str(pat_proc.count())) #150,127 rows, the two columns are ['label', 'features']
    
    #pat_proc = pat_proc.toDF() #lr.fit(), where lr is an instance of LogisticRegression, needs DataFrame
    anom = pat_proc.filter(pat_proc.label == 1)
    benign = pat_proc.filter(pat_proc.label == 0)
    n_benign = benign.count()
    
    #Take a random sample of 50K from benign
    frac = 50000/n_benign
    (into_model, for_finding_more) = benign.randomSplit([frac, 1 - frac])
    print("into_model.count() = " + str(into_model.count()) + ", for_finding_more.count() = " + str(for_finding_more.count()))
    
    for_modeling = anom.unionAll(into_model)
    #for_modeling = for_modeling.rdd #LogisticRegressionWithSGD works on RDD of LabeledPoint objects
    (train, test) = for_modeling.randomSplit([0.5, 0.5])
    test_data_size = test.count()
    print("train.count() = " + str(train.count()) + ", test.count() = " + str(test_data_size))
    
    lr = LogisticRegression(maxIter = 10, regParam = 0.3, elasticNetParam = 0.8)
    t0 = time()
    model = lr.fit(train)
    #model = LogisticRegressionWithSGD.train(train)
    tt = time() - t0
    #print "Classifier trained in {0} seconds".format(round(tt,3)) 
    
    print("test.class = " + test.__class__.__name__) #DataFrame
    print(test.take(5))
    #test = test.map(lambda p: p.features)
    #print("test.class = " + test.__class__.__name__) #PipelinedRDD
    #print(test.take(5))
    #test = test.collect()
    #print("test.class = " + test.__class__.__name__) #list
    #print(test[:5])
    #test = test.toDF()
    #print("test.class = " + test.__class__.__name__)
    #print(test.take(5))
    t0 = time()
    predictions = model.transform(test) #Feed the test DataFrame as-is, do not need to feed the features only
    tt = time() - t0
    #print "Prediction made in {0} seconds".format(round(tt,3))

    # Print the coefficients and intercept for logistic regression
    print("Coefficients: " + str(model.coefficients))
    print("Intercept: " + str(model.intercept))
    print(predictions.take(5))
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return 

#prepare_data()   
anom_with_lr()