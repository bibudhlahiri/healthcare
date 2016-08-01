from __future__ import division
from pyspark import SparkContext
from pyspark.sql import SQLContext
from pyspark.sql import Row
from pyspark.sql.functions import lit
from pyspark.sql.functions import UserDefinedFunction
from pyspark.sql.types import IntegerType
from pyspark.ml.classification import LogisticRegression
from pyspark.mllib.classification import LogisticRegressionWithSGD
from pyspark.mllib.classification import NaiveBayes, NaiveBayesModel
from pyspark.mllib.util import MLUtils
from pyspark.mllib.regression import LabeledPoint
from time import time
import sys, traceback 

home_folder = "file:///Users/blahiri"
#home_folder = "file:///home/impadmin/bibudh"
#home_folder = "hdfs://master:54310/bibudh"
    
if __name__ == "__main__":
 #sc = SparkContext(appName = "AnalyzeAnomaly")
 sqlContext = SQLContext(sc)

 def custom_encode1(unicode_val):return str(unicode_val.encode('ascii', 'ignore') if unicode_val else unicode_val)
   
 def convert_to_libsvm_format(row, age_groups, genders, income_groups):
   #Map each categorical feature to a numeric value.    
   age_group = custom_encode1((row.asDict())["age_group"])
   age_group = age_groups.index(age_group)   
   gender = custom_encode1((row.asDict())["gender"])
   gender = genders.index(gender)   
   income_group = custom_encode1((row.asDict())["income_group"])
   income_group = income_groups.index(income_group)   
   is_anomalous = (row.asDict())["is_anomalous"]
   procedures = custom_encode1((row.asDict())["procedures"])  
   #Names of features cannot be strings: should be all numbers  
   return str(is_anomalous) + " 1:" + str(age_group) + " 2:" + str(gender) + " 3:" + str(income_group) + ("" if procedures == "None" else " " + procedures) 
     
 def prepare_data():
  try:
    patients_df = sqlContext.read.format("com.databricks.spark.csv").option("header", "false").load(home_folder + "/healthcare/data/cloudera_challenge/sampled_patients.csv")
    #patients_df = sqlContext.read.format("com.databricks.spark.csv").option("header", "false").load(home_folder + "/healthcare/data/cloudera_challenge/patients.csv")
    oldColumns = patients_df.schema.names
    newColumns = ["patient_id", "age_group", "gender", "income_group"]
    patients_df = reduce(lambda data, idx: data.withColumnRenamed(oldColumns[idx], newColumns[idx]), xrange(len(oldColumns)), patients_df)
        
    reviews_df = sqlContext.read.format("com.databricks.spark.csv").option("header", "false").load(home_folder + "/healthcare/data/cloudera_challenge/reviews.csv")
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
            
    procedures_df = sqlContext.read.format("com.databricks.spark.csv").option("header", "false").load(home_folder + "/healthcare/data/cloudera_challenge/sampled_procedures.csv")
    #procedures_df = sqlContext.read.format("com.databricks.spark.csv").option("header", "false").load(home_folder + "/healthcare/data/cloudera_challenge/PCDR2011/*.csv")
    oldColumns = procedures_df.schema.names
    newColumns = ["date", "patient_id2", "proc_code", "junk1", "junk2"]
    procedures_df = reduce(lambda data, idx: data.withColumnRenamed(oldColumns[idx], newColumns[idx]), xrange(len(oldColumns)), procedures_df)
    procedures_df = procedures_df.drop('date').drop('junk1').drop('junk2')
    
    #Map all procedure codes to numbers between 4 and n + 3, where n is the number of distinct procedures. We start from 4 because the first 3 are reserved for demographic features.
    procedure_codes = procedures_df.select(procedures_df.proc_code).distinct().collect() #On full dataset, 17 min (236 tasks) for distinct(), 9 s (200 tasks) for collect()
    procedure_codes = map(lambda row: row.asDict().values()[0], procedure_codes) #Extract the actual procedure codes from list of Row objects.  
    procedure_codes = map(lambda unicode_val: custom_encode(unicode_val), procedure_codes) #Remove the 'u' from u'286'
    #procedure_code_table = {key:val for (key, val) in zip(procedure_codes, range(4, 4 + len(procedure_codes)))} #The "stop" element is not included in python range() function
    procedure_code_table = dict(zip(procedure_codes, range(4, 4 + len(procedure_codes))))

    #We saw inputs like "0 1:1 2:0 3:1 32:1 110:1 29:1 30:1" when we ran on full data where 110 should have come after 30. So we are mapping to the numeric procedure codes first and then sorting on that.
    sqlContext.registerFunction("convert_proc_code", lambda x: procedure_code_table[custom_encode(x)])
    sqlContext.registerDataFrameAsTable(procedures_df, "procedures_df")
    procedures_df = sqlContext.sql("SELECT *, convert_proc_code(proc_code) as num_proc_code FROM procedures_df")

    #Combine all procedures for a patient to a single row and add ":1" after each. Look up the number for the procedure code from procedure_code_table.
    procedures_rdd = procedures_df.drop('proc_code').rdd
    procedures_rdd = procedures_rdd.groupByKey().mapValues(combine_procedures)

    procedures_df = procedures_rdd.toDF(['patient_id2', 'procedures'])    
    patients_df = patients_df.join(procedures_df, patients_df.patient_id == procedures_df.patient_id2, 'left_outer').drop('patient_id2')
    patients_df = patients_df.map(lambda x: convert_to_libsvm_format(x, age_groups, genders, income_groups)) 
    patients_df.saveAsTextFile(home_folder + '/healthcare/data/cloudera_challenge/pat_proc_libsvm_format')
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return 

def custom_encode(unicode_val):return str(unicode_val.encode('ascii') if unicode_val else unicode_val)

def combine_procedures(l_procs): 
  ascii_procs = map(lambda x: int(custom_encode(x)), l_procs) #Convert unicode to ASCII and convert string to integer
  return ' '.join([str(x) + ":1" for x in sorted(ascii_procs)])

def check_for_ascending(input):
   #Take a string of the format 0 1:2 2:1 3:3 7:1 30:1 31:1 104:1
   #print("In method check_for_ascending for input = " + input)
   indices = map(lambda w: w.split(':')[0], input.split()[1:])
   indices = [int(x) for x in indices]
   length = len(indices)
   for i in range(1, length):
     if indices[i] <= indices[i-1]:
       print("input = " + input + ", error in input between " + str(indices[i-1]) + " and " + str(indices[i]))
       return
   return

def split_data():
  try:
    #pat_proc = sc.textFile("hdfs://master:54310/bibudh/healthcare/data/cloudera_challenge/pat_proc_libsvm_format") 
    #sqlContext.createDataFrame(pat_proc.map(lambda x: custom_encode(x)).take(10000)).foreach(check_for_ascending)
    #map(lambda w: check_for_ascending(w), pat_proc.map(lambda x: custom_encode(x)).take(10000000))
    #pat_proc = sqlContext.read.format("libsvm").load(home_folder + '/healthcare/data/cloudera_challenge/pat_proc_libsvm_format/part-*') #This gives a DataFrame
    pat_proc = MLUtils.loadLibSVMFile(sc, home_folder + '/healthcare/data/cloudera_challenge/pat_proc_libsvm_format/part-*').toDF() #Naive Bayes expects
    #data as an RDD of LabeledPoint
    print("pat_proc.count() = " + str(pat_proc.count())) #150,127 rows, the two columns are ['label', 'features']
    
    anom = pat_proc.filter(pat_proc.label == 1) #This can be done since we have called toDF() on output of loadLibSVMFile()
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
    ret_obj = {'train': train, 'test': test, 'for_finding_more': for_finding_more}
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return ret_obj
  
#Train the model on a set comprising of the 50K anomalous and a sample of 50K from the remaining data. Next, apply the model on the data that remains after taking off this 100K. 
def anom_with_lr():
  try:
    prepared_data = split_data()
    train = prepared_data['train']
    test = prepared_data['test']
    for_finding_more = prepared_data['for_finding_more']
    lr = LogisticRegression(maxIter = 10, regParam = 0.0, elasticNetParam = 0.0) #We set regParam = 0 to make it comparable with LogisticRegressionWithSGD that we used before, which does not do 
    #any regularization by default. With regParam = 0, value of elasticNetParam should not matter. elasticNetParam = 0 is Ridge regression (L2), keeps all features. elasticNetParam = 1 is LASSO (L1), performs feature selection.
    #With regParam = 0, test accuracy is 0.9454, fpr is 0.0713, fnr is 0.0375, on a sample of 50K test data points. 
    t0 = time()
    model = lr.fit(train)
    tt = time() - t0
    print "Classifier trained in {0} seconds".format(round(tt,3)) 
    
    t0 = time()
    predictions = model.transform(test) #Feed the test DataFrame as-is, do not need to feed the features only
    tt = time() - t0
    print "Prediction made in {0} seconds".format(round(tt,3))
 
    #Adding proabability to test data set for calibration
    labelsAndPreds = predictions.map(lambda p: (p.label, p.prediction, round(p.probability[1], 5)))   
    labelsAndPreds.toDF(["label", "predicted_label", "predicted_prob"]).write.format('com.databricks.spark.csv').save(home_folder + '/healthcare/data/cloudera_challenge/labelsAndPreds/logistic_regression')   
 
    test_accuracy = labelsAndPreds.filter(lambda (v, p, r): v == p).count()/float(test_data_size)        
    fpr = labelsAndPreds.filter(lambda (v, p, r): (v == 0 and p == 1)).count()/labelsAndPreds.filter(lambda (v, p, r): v == 0).count() 
    fnr = labelsAndPreds.filter(lambda (v, p, r): (v == 1 and p == 0)).count()/labelsAndPreds.filter(lambda (v, p, r): v == 1).count()
    print "Test accuracy is {0}, fpr is {1}, fnr is {2}".format(round(test_accuracy, 4), round(fpr, 4), round(fnr, 4))
    
    for_finding_more = model.transform(for_finding_more).map(lambda p: (p.label, round(p.probability[1], 5))) #toDF() in next line did not work without round(): some issue with float
    for_finding_more = for_finding_more.toDF(["label", "predicted_prob"])
    for_finding_more = for_finding_more.orderBy(for_finding_more.predicted_prob.desc())
    for_finding_more.select('predicted_prob').limit(10000).write.format('com.databricks.spark.csv').save(home_folder + '/healthcare/data/cloudera_challenge/additional_10000_from_spark') #Top one has 
    #probability of 0.9999, last one has probability 0.05159, 75 of them above 0.99
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return 
  
def create_labeled_point(row):
  row_as_dict = row.asDict()
  return LabeledPoint(row_as_dict['label'], row_as_dict['features'])
  
#Using Naive Bayes as it is another probabilistic classifier: want to compare the estimated probabilities with those
#generated by Logistic Regression. However, API in Spark does not return the probabilities.
def anom_with_nb():
  try:
    prepared_data = split_data()
    train = prepared_data['train'].rdd #NaiveBayes works on RDD of LabeledPoint objects. This returns an RDD of Row objects, with two fields,
    #a label and a SparseVector.
    test = prepared_data['test'].rdd
	
    training_data = train.map(lambda x: create_labeled_point(x))
    test_data = test.map(lambda x: create_labeled_point(x))
    	
    t0 = time()
    nb = NaiveBayes.train(training_data, 1.0) 
    tt = time() - t0
    print "Classifier trained in {0} seconds".format(round(tt,3)) #Classifier trained in 349.688 seconds
    
    t0 = time()
    #Adding proabability to test data set for calibration
    labelsAndPreds = test_data.map(lambda p: (p.label, nb.predict(p.features), round(p.probability[1], 5)))
    tt = time() - t0
    print "Prediction made in {0} seconds".format(round(tt,3))
       
    labelsAndPreds.toDF(["label", "predicted_label", "predicted_prob"]).write.format('com.databricks.spark.csv').save(home_folder + '/healthcare/data/cloudera_challenge/labelsAndPreds/naive_bayes')   
 
    test_accuracy = labelsAndPreds.filter(lambda (v, p, r): v == p).count()/float(test_data_size)        
    fpr = labelsAndPreds.filter(lambda (v, p, r): (v == 0 and p == 1)).count()/labelsAndPreds.filter(lambda (v, p, r): v == 0).count() 
    fnr = labelsAndPreds.filter(lambda (v, p, r): (v == 1 and p == 0)).count()/labelsAndPreds.filter(lambda (v, p, r): v == 1).count()
    print "Test accuracy is {0}, fpr is {1}, fnr is {2}".format(round(test_accuracy, 4), round(fpr, 4), round(fnr, 4))    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return

#prepare_data()   
#anom_with_lr()
anom_with_nb()
