#from pyspark import SparkConf, SparkContext
#conf = (SparkConf().setMaster("local[*]").setAppName("AnalyzeAnomaly").set("spark.executor.memory", "5g"))
#sc = SparkContext(conf = conf)
from __future__ import division
from pyspark.mllib.regression import LabeledPoint
from pyspark.mllib.tree import DecisionTree, DecisionTreeModel
from pyspark.mllib.classification import LogisticRegressionWithSGD, LogisticRegressionWithLBFGS, LogisticRegressionModel
from time import time
from random import random

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
    #sc.parallelize(pat_proc, 30).collect()
    print("pat_proc.getNumPartitions() = " + str(pat_proc.getNumPartitions())) #4 partitions: the default should be the number of logical cores, which is 8
    
    (train, test) = pat_proc.randomSplit([0.5, 0.5])
    test_data_size = test.count()
    print("train.count() = " + str(train.count()) + ", test.count() = " + str(test_data_size))
    training_data = train.map(lambda x: create_labeled_point(x, features, categorical_features, dict_cat_features, procedure_features))
    print("training_data.count() = " + str(training_data.count()))
    
    #Populate the actual categoricalFeaturesInfo dictionary
    cat_features_info = dict([(value[0], value[1]) for (key, value) in dict_cat_features.iteritems()])
    procedure_features_info = dict([(feature_id, 2) for feature_id in range(3, 2 + len(procedure_features))])
    cat_features_info = dict(cat_features_info.items() + procedure_features_info.items())
    
    t0 = time()
    model = DecisionTree.trainClassifier(training_data, numClasses = 2, categoricalFeaturesInfo = cat_features_info, impurity = 'gini', maxDepth = 2, maxBins = 32) 
    #Under the hood in DecisionTree.scala, RandomForest is called with numTrees = 1 and featureSubsetStrategy = "all".
    tt = time() - t0
    print "Classifier trained in {} seconds".format(round(tt,3)) #63.355 seconds (5.5 times compared to standalone R). Even when maxDepth was reduced from 5 to 2, time to train was 61.942 seconds.
    print(model)
    
    test_data = test.map(lambda x: create_labeled_point(x, features, categorical_features, dict_cat_features, procedure_features))
    
    t0 = time()
    predictions = model.predict(test_data.map(lambda p: p.features))
    tt = time() - t0
    print "Prediction made in {} seconds".format(round(tt,3)) #0.014 seconds
    
    labels_and_preds = test_data.map(lambda p: p.label).zip(predictions) #Create a list of tuples with each tuple having the actual and the predicted label
    test_accuracy = labels_and_preds.filter(lambda (v, p): v == p).count() / float(test_data_size)
    fpr = labels_and_preds.filter(lambda (v, p): (v == 0 and p == 1)).count()/labels_and_preds.filter(lambda (v, p): v == 0).count() 
    fnr = labels_and_preds.filter(lambda (v, p): (v == 1 and p == 0)).count()/labels_and_preds.filter(lambda (v, p): v == 1).count()
    print "Test accuracy is {}, fpr is {}, fnr is {}".format(round(test_accuracy, 4), round(fpr, 4), round(fnr, 4)) #With maxDepth = 5, test accuracy is 0.9084, fpr is 0.1555, fnr is 0.0272.
    #With maxDepth = 2, test accuracy is 0.861, fpr is 0.2591, fnr is 0.018
    print model.toDebugString()
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return model 
   
   
def anom_with_lr():
  try:
    plaintext_rdd = sc.textFile("file:///Users/blahiri/healthcare/data/cloudera_challenge/pat_proc_larger.csv") #69.2 MB
    pat_proc = pycsv.csvToDataFrame(sqlContext, plaintext_rdd, sep = ",")
    anom = pat_proc.filter(pat_proc.is_anomalous == 1)
    benign = pat_proc.filter(pat_proc.is_anomalous == 0)
    n_benign = benign.count()
    
    #Take a random sample of 50K from the unlabeled 100K
    #benign = benign.map(lambda row: Row(**dict(row.asDict(), random_number = random())))
    sqlContext.registerFunction("my_random", lambda x: x - x + random())
    sqlContext.registerDataFrameAsTable(benign, "benign")
    #benign = sqlContext.sql("SELECT *, my_random(is_anomalous) as random_number FROM benign").collect().toDF()
    benign = sqlContext.sql("SELECT *, my_random(is_anomalous) as random_number FROM benign")
    #benign.withColumn('random_number', random()).collect()
    
    threshold = 50000/n_benign
    into_model = benign.filter(benign.random_number <= threshold)
    for_finding_more = benign.filter(benign.random_number > threshold)
    
    for_modeling = anom.unionAll(into_model.drop(into_model.random_number))
    for_finding_more = for_finding_more.drop(for_finding_more.random_number)
    #Try to pull this from a much larger sample, or, the entire data, because the ones with lowest probabilities, among
    #the selected 10,000, have probabilities around 0.05
    
    print("anom.count() = " + str(anom.count()) + ", benign.count() = " + str(benign.count()) + ", into_model.count() = " + str(into_model.count()) 
            + ", for_modeling.count() = " + str(for_modeling.count()) + ", for_finding_more.count() = " + str(for_finding_more.count()))
    
    all_columns = for_modeling.columns
    features = [x for x in all_columns if (x not in ["patient_id", "is_anomalous"])]
    categorical_features = ["age_group", "gender", "income_range"] #We are listing these 3 as categorical features only as the procedure features have 0-1 values anyway 
    procedure_features = [x for x in features if (x not in categorical_features)]

    #Unlike decision tree, logistic regression does not need the map categoricalFeaturesInfo, just an RDD of LabeledPoint objects.
    
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
       
    for_modeling = for_modeling.rdd
    print("for_modeling.getNumPartitions() = " + str(for_modeling.getNumPartitions())) #4 partitions: the default should be the number of logical cores, which is 8
    
    (train, test) = for_modeling.randomSplit([0.5, 0.5])
    test_data_size = test.count()
    print("train.count() = " + str(train.count()) + ", test.count() = " + str(test_data_size))
    training_data = train.map(lambda x: create_labeled_point(x, features, categorical_features, dict_cat_features, procedure_features))
    print("training_data.count() = " + str(training_data.count()))
    
    t0 = time()
    #model = LogisticRegressionWithLBFGS.train(training_data) #LBFGS took 66.766 seconds
    model = LogisticRegressionWithSGD.train(training_data) #SGCD took 69.261 seconds
    tt = time() - t0
    print "Classifier trained in {} seconds".format(round(tt,3)) 
    
    test_data = test.map(lambda x: create_labeled_point(x, features, categorical_features, dict_cat_features, procedure_features))
    
    t0 = time()
    predictions = model.predict(test_data.map(lambda p: p.features))
    tt = time() - t0
    print "Prediction made in {} seconds".format(round(tt,3)) #Reports as 0.0 seconds
    
    labelsAndPreds = test_data.map(lambda p: (p.label, model.predict(p.features)))
    test_accuracy = labelsAndPreds.filter(lambda (v, p): v == p).count()/float(test_data_size)

    fpr = labelsAndPreds.filter(lambda (v, p): (v == 0 and p == 1)).count()/labelsAndPreds.filter(lambda (v, p): v == 0).count() 
    fnr = labelsAndPreds.filter(lambda (v, p): (v == 1 and p == 0)).count()/labelsAndPreds.filter(lambda (v, p): v == 1).count()
    print "Test accuracy is {}, fpr is {}, fnr is {}".format(round(test_accuracy, 4), round(fpr, 4), round(fnr, 4)) #Test accuracy is 0.9458, fpr is 0.0761, fnr is 0.0322
    
    model.clearThreshold()
    for_finding_more = for_finding_more.map(lambda x: create_labeled_point(x, features, categorical_features, dict_cat_features, procedure_features)) #OK
    for_finding_more = for_finding_more.map(lambda p: (p.features, model.predict(p.features), p.label)) #OK
    #Reverse-sort the additional patients by their predicted probabilities of being anomalous and take the top 10,000
    #for_finding_more.take(5)
    
  except Exception:
    print("Exception in user code:")
    traceback.print_exc(file = sys.stdout)
  return for_finding_more
  

for_finding_more = anom_with_lr()
#pat_proc.take(5)
