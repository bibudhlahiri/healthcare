
train_validate_test_rpart <- function()
 {
   #sc <- sparkR.init(master = "local", sparkPackages = "com.databricks:spark-csv_2.11:1.0.3")
   #sqlContext <- sparkRSQL.init(sc)

   set.seed(1)
   customSchema <- structType(
    structField("patient_id", "integer"),
    structField("age_group", "string"),
    structField("gender", "string"),
    structField("income_range", "string"),
    structField("is_anomalous", "integer"),
    structField("12", "string"),
    structField("13", "string"))
   pat_proc <- read.df(sqlContext, path = "/Users/blahiri/healthcare/data/cloudera_challenge/pat_proc_tiny_few_columns.csv", 
                       source = "com.databricks.spark.csv", 
                       #inferSchema = "true", 
                       header = "true", delimiter = ",", charset = "UTF-8", schema = customSchema)
   print(head(select(pat_proc, pat_proc$income_range)))
   print(colnames(pat_proc)) #returns correct results
 }



