
train_validate_test_rpart <- function()
 {
   set.seed(1)
   pat_proc <- read.df(sqlContext, path = "/Users/blahiri/healthcare/data/cloudera_challenge/pat_proc_larger.csv", 
                       source = "com.databricks.spark.csv", 
                       inferSchema = "true", 
                       header = "true", delimiter = ",", charset = "UTF-8") #DAG visualization shows this simply calls textFile() and mapPartitions() under the hood
   
   anom <- filter(pat_proc, (pat_proc$is_anomalous == '1')) #Filter does not call any new Spark job!
   benign <- filter(pat_proc, (pat_proc$is_anomalous == '0'))
   cat(paste("nrow(anom) = ", nrow(anom), ", nrow(benign) = ", nrow(benign), "\n", sep = ""))
   n_benign <- nrow(benign) 
   
   #DAG visualization shows nrow() calls textFile(), mapPartitions(), flatMap(), mapPartitions(), TungstenAggregate(), TungstenExchange(), TungstenExchange(), TungstenAggregate() and map() in sequence.
   
   sample_from_benign <- sample(benign, withReplacement = FALSE, fraction = 50000/n_benign)
   pat_proc <- rbind(anom, sample_from_benign)
   n_pat_proc <- nrow(pat_proc)
   
   features <- setdiff(names(pat_proc), c("patient_id", "is_anomalous"))
   x <- select(pat_proc, features)
   y <- select(pat_proc, "is_anomalous")
   
   pat_proc_test <- sample(pat_proc, FALSE, 0.5)
   test_ids <- (collect(select(pat_proc_test, "patient_id")))$patient_id
   cat(paste("length(test_ids) = ", length(test_ids), "\n", sep = ""))
   pat_proc$IS_TEST <- (pat_proc$patient_id %in% test_ids)
   print(pat_proc$IS_TEST)
   cat(paste("length(pat_proc$IS_TEST) = ", length(pat_proc$IS_TEST), "\n", sep = ""))
   pat_proc_train <- subset(pat_proc, pat_proc$IS_TEST == FALSE)
   cat(paste("Size of training data = ", length(pat_proc_train), ", size of test data = ", (nrow(pat_proc) - length(pat_proc_train)), "\n", sep = ""))
 }



