from pyspark import SparkContext


#if __name__ == "__main__":

def train_validate_test_rpart():
    sc = SparkContext(appName = "AnalyzeAnomalyWithSpark")
    pat_proc = sc.textFile("/Users/blahiri/healthcare/data/cloudera_challenge/pat_proc_larger.csv").map(lambda line: line.split(",")).filter(lambda line: len(line) > 1).collect()
    print type(pat_proc) #<type 'list'>
    print len(pat_proc) #246949: works OK when run from Unix command shell by ./bin/spark-submit...
    return pat_proc 
   

pat_proc = train_validate_test_rpart()
#pat_proc.take(5)
