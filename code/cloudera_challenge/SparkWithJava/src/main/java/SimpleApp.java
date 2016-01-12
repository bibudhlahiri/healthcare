/*** SimpleApp.java ***/
import org.apache.spark.api.java.*;
import org.apache.spark.api.java.function.Function;
import org.apache.spark.SparkConf;

public class SimpleApp {
  public static void main(String[] args) {
      
    String logFile = "file:///Users/blahiri/healthcare/code/cloudera_challenge/SparkWithJava/data/README.md";
    JavaSparkContext sc = new JavaSparkContext("local[1]", "Simple App",
      "$SPARK_HOME", new String[]{"target/simple-project-1.0.jar"});
      
    JavaRDD<String> logData = sc.textFile(logFile).cache();

    long numAs = logData.filter(new Function<String, Boolean>() {
      public Boolean call(String s) { return s.contains("a"); }
    }).count();

    long numBs = logData.filter(new Function<String, Boolean>() {
      public Boolean call(String s) { return s.contains("b"); }
    }).count();

    System.out.println("Lines with a: " + numAs + ", lines with b: " + numBs);
  }
}