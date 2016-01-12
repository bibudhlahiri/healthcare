import java.util.HashMap;
import java.util.regex.Pattern;

import scala.Tuple2;
import org.apache.spark.api.java.function.Function2;
import org.apache.spark.api.java.JavaPairRDD;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
import org.apache.spark.api.java.function.Function;
import org.apache.spark.api.java.function.PairFunction;
import org.apache.spark.mllib.regression.LabeledPoint;
import org.apache.spark.mllib.tree.DecisionTree;
import org.apache.spark.mllib.tree.model.DecisionTreeModel;
import org.apache.spark.mllib.util.MLUtils;
import org.apache.spark.SparkConf;
import org.apache.spark.mllib.linalg.Vectors;
import org.apache.spark.mllib.regression.LabeledPoint;

public class AnalyzeAnomaly {

  static class ParsePoint implements Function<String, LabeledPoint> {
    private static final Pattern COMMA = Pattern.compile(",");

    @Override
    public LabeledPoint call(String line) {
      String[] parts = COMMA.split(line);
      double y = Double.parseDouble(parts[0]);
      String[] tok = COMMA.split(parts[1]);
      double[] x = new double[tok.length];
      for (int i = 0; i < tok.length; ++i) {
        x[i] = Double.parseDouble(tok[i]);
      }
      return new LabeledPoint(y, Vectors.dense(x));
    }
  }
  
  public static void main(String[] args) {
  
    // Load and parse the data file.
    // Cache the data since we will use it again to compute training error.
    String datapath = "data/pat_proc_larger.csv";
    
    JavaSparkContext sc = new JavaSparkContext("local[1]", "AnalyzeAnomaly",
      "$SPARK_HOME", new String[]{"target/analyze-anomaly-1.0.jar"});
    
    JavaRDD<String> data = sc.textFile(datapath);
    JavaRDD<LabeledPoint> points = lines.map(new ParsePoint()).cache();
  }
}