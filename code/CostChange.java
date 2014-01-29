import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class CostChange {
    /*To run logistic regression, from /Users/blahiri/mahout/trunk, set export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_25.jdk/Contents/Home and 
      issued bin/mahout org.apache.mahout.classifier.sgd.TrainLogistic --passes 1 --rate 1 --lambda 0.5 --input ./examples/src/main/resources/donut.csv 
      --features 21 --output donut_example/donut.model --target color --categories 2 --predictors x y xx xy yy a b c --types n n.*/
    public static void main(String[] args) {
      try
      { 
        String command = "/Users/blahiri/mahout/trunk/bin/mahout org.apache.mahout.classifier.sgd.TrainLogistic --passes 1 --rate 1 --lambda 0.5 --input /Users/blahiri/mahout/trunk/examples/src/main/resources/donut.csv --features 21 --output /Users/blahiri/mahout/trunk/donut_example/donut.model --target color --categories 2 --predictors x y xx xy yy a b c --types n n";
        Process p = 
        Runtime.getRuntime().exec(command);
        /*BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));  
        String line = null;  
        while ((line = br.readLine()) != null) {  
          System.out.println(line);  
        }*/
        //This is done to make sure the separate process that runs the command finishes
        int exitCode = p.waitFor();
        System.out.println("Process p returned: " + exitCode);
      }
      catch(IOException io)
      {
        io.printStackTrace();
      }
      catch(InterruptedException ie)
      {
        ie.printStackTrace();
      }
    }
}
