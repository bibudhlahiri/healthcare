import java.io.BufferedReader;
import java.io.FileReader;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.ArrayList;
import java.util.StringTokenizer;

class SamplePatients {
    public static void main(String[] args) {
      try
      { 
        BufferedReader in = new BufferedReader(new FileReader("/Users/blahiri/healthcare/data/cloudera_challenge/REVIEW.TXT"));
        List<Integer> reviewedPatients = new ArrayList<Integer>();
        while (in.ready() 
         //&& loopc <= 20
          ) { 
           String line = in.readLine();
           reviewedPatients.add(Integer.parseInt(line));
       }
       System.out.println("size of reviewedPatients = " + reviewedPatients.size());
       //If patient is reviewed, add it. Otherwise, add it with a sampling probability.
       in = new BufferedReader(new FileReader("/Users/blahiri/healthcare/data/cloudera_challenge/patients.csv"));
       PrintWriter out = new PrintWriter(new FileWriter("/Users/blahiri/healthcare/data/cloudera_challenge/sampled_patients.csv"));
       while (in.ready() 
         //&& loopc <= 20
          ) { 
           String line = in.readLine();
           StringTokenizer st = new StringTokenizer(line, ",");
           Integer patientId = Integer.parseInt(st.nextToken());
           if (reviewedPatients.contains(patientId))
           {
             out.println(line + ", 1");
           }
           else 
           {
             double r = Math.random();
             if (r <= 0.001)
             {
               out.println(line + ", 0");
             }
           }
       }
       out.close();
      }
       catch(Exception e)
      {
        e.printStackTrace();
      }
    }
  }
