import java.io.BufferedReader;
import java.io.FileReader;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.ArrayList;

class SampleProcedures {
    public static void main(String[] args) {
      try
      { 
        //Take only those patients who have been sampled. That will include all anomalous patients.
        BufferedReader in = new BufferedReader(new FileReader("/Users/blahiri/healthcare/data/cloudera_challenge/sampled_patients.csv"));
        List<Integer> sampledPatients = new ArrayList<Integer>();
        int loopc = 0;
        DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");

        while (in.ready()) 
        { 
           String line = in.readLine();
           sampledPatients.add(Integer.parseInt((line.split(","))[0]));
        }
       System.out.println("size of sampledPatients = " + sampledPatients.size());
       PrintWriter out = new PrintWriter(new FileWriter("/Users/blahiri/healthcare/data/cloudera_challenge/sampled_procedures.csv"));
       for (int i = 1; i <= 12; i++)
       {
         String filename = "/Users/blahiri/healthcare/data/cloudera_challenge/PCDR2011/PCDR11" + ((i < 10)?"0":"") + i + ".csv";
         in = new BufferedReader(new FileReader(filename));
         
         while (in.ready()) 
         { 
           String line = in.readLine();
           Integer patientId = Integer.parseInt((line.split(","))[1]);
           if (sampledPatients.contains(patientId))
           {
             out.println(line);
           }
           loopc++;
           if (loopc % 100000 == 0)
           {
             Date date = new Date();
             System.out.println("loopc = " + loopc + ", time = " + dateFormat.format(date));
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
