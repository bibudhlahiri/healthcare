import java.io.*;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

class ConvertXmlToCsv {
    public static void main(String[] args) {
      try
      { 
        BufferedReader in = new BufferedReader(new FileReader("/Users/blahiri/healthcare/data/cloudera_challenge/PNTSDUMP.XML")); 
        PrintWriter out = new PrintWriter(new FileWriter("/Users/blahiri/healthcare/data/cloudera_challenge/patients.csv")); 
        String outputLine = "";
        int loopc = 0;
        DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");

        while (in.ready() 
         //&& loopc <= 20
          ) { 
           String line = in.readLine(); 
           if (line.indexOf("<rows>") != -1)
           {
             outputLine = "";
           }
           int startTagEndsAt = line.indexOf(">");
           int endTagStartsAt = line.indexOf("</field>");
           if (line.indexOf("id") != -1)
           {
             //System.out.println("line with id, endTagStartsAt = " + endTagStartsAt + ", substring = " + line.substring(startTagEndsAt + 1, endTagStartsAt));
             outputLine = outputLine + line.substring(startTagEndsAt + 1, endTagStartsAt);
           }
           if (line.indexOf("age") != -1)
           {
             outputLine = outputLine + ", " + line.substring(startTagEndsAt + 1, endTagStartsAt);
           }
           if (line.indexOf("gndr") != -1)
           {
             outputLine = outputLine + ", " + line.substring(startTagEndsAt + 1, endTagStartsAt);
           }
           if (line.indexOf("inc") != -1)
           {
             outputLine = outputLine + ", " + line.substring(startTagEndsAt + 1, endTagStartsAt);
           }
           if (line.indexOf("</rows>") != -1)
           {
             outputLine = outputLine.replaceAll("&lt;", "<");
             out.println(outputLine);
             loopc++;
             if (loopc % 1000000 == 0)
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
