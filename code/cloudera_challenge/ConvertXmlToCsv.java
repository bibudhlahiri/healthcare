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
        int field_number_to_read = 0;

        while (in.ready() 
         //&& loopc <= 20
          ) { 
           String line = in.readLine(); 
           if (line.indexOf("<rows>") != -1)
           {
             outputLine = "";
             field_number_to_read = 1;
             continue;
           }
           int startTagEndsAt = line.indexOf(">");
           int endTagStartsAt = line.indexOf("</field>");
           if (field_number_to_read == 1)
           {
            if (line.indexOf("id") != -1)
            {
             //System.out.println("line with id, endTagStartsAt = " + endTagStartsAt + ", substring = " + line.substring(startTagEndsAt + 1, endTagStartsAt));
             outputLine = outputLine + line.substring(startTagEndsAt + 1, endTagStartsAt);
            }
            field_number_to_read = 2;
            continue;
           }
           if (field_number_to_read == 2)
           {
            if (line.indexOf("age") != -1)
            {
             outputLine = outputLine + ", " + line.substring(startTagEndsAt + 1, endTagStartsAt);
            }
            else
            {
              outputLine = outputLine + ", ";
            }
            field_number_to_read = 3;
            continue;
           }

           if (field_number_to_read == 3)
           {
            if (line.indexOf("gndr") != -1)
            {
             outputLine = outputLine + ", " + line.substring(startTagEndsAt + 1, endTagStartsAt);
            }
            else
            {
              outputLine = outputLine + ", ";
            }
            field_number_to_read = 4;
            continue;
           }

           if (field_number_to_read == 4)
           {
            if (line.indexOf("inc") != -1)
            {
             outputLine = outputLine + ", " + line.substring(startTagEndsAt + 1, endTagStartsAt);
            }
            else
            {
              outputLine = outputLine + ", ";
            }
            field_number_to_read = 0;
            continue;
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
