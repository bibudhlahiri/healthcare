/*import org.apache.hadoop.hive.ql.exec.Description;
import org.apache.hadoop.hive.ql.exec.UDF;
import org.apache.hadoop.io.Text;*/
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.text.ParseException;

/*class FindAge extends UDF {

    public Text evaluate(Text ageOnYear, Text birthDate) {
    if (birthDate == null) return null;
    return new Text("Hello " + input.toString());
  }
}*/

class FindAge {

    public Long evaluate(String ageOnYear, String birthDate) {
     if (birthDate == null) return null;
     DateFormat formatter = new SimpleDateFormat("yyyyMMdd");
     String startOfDestYear = ageOnYear + "0101";
     long diffInDays = 0;
     int diffInYears = 0;
     try {
       java.util.Date ageAsOn = formatter.parse(startOfDestYear);
       java.util.Date dateOfBirth = formatter.parse(birthDate);
       long MILLIS_PER_DAY = 1000*60*60*24;
       long msDiff = ageAsOn.getTime() - dateOfBirth.getTime();
       diffInDays = Math.round(msDiff/((double)MILLIS_PER_DAY));
       diffInYears = (int)(((double)diffInDays)/((double)365));
     }
     catch(ParseException pe)
     {
       pe.printStackTrace();
     }
     return new Long(diffInYears);
  }

   public static void main(String[] args) {
        FindAge fa = new FindAge();
        fa.evaluate("2010", "19230501");
  }
}
