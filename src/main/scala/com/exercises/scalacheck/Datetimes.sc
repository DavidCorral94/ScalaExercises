import com.fortysevendeg.scalacheck.datetime.joda.ArbitraryJoda._
import org.joda.time.DateTime
import org.scalacheck.Prop.forAll
import java.time._
import org.joda.time._
import com.fortysevendeg.scalacheck.datetime.jdk8.ArbitraryJdk8._
import com.fortysevendeg.scalacheck.datetime.jdk8.granularity.years
import com.fortysevendeg.scalacheck.datetime.GenDateTime.genDateTimeWithinRange
import org.scalacheck.Prop.forAll

object Datetimes {

  val checkDates = forAll { dt: DateTime =>
    (dt.getDayOfMonth >= 1 && dt.getDayOfMonth <= 31)
  }
  checkDates.check()

  val restrictedDate =
    forAll { zdt: ZonedDateTime =>
      (zdt.getMonth == Month.JANUARY) &&
      (zdt.getDayOfMonth == 1) &&
      (zdt.getHour == 0) &&
      (zdt.getMinute == 0) &&
      (zdt.getSecond == 0) &&
      (zdt.getNano == 0)
    }

  restrictedDate.check()

}
