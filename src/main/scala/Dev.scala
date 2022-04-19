import java.time._
import java.time.temporal.ChronoUnit

object Dev extends App {
  //implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  //println(Pencilcase.isEarlier("2000-02-01T00:01:01", "2000-02-01T00:01:01"))
  val x = LocalDateTime.parse("2000-02-01T00:01:01")
  val y = LocalDateTime.parse("2001-02-01T00:01:01")
  val z = ChronoUnit.SECONDS.between(x, y)
  val c = Duration.between(x, y).toSeconds()
  println(c)
}