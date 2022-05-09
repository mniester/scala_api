//import akka.event.NoLogging
//import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
//import akka.stream.scaladsl.Flow
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import Settings._
import Routes._

class RoutesTests extends AsyncFlatSpec with Matchers with ScalatestRouteTest {

  val test = "aaa"

  "Service" should "respond to single IP query" in {
  Get(s"http://127.0.0.1:8080/test/${test}") ~> testRoute ~> check {
    response.status shouldBe OK
    responseAs[String] shouldBe test
    }
  }
}