import akka.event.NoLogging
import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.scaladsl.Flow
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import Settings._
import Routes._

class ServiceSpec extends AsyncFlatSpec with Matchers with ScalatestRouteTest {
  // Get(s"/ip/${ip2Info.query}") ~> routes ~> check {
  //     status shouldBe OK
  //     contentType shouldBe `application/json`
  //     responseAs[IpInfo] shouldBe ip2Info
  //   }
  val testUser = "aaa"

  Get(s"user${testUser}") ~> userGet ~> check {
    status shouldBe OK
  }
}