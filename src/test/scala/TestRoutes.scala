//import akka.event.NoLogging
import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
//import akka.stream.scaladsl.Flow
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json._
import DefaultJsonProtocol._

import Settings._
import Routes._
import Factories._
import Strings.JWTCoder
import DBs.{SQLite}

class RoutesTests extends AsyncFlatSpec with Matchers with ScalatestRouteTest {

  val db = SQLite
  val test = "aaa"
  val user = UserFactory(key= 1, uuid = "1", name = "a").get
  val project = ProjectFactory(key = 1, name = "Test", user = 1, startTime = "2000-01-01T00:01:01").get;
  val task1 = TaskFactory(name = "task1",
                          user = 1,
                          startTime = "2000-02-01T00:01:01",
                          endTime = "2001-02-01T00:01:01",
                          project = 1,
                          volume = 1, 
                          comment = "abc").get
  val task2 = TaskFactory(name = "task2",
                          user = 1,
                          startTime = "2000-02-01T00:01:01",
                          endTime = "2001-02-01T00:01:01",
                          project = 1,
                          volume = 1, 
                          comment = "abc").get
  val userJSON = user.toJson
  db.setup()
  db.reset()
  db.addUser(user)

  "Service" should "respond to single IP query" in {
  
    Get(s"http://127.0.0.1:8080/test/${test}") ~> testRoute ~> check {
      response.status shouldBe OK
      contentType shouldBe `text/plain(UTF-8)`
      responseAs[String] shouldBe test
      }
    
    Get(s"http://127.0.0.1:8080/user/${user.key}") ~> testRoute ~> check {
      response.status shouldBe OK
      contentType shouldBe `text/plain(UTF-8)`
      responseAs[String] shouldBe test
      response.entity.toString().parseJson shouldBe userJSON
      }
    }
}