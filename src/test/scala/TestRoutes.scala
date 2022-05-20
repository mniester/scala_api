//import akka.event.NoLogging
import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.Unmarshal
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json._
import scala.concurrent.Await
import DefaultJsonProtocol._

import Settings._
import Routes._
import Factories._
import Strings.JWTCoder
import DBs.{SQLite}
import DataModels.UserModel

class RoutesTests extends AsyncFlatSpec with Matchers with ScalatestRouteTest {

  val db = SQLite
  val test = "aaa"
  val user = UserFactory(key= 1, uuid = "1", name = "a").get
  val codedUser = JWTCoder.encode(user.toJson.toString())
  val project = ProjectFactory(key = 1, name = "Test", user = 1, startTime = "2000-01-01T00:01:01").get;
  val codedProject = JWTCoder.encode(project.toJson.toString())
  val task1 = TaskFactory(name = "task1",
                          user = 1,
                          startTime = "2000-02-01T00:01:01",
                          endTime = "2001-02-01T00:01:01",
                          project = 1,
                          volume = 1, 
                          comment = "abc").get
  val task2 = TaskFactory(name = "task2",
                          user = 2,
                          startTime = "2000-02-01T00:01:01",
                          endTime = "2001-02-01T00:01:01",
                          project = 1,
                          volume = 1, 
                          comment = "abc").get
  val codedTask1 = JWTCoder.encode(task1.toJson.toString())
  val codedTask2 = JWTCoder.encode(task2.toJson.toString())
  db.setup()
  db.reset()

  "Test" should "return response code 200" in {
    Get(s"http://127.0.0.1:8080/test/${test}") ~> testRoute ~> check {
      response.status shouldBe OK
      contentType shouldBe `text/plain(UTF-8)`
      responseAs[String] shouldBe test
      }
  }

  "User" should "always return a JSON and proper HTTP Code" in {
    
    Post(s"http://127.0.0.1:8080/user/${codedUser}") ~> userPost ~> check {
      response.status shouldBe Created
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[UserModel], Settings.dbWaitingDuration)  shouldBe user
      }
    
    Get(s"http://127.0.0.1:8080/user/${user.key.toString}") ~> userGet ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[UserModel], Settings.dbWaitingDuration)  shouldBe user
      }
    
    Post(s"http://127.0.0.1:8080/user/${codedUser}") ~> userPost ~> check {
      response.status shouldBe Accepted
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[UserModel], Settings.dbWaitingDuration)  shouldBe user
      }

    // Get(s"http://127.0.0.1:8080/user/${(user.key + 1).toString}") ~> userGet ~> check {
    //   response.status shouldBe NotFound 
    //   contentType shouldBe `application/json`
    //   }
    
    // Post(s"http://127.0.0.1:8080/project/${codedProject}") ~> projectPost ~> check {
    //   response.status shouldBe Created
    //   contentType shouldBe `application/json`
    //   response.entity.toString().parseJson shouldBe project.toJson
    //   }
    
    // Get(s"http://127.0.0.1:8080/project/${project.key.toString}") ~> projectGet ~> check {
    //   response.status shouldBe OK
    //   contentType shouldBe `application/json`
    //   response.entity.toString().parseJson shouldBe project.toJson
    //   }

    // Get(s"http://127.0.0.1:8080/project/${(user.key + 1).toString}") ~> projectGet ~> check {
    //   response.status shouldBe NotFound 
    //   contentType shouldBe `application/json`
    //   }
    
    // Post(s"http://127.0.0.1:8080/task/${codedTask1}") ~> taskPost ~> check {
    //   response.status shouldBe Created
    //   contentType shouldBe `application/json`
    //   response.entity.toString().parseJson shouldBe project.toJson
    //   }
    
    // Get(s"http://127.0.0.1:8080/project/${task1.key.toString}") ~> taskGet ~> check {
    //   response.status shouldBe OK
    //   contentType shouldBe `application/json`
    //   response.entity.toString().parseJson shouldBe project.toJson
    //   }

    // Get(s"http://127.0.0.1:8080/project/${(task1.key + 1).toString}") ~> taskGet ~> check {
    //   response.status shouldBe NotFound 
    //   contentType shouldBe `application/json`
    //   }
    
    }
}