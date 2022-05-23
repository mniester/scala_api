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
import Strings.JwtCoder
import DBs.{SQLite}
import DataModels._
import DataModels.IntQuery

class RoutesTests extends AsyncFlatSpec with Matchers with ScalatestRouteTest {

  val db = SQLite
  val test = "aaa"
  
  val user = UserFactory(key = 1, uuid = "1", name = "RoutesTest").get
  val codedUser = JwtCoder.encode(user.toJson.toString())
  val userQuery = IntQuery(number = user.key, uuid = user.uuid)
  val codedUserQuery = JwtCoder.encode(userQuery.toJson.toString())
  val queryNotFound = IntQuery(number = user.key + 1, uuid = user.uuid)
  val codedQueryNotFound = JwtCoder.encode(queryNotFound.toJson.toString())
  
  val userFail = UserFactory(uuid = "2", name = "b").get
  val userFailQuery = IntQuery(number = userFail.key, uuid = userFail.uuid)
  val codedUserFailQuery = JwtCoder.encode(userFailQuery.toJson.toString())

  val task1 = TaskFactory(key = 1, 
                          name = "Test", 
                          user = 1, 
                          startTime = "2000-01-01T00:01:01", 
                          endTime = "2000-02-01T00:01:01", 
                          project = 1, 
                          volume = -1, 
                          comment = "Test").get;
  val delTask1 = DelData(dataKey = task1.key, userKey = task1.user, userUuid = user.uuid)
  val codedDelTask1 = JwtCoder.encode(delTask1.toJson.toString()) 
  val taskToPut = TaskFactory(key = 1, 
                          name = "PUT", 
                          user = 1, 
                          startTime = "2000-01-01T00:01:01", 
                          endTime = "2000-02-01T00:01:01", 
                          project = 1, 
                          volume = -1, 
                          comment = "Test").get;
  val task1_InvalidUser = TaskFactory(key = 1, 
                          name = "Test", 
                          user = 11, 
                          startTime = "2000-01-01T00:01:01", 
                          endTime = "2000-02-01T00:01:01", 
                          project = 1, 
                          volume = -1, 
                          comment = "Test").get;
  val codedTask1_InvalidUser = JwtCoder.encode(task1_InvalidUser.toJson.toString)
  val delTask1_InvalidUser = DelData(dataKey = task1_InvalidUser .key, userKey = task1.user, userUuid = user.uuid)
  val codedDelTask1_InvalidUser = JwtCoder.encode(delTask1_InvalidUser.toJson.toString)
  val task2 = TaskFactory(name = "task2",
                          user = 2,
                          startTime = "2000-02-01T00:01:01",
                          endTime = "2001-02-01T00:01:01",
                          project = 1,
                          volume = 1, 
                          comment = "abc").get
  val codedTask1 = JwtCoder.encode(task1.toJson.toString())
  val task = task1
  val codedTask = codedTask1
  val taskQuery = IntQuery(task.key, user.uuid)
  val codedTaskQuery = JwtCoder.encode(taskQuery.toJson.toString)
  val taskNotFound = IntQuery(task2.key, user.uuid) 
  val codedTaskNotFound = JwtCoder.encode(taskNotFound.toJson.toString)
  val taskFailQuery = IntQuery(task.key, "Fake-UUID")
  val codedTaskFailQuery = JwtCoder.encode(taskFailQuery.toJson.toString)
  val codedTask2 = JwtCoder.encode(task2.toJson.toString())
  val codedTaskToPut = JwtCoder.encode(taskToPut.toJson.toString())

  val project = ProjectFactory(key = 1, name = "Test", user = 1, startTime = "2000-01-01T00:01:01").get;
  val codedProject = JwtCoder.encode(project.toJson.toString())
  
  db.setup()
  db.reset()

  "Test" should "return response code 200\n" in {
    Get(s"http://127.0.0.1:8080/test/${test}") ~> testRoute ~> check {
      response.status shouldBe OK
      contentType shouldBe `text/plain(UTF-8)`
      responseAs[String] shouldBe test
      }
  }

  "User Methods" should "always return a JSON and proper HTTP Code\n" in {
    
    Post(s"http://127.0.0.1:8080/user/${codedUser}") ~> userPost ~> check { //OK
      response.status shouldBe Created
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[UserModel], Settings.dbWaitingDuration)  shouldBe user
      }
    
    Get(s"http://127.0.0.1:8080/user/${codedUserQuery}") ~> userGet ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[UserModel], Settings.dbWaitingDuration)  shouldBe user
      }
    
    Post(s"http://127.0.0.1:8080/user/${codedUser}") ~> userPost ~> check { // Fail - user name is now used 
      response.status shouldBe Accepted
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[UserModel], Settings.dbWaitingDuration)  shouldBe user
      }

    Get(s"http://127.0.0.1:8080/user/${user.key}") ~> userGet ~> check {
      response.status shouldBe BadRequest 
      contentType shouldBe `application/json`
      }
    
    Get(s"http://127.0.0.1:8080/user/${codedQueryNotFound}") ~> userGet ~> check {
      response.status shouldBe NotFound 
      contentType shouldBe `application/json`
      }
    
    Get(s"http://127.0.0.1:8080/user/${codedUserFailQuery}") ~> userGet ~> check {
      response.status shouldBe Forbidden 
      contentType shouldBe `application/json`
      }
    
    Delete(s"http://127.0.0.1:8080/user/${user.key.toString}") ~> userDelete ~> check {
      response.status shouldBe MethodNotAllowed
      contentType shouldBe `application/json`
      }
    }

    "Task Methods" should "always return a JSON and proper HTTP Code\n" in {
    
    Post(s"http://127.0.0.1:8080/task/${codedTask}") ~> taskPost ~> check { // OK
      response.status shouldBe Created
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[TaskModel], Settings.dbWaitingDuration)  shouldBe task
      }
    
    Get(s"http://127.0.0.1:8080/task/${codedTaskQuery}") ~> taskGet ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[TaskModel], Settings.dbWaitingDuration)  shouldBe task
      }
    
    Post(s"http://127.0.0.1:8080/task/${codedTask}") ~> taskPost ~> check { // fail - Overlapping Task
      response.status shouldBe Accepted
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[TaskModel], Settings.dbWaitingDuration)  shouldBe task
      }

    Get(s"http://127.0.0.1:8080/task/${task.key}") ~> taskGet ~> check {
      response.status shouldBe BadRequest 
      contentType shouldBe `application/json`
      }
    
    Get(s"http://127.0.0.1:8080/task/${codedTaskNotFound}") ~> taskGet ~> check {
      response.status shouldBe NotFound 
      contentType shouldBe `application/json`
      }
    
    Get(s"http://127.0.0.1:8080/task/${codedTaskFailQuery}") ~> taskGet ~> check {
      response.status shouldBe Forbidden 
      contentType shouldBe `application/json`
      }
    
    db.reset; Post(s"http://127.0.0.1:8080/task/${codedTask}") ~> taskPost;
    
    Put(s"http://127.0.0.1:8080/task/${codedTaskToPut}") ~> taskPut ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      }
    
    Put(s"http://127.0.0.1:8080/task/${codedTask1_InvalidUser}") ~> taskPut ~> check {
      response.status shouldBe Accepted
      contentType shouldBe `application/json`
    }

    Post(s"http://127.0.0.1:8080/user/${codedUser}") ~> userPost;

    Delete(s"http://127.0.0.1:8080/task/${codedDelTask1}") ~> taskDelete ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      }
    
    db.reset; Post(s"http://127.0.0.1:8080/task/${codedTask}") ~> taskPost;
    Delete(s"http://127.0.0.1:8080/task/${codedDelTask1_InvalidUser}") ~> taskDelete ~> check {
      response.status shouldBe Forbidden
      contentType shouldBe `application/json`
      }
    }


    }
    
    
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