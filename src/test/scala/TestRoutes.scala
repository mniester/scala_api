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
import Coders.JwtCoder
import DBs.{SQLite}
import DataModels._
import Cmds._

class RoutesTests extends AsyncFlatSpec with Matchers with ScalatestRouteTest {

  val db = SQLite
  val test = "aaa"
  
  val user = UserFactory(key = 1, uuid = "1", name = "RoutesTest").get
  val codedUser = JwtCoder.encode(user.toJson.toString())
  val userQuery = IntQuery(number = user.key, uuid = user.uuid)
  val codedUserQuery = JwtCoder.encode(userQuery.toJson.toString())
  val queryNotFound = IntQuery(number = user.key + 1, uuid = user.uuid)
  val codedQueryNotFound = JwtCoder.encode(queryNotFound.toJson.toString())
  
  val userWrong = UserFactory(uuid = "2", name = "b").get
  val userWrongQuery = IntQuery(number = userWrong.key, uuid = userWrong.uuid)
  val codedUserWrongQuery = JwtCoder.encode(userWrongQuery.toJson.toString())

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
  val delTask1_InvalidUser = DelData(dataKey = task1_InvalidUser.key, userKey = task1_InvalidUser.user, userUuid = user.uuid)
  val codedDelTask1_InvalidUser = JwtCoder.encode(delTask1_InvalidUser.toJson.toString)
  val codedTask1 = JwtCoder.encode(task1.toJson.toString())
  val task = task1
  val codedTask = codedTask1
  val taskQuery = IntQuery(task.key, user.uuid)
  val codedTaskQuery = JwtCoder.encode(taskQuery.toJson.toString)
  
  val taskNotFound = IntQuery(task1.key + 1, user.uuid) 
  val codedTaskNotFound = JwtCoder.encode(taskNotFound.toJson.toString)
  
  val taskWrongQuery = IntQuery(task.key, "Fake-UUID")
  val codedTaskWrongQuery = JwtCoder.encode(taskWrongQuery.toJson.toString)
  
  val task2 = TaskFactory(name = "task2",
                          user = 2,
                          startTime = "2000-02-01T00:01:01",
                          endTime = "2001-02-01T00:01:01",
                          project = 1,
                          volume = 1, 
                          comment = "abc").get
  val codedTask2 = JwtCoder.encode(task2.toJson.toString())
  val codedTaskToPut = JwtCoder.encode(taskToPut.toJson.toString())

  val project = ProjectFactory(key = 1, name = "Test", user = 1, startTime = "2000-01-01T00:01:01").get;
  val codedProject = JwtCoder.encode(project.toJson.toString())
  val projectQuery = IntQuery(project.key, user.uuid)
  val codedProjectQuery = JwtCoder.encode(projectQuery.toJson.toString)
  val projectNotFoundQuery = IntQuery(project.key + 1, user.uuid)
  val codedProjectNotFound = JwtCoder.encode(projectNotFoundQuery.toJson.toString)
  val delProject = DelData(dataKey = project.key, userKey = project.user, userUuid = user.uuid)
  val codedDelProject = JwtCoder.encode(delProject.toJson.toString)

  val projectWrongUuid = IntQuery(project.key, "Fake-UUID")
  val codedProjectWrongQuery = JwtCoder.encode(projectWrongUuid.toJson.toString)
  val projectToPut = ProjectFactory(key = 1, name = "PUT", user = 1, startTime = "2000-01-01T00:01:01").get;
  val codedProjectToPut = JwtCoder.encode(projectToPut.toJson.toString)
  val projectInvalidUser = ProjectFactory(key = 1, name = "Test", user = 11, startTime = "2000-01-01T00:01:01").get
  val codedProjectInvalidUser = JwtCoder.encode(projectInvalidUser.toJson.toString)
  val delProjectInvalidUser = DelData(dataKey = projectInvalidUser.key, userKey = projectInvalidUser.user, userUuid = user.uuid)
  val codedDelProjectInvalidUser = JwtCoder.encode(delProjectInvalidUser.toJson.toString())
  val fullProjectQuery = FullProjectQuery(searchedPage = 1,
                                          listOfNames = List("1"),
                                          moment = "2000-01-01T00:01:01",
                                          since = true,
                                          deleted = false,
                                          sortingFactor = "create",
                                          sortingAsc = true)

  db.setup()
  db.reset()

  "Test" should "return response code 200\n" in {
    Get(s"http://127.0.0.1:8080/test/${test}") ~> testRoute ~> check {
      response.status shouldBe OK
      contentType shouldBe `text/plain(UTF-8)`
      responseAs[String] shouldBe test
      }
  }

  "Parsing Json" should "return proper JSON (with lists)"  in {
    val json = fullProjectQuery.toJson.toString()
    val query =  Await.result(Unmarshal(json).to[FullProjectQuery], Settings.dbWaitingDuration)
    assert (query == fullProjectQuery)
  }

  "User Methods" should "always return a JSON and proper HTTP Code\n" in {
    
    Post(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedUser}") ~> userPost ~> check { //OK
      response.status shouldBe Created
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[UserModel], Settings.dbWaitingDuration)  shouldBe user
      }
    
    Get(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedUserQuery}") ~> userGet ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[UserModel], Settings.dbWaitingDuration)  shouldBe user
      }
    
    Post(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedUser}") ~> userPost ~> check { // Fail - user name is now used 
      response.status shouldBe Accepted
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[UserModel], Settings.dbWaitingDuration)  shouldBe user
      }

    Get(s"http://127.0.0.1:8080/${Settings.userRoute}/${user.key}") ~> userGet ~> check {
      response.status shouldBe BadRequest 
      contentType shouldBe `application/json`
      }
    
    Get(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedQueryNotFound}") ~> userGet ~> check {
      response.status shouldBe NotFound 
      contentType shouldBe `application/json`
      }
    
    Get(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedUserWrongQuery}") ~> userGet ~> check {
      response.status shouldBe Forbidden 
      contentType shouldBe `application/json`
      }
    
    Delete(s"http://127.0.0.1:8080/${Settings.userRoute}/${user.key.toString}") ~> userDelete ~> check {
      response.status shouldBe MethodNotAllowed
      contentType shouldBe `application/json`
      }
    }

  "Task Methods" should "always return a JSON and proper HTTP Code\n" in {
    
    Post(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedTask}") ~> taskPost ~> check { // OK
      response.status shouldBe Created
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[TaskModel], Settings.dbWaitingDuration)  shouldBe task
      }
    
    Get(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedTaskQuery}") ~> taskGet ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[TaskModel], Settings.dbWaitingDuration)  shouldBe task
      }
    
    Post(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedTask}") ~> taskPost ~> check { // fail - Overlapping Task
      response.status shouldBe Accepted
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[TaskModel], Settings.dbWaitingDuration)  shouldBe task
      }

    Get(s"http://127.0.0.1:8080/${Settings.taskRoute}/${task.key}") ~> taskGet ~> check {
      response.status shouldBe BadRequest 
      contentType shouldBe `application/json`
      }
    
    Get(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedTaskNotFound}") ~> taskGet ~> check {
      response.status shouldBe NotFound 
      contentType shouldBe `application/json`
      }
    
    Get(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedTaskWrongQuery}") ~> taskGet ~> check {
      response.status shouldBe Forbidden 
      contentType shouldBe `application/json`
      }
    
    db.reset; Post(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedTask}") ~> taskPost;
    
    Put(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedTaskToPut}") ~> taskPut ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      }
    
    Put(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedTask1_InvalidUser}") ~> taskPut ~> check {
      response.status shouldBe Accepted
      contentType shouldBe `application/json`
    }

    Post(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedUser}") ~> userPost;

    Delete(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedDelTask1}") ~> taskDelete ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      }
    
    db.reset; Post(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedTask}") ~> taskPost;  Post(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedUser}") ~> userPost;
    Delete(s"http://127.0.0.1:8080/${Settings.taskRoute}/${codedDelTask1_InvalidUser}") ~> taskDelete ~> check {
      response.status shouldBe Forbidden
      contentType shouldBe `application/json`
      }
    }

  "Project Methods" should "always return a JSON and proper HTTP Code\n" in {
    
    Post(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProject}") ~> projectPost ~> check { // OK
      response.status shouldBe Created
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[ProjectModel], Settings.dbWaitingDuration)  shouldBe project
      }
    
    Get(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProjectQuery}") ~> projectGet ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[ProjectModel], Settings.dbWaitingDuration)  shouldBe project
      }
    
    Post(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProject}") ~> projectPost ~> check { // fail - Overlapping Task
      response.status shouldBe Accepted
      contentType shouldBe `application/json`
      Await.result(Unmarshal(response).to[ProjectModel], Settings.dbWaitingDuration)  shouldBe project
      }

    Get(s"http://127.0.0.1:8080/${Settings.projectRoute}/${project.key}") ~> projectGet ~> check {
      response.status shouldBe BadRequest 
      contentType shouldBe `application/json`
      }
    
    Get(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProjectNotFound}") ~> projectGet ~> check {
      response.status shouldBe NotFound 
      contentType shouldBe `application/json`
      }
    
    Get(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProjectWrongQuery}") ~> projectGet ~> check {
      response.status shouldBe Forbidden 
      contentType shouldBe `application/json`
      }
    
    db.reset; Post(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProject}") ~> projectPost;
    
    Put(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProjectToPut}") ~> projectPut ~> check {
      response.status shouldBe MethodNotAllowed
      contentType shouldBe `application/json`
      }

    db.reset; Post(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProject}") ~> projectPost; Post(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedUser}") ~> userPost;

    Delete(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedDelProject}") ~> projectDelete ~> check {
      response.status shouldBe OK
      contentType shouldBe `application/json`
      }
    
    db.reset; Post(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProject}") ~> projectPost; Post(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedUser}") ~> userPost;
    
    Delete(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedDelProjectInvalidUser}") ~> projectDelete ~> check {
      response.status shouldBe Forbidden
      contentType shouldBe `application/json`
      }
    }

  "Get Full Project (with tasks)" should "always return a JSON and proper HTTP Code\n" in {
    db.reset;
    for (x <- (1 to 100)) {val project = ProjectFactory(name = x.toString, user = x, startTime = "2001-01-01T00:01:01").get;
                          val task1 = TaskFactory(name = x.toString, 
                                                user = x, 
                                                startTime = "2002-01-01T00:01:01", 
                                                endTime = "2002-02-01T00:01:01", 
                                                project = x, 
                                                volume = -1, 
                                                comment = ("x" * (Settings.maxTaskCommentLength - 150))).get;
                          val task2 = TaskFactory(name = x.toString, 
                                                user = x, 
                                                startTime = "2003-01-01T00:01:01", 
                                                endTime = "2003-02-01T00:01:01", 
                                                project = x, 
                                                volume = -1, 
                                                comment = ("x" * (Settings.maxTaskCommentLength - 150))).get;
                          db.addProject(project); db.addTask(task1); db.addTask(task2)}
    
    val fullProjectQuery = FullProjectQuery(searchedPage = 1,
                                          listOfNames = List("1"),
                                          moment = "2000-01-01T00:01:01",
                                          since = true,
                                          deleted = false,
                                          sortingFactor = "create",
                                          sortingAsc = true)
    val codedFullProjectQuery = JwtCoder.encode(fullProjectQuery.toJson.toString())
    
    Get(s"http://127.0.0.1:8080/${Settings.projectsListsRoute}/${codedFullProjectQuery}") ~> projectsListGet ~> check {
      response.status shouldBe OK 
      contentType shouldBe `application/json`
      val result = Await.result(Unmarshal(response).to[FullProjectQueryResponse], Settings.dbWaitingDuration) // if smth is wrong, here should be error
      assert (result.isInstanceOf[FullProjectQueryResponse] ==  true)
    }

    val fullProjectQueryWrongDate = FullProjectQuery(searchedPage = 1,
                                          listOfNames = List("1"),
                                          moment = "2000-01-01T00:01:01",
                                          since = true,
                                          deleted = false,
                                          sortingFactor = "remove",
                                          sortingAsc = true)
    val codedFullProjectQueryWrongDate= JwtCoder.encode(fullProjectQueryWrongDate.toJson.toString())

    Get(s"http://127.0.0.1:8080/${Settings.projectsListsRoute}/${codedFullProjectQueryWrongDate}") ~> projectsListGet ~> check {
      response.status shouldBe BadRequest 
      contentType shouldBe `application/json`
    }

    val fullProjectQueryWrongSortingFactor = FullProjectQuery(searchedPage = 1,
                                          listOfNames = List("1"),
                                          moment = "1st January 2000",
                                          since = true,
                                          deleted = false,
                                          sortingFactor = "create",
                                          sortingAsc = true)
    val codedFullProjectQueryWrongSortingFactor = JwtCoder.encode(fullProjectQueryWrongDate.toJson.toString())

    Get(s"http://127.0.0.1:8080/${Settings.projectsListsRoute}/${codedFullProjectQueryWrongSortingFactor}") ~> projectsListGet ~> check {
      response.status shouldBe BadRequest 
      contentType shouldBe `application/json`
    }
  }

"All responses for wrong requests" should " return Json with proper string" in {

  db.reset;

  val userNameTooLong = UserModel(key = 1, uuid = "1", name = "Xy" * Settings.maxUserNameLength)
  val codedUserNameTooLong = JwtCoder.encode(userNameTooLong.toJson.toString)

  Post(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedUserNameTooLong}") ~> userPost ~> check {
      response.status shouldBe PayloadTooLarge
      contentType shouldBe `application/json`
    }

  val userNameTooShort = UserModel(key = 1, uuid = "1", name = "")
  val codedUserNameTooShort = JwtCoder.encode(userNameTooShort.toJson.toString)

  Post(s"http://127.0.0.1:8080/${Settings.userRoute}/${codedUserNameTooShort}") ~> userPost ~> check {
      response.status shouldBe BadRequest
      contentType shouldBe `application/json`
    }

  val projectNameTooLong = ProjectModel(key = 1, name = "Xy" * Settings.maxProjectNameLength, user = 1, startTime = "2001-01-01T00:01:01")
  val codedProjectNameTooLong = JwtCoder.encode(projectNameTooLong.toJson.toString)


  Post(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProjectNameTooLong}") ~> projectPost ~> check {
      response.status shouldBe PayloadTooLarge
      contentType shouldBe `application/json`
    }

  val projectNameTooShort = ProjectModel(key = 1, name = "", user = 1, startTime = "2001-01-01T00:01:01")
  val codedProjectNameTooShort = JwtCoder.encode(projectNameTooShort.toJson.toString)
  
  Post(s"http://127.0.0.1:8080/${Settings.projectRoute}/${codedProjectNameTooShort}") ~> projectPost ~> check {
      response.status shouldBe BadRequest
      contentType shouldBe `application/json`
    }
    
  val taskNameTooLong = TaskModel(key = 1, 
                          name = "Xy" * Settings.maxTaskNameLength, 
                          user = 1, 
                          startTime = "2000-01-01T00:01:01", 
                          endTime = "2000-02-01T00:01:01",
                          deleteTime = "",
                          project = 1, 
                          volume = -1, 
                          comment = "Test")
  val encodedTaskNameTooLong = JwtCoder.encode(taskNameTooLong.toJson.toString)


  Post(s"http://127.0.0.1:8080/${Settings.taskRoute}/${encodedTaskNameTooLong}") ~> taskPost ~> check {
      response.status shouldBe PayloadTooLarge
      contentType shouldBe `application/json`
    }

  val taskNameTooShort = TaskModel(key = 1, 
                          name = "", 
                          user = 1, 
                          startTime = "2000-01-01T00:01:01", 
                          endTime = "2000-02-01T00:01:01",
                          deleteTime = "",
                          project = 1, 
                          volume = -1, 
                          comment = "Test")
  val encodedTaskNameTooShort = JwtCoder.encode(taskNameTooShort.toJson.toString)
  
  Post(s"http://127.0.0.1:8080/${Settings.taskRoute}/${encodedTaskNameTooShort}") ~> taskPost ~> check {
      response.status shouldBe BadRequest
      contentType shouldBe `application/json`
    }
  
  val taskCommentTooLong = TaskModel(key = 1, 
                          name = "Test", 
                          user = 1, 
                          startTime = "2000-01-01T00:01:01", 
                          endTime = "2000-02-01T00:01:01",
                          deleteTime = "", 
                          project = 1, 
                          volume = -1, 
                          comment = "Xy" * Settings.maxTaskCommentLength)
  val encodedTaskCommentTooLong = JwtCoder.encode(taskCommentTooLong.toJson.toString)
  
  Post(s"http://127.0.0.1:8080/${Settings.taskRoute}/${encodedTaskCommentTooLong}") ~> taskPost ~> check {
      response.status shouldBe PayloadTooLarge
      contentType shouldBe `application/json`
    }

  val taskNoIsoDatetime = TaskModel(key = 1, 
                          name = "Test", 
                          user = 1, 
                          startTime = "1st January 2000", 
                          endTime = "2000-02-01T00:01:01",
                          deleteTime = "", 
                          project = 1, 
                          volume = -1, 
                          comment = "Test")
  
  val encodedTaskNoIsoDatetime = JwtCoder.encode(taskNoIsoDatetime.toJson.toString)
  
  Post(s"http://127.0.0.1:8080/${Settings.taskRoute}/${encodedTaskNoIsoDatetime}") ~> taskPost ~> check {
      response.status shouldBe BadRequest
      contentType shouldBe `application/json`
    }

  val taskDatesInWrongOrder = TaskModel(key = 1, 
                          name = "", 
                          user = 1, 
                          startTime = "2000-02-01T00:01:01", 
                          endTime = "2000-01-01T00:01:01",
                          deleteTime = "", 
                          project = 1, 
                          volume = -1, 
                          comment = "Test")
    
  val encodedTaskDatesInWrongOrder = JwtCoder.encode(taskDatesInWrongOrder.toJson.toString)

  Post(s"http://127.0.0.1:8080/${Settings.taskRoute}/${encodedTaskDatesInWrongOrder}") ~> taskPost ~> check {
      response.status shouldBe BadRequest
      contentType shouldBe `application/json`
    }
  }
}