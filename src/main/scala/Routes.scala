import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.PathMatcher


import Strings.JWTCoder
import DataModels._
import DBs.SQLite
import Strings.isStringNumber
import Strings.checkISOTimeFormat
import Strings.isStringBoolean
import com.typesafe.sslconfig.ssl.FakeChainedKeyStore

trait JsonProtocols extends DefaultJsonProtocol {
  implicit val responseMessageFormat = jsonFormat2(ResponseMessage)
  implicit val userFormat = jsonFormat3(UserModel)
  implicit val projectFormat = jsonFormat5(ProjectModel)
  implicit val taskFormat = jsonFormat9(TaskModel)
}

// (get & pathPrefix("projectslist") & path("searchedPage" / Segment / "names" / Segment / "moment" / Segment / "since" / Segment / "deleted" / Segment/ "sortingFactor"/ Segment / "sortingAsc" / Segment))

trait checkUrlArguments extends isStringNumber with isStringBoolean with checkISOTimeFormat{
  def checkUrlArguments (data: (String, String, String, String, String, String, String)): ResponseMessage = {
    if (!isStringNumber(data._1)) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "Only Integers are allowed")
    } else if (!checkISOTimeFormat(data._3)) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "Moment is not properly formatted - use ISO 8601")
    } else if (!isStringBoolean(data._4)) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "since argument can be only boolean - only true and false are allowed")
    } else if (!isStringBoolean(data._5)) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "deleted argument can be only boolean - only true and false are allowed")
    } else if ((data._6 != "create") && (data._6 != "update")) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "sorting Factor valid arguments: create, update")
    } else if (!isStringBoolean(data._7)) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "sortingAsc (Sorting ascending) argument can be only boolean - only true and false are allowed")
    } else {
      ResponseMessage(StatusCodes.OK.intValue,  "Data Accepted")
    }
  }
}

trait projectJsonSerializer extends JsonProtocols {

  def serializeListOfProjects(projects: List[ProjectModelWithTasks]): String = {
    if (projects.length > 0) {"""{ "projects": [""" + (projects.map(project => serializeProject(project))).toString.drop(5).dropRight(1) + "]}"}
    else {ResponseMessage(StatusCodes.NotFound.intValue, "No data was found").toJson.toString}
  }

  def serializeProject(project: ProjectModelWithTasks): String = {
    project.dropTasks().toJson.toString.dropRight(1) + """, "tasks" :[""" + project.tasks.map(task => task.toJson.toString).mkString(",") + "]}"
  }
}

object Routes extends SprayJsonSupport with JsonProtocols with checkUrlArguments with projectJsonSerializer {
  val badRequest = ResponseMessage(400, "Bad request")
  val db = SQLite
  db.setup()
  val testRoute =
     {
      (get & pathPrefix("test") & pathSuffix(Segment)) {jwt => complete(jwt)}
    }
  
  val userGet =
    {
      (get & pathPrefix("user") & pathSuffix(Segment)) 
        {number => isStringNumber(number) match {
          case false => complete(HttpResponse(StatusCodes.BadRequest, entity = HttpEntity(ContentTypes.`application/json`, new ResponseMessage(StatusCodes.BadRequest.intValue, "Only Integers are allowed").toJson.toString)))
          case true =>  db.getUserByKey(number.toInt).getOrElse(null) match {
            case user: UserModel => complete(HttpResponse(status = StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, user.toJson.toString)))
            case null => complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity(ContentTypes.`application/json`, new ResponseMessage(StatusCodes.NotFound.intValue, "User not found").toJson.toString)))
          }
        }
      }
    }
  
  // val userPost =
  //   path("user") {
  //     post {
  //       parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
  //     }
  //   }

  val userPost = {
    (post & pathPrefix("user") & pathSuffix(Segment))
      {token => JWTCoder.decodeInput(token).getOrElse(null) match {
        case null => complete(HttpResponse(StatusCodes.BadRequest, entity = HttpEntity(ContentTypes.`application/json`, new ResponseMessage(StatusCodes.BadRequest.intValue, "JWT is not proper").toJson.toString)))
        case json => db.addUser(json.parseJson.convertTo[UserModel]).getOrElse(null) match {
          case null => complete(HttpResponse(StatusCodes.Created, entity = HttpEntity(ContentTypes.`application/json`, json)))
          case userModel: UserModel => complete(HttpResponse(StatusCodes.Accepted, entity = HttpEntity(ContentTypes.`application/json`, userModel.toJson.toString)))
        }
      }
      }
    }

  val userDelete =
    path("user") {
      delete {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }
  
  val taskGet =
    path("task") {
      get {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }
  
  val taskPost =
    path("task") {
      post {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }
  
  val taskDelete =
    path("task") {
      delete {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }

  val taskPut =
    path("task") {
      put {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }

  val projectGet =
    path("project") {
      get {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }
  
  val projectPost =
    path("project") {
      post {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }
  
  val projectDelete =
    path("project") {
      delete {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }

  val projectPut =
    path("project") {
      put {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }

  def projectsList() = {
    (get & pathPrefix("projectslist") & path("searchedPage" / Segment / "names" / Segment / "moment" / Segment / "since" / Segment / "deleted" / Segment/ "sortingFactor"/ Segment / "sortingAsc" / Segment))
      {
        (searchedPage, names, moment, since, deleted, sortingFactor, sortingAsc) => val data = (searchedPage, names, moment, since, deleted, sortingFactor, sortingAsc)
        val response = checkUrlArguments(data)
        response match {
          case ResponseMessage(200, _) => complete(HttpResponse(response.code, entity = HttpEntity(ContentTypes.`application/json`, 
                                                    serializeListOfProjects(db.getListOfProjects(searchedPage = data._1.toInt, 
                                                                        listOfNames = data._2.split(",").toList,
                                                                        moment = data._3,
                                                                        since = data._4.toBoolean,
                                                                        deleted = data._5.toBoolean,
                                                                        sortingFactor = data._6,
                                                                        sortingAsc = data._7.toBoolean)))))
          case ResponseMessage(_ , _) => complete(HttpResponse(response.code, entity = HttpEntity(ContentTypes.`application/json`, response.toJson.toString)))
        }
      } 
  }
  
  val allRoutes = concat(testRoute, userGet, userPost, userDelete, taskGet, taskPost, taskDelete, taskPut, projectGet, projectPost, projectDelete, projectPut, projectsList)
}
