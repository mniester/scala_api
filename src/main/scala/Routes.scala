import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.PathMatcher


import Strings.JwtCoder
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
  implicit val intQueryFormat = jsonFormat2(IntQuery)
  implicit val delDataFormat = jsonFormat3(DelData)
  implicit val fullProjectQueryFormat = jsonFormat7(FullProjectQuery)
  implicit val fullProjectModelFormat = jsonFormat6(FullProjectModel)
  //implicit val fullProjectQueryResponse = jsonFormat1(FullProjectQueryResponse)
}

// (get & pathPrefix("projectslist") & path("searchedPage" / Segment / "names" / Segment / "moment" / Segment / "since" / Segment / "deleted" / Segment/ "sortingFactor"/ Segment / "sortingAsc" / Segment))

trait checkQueryArguments extends checkISOTimeFormat{

  val sortingFactors = List("create", "update")

  def checkQueryArguments (query: FullProjectQuery): Option[ResponseMessage] = {
    if (!checkISOTimeFormat(query.moment)) {
      Some(ResponseMessage(StatusCodes.BadRequest.intValue, "Moment is not properly formatted - use ISO 8601"))
    } else if (sortingFactors.contains(query.sortingFactor)) {
      Some(ResponseMessage(StatusCodes.BadRequest.intValue, s"sorting Factor valid arguments: ${sortingFactors.toString.drop(5).dropRight(1)}"))
    } else {
      Some(ResponseMessage(StatusCodes.OK.intValue,  "Data Accepted"))
    }
  }
}

object Routes extends SprayJsonSupport with JsonProtocols with checkQueryArguments {
  
  val taskRoute = "task"
  val projectRoute = "project"
  val userRoute = "user"
  val fullProjects = "projectslist"
  
  val badRequest = ResponseMessage(400, "Bad request").toJson.toString
  val jwtNotProper = ResponseMessage(StatusCodes.BadRequest.intValue, "JWT is not proper").toJson.toString
  val jwtNotProperResponse = complete(HttpResponse(StatusCodes.BadRequest, entity = HttpEntity(ContentTypes.`application/json`, jwtNotProper)))
  val forbidden = ResponseMessage(StatusCodes.Forbidden.intValue, "You do not have permission").toJson.toString
  val forbiddenResponse = complete(HttpResponse(StatusCodes.Forbidden, entity = HttpEntity(ContentTypes.`application/json`, forbidden)))
  val notFound = ResponseMessage(StatusCodes.NotFound.intValue, "User not found").toJson.toString
  val deleted = ResponseMessage(StatusCodes.OK.intValue, "Data was deleted").toJson.toString
  
  val db = SQLite
  db.setup()
  
  val testRoute =
     {
      (get & pathPrefix("test") & pathSuffix(Segment)) {jwt => complete(jwt)}
    }
  
  def getData(route: String) = {
    
    val DBMethod = route match {
      case `userRoute` => db.getUserByKey(_)
      case `taskRoute` => db.getTaskByKey(_)
      case `projectRoute` => db.getProjectByKey(_)
    }

    (get & pathPrefix(route) & pathSuffix(Segment)) 
      {token => JwtCoder.decodeInput(token).getOrElse(null) match {
        case null => jwtNotProperResponse
        case json => val query = json.parseJson.convertTo[IntQuery]; db.checkUuid(query.uuid) match {
          case false => forbiddenResponse
          case true => DBMethod(query.number).getOrElse(null) match {
            case user: UserModel => complete(HttpResponse(status = StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, user.toJson.toString)))
            case task: TaskModel => complete(HttpResponse(status = StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, task.toJson.toString)))
            case project: ProjectModel => complete(HttpResponse(status = StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, project.toJson.toString)))
            case null => complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity(ContentTypes.`application/json`, notFound)))
            }
          }
        }
      }
    }
  
  def delData(route: String) = {

    val DBMethod = route match {
      case `taskRoute` => db.delTask(_)
      case `projectRoute` => db.delProject(_)
    }

    (delete & pathPrefix(route) & pathSuffix(Segment)) 
      {token => JwtCoder.decodeInput(token).getOrElse(null) match {
        case null => jwtNotProperResponse
        case json => DBMethod(json.parseJson.convertTo[DelData]) match {
          case true => complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, deleted)))
          case false => complete(HttpResponse(StatusCodes.Forbidden, entity = HttpEntity(ContentTypes.`application/json`, forbidden)))
        }
      }
    }
  }
 
  val userGet = getData(userRoute)

  val userPost = {
    (post & pathPrefix(userRoute) & pathSuffix(Segment))
      {token => JwtCoder.decodeInput(token).getOrElse(null) match {
        case null => jwtNotProperResponse
        case json => db.addUser(json.parseJson.convertTo[UserModel]).getOrElse(null) match {
          case null => complete(HttpResponse(StatusCodes.Created, entity = HttpEntity(ContentTypes.`application/json`, json)))
          case userModel: UserModel => complete(HttpResponse(StatusCodes.Accepted, entity = HttpEntity(ContentTypes.`application/json`, userModel.toJson.toString)))
        }
      }
    }
  }

  val userDelete = {
    (delete & pathPrefix(userRoute) & pathSuffix(Neutral)) {
       complete(HttpResponse(StatusCodes.MethodNotAllowed, entity = HttpEntity(ContentTypes.`application/json`, ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "This method is not allowed - please contact admin").toJson.toString())))
      }
    }
  
  val userPut = {
    (put & pathPrefix(userRoute) & pathSuffix(Neutral)) {
       complete(HttpResponse(StatusCodes.MethodNotAllowed, entity = HttpEntity(ContentTypes.`application/json`, ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "This method is not allowed - please contact admin").toJson.toString())))
      }
    }

  val taskGet = getData(taskRoute)
  
  val taskPost = {
    (post & pathPrefix(taskRoute) & pathSuffix(Segment))
      {token => JwtCoder.decodeInput(token).getOrElse(null) match {
        case null => jwtNotProperResponse
        case json => db.addTask(json.parseJson.convertTo[TaskModel]).getOrElse(null) match {
          case null => complete(HttpResponse(StatusCodes.Created, entity = HttpEntity(ContentTypes.`application/json`, json)))
          case task: TaskModel => complete(HttpResponse(StatusCodes.Accepted, entity = HttpEntity(ContentTypes.`application/json`, task.toJson.toString)))
        }
      }
    }
  }
  
  val taskDelete = delData(taskRoute)

  val taskPut = {
    (put & pathPrefix(taskRoute) & pathSuffix(Segment)) 
      {token => JwtCoder.decodeInput(token).getOrElse(null) match {
        case null => jwtNotProperResponse
        case json => db.modifyTask(json.parseJson.convertTo[TaskModel]).getOrElse(null) match {
          case null => complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, json)))
          case task: TaskModel => complete(HttpResponse(StatusCodes.Accepted, entity = HttpEntity(ContentTypes.`application/json`, task.toJson.toString)))
        }
      }
    }
  }

  val projectGet = getData(projectRoute)
  
  val projectPost = {
    (post & pathPrefix(projectRoute) & pathSuffix(Segment))
      {token => JwtCoder.decodeInput(token).getOrElse(null) match {
        case null => jwtNotProperResponse
        case json => db.addProject(json.parseJson.convertTo[ProjectModel]).getOrElse(null) match {
          case null => complete(HttpResponse(StatusCodes.Created, entity = HttpEntity(ContentTypes.`application/json`, json)))
          case project: ProjectModel => complete(HttpResponse(StatusCodes.Accepted, entity = HttpEntity(ContentTypes.`application/json`, project.toJson.toString)))
        }
      }
    }
  }
  
val projectDelete = delData(projectRoute)
  
val projectPut = {
  (put & pathPrefix(projectRoute) & pathSuffix(Neutral)) {
      complete(HttpResponse(StatusCodes.MethodNotAllowed, entity = HttpEntity(ContentTypes.`application/json`, ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "This method is not allowed - please contact admin").toJson.toString())))
  }
}

val projectsListGet = {
  (get & pathPrefix("projectslist") & path(Segment))
    {token => JwtCoder.decodeInput(token).getOrElse(null) match {
      case null => jwtNotProperResponse
      case json => checkQueryArguments(json.parseJson.convertTo[FullProjectQuery]).getOrElse(null) match {
        case null => complete(HttpResponse(StatusCodes.MethodNotAllowed, entity = HttpEntity(ContentTypes.`application/json`, badRequest)))
        case response: ResponseMessage => complete(HttpResponse(StatusCodes.MethodNotAllowed, entity = HttpEntity(ContentTypes.`application/json`, response.toJson.toString)))
      }
    }
  }
}
  // def projectsList() = {
  //   (get & pathPrefix("projectslist") & path(Segment))
  //     {
  //       (searchedPage, names, moment, since, deleted, sortingFactor, sortingAsc) => val data = (searchedPage, names, moment, since, deleted, sortingFactor, sortingAsc)
  //       val response = checkQueryArguments(data)
  //       response match {
  //         case ResponseMessage(200, _) => complete(HttpResponse(response.code, entity = HttpEntity(ContentTypes.`application/json`, 
  //                                                   serializeListOfProjects(db.getListOfProjects(searchedPage = data._1.toInt, 
  //                                                                       listOfNames = data._2.split(",").toList,
  //                                                                       moment = data._3,
  //                                                                       since = data._4.toBoolean,
  //                                                                       deleted = data._5.toBoolean,
  //                                                                       sortingFactor = data._6,
  //                                                                       sortingAsc = data._7.toBoolean)))))
  //         case ResponseMessage(_ , _) => complete(HttpResponse(response.code, entity = HttpEntity(ContentTypes.`application/json`, response.toJson.toString)))
  //       }
  //     } 
  // }
  
  val allRoutes = concat(testRoute, 
                        userGet, userPost, userDelete, userPut, 
                        taskGet, taskPost, taskDelete, taskPut, 
                        projectGet, projectPost, projectDelete, projectPut, projectsListGet)
}
