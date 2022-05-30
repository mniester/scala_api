import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.PathMatcher

import Strings.JwtCoder
import Cmds._
import DataModels._
import DBs.SQLite
import Validators._
import com.typesafe.sslconfig.ssl.FakeChainedKeyStore
import javax.xml.crypto.Data

trait JsonProtocols extends DefaultJsonProtocol {
  implicit val responseMessageFormat = jsonFormat2(ResponseMessage)
  implicit val userFormat = jsonFormat3(UserModel)
  implicit val projectFormat = jsonFormat5(ProjectModel)
  implicit val taskFormat = jsonFormat9(TaskModel)
  implicit val intQueryFormat = jsonFormat2(IntQuery)
  implicit val delDataFormat = jsonFormat3(DelData)
  implicit val fullProjectQueryFormat = jsonFormat7(FullProjectQuery)
  implicit val fullProjectModelFormat = jsonFormat6(FullProjectModel)
  implicit val fullProjectQueryResponse = jsonFormat1(FullProjectQueryResponse)
}

trait CheckQueryArguments extends validateIsoTimeFormat{

  val sortingFactors = List("create", "update")

  def checkQueryArguments (query: FullProjectQuery): Option[ResponseMessage] = {
    if (!validateIsoTimeFormat(query.moment)) {
      Some(ResponseMessage(StatusCodes.BadRequest.intValue, "Moment is not properly formatted - use ISO 8601"))
    } else if (sortingFactors.contains(query.sortingFactor)) {
      Some(ResponseMessage(StatusCodes.BadRequest.intValue, s"sorting Factor valid arguments: ${sortingFactors.toString.drop(5).dropRight(1)}"))
    } else {
      Some(ResponseMessage(StatusCodes.OK.intValue,  "Data Accepted"))
    }
  }
}

object Routes extends SprayJsonSupport with JsonProtocols with CheckQueryArguments {
  
  def parseAndConvertToModel(json: String, routeAndModel: String): Option[DataModel] = {
    try {
      routeAndModel match {
        case `taskRoute` => Some(json.parseJson.convertTo[TaskModel])
        case `userRoute` => Some(json.parseJson.convertTo[UserModel])
        case `projectRoute` => Some(json.parseJson.convertTo[ProjectModel])
        case `projectsList` => Some(json.parseJson.convertTo[FullProjectModel])
        case _ => None
      }
    } catch {
        case _: Throwable => None
      }
  }

  def convertToJsonString(dataModel: DataModel): Option[String] = {
    dataModel match {
      case data: UserModel => Some(data.toJson.toString)
      case data: ProjectModel => Some(data.toJson.toString)
      case data: FullProjectModel => Some(data.toJson.toString)
      case data: TaskModel => Some(data.toJson.toString)
      case _ => None
    }  
  } 

  val taskRoute = Settings.taskRoute
  val projectRoute = Settings.projectRoute
  val userRoute = Settings.userRoute
  val projectsList = Settings.projectsListsRoute
  

  val notDoneYet = ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "Route needs to be done").toJson.toString
  val notDoneYetResponse = complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity(ContentTypes.`application/json`, notDoneYet)))
  val badRequest = ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "Bad request").toJson.toString
  val badRequestResponse = complete(HttpResponse(StatusCodes.MethodNotAllowed.intValue, entity = HttpEntity(ContentTypes.`application/json`, badRequest)))
  val methodNotAllowed = ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "This method is not allowed - please contact admin").toJson.toString()
  val methodNotAllowedResponse = complete(HttpResponse(StatusCodes.MethodNotAllowed, entity = HttpEntity(ContentTypes.`application/json`, methodNotAllowed)))
  val jwtNotProper = ResponseMessage(StatusCodes.BadRequest.intValue, "JWT is not proper").toJson.toString
  val jwtNotProperResponse = complete(HttpResponse(StatusCodes.BadRequest, entity = HttpEntity(ContentTypes.`application/json`, jwtNotProper)))
  val forbidden = ResponseMessage(StatusCodes.Forbidden.intValue, "You do not have permission").toJson.toString
  val forbiddenResponse = complete(HttpResponse(StatusCodes.Forbidden, entity = HttpEntity(ContentTypes.`application/json`, forbidden)))
  val notFound = ResponseMessage(StatusCodes.NotFound.intValue, "User not found").toJson.toString
  val notFoundResponse = complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity(ContentTypes.`application/json`, notFound)))
  val deleted = ResponseMessage(StatusCodes.OK.intValue, "Data was deleted").toJson.toString
  val deletedResponse = complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, deleted)))
  
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
        case json: String => val query = json.parseJson.convertTo[IntQuery]; db.checkUuid(query.uuid) match {
          case false => forbiddenResponse
          case true => DBMethod(query.number).getOrElse(null) match {
            case user: UserModel => complete(HttpResponse(status = StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, user.toJson.toString)))
            case task: TaskModel => complete(HttpResponse(status = StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, task.toJson.toString)))
            case project: ProjectModel => complete(HttpResponse(status = StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, project.toJson.toString)))
            case null => notFoundResponse
            }
          }
        }
      }
    }

  def postData (routeAndModel: String) ={
    (post & pathPrefix(routeAndModel) & pathSuffix(Segment))
      {token => JwtCoder.decodeInput(token).getOrElse(null) match {
        case null => jwtNotProperResponse
        case json => parseAndConvertToModel(json, routeAndModel).getOrElse(null) match {
          case null => complete(HttpResponse(StatusCodes.Accepted, entity = HttpEntity(ContentTypes.`application/json`, json)))
          case dataModel: DataModel => db.addData(dataModel).getOrElse(null) match {
            case null => complete(HttpResponse(StatusCodes.Created, entity = HttpEntity(ContentTypes.`application/json`, json)))
            case dataModel: DataModel => complete(HttpResponse(StatusCodes.Accepted, entity = HttpEntity(ContentTypes.`application/json`, convertToJsonString(dataModel).getOrElse("Data Accepted"))))
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
          case true => deletedResponse
          case false => forbiddenResponse
        }
      }
    }
  }
 
  val userGet = getData(userRoute)

  val userPost = postData(userRoute)

  val userDelete = {
    (delete & pathPrefix(userRoute) & pathSuffix(Neutral)) {
        methodNotAllowedResponse
      }
    }
  
  val userPut = {
    (put & pathPrefix(userRoute) & pathSuffix(Neutral)) {
        methodNotAllowedResponse
      }
    }

  val taskGet = getData(taskRoute)
  
  val taskPost = postData(taskRoute)
  
  val taskDelete = delData(taskRoute)

  val taskPut = {
    (put & pathPrefix(taskRoute) & pathSuffix(Segment)) 
      {token => JwtCoder.decodeInput(token).getOrElse(null) match {
        case null => jwtNotProperResponse
        case json: String => db.modifyTask(json.parseJson.convertTo[TaskModel]).getOrElse(null) match {
          case null => complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, json)))
          case task: TaskModel => complete(HttpResponse(StatusCodes.Accepted, entity = HttpEntity(ContentTypes.`application/json`, task.toJson.toString)))
        }
      }
    }
  }

  val projectGet = getData(projectRoute)
  
  val projectPost = postData(projectRoute)
  
  val projectDelete = delData(projectRoute)
  
  val projectPut = {
  (put & pathPrefix(projectRoute) & pathSuffix(Neutral)) {
      methodNotAllowedResponse
    }
  }

  val projectsListGet = {  
    (get & pathPrefix(projectRoute) & pathSuffix(Segment)) {
      {token =>  JwtCoder.decodeInput(token).getOrElse(null) match {
        case null => jwtNotProperResponse
        case json: String =>  complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, db.getListOfProjects(json.parseJson.convertTo[FullProjectQuery]).toJson.toString))) 
      }
    }
  }
} 
  val allRoutes = concat(testRoute, 
                        userGet, userPost, userDelete, userPut, 
                        taskGet, taskPost, taskDelete, taskPut, 
                        projectGet, projectPost, projectDelete, projectPut, projectsListGet)
}
