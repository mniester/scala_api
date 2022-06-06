import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.PathMatcher

import Coders.JwtCoder
import Cmds._
import DataModels._
import DBs.SQLite
import Validators._
import Responses._
import MessageModels._

trait JsonProtocols extends DefaultJsonProtocol with ApiMessagesJsonProtocol {
  implicit val userFormat = jsonFormat3(UserModel)
  implicit val projectFormat = jsonFormat5(ProjectModel)
  implicit val taskFormat = jsonFormat9(TaskModel)
  implicit val intQueryFormat = jsonFormat2(IntQuery)
  implicit val delDataFormat = jsonFormat3(DelData)
  implicit val fullProjectQueryFormat = jsonFormat7(FullProjectQuery)
  implicit val fullProjectModelFormat = jsonFormat6(FullProjectModel)
  implicit val fullProjectQueryResponse = jsonFormat1(FullProjectQueryResponse)
}

object Routes extends SprayJsonSupport with JsonProtocols with FullProjectQueryValidation  with InputValidation{
  
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
      {token => JwtCoder.decodeInput(token).getOrElse(null); JwtCoder.decodeInput(token).getOrElse(null) match {
        case null => jwtNotProperResponse
        case json => val dataModel = parseAndConvertToModel(json, routeAndModel).getOrElse(null); dataModel match {
          case null => complete(HttpResponse(StatusCodes.Accepted, entity = HttpEntity(ContentTypes.`application/json`, json)))
          case input: DataModel => validateInput(dataModel).getOrElse(null) match {
            case response: ResponseMessage => complete(HttpResponse(response.code, entity = HttpEntity(ContentTypes.`application/json`, response.toJson.toString)))
            case null => db.addData(dataModel).getOrElse(null) match {
              case null => complete(HttpResponse(StatusCodes.Created, entity = HttpEntity(ContentTypes.`application/json`, json)))
              case response: DataModel => complete(HttpResponse(StatusCodes.Accepted, entity = HttpEntity(ContentTypes.`application/json`, convertToJsonString(response).getOrElse("Your Data was not accepted"))))
            }
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
        case json: String => val query = json.parseJson.convertTo[FullProjectQuery]; validateFullProjectQuery(query).getOrElse(null) match {
          case response: ResponseMessage => complete(HttpResponse(response.code, entity = HttpEntity(ContentTypes.`application/json`, response.toJson.toString)))
          case null => complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, db.getListOfProjects(query).toJson.toString)))
        }
      }
    }
  }
} 
  val allRoutes = concat(testRoute, 
                        userGet, userPost, userDelete, userPut, 
                        taskGet, taskPost, taskDelete, taskPut, 
                        projectGet, projectPost, projectDelete, projectPut, projectsListGet)
}
