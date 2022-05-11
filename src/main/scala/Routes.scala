import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.PathMatcher


import Strings.JWTCoder
import DataModels._
import DBs.SQLite

case class ResponseMessage(code: Int, message: String)

trait JsonProtocols extends DefaultJsonProtocol {
  implicit val responseMessageFormat = jsonFormat2(ResponseMessage)
  implicit val userFormat = jsonFormat3(UserModel)
  implicit val projectFormat = jsonFormat5(ProjectModel)
  implicit val taskFormat = jsonFormat9(TaskModel)
}

object isStringNumber {
  def apply(string: String): Boolean = {
    string.forall(Character.isDigit)
  }
}

object Routes extends JsonProtocols with SprayJsonSupport {
  val db = SQLite
  db.setup()
  val testRoute =
     {
      (pathPrefix("test") & get & pathSuffix(Segment)) {jwt => complete(jwt)}
    }
  
  val userGet =
    {
      (get & pathPrefix("user") & pathSuffix(Segment)) 
        {number => isStringNumber(number) match {
          case false => complete(HttpResponse(StatusCodes.BadRequest, entity =  new ResponseMessage(404, "Only Numbers are allowed").toJson.toString))
          case true => db.getUserByKey(number.toInt).getOrElse(null) match {
            case user: UserModel => complete(HttpResponse(StatusCodes.OK, entity = user.toJson.toString))
            case null => complete(HttpResponse(StatusCodes.BadRequest, entity = new ResponseMessage(404, "User not found").toJson.toString))
          }
        }
      }
    }
    
  val userPost =
    path("user") {
      post {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
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
  
  // val projectsList = {
  //   (pathPrefix("projectslist") & get & parameters("aaa", "bbb"))
  //     {
  //       (ccc: String, vvv: String) => complete(ccc.concat(vvv))
  //     } 
  // }

  // (listOfNames: List[String] = Nil, moment: String = "", since: Boolean = true, deleted: Boolean = false, sortingFactor: String = null, sortingAsc: Boolean = true, searchedPage: Int = 1):
  // val projectsList = {
  //   (get & pathPrefix("projectslist") & path("names" / Segment / "moment" / Segment / "since" / Segment))
  //     {
  //       ???
  //     } 
  // }
  
  val projectsList =
    path("projectslist") {
      get {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }
  
  val allRoutes = concat(testRoute, userGet, userPost, userDelete, taskGet, taskPost, taskDelete, taskPut, projectGet, projectPost, projectDelete, projectPut, projectsList)
}
