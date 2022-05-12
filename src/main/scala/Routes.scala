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

case class ResponseMessage(code: Int, message: String)

trait JsonProtocols extends DefaultJsonProtocol {
  implicit val responseMessageFormat = jsonFormat2(ResponseMessage)
  implicit val userFormat = jsonFormat3(UserModel)
  implicit val projectFormat = jsonFormat5(ProjectModel)
  implicit val taskFormat = jsonFormat9(TaskModel)
}

// (get & pathPrefix("projectslist") & path("searchedPage" / Segment / "names" / Segment / "moment" / Segment / "since" / Segment / "deleted" / Segment/ "sortingFactor"/ Segment / "sortingAsc" / Segment))

trait checkUrlArguments extends isStringNumber with checkISOTimeFormat{
  def checkUrlArguments (data: (String, String, String, String, String, String, String)): ResponseMessage = {
    if (!isStringNumber(data._1)) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "Only Integers are allowed")
    } else if (!checkISOTimeFormat(data._3)) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "Date and time are not properly formatted")
    } else if (!isStringNumber(data._4)) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "since argument is not boolean - only true and false are allowed")
    } else if (!isStringNumber(data._5)) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "deleted argument is not boolean - only true and false are allowed")
    } else if (!isStringNumber(data._7)) {
      ResponseMessage(StatusCodes.BadRequest.intValue, "sortingAsc (Sorting ascending) argument is not boolean - only true and false are allowed")
    } else {
      ResponseMessage(StatusCodes.OK.intValue,  "Data Accepted")
    }
  }
}

object Routes extends JsonProtocols with SprayJsonSupport with checkUrlArguments  {
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
          case false => complete(HttpResponse(StatusCodes.BadRequest, entity = HttpEntity(ContentTypes.`application/json`, new ResponseMessage(StatusCodes.BadRequest.intValue, "Only Integers are allowed").toJson.toString)))
          case true =>  db.getUserByKey(number.toInt).getOrElse(null) match {
            case user: UserModel => complete(HttpResponse(status = StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, user.toJson.toString)))
            case null => complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity(ContentTypes.`application/json`, new ResponseMessage(StatusCodes.NotFound.intValue, "User not found").toJson.toString)))
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

  def projectsList() = {
    (get & pathPrefix("projectslist") & path("searchedPage" / Segment / "names" / Segment / "moment" / Segment / "since" / Segment / "deleted" / Segment/ "sortingFactor"/ Segment / "sortingAsc" / Segment))
      {
        (aaa, bbb, ccc, ddd, eee, fff, ggg) => complete(aaa + bbb + ccc + ddd +eee + fff + ggg)
      } 
  }
  
  // val projectsList =
  //   path("projectslist") {
  //     get {
  //       parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
  //     }
  //   }
  
  val allRoutes = concat(testRoute, userGet, userPost, userDelete, taskGet, taskPost, taskDelete, taskPut, projectGet, projectPost, projectDelete, projectPut, projectsList)
}
