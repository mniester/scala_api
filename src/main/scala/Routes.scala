import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.PathMatcher

import Strings.JWTCoder
import DataModels._

trait JsonProtocols extends DefaultJsonProtocol {
  implicit val userFormat = jsonFormat3(UserModel)
  implicit val projectFormat = jsonFormat5(ProjectModel)
  implicit val taskFormat = jsonFormat9(TaskModel)
}

object Routes extends JsonProtocols with SprayJsonSupport {

   val testRoute =
     {
      (pathPrefix("test") & get & pathSuffix(Segment)) {jwt => complete(jwt)}
    }
  
  val userGet =
     {
      (pathPrefix("user") & get & pathSuffix(Segment)) {jwt => complete(jwt)}
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
  
  val projectsList =
    path("projectslist") {
      get {
        parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
      }
    }
  
  val allRoutes = concat(testRoute, userGet, userPost, userDelete, taskGet, taskPost, taskDelete, taskPut, projectGet, projectPost, projectDelete, projectPut, projectsList)
}
