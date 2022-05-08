import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import spray.json._

import Strings.JWTCoder
import DataModels._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.PathMatcher


trait JsonProtocols extends DefaultJsonProtocol {
  implicit val userFormat = jsonFormat3(UserModel)
}

object Routes extends JsonProtocols with SprayJsonSupport {
  
  val user =
    path("user") {
      parameter("JWT") {jwt => complete(HttpEntity(ContentTypes.`application/json`, jwt))}
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
  
  val task =
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

  val project =
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
  
  val allRoutes = concat(user, userPost, userDelete, task, taskPost, taskDelete, taskPut, project, projectPost, projectDelete, projectPut, projectsList)
}
