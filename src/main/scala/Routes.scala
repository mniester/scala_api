import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._

object Routes {
  val hello =
    path("hello") {
      get {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "Hi!"}"""))
    }
  }

  val user =
    path("user") {
      get {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "user"}"""))
      }
    }

  val userNew =
    path("user/new") {
      post {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "user/new"}"""))
      }
    }

  val userDelete =
    path("user/delete") {
      delete {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "user/delete"}"""))
      }
    }
  
  val task =
    path("task") {
      get {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "task"}"""))
      }
    }
  
  val taskNew =
    path("task/new") {
      post {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "task/new"}"""))
      }
    }
  
  val taskDelete =
    path("task/delete") {
      delete {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "task/delete"}"""))
      }
    }

  val taskModify =
    path("task/modify") {
      put {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "task/modify"}"""))
      }
    }

  val project =
    path("project") {
      get {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "project"}"""))
      }
    }
  
  val projectNew =
    path("project/New") {
      post {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "project/New"}"""))
      }
    }
  
  val projectDelete =
    path("project/delete") {
      delete {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "project/delete"}"""))
      }
    }

  val projectModify =
    path("project/modify") {
      put {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "project/modify"}"""))
      }
    }
  
  val projectsList =
    path("projects") {
      get {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "projectsList"}"""))
      }
    }
  
  val allRoutes = concat(user, userNew, userDelete, task, taskNew, taskDelete, taskModify, project, projectNew, projectDelete, projectModify, projectsList)
  val allGets = concat(user, task, project, projectsList)
}
