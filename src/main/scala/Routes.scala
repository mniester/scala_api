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

  val userPost =
    path("user") {
      post {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "user_post"}"""))
        //complete(StatusCodes.Created)
      }
    }

  val userDelete =
    path("user") {
      delete {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "user_delete"}"""))
      }
    }
  
  val task =
    path("task") {
      get {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "task"}"""))
      }
    }
  
  val taskPost =
    path("task") {
      post {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "task_post"}"""))
      }
    }
  
  val taskDelete =
    path("task") {
      delete {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "task_delete"}"""))
      }
    }

  val taskPut =
    path("task") {
      put {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "task_put"}"""))
      }
    }

  val project =
    path("project") {
      get {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "project"}"""))
      }
    }
  
  val projectPost =
    path("project") {
      post {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "project_post"}"""))
      }
    }
  
  val projectDelete =
    path("project") {
      delete {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "project_delete"}"""))
      }
    }

  val projectPut =
    path("project") {
      put {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "project_put"}"""))
      }
    }
  
  val projectsList =
    path("projectslist") {
      get {
        complete(HttpEntity(ContentTypes.`application/json`, """{"Hello!": "projectsList"}"""))
      }
    }
  
  val allRoutes = concat(user, userPost, userDelete, task, taskPost, taskDelete, taskPut, project, projectPost, projectDelete, projectPut, projectsList)
}
