import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration

import akka.actor.ActorSystem
import akka.http.scaladsl.client.RequestBuilding.Get
import akka.http.scaladsl.model._
import akka.http.scaladsl.Http


import Settings._
import Routes._
import akka.stream.ActorMaterializer

object Dev extends App{
  implicit val system = ActorSystem()
  implicit val materialize = ActorMaterializer()
  import system.dispatcher
  val request = HttpRequest(
  method = HttpMethods.GET,
  //uri = "http://" + localHostName + "/" + "users/new",
  uri = "http://localhost/users",
  entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, "data")
  )
  val response: HttpResponse = Await.result(Http().singleRequest(request).map(xxx => xxx), dbWaitingDuration)
  println(response)
}