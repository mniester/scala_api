import scala.concurrent.{Future, Await}
import akka.actor.ActorSystem
import akka.http.scaladsl.client.RequestBuilding.Post
import akka.http.scaladsl.model._
import akka.http.scaladsl.Http

import Settings._
import Routes._

object RoutesTests {
  implicit val system = ActorSystem()
  import system.dispatcher
  val request = HttpRequest(
  method = HttpMethods.POST,
  uri = localHostName + "/" + "users/new",
  entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, "data")
  )
  val response: Future[HttpResponse] = Http().singleRequest(request)
  println(response)
}