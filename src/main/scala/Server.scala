import akka.actor.Actor
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import scala.io.StdIn

import Routes._
import Settings._

object AkkaHttpMicroservice extends App {

  implicit val system = ActorSystem(Behaviors.empty, "main")
  implicit val executionContext = system.executionContext

  val bindingFuture = Http().newServerAt(localHostName, port).bind(allRoutes)

  println(s"Server now online. Press RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())
}