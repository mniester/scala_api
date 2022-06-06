import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model._

import ApiMessages._

object Responses  {
  val notDoneYetResponse = complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity(ContentTypes.`application/json`, notDoneYetMessage)))
  val badRequestResponse = complete(HttpResponse(StatusCodes.MethodNotAllowed.intValue, entity = HttpEntity(ContentTypes.`application/json`, badRequestMessage)))
  val methodNotAllowedResponse = complete(HttpResponse(StatusCodes.MethodNotAllowed, entity = HttpEntity(ContentTypes.`application/json`, methodNotAllowedMessage)))
  val jwtNotProperResponse = complete(HttpResponse(StatusCodes.BadRequest, entity = HttpEntity(ContentTypes.`application/json`, jwtNotProperMessage)))
  val forbiddenResponse = complete(HttpResponse(StatusCodes.Forbidden, entity = HttpEntity(ContentTypes.`application/json`, forbiddenMessage)))
  val notFoundResponse = complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity(ContentTypes.`application/json`, notFoundMessage)))
  val deletedResponse = complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, deletedMessage)))
 }