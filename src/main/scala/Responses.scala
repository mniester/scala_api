import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model._

import ApiMessages._

object Responses  {
  
  val notDoneYetResponse = complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity(ContentTypes.`application/json`, notDoneYetMessage)))
  
  //val badRequestMessage = ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "Bad request").toJson.toString
  val badRequestResponse = complete(HttpResponse(StatusCodes.MethodNotAllowed.intValue, entity = HttpEntity(ContentTypes.`application/json`, badRequestMessage)))
  
  //val methodNotAllowedMessage = ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "This method is not allowed - please contact admin").toJson.toString()
  val methodNotAllowedResponse = complete(HttpResponse(StatusCodes.MethodNotAllowed, entity = HttpEntity(ContentTypes.`application/json`, methodNotAllowedMessage)))
  
  //val jwtNotProperMessage = ResponseMessage(StatusCodes.BadRequest.intValue, "JWT is not proper").toJson.toString
  val jwtNotProperResponse = complete(HttpResponse(StatusCodes.BadRequest, entity = HttpEntity(ContentTypes.`application/json`, jwtNotProperMessage)))
  
  //
  val forbiddenResponse = complete(HttpResponse(StatusCodes.Forbidden, entity = HttpEntity(ContentTypes.`application/json`, forbiddenMessage)))
  
  //
  val notFoundResponse = complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity(ContentTypes.`application/json`, notFoundMessage)))
  
  //
  val deletedResponse = complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`, deletedMessage)))
 }