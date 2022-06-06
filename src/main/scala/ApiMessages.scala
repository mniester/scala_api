import akka.http.scaladsl.model._
import spray.json._
import MessageModels._

trait ApiMessagesJsonProtocol extends DefaultJsonProtocol {
  implicit val responseMessageFormat = jsonFormat2(ResponseMessage)
}

object ApiMessages extends ApiMessagesJsonProtocol {
  val notDoneYetMessage = ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "Route needs to be done").toJson.toString
  val badRequestMessage = ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "Bad request").toJson.toString
  val methodNotAllowedMessage = ResponseMessage(StatusCodes.MethodNotAllowed.intValue, "This method is not allowed - please contact admin").toJson.toString
  val jwtNotProperMessage = ResponseMessage(StatusCodes.BadRequest.intValue, "JWT is not proper").toJson.toString
  val forbiddenMessage = ResponseMessage(StatusCodes.Forbidden.intValue, "You do not have permission").toJson.toString
  val notFoundMessage = ResponseMessage(StatusCodes.NotFound.intValue, "User not found").toJson.toString
  val deletedMessage = ResponseMessage(StatusCodes.OK.intValue, "Data was deleted").toJson.toString
}