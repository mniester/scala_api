package Validators


import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.PathMatcher
import java.time.LocalDateTime
import spray.json._

import Settings._
import DataModels._
import Cmds._
import Responses._
import ApiMessages.ResponseMessage

trait IsStringNumber {
  def isStringNumber(string: String): Boolean = {
    string.forall(Character.isDigit)
  }
}


trait IsStringBoolean {
  def isStringBoolean(string: String): Boolean = {
    if ((string == "true") || (string == "false")) {true} else {false}
  }
}

trait UserValidators {
  
  def validateUserNameMaxLength(name: String): Boolean = {
    name.length <= Settings.maxUserNameLength
  }

  def validateUserNameMinLength(name: String): Boolean = {
    name.length >= Settings.minUserNameLength
  } 
}


trait ProjectValidators {
  
  def validateProjectNameMaxLength(name: String): Boolean = {
    name.length <= Settings.maxProjectNameLength
  }

  def validateProjectNameMinLength(name: String): Boolean = {
    name.length >= Settings.minProjectNameLength
  }
}

trait TaskValidators {

  def validateTaskNameMaxLength(name: String): Boolean = {
    name.length <= Settings.maxTaskNameLength
  }

  def validateTaskNameMinLength(name: String): Boolean = {
    name.length >= Settings.minTaskNameLength
  }

  def validateTaskCommentMaxLength(comment: String): Boolean = {
    comment.length <= Settings.maxTaskCommentLength 
  }

  def validateTaskCommentMinLength(comment: String): Boolean = {
    comment.length >= Settings.minTaskCommentLength 
  }

}

trait TimeValidators {
  
  def isEarlier(time1: String, time2: String): Boolean = {
    LocalDateTime.parse(time1).isBefore(LocalDateTime.parse(time2))
  }

  def validateIsoTimeFormat (string: String): Boolean =
    try {
      LocalDateTime.parse(string)
      true
    }
    catch {
      case _: Throwable => false
    }
  }

trait FullProjectQueryValidation extends TimeValidators {

  val sortingFactors = List("create", "update")

  def validateFullProjectQuery (query: FullProjectQuery): Option[ResponseMessage] = {
    if (!validateIsoTimeFormat(query.moment)) {
      Some(ResponseMessage(StatusCodes.BadRequest.intValue, "Moment is not properly formatted - use ISO 8601"))
    } else if (!sortingFactors.contains(query.sortingFactor)) {
      Some(ResponseMessage(StatusCodes.BadRequest.intValue, s"sorting Factor valid arguments: ${sortingFactors.toString.drop(5).dropRight(1)}"))
    } else {
      None
    }
  }
}

trait InputValidation extends UserValidators with ProjectValidators with TaskValidators with TimeValidators {
    
    def validateInput(dataModel: DataModel): Option[ResponseMessage] = {
      dataModel match {
        case user: UserModel => validateUser(user)
        case project: ProjectModel => validateProject(project)
        case task: TaskModel => validateTask(task)
        case _ => Some(ResponseMessage(StatusCodes.BadRequest.intValue, "Input type was not recognized"))
      }
    }

    def validateUser(user: UserModel): Option[ResponseMessage] = {
      validateUserNameMinLength(user.name) match {
        case false => Some(ResponseMessage(StatusCodes.BadRequest.intValue, s"User Name is too short. Min. Length is ${Settings.minUserNameLength}."))
        case true => validateUserNameMaxLength(user.name) match {
          case false => Some(ResponseMessage(StatusCodes.PayloadTooLarge.intValue, s"User Name is too long. Max. Length is ${Settings.maxUserNameLength}."))
          case true => None
        }
      }
    }

    def validateProject(project: ProjectModel): Option[ResponseMessage] = {
      validateProjectNameMinLength(project.name) match {
        case false => Some(ResponseMessage(StatusCodes.BadRequest.intValue, s"Project Name is too short. Min. Length is ${Settings.minProjectNameLength}."))
        case true => validateProjectNameMaxLength(project.name) match {
          case false => Some(ResponseMessage(StatusCodes.PayloadTooLarge.intValue, s"Project Name is too long. Max. Length is ${Settings.maxProjectNameLength}."))
          case true => None
      } 
    }
  }
  def validateTask(task: TaskModel): Option[ResponseMessage] = {
    validateTaskNameMinLength(task.name) match {
      case false => Some(ResponseMessage(StatusCodes.BadRequest.intValue, s"Task Name is too short. Min. Length is ${Settings.minTaskNameLength}."))
      case true => validateTaskNameMaxLength(task.name) match {
        case false => Some(ResponseMessage(StatusCodes.PayloadTooLarge.intValue, s"Task Name is too long. Max. Length is ${Settings.maxTaskNameLength}."))
        case true => validateIsoTimeFormat(task.startTime) match {
          case false => Some(ResponseMessage(StatusCodes.BadRequest.intValue, "Start Time is not proper ISO Time"))
          case true => validateIsoTimeFormat(task.endTime) match {
            case false => Some(ResponseMessage(StatusCodes.BadRequest.intValue, "End Time is not proper ISO Time"))
            case true => isEarlier(task.startTime, task.endTime) match {
              case false => Some(ResponseMessage(StatusCodes.BadRequest.intValue, "Start Time is later than End Time"))
              case true => validateTaskCommentMaxLength(task.comment) match {
                case false => Some(ResponseMessage(StatusCodes.PayloadTooLarge.intValue, s"Comment is too long. Max. Length: ${Settings.maxTaskCommentLength}"))
                case true => None
              }
            }
          }
        }
      }
    }
  }
}