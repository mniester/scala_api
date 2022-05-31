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

trait ValidateIsoTimeFormat {
  def validateIsoTimeFormat (string: String): Boolean =
    try {
      LocalDateTime.parse(string)
      true
    }
    catch {
      case _: Throwable => false
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
}

trait FullProjectQueryValidation extends ValidateIsoTimeFormat {

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