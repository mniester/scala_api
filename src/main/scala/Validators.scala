package Validators

import java.time.LocalDateTime

import Settings._


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