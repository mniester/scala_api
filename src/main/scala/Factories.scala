package Factories

import java.time.LocalDateTime

import Models._

object CheckISOTimeFormat {
  def apply (string: String): Boolean =
    try {
      LocalDateTime.parse(string)
      true
    }
    catch {
      case _: Throwable => false
    }
}

object UserFactory {
  def apply (key: Int = -1, name: String): Option[UserModel] = // this fake key is used only in new inputs, because schemas demand any. 
    if ((name.length <= Settings.maxUserNameLength) && (name.length >= Settings.minUserNameLength)) {
      Some(UserModel(key, name))
    } else {
      None
    }
}

object ProjectFactory {
  def apply (key: Int = -1, name: String, author: String, startTime: String,  deleteTime: String = ""): Option[ProjectModel] =
    if ((name.length <= Settings.maxProjectNameLength) && (author.length <= Settings.maxUserNameLength) && CheckISOTimeFormat(startTime)) {
      Some(new ProjectModel(key, name, author, LocalDateTime.parse(startTime), deleteTime))
    } else {
      None
    }
}

object ProjectWithTasksFactory {
  def apply (project: ProjectModel, tasks: List[TaskModel]): Option[ProjectWithTasksModel] = {
    Some(ProjectWithTasksModel(project, tasks))
  }
}

object TaskFactory {
  def apply (key: Int = -1, name: String, author: String, startTime: String, endTime: String, project: String, time: Int, volume: Int = -1, comment: String = "", deleteTime: String = ""): Option[TaskModel] =
    if ((comment.length <= Settings.maxTaskCommentLength) && CheckISOTimeFormat(startTime) && CheckISOTimeFormat(endTime) && (LocalDateTime.parse(startTime).isBefore(LocalDateTime.parse(endTime)))) {
      Some(TaskModel(key, name, author, LocalDateTime.parse(startTime), LocalDateTime.parse(endTime), project, time, volume, comment, deleteTime))
    } else {
      None
    }
}