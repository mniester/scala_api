package Factories

import java.time.LocalDateTime

import DataModels._
import Strings.checkISOTimeFormat

object UserFactory {
  def apply (key: Int = -1, uuid: String, name: String): Option[UserModel] = {// this fake key is used only in new inputs, because schemas demand any. 
    if ((name.length <= Settings.maxUserNameLength) && (name.length >= Settings.minUserNameLength)) {
      Some(UserModel(key, uuid, name))
    } else {
      None
    }
  }
}

object ProjectFactory extends checkISOTimeFormat {
  def apply (key: Int = -1, name: String, user: Int, startTime: String,  deleteTime: String = ""): Option[ProjectModel] =
    if ((name.length <= Settings.maxProjectNameLength) && checkISOTimeFormat(startTime)) {
      Some(new ProjectModel(key, name, user, startTime, deleteTime))
    } else {
      None
    }
}

object FullProjectModelFactory {
  def apply (project: ProjectModel, tasks: List[TaskModel]): Option[FullProjectModel] = {
    Some(FullProjectModel(key = project.key, name = project.name, user = project.user, startTime = project.startTime, deleteTime = project.deleteTime, tasks = tasks))
  }
}

object TaskFactory extends checkISOTimeFormat {
  def apply (key: Int = -1, name: String, user: Int, startTime: String, endTime: String, project: Int, volume: Int = -1, comment: String = "", deleteTime: String = ""): Option[TaskModel] =
    if ((comment.length <= Settings.maxTaskCommentLength) && checkISOTimeFormat(startTime) && checkISOTimeFormat(endTime) && (LocalDateTime.parse(startTime).isBefore(LocalDateTime.parse(endTime)))) {
      Some(TaskModel(key, name, user, startTime, endTime, project, volume, comment, deleteTime))
    } else {
      None
    }
}