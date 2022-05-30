package Factories

import java.time.LocalDateTime

import DataModels._
import Validators._

object UserFactory extends UserValidators {
  def apply (key: Int = -1, uuid: String, name: String): Option[UserModel] = {// this fake key is used only in new inputs, because schemas demand any. 
    if (validateUserNameMaxLength(name) && validateUserNameMinLength(name)) {
      Some(UserModel(key, uuid, name))
    } else {
      None
    }
  }
}

object ProjectFactory extends validateIsoTimeFormat with ProjectValidators {
  def apply (key: Int = -1, name: String, user: Int, startTime: String,  deleteTime: String = ""): Option[ProjectModel] =
    if (validateProjectNameMaxLength(name) && validateProjectNameMinLength(name) && validateIsoTimeFormat(startTime)) {
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

object TaskFactory extends validateIsoTimeFormat with TaskValidators with TimeValidators{
  def apply (key: Int = -1, name: String, user: Int, startTime: String, endTime: String, project: Int, volume: Int = -1, comment: String = "", deleteTime: String = ""): Option[TaskModel] = {
    if (validateTaskNameMaxLength(name) && validateTaskNameMinLength(name) && 
        validateTaskCommentMaxLength(comment) && validateTaskCommentMinLength(comment) && 
        validateIsoTimeFormat(startTime) && validateIsoTimeFormat(endTime) && isEarlier(startTime, endTime)) {
        Some(TaskModel(key, name, user, startTime, endTime, project, volume, comment, deleteTime))
    } else {
      None
    }
  }
}