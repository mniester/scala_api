package Schemas

import slick.jdbc.SQLiteProfile.api._

import DataModels._

class UserSchema (tag: Tag) extends Table [UserModel](tag, "users") {
  val key = column[Int]("key", O.PrimaryKey, O.AutoInc)
  val uuid = column[String]("uuid")
  val name = column[String]("name")
  val * = (key, uuid, name) <> (UserModel.tupled, UserModel.unapply)
}

class ProjectSchema (tag: Tag) extends Table [ProjectModel] (tag, "projects") {
  def key = column[Int]("key", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.Unique)
  def user = column[Int]("user")
  def startTime = column[String]("start_time")
  def deleteTime = column[String] ("delete_time", O.Default(""))
  def * = (key, name, user, startTime, deleteTime) <> (ProjectModel.tupled, ProjectModel.unapply)
}

class TaskSchema (tag: Tag) extends Table [TaskModel] (tag, "tasks") {
  def key = column[Int]("key", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def user = column[Int]("user")
  def startTime = column[String]("start_time")
  def endTime = column[String]("end_time")
  def project = column[Int]("project")
  def volume = column[Int] ("volume", O.Default(-1))
  def comment = column[String] ("comment", O.Default(null))
  def deleteTime = column[String] ("delete_time", O.Default(null))
  def * = (key, name, user, startTime, endTime, project, volume, comment, deleteTime) <> (TaskModel.tupled, TaskModel.unapply)
}