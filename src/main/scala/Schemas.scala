package Schemas

import slick.jdbc.SQLiteProfile.api._


class UserSchema (tag: Tag) extends Table [(Int, String, String)](tag, "users") {
    def key = column[Int]("key", O.PrimaryKey, O.AutoInc)
    def uuid = column[String]("uuid")
    def name = column[String]("name")
    def * = (key, uuid, name)
}

class ProjectSchema (tag: Tag) extends Table [(Int, String, String, String, String)] (tag, "projects") {
  def key = column[Int]("key", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def author = column[String]("author")
  def startTime = column[String]("start_time")
  def deleteTime = column[String] ("delete_time", O.Default(""))
  def * = (key, name, author, startTime, deleteTime)
}

class TaskSchema (tag: Tag) extends Table [(Int, String, String, String, String, String, Int, Int, String, String)] (tag, "tasks") {
  def key = column[Int]("key", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def author = column[String]("author")
  def startTime = column[String]("start_time")
  def endTime = column[String]("end_time")
  def project = column[String]("project")
  def time = column[Int]("time")
  def volume = column[Int] ("volume", O.Default(-1))
  def comment = column[String] ("comment", O.Default(null))
  def deleteTime = column[String] ("delete_time", O.Default(null))
  def * = (key, name, author, startTime, endTime, project, time, volume, comment, deleteTime)
}