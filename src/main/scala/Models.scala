package  Models

import java.time.LocalDateTime
import java.time.Duration

import Settings._


abstract class Model {
  def toInputTuple (): Product
}

case class UserModel(key: Int, name: String) extends  Model {
  def toInputTuple(): Tuple2[Int, String] =
    (key, name)
}

case class ProjectModel(key: Int, name: String, author: String, startTime: LocalDateTime, deleteTime: String = "") extends Model {
  def toInputTuple(): Tuple5[Int, String, String, String, String] =
    (key, name, author, startTime.toString(), deleteTime)
}

case class TaskModel(key: Int, name: String, author: String, startTime: LocalDateTime, endTime: LocalDateTime, project: String, time: Int,  volume: Int, comment: String, deleteTime: String) extends  Model {
  
  def getProjectModel(): ProjectModel = ProjectModel(key = -1, name = project, author = this.author, startTime = this.startTime, deleteTime = "") 
  
  lazy val duration =  Duration.between(startTime, endTime).toSeconds()

  def toInputTuple(): Tuple10[Int, String, String, String, String, String, Int, Int, String, String] = 
    (key, name, author, startTime.toString(), endTime.toString(), project, time, volume, comment, deleteTime)
  
  def checkLocalTimeDateOverlap (otherTask: TaskModel): Boolean =
    !(this.endTime).isBefore(otherTask.startTime)
}