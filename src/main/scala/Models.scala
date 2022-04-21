package  Models

import java.time.LocalDateTime
import java.time.Duration
import io.jvm.uuid._

import Settings._


abstract class Model {
  def toInputTuple (): Product
}

case class UserModel(key: Int, uuid: String, name: String) extends  Model {
  def toInputTuple(): Tuple3[Int, String, String] =
    (key, uuid, name)
}

case class ProjectModel(key: Int, name: String, author: String, startTime: LocalDateTime, deleteTime: String = "") extends Model {
  def toInputTuple(): Tuple5[Int, String, String, String, String] =
    (key, name, author, startTime.toString(), deleteTime)
}

case class ProjectWithTasksModel(project: ProjectModel, tasks: List[TaskModel]) {
  lazy val duration = (for (t <- tasks) yield t.duration).sum
  lazy val lastUpdate = if (tasks.isEmpty) {project.startTime} else {(for (t <- tasks) yield t.endTime).max} 
}


case class TaskModel(key: Int, name: String, author: String, startTime: LocalDateTime, endTime: LocalDateTime, project: String, time: Int,  volume: Int, comment: String, deleteTime: String) extends  Model {
  
  def getProjectModel(): ProjectModel = new ProjectModel(key = -1, name = project, author = this.author, startTime = this.startTime, deleteTime = "") 
  
  lazy val duration = Duration.between(startTime, endTime).toSeconds()

  def toInputTuple(): Tuple10[Int, String, String, String, String, String, Int, Int, String, String] = 
    (key, name, author, startTime.toString(), endTime.toString(), project, time, volume, comment, deleteTime)
  
  def checkLocalTimeDateOverlap (otherTask: TaskModel): Boolean =
    !(this.endTime).isBefore(otherTask.startTime)
}