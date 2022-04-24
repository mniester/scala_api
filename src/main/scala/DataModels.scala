package  DataModels

import java.time.LocalDateTime
import java.time.Duration
import io.jvm.uuid._

import Settings._


case class UserModel(key: Int, uuid: String, name: String)

case class ProjectModel(key: Int, name: String, author: String, startTime: String, deleteTime: String = "")

case class ProjectWithTasksModel(project: ProjectModel, tasks: List[TaskModel])

case class TaskModel(key: Int, name: String, author: String, startTime: String, endTime: String, project: Int, time: Int,  volume: Int, comment: String, deleteTime: String) {
  
  lazy val startMoment = LocalDateTime.parse(startTime)
  lazy val endMoment = LocalDateTime.parse(endTime)

  lazy val duration = Duration.between(startMoment, endMoment).toSeconds()

  def checkLocalTimeDateOverlap (otherTask: TaskModel): Boolean =
    //!(LocalDateTime.parse(this.endTime)).isBefore(LocalDateTime.parse(otherTask.startTime))
    !this.endMoment.isBefore(otherTask.startMoment)
}