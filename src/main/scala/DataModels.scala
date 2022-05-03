package  DataModels

import java.time.LocalDateTime
import java.time.Duration
import io.jvm.uuid._

import DBs.DBFacade


abstract class AbstractProjectModel (key: Int, name: String, user: Int, startTime: String, deleteTime: String = "")

case class UserModel(key: Int, uuid: String, name: String)

case class ProjectModel (key: Int, 
                        name: String, 
                        user: Int, 
                        startTime: String, 
                        deleteTime: String = "") 
                        extends AbstractProjectModel (key: Int, 
                                                      name: String, 
                                                      user: Int, 
                                                      startTime: String, 
                                                      deleteTime: String)

case class ProjectModelWithTasks (key: Int, 
                                  name: String, 
                                  user: Int, 
                                  startTime: String, 
                                  deleteTime: String = "",
                                  tasks: List[TaskModel])
                                  extends AbstractProjectModel (key: Int, 
                                                              name: String, 
                                                              user: Int, 
                                                              startTime: String, 
                                                              deleteTime: String) {
  lazy val duration = (for (task <- this.tasks) yield task.duration).sum
  lazy val lastUpdate = if (tasks.isEmpty) {startTime} else (for (t <- tasks) yield t.startTime).max
  val numberOfChars: Int = key.toString.length + name.length + Settings.maxUserNameLength + startTime.length + deleteTime.length + (for (task <- tasks) yield task.numberOfChars).sum
  }; 
                                                              


case class TaskModel(key: Int, name: String, user: Int, startTime: String, endTime: String, project: Int, volume: Int, comment: String, deleteTime: String) {
  
  lazy val startMoment = LocalDateTime.parse(startTime)
  lazy val endMoment = LocalDateTime.parse(endTime)
  lazy val duration = Duration.between(startMoment, endMoment).getSeconds()
  val numberOfChars: Int = key.toString.length + name.length +  Settings.maxUserNameLength + startTime.length + endTime.length + volume.toString.length + comment.length + deleteTime.length

  def checkLocalTimeDateOverlap (otherTask: TaskModel): Boolean =
    !this.endMoment.isBefore(otherTask.startMoment) && !otherTask.endMoment.isBefore(this.startMoment)
}