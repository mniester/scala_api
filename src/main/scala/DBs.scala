package DBs

import java.io.File
import scala.concurrent.Await

import os.pwd
import com.typesafe.config.{Config, ConfigFactory}

import slick.basic.BasicBackend
import slick.jdbc.SQLiteProfile.api._
import java.time.LocalDateTime

import Settings._
import Schemas._
import Models._
import Queries._ 


abstract class DBBase {
  val configFile: Config
  val cursor: Database
  lazy val users = TableQuery[UserSchema]
  lazy val projects = TableQuery[ProjectSchema]
  lazy val tasks = TableQuery[TaskSchema]
  def resetSequences: Unit

  def purge (): Unit = {
    cursor.run(users.filter(_.name.length > 0).delete)
    cursor.run(projects.filter(_.name.length > 0).delete)
    cursor.run(tasks.filter(_.name.length > 0).delete)
    resetSequences
  }
  
  def setup (): Unit =
    {val createDB = DBIO.seq((users.schema ++ projects.schema ++ tasks.schema).createIfNotExists)
    Await.result(cursor.run(createDB), Settings.dbWaitingDuration)}
  
  def addUser (user: UserModel): Unit =
    Await.result(cursor.run(users += user.toInputTuple), Settings.dbWaitingDuration) 
     
  def addProject (project: ProjectModel): Unit =
    Await.result(cursor.run(projects += project.toInputTuple), Settings.dbWaitingDuration)
  
  def addTask (task: TaskModel): Unit =
    Await.result(cursor.run(tasks += task.toInputTuple), Settings.dbWaitingDuration)
  
  def addTasks (newTasks: Seq[TaskModel]): Unit = {
    val nt = for (x <- newTasks) yield x.toInputTuple;
    Await.result(cursor.run(tasks ++= nt), Settings.dbWaitingDuration)
  }

  def getUsersByName(query: UserQueryByName): Seq[UserModel] = {
    val action = cursor.run(users.filter(_.name === query.name).result)
    Await.result(action, Settings.dbWaitingDuration).map(x => UserModel(x._1, x._2))
  }

  def delUsersByName(query: UserQueryByName): Unit = {
    Await.result(cursor.run(users.filter(_.name === query.name).delete), Settings.dbWaitingDuration)
  }

  def getProjectsByName(query: ProjectQueryByName): Seq[ProjectModel] = {
    val action = cursor.run(projects.filter(_.name === query.name).filter(_.deleteTime.length === 0).result)
    Await.result(action, Settings.dbWaitingDuration).map(x => ProjectModel(x._1, x._2, x._3, LocalDateTime.parse(x._4), x._5))
  }

  def delProjectsByName(query: ProjectQueryByName): Unit = {
    val removeProject = cursor.run(projects.filter(_.name === query.name).map(_.deleteTime).update(Pencilcase.stringTimeZonedNow()))
    val removeTasks = cursor.run(tasks.filter(_.project === query.name).map(_.deleteTime).update(Pencilcase.stringTimeZonedNow()))
    Await.result(removeProject, Settings.dbWaitingDuration)
    Await.result(removeTasks, Settings.dbWaitingDuration)
  }

  def getTasksByName(query: TaskQueryByName): Seq[TaskModel] = {
    val action = cursor.run(tasks.filter(_.name === query.name).filter(_.deleteTime.length === 0).result)
    Await.result(action, Settings.dbWaitingDuration).map(x => TaskModel(x._1, x._2, x._3, LocalDateTime.parse(x._4), LocalDateTime.parse(x._5), x._6, x._7, x._8, x._9, x._10))
  }

  def getTasksByProject(query: TaskQueryByProject): Seq[TaskModel] = {
    val action = cursor.run(tasks.filter(_.project === query.project).filter(_.deleteTime.length === 0).result)
    Await.result(action, Settings.dbWaitingDuration).map(x => TaskModel(x._1, x._2, x._3, LocalDateTime.parse(x._4), LocalDateTime.parse(x._5), x._6, x._7, x._8, x._9, x._10))
  }

  def delTasksByName(query: TaskQueryByName): Unit = {
    Await.result(cursor.run(tasks.filter(_.name === query.name).map(_.deleteTime).update(Pencilcase.stringTimeZonedNow())), Settings.dbWaitingDuration)
  }
}

abstract class DBFacade extends DBBase {
   
  def checkOverlappingTasksInProject(task: TaskModel): Seq[TaskModel] = {
    val tasksOfProject = getTasksByProject(TaskQueryByProject(task.project))
    for (t <- tasksOfProject if task.checkLocalTimeDateOverlap(t)) yield {t}
  }

  def addUserFacade(newUser: UserModel): Option[UserModel] = {
    val userWithSameName = getUsersByName(UserQueryByName(newUser.name))
    if (userWithSameName.isEmpty) {addUser(newUser); None} else {Some(userWithSameName.head)}
  }

  def addTaskFacade(newTask: TaskModel): Seq[TaskModel] = {
    val overlappingTasks = checkOverlappingTasksInProject(newTask)
    if (overlappingTasks.isEmpty) {addTask(newTask); Seq()} else overlappingTasks
  }

  def addProjectFacade(newProject: ProjectModel): Option[ProjectModel] = {
    val projectWithSameName = getProjectsByName(ProjectQueryByName(newProject.name))
    if (projectWithSameName.isEmpty) {addProject(newProject); None} else {Some(projectWithSameName.head)}
  }
}

object SQLite extends DBFacade {
  val configFile = ConfigFactory.parseFile(new File(s"${os.pwd}/src/resources/application.conf"))
  val cursor = Database.forConfig(path = "", config = configFile.getConfig("db.sqlite3"))

  def resetSequences: Unit = {
    val resetSequences = sqlu"""UPDATE sqlite_sequence SET seq = 0; VACUUM;"""
    Await.result(cursor.run(resetSequences), Settings.dbWaitingDuration)
  }
}