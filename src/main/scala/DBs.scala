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
import DataModels._
import Factories._



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
  
  def addNewUser (user: UserModel): Unit =
    Await.result(cursor.run(users += user), Settings.dbWaitingDuration) 
     
  def addNewProject (project: ProjectModel): Unit =
    Await.result(cursor.run(projects += project), Settings.dbWaitingDuration)
  
  def addNewTask (task: TaskModel): Unit =
    Await.result(cursor.run(tasks += task), Settings.dbWaitingDuration)
  
  def addNewTasks (newTasks: Seq[TaskModel]): Unit = {
    val nt = for (x <- newTasks) yield x;
    Await.result(cursor.run(tasks ++= nt), Settings.dbWaitingDuration)
  }

  def filterUserByName(source: TableQuery[UserSchema], filter: String) = {
    source.filter(_.name  === filter)
  }

  def getUserByName(query: String): Seq[UserModel] = {
    val action1 = filterUserByName(users, query)
    val action2 = cursor.run(action1.result)
    Await.result(action2, Settings.dbWaitingDuration)
  }

  def delUsersByName(query: String): Unit = {
    Await.result(cursor.run(users.filter(_.name === query).delete), Settings.dbWaitingDuration)
  }

  def getProjectsByName(query: String): Seq[ProjectModel] = {
    val action = cursor.run(projects.filter(_.name === query).filter(_.deleteTime.length === 0).result)
    Await.result(action, Settings.dbWaitingDuration)
  }

  def getProjectByKey(query: Int): Seq[ProjectModel] = {
    val action = cursor.run(projects.filter(_.key === query).filter(_.deleteTime.length === 0).result)
    Await.result(action, Settings.dbWaitingDuration)
  }  

  def delProjectByKey(key: Int): Unit = {
    val removeProject = cursor.run(projects.filter(_.key === key).map(_.deleteTime).update(LocalDateTime.now().toString()))
    val removeTasks = cursor.run(tasks.filter(_.project === key).map(_.deleteTime).update(LocalDateTime.now().toString()))
    Await.result(removeProject, Settings.dbWaitingDuration)
    Await.result(removeTasks, Settings.dbWaitingDuration)
  }

  def getTasksByName(query: String): Seq[TaskModel] = {
    val action = cursor.run(tasks.filter(_.name === query).filter(_.deleteTime.length === 0).result)
    Await.result(action, Settings.dbWaitingDuration)
  }

  def getTasksByProject(key: Int): Seq[TaskModel] = {
    val getTasks = cursor.run(tasks.filter(_.project === key).filter(_.deleteTime.length === 0).result)
    Await.result(getTasks, Settings.dbWaitingDuration)
  }

  def delTasksByName(query: String): Unit = {
    Await.result(cursor.run(tasks.filter(_.name === query).map(_.deleteTime).update(LocalDateTime.now().toString())), Settings.dbWaitingDuration)
  }
}

abstract class DBFacade extends DBBase {
   
  def checkOverlappingTasksInProject(task: TaskModel): Seq[TaskModel] = {
    val tasksOfProject = getTasksByProject(task.project)
    for (t <- tasksOfProject if task.checkLocalTimeDateOverlap(t)) yield {t}
  }

  def addUser(newUser: UserModel): Option[UserModel] = {
    val userWithSameName = getUserByName(newUser.name)
    if (userWithSameName.isEmpty) {super.addNewUser(newUser); None} else {Some(userWithSameName.head)}
  }

  def addTask(newTask: TaskModel): Seq[TaskModel] = {
    val overlappingTasks = checkOverlappingTasksInProject(newTask)
    if (overlappingTasks.isEmpty) {addNewTask(newTask); Seq()} else overlappingTasks 
  }

  def addProject(newProject: ProjectModel): Unit = {
    addNewProject(newProject)
  }

  def getProjectWithTasks (query: Int): Option[ProjectModelwithTasks] = {
    val project = getProjectByKey(query).head
    val key = project.key
    val tasks = getTasksByProject(key).toList
    ProjectModelwithTasksFactory(project, tasks)
  }

  def getListOfProjects (listOfNames: List[String] = Nil, moment: String = "", since: Boolean, deleted: Boolean = false): Seq[ProjectModel] = {
    val filtered1 = if (!listOfNames.isEmpty) { projects.filter(alpha => alpha.name inSet listOfNames) } else {projects}
    val filtered2 = if (since) {filtered1.filter(beta => beta.startTime > moment)} else {filtered1.filter(gamma => gamma.startTime < moment)}
    val filtered3 = if (deleted) {filtered2.filter(delta => delta.deleteTime.length > 0)} else  {filtered2.filter(delta => delta.deleteTime.length > 0)}
    Await.result(cursor.run(filtered3.result), atMost = Settings.dbWaitingDuration)
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