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
import scala.collection.mutable.ArrayBuffer



abstract class DBBase {
  val configFile: Config
  val cursor: Database
  lazy val users = TableQuery[UserSchema]
  lazy val projects = TableQuery[ProjectSchema]
  lazy val tasks = TableQuery[TaskSchema]
  def resetSequences: Unit

  def reset (): Unit = {
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

  def getUserByName(query: String): Option[UserModel] = {
    val action = cursor.run(users.filter(_.name  === query).result)
    val result = Await.result(action, Settings.dbWaitingDuration).toList
    result match {
      case Nil => None
      case _ => Some(result.head)
    }
  }

  def getUserByKey(query: Int): Option[UserModel] = {
    val action = cursor.run(users.filter(_.key === query).result)
    val result = Await.result(action, Settings.dbWaitingDuration)
    result match {
      case Nil => None
      case _ => Some(result.head)
    }
  }

  def delUserByName(query: String): Unit = {
    Await.result(cursor.run(users.filter(_.name === query).delete), Settings.dbWaitingDuration)
  }

  def delUserByKey(query: Int): Unit = {
    Await.result(cursor.run(users.filter(_.key === query).delete), Settings.dbWaitingDuration)
  }

  def getProjectByName(query: String): List[ProjectModel] = {
    val action = cursor.run(projects.filter(_.name === query).filter(_.deleteTime.length === 0).result)
    Await.result(action, Settings.dbWaitingDuration).toList
  }

  def changeProjectName(projectKey: Int, newName: String) = {
    cursor.run(projects.filter(_.key === projectKey).map(_.name).update(newName))
  }

  def getProjectByKey(query: Int): Option[ProjectModel] = {
    val action = cursor.run(projects.filter(_.key === query).filter(_.deleteTime.length === 0).result)
    val result = Await.result(action, Settings.dbWaitingDuration).toList
    result match {
      case Nil => None
      case _ => Some(result.head)
    }
  }  

  def delProjectByKey(key: Int): Unit = {
    val removeProject = cursor.run(projects.filter(_.key === key).map(_.deleteTime).update(LocalDateTime.now().toString()))
    val removeTasks = cursor.run(tasks.filter(_.project === key).map(_.deleteTime).update(LocalDateTime.now().toString()))
    Await.result(removeProject, Settings.dbWaitingDuration)
    Await.result(removeTasks, Settings.dbWaitingDuration)
  }

  def getTaskByKey(query: Int): Option[TaskModel] = {
    val action = cursor.run(tasks.filter(_.key === query).filter(_.deleteTime.length === 0).result)
    val result = Await.result(action, Settings.dbWaitingDuration).toList
    result match {
      case Nil => None
      case _ => Some(result.head)
    }
  }

  def getTasksByProject(key: Int): List[TaskModel] = {
    val getTasks = cursor.run(tasks.filter(_.project === key).filter(_.deleteTime.length === 0).result)
    Await.result(getTasks, Settings.dbWaitingDuration).toList
  }

  def delTaskByKey(query: Int): Unit = {
    Await.result(cursor.run(tasks.filter(_.key === query).map(_.deleteTime).update(LocalDateTime.now().toString())), Settings.dbWaitingDuration)
  }
}

abstract class DBFacade extends DBBase {
   
  type ProjectQuery = Query[ProjectSchema,ProjectModel,Seq]

  def checkOverlappingTasksInProject(task: TaskModel): List[TaskModel] = {
    val tasksOfProject = getTasksByProject(task.project)
    (for (t <- tasksOfProject if task.checkLocalTimeDateOverlap(t)) yield t).toList
  }

  def addUser(newUser: UserModel): Option[UserModel] = {
    val userWithSameName = getUserByName(newUser.name)
    if (userWithSameName.isEmpty) {super.addNewUser(newUser); None} else {Some(userWithSameName.head)}
  }

  def addTask(newTask: TaskModel): List[TaskModel] = {
    val overlappingTasks = checkOverlappingTasksInProject(newTask)
    if (overlappingTasks.isEmpty) {addNewTask(newTask); Nil} else overlappingTasks 
  }

  def replaceTask(newTask: TaskModel): List[TaskModel] = {
    checkOverlappingTasksInProject(newTask).filter(_.key != newTask.key) match {
      case Nil => {delTaskByKey(newTask.key); addNewTask(newTask); Nil}
      case listOfOverlapping => listOfOverlapping
    }
  }

  def addProject(newProject: ProjectModel): List[ProjectModel] = {
    val projectWithTheSameName = getProjectByName(newProject.name)
    if (projectWithTheSameName.isEmpty) {addNewProject(newProject); Nil} else {projectWithTheSameName}
  }

  def getProjectWithTasks (query: Int): Option[ProjectModelWithTasks] = {
    Some(getProjectByKey(query)
      .flatMap(project => ProjectModelWithTasksFactory(project, getTasksByProject(project.key))).head)
  }

  def filterProjects (listOfNames: List[String], moment: String, since: Boolean, deleted: Boolean): ProjectQuery = {
    val filteredByName = if (!listOfNames.isEmpty) { projects.filter(alpha => alpha.name inSet listOfNames) } else {projects}
    val filteredByTime = if (moment.length > 0) {if (since) {filteredByName.filter(beta => beta.startTime > moment)} else {filteredByName.filter(gamma => gamma.startTime < moment)}} else {filteredByName}
    if (deleted) {filteredByTime.filter(delta => delta.deleteTime.length > 0)} else {filteredByTime.filter(delta => delta.deleteTime.length === 0)}
  }

  def addTasksToProject (seqOfProjects: List[ProjectModel]): List[ProjectModelWithTasks] = {
    for (project <- seqOfProjects) yield (ProjectModelWithTasksFactory(project, getTasksByProject(project.key)).get)
  }

  def sortProjects (projects:  List[ProjectModelWithTasks], sortingFactor: String, asc: Boolean): List[ProjectModelWithTasks] = {
    sortingFactor match {
      case "update" => asc match {
        case true => projects.sortBy(_.lastUpdate)
        case false => projects.sortBy(_.lastUpdate).reverse
        }
      case "create" => asc match {
        case true =>  projects.sortBy(_.startTime)
        case false => projects.sortBy(_.startTime).reverse
        }
      case _ => projects
      }
    }
  
  def pagination(projects: List[ProjectModelWithTasks], searchedPage: Int): List[ProjectModelWithTasks] = {
    val lowerBound = Settings.maxCharsInPage * (searchedPage - 1)
    val higherBound = lowerBound + Settings.maxCharsInPage
    def totalNumberOfChars (projects: List[ProjectModelWithTasks]) = (for (p <- projects) yield p.numberOfChars).sum
    val result = projects
        .map(project => project.numberOfChars)
        .scan(0)((prevNumber, nextNumber) => prevNumber + nextNumber)
        .zip(projects)
        .dropWhile(d => d._1 < lowerBound)
        .takeWhile(e => e._1 <= higherBound)
        .map(f => f._2)
    if (totalNumberOfChars(result) < higherBound) {result} else {result.init}
  } 
  
  def getListOfProjects (searchedPage: Int = 1, listOfNames: List[String] = Nil, moment: String = "", since: Boolean = true, deleted: Boolean = false, sortingFactor: String = null, sortingAsc: Boolean = true): List[ProjectModelWithTasks] = {
    val filteredProjects = filterProjects (listOfNames, moment, since, deleted)
    val projects = Await.result(cursor.run(filteredProjects.result), Settings.dbWaitingDuration).toList
    val sortedProjects = sortProjects(addTasksToProject(projects), sortingFactor, asc = sortingAsc)
    pagination(sortedProjects, searchedPage)
  }

  def checkIfUserIsAuthor(data: TaskModel): Boolean = {
    if (data.user == getTaskByKey(data.key).head.user) {true} else {false} 
  }

  def modifyTask(task: TaskModel): List[TaskModel] = {
    if (checkIfUserIsAuthor(task)) {replaceTask(task)} else {Nil}
  }
  
  def checkIfUserIsAuthor(data: ProjectModel): Boolean = {
    if (data.user == getProjectByKey(data.key).head.user) {true} else {false} 
  }

  def delProject(project: ProjectModel): Unit =
    if (checkIfUserIsAuthor(project)) {delProjectByKey(project.key)}
  
  def checkIfUserIsAuthor(projectKey: Int): Boolean = {
    if (projectKey == getProjectByKey(projectKey).head.user) {true} else {false} 
  } 
  def checkAndChangeProjectName(projectKey: Int, newName: String) = {
    if (checkIfUserIsAuthor(projectKey)) {changeProjectName(projectKey, newName)}
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