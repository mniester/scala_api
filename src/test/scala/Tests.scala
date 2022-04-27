import scala.concurrent.Await
import scala.concurrent.ExecutionContext

import org.scalatest.funsuite.AnyFunSuite

import io.jvm.uuid._

import Settings._
import DataModels._
import Factories._
import Strings._
import DBs.SQLite

class UnitTests extends AnyFunSuite {
  implicit val execution = ExecutionContext.global
  val db = SQLite
  db.setup()
  test("test ScalaTest") {assert ((true == true) && (false ==  false))}

  test("CheckISOTimeFormat - ok") {assert (CheckISOTimeFormat("2222-02-02T22:22:22"))}
  test("CheckISOTimeFormat - fail; not proper datetime") {assert (!CheckISOTimeFormat("2222-33-02T22:22:22"))}
  test("CheckISOTimeFormat - fail; string is not a datetime") {assert (!CheckISOTimeFormat("abcd"))}

  test("UserFactory.apply - fail; name too long") {assert (UserFactory(uuid = UUID.random.toString, name = "ab" * Settings.maxUserNameLength) == None)}
  test("ProjectFactory.apply - fail; name too long") {assert (ProjectFactory(name = "abc" * Settings.maxProjectNameLength, author = "abc", startTime = "2000-01-01T00:01:01") == None)}
  test("ProjectFactory.apply - fail; user name too long") {assert (ProjectFactory(name = "abc", author = "abc" * Settings.maxProjectNameLength, startTime = "2000-01-01T00:01:01") == None)}
  test("ProjectFactory.apply - fail; datetime not ok") {assert (ProjectFactory(name = "abc" * Settings.maxProjectNameLength, author = "abc", startTime = "2000-13-01T00:01:01") == None)}
  test("TaskFactory.apply - fail; comment too long") {assert (TaskFactory(name = "Test",
                                                        author = "Test",
                                                        startTime = "2000-01-01T00:01:01",
                                                        endTime = "2000-02-01T00:01:01",
                                                        project = 123, time = 1,
                                                        volume = 1, 
                                                        comment = "abc" * Settings.maxTaskCommentLength) == None)}
  
  test("TaskFactory.apply - fail; endTime is earlier than startTime") {assert (TaskFactory(name = "Test",
                                                        author = "Test",
                                                        startTime = "2001-01-01T00:01:01",
                                                        endTime = "2000-02-01T00:01:01",
                                                        project = 123, time = 1,
                                                        volume = 1, 
                                                        comment = "abc") == None)}
  
  test("DBFacade.addUser, DBFacade,delUserByName") {db.purge;
                                        val user = UserFactory(key = 1, uuid = UUID.random.toString, name = "Test").get;
                                        val userQuery = "Test"
                                        db.addUser(user);
                                        var dbResult = db.getUserByName(userQuery).last; 
                                        assert (user == dbResult);
                                        db.delUsersByName(userQuery);
                                        var dbResult2 = db.getUserByName(userQuery);
                                        assert (dbResult2.length == 0);
                                        }
  
  test("DBFacade.addProject, DBFacade.delProjectByKey, DBFacade.db.getProjectsByName") {db.purge;
                                        val project = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
                                        val projectQuery = "Test"
                                        db.addProject(project);
                                        val dbResult = db.getProjectsByName(projectQuery).last; 
                                        assert (project == dbResult);
                                        db.delProjectByKey(1);
                                        var dbResult2 = db.getProjectsByName(projectQuery);
                                        assert (dbResult2.length == 0);
                                      }
  
  test("DBFacade.addTask, DBFacade.delTasksByName, DBFacade.getTasksByName") {db.purge;
                                        val task = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
                                        val taskQuery = "Test"
                                        db.addTask(task);
                                        val dbResult = db.getTasksByName(taskQuery)(0);
                                        assert (task == dbResult);
                                        db.delTasksByName(taskQuery);
                                        var dbResult2 = db.getTasksByName(taskQuery);
                                        assert (dbResult2.isEmpty);
                                      }

  test("DBFacade.addTask, DBFacade.getTasksByProject") {db.purge;
                                        val task = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
                                        db.addTask(task);
                                        val dbResult = db.getTasksByProject(task.project).head;
                                        assert (task == dbResult);
                                      }

  test("DBFacade.addTask, DBFacace.addProject, DBFacade.delProjectByKey, DBFacade.getProjectByName, DBFacade.getTasksByName") {db.purge;
                                        val task = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
                                        val project = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
                                        val taskQuery = "Test"
                                        val projectQuery = "Test"
                                        db.addTask(task);
                                        db.addProject(project);
                                        db.delProjectByKey(1);
                                        val projectResult = db.getProjectsByName(projectQuery);
                                        assert (projectResult.isEmpty);
                                        var TaskResult = db.getTasksByName(taskQuery);
                                        assert (TaskResult.isEmpty);
                                      }
  
  test("Task.checkLocalTimeDateOverlap true") {
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
    assert(task1.checkLocalTimeDateOverlap(task2))
    assert(task2.checkLocalTimeDateOverlap(task1))
  }

  test("Task.checkLocalTimeDateOverlap false") {
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2001-01-01T00:01:01", endTime = "2001-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
    assert(!task1.checkLocalTimeDateOverlap(task2))
  }

  test("DB.checkOverlappingTasksInProject - found") {db.purge;
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task3 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 2, time = 1, volume = -1, comment = "Test").get;
    db.addNewTasks(Seq(task2, task3))
    val result = db.checkOverlappingTasksInProject(task1)
    assert(result.length == 1)
  }

  test("DB.checkOverlappingTasksInProject - no found") {db.purge;
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2001-01-01T00:01:01", endTime = "2001-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task3 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
    db.addNewTasks(Seq(task2, task3))
    val result = db.checkOverlappingTasksInProject(task1)
    assert(result.length == 0)
  }

  test("DBFacade.addUser, DBFacade.getUserByName, DBFacade.delUsersByName - OK") {db.purge;
                                        val user = UserFactory(key = 1, uuid = UUID.random.toString, name = "Test").get;
                                        val userQuery = "Test"
                                        db.addUser(user);
                                        val dbResult = db.getUserByName(userQuery).last; 
                                        assert (user == dbResult);
                                        db.delUsersByName(userQuery);
                                        var dbResult2 = db.getUserByName(userQuery);
                                        assert (dbResult2.length == 0);
                                        }
  
  test("DBFacade.addUser (twice with same name)") {db.purge;
                                        val user = UserFactory(key = 1, uuid = UUID.random.toString, name = "Test").get;
                                        val userQuery = "Test"
                                        db.addUser(user);
                                        val dbResult = db.addUser(user).get;
                                        assert (dbResult == user)
                                        }

  test("DBFacade.addTask using Facade ok") {db.purge;
                                        val task = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
                                        val taskQuery = "Test"
                                        db.addTask(task);
                                        val dbResult = db.getTasksByName(taskQuery)(0);
                                        assert (task == dbResult);
                                        }

  test("DBFacade.addNewTasks, DBFacade.addTask (return overlapping task)") {db.purge;
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task3 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 2, time = 1, volume = -1, comment = "Test").get;
    db.addNewTasks(Seq(task2, task3))
    val result = db.addTask(task1).head
    assert(result == task2)
  }

  test("DBFacade.addProject, DBFacade.delProjectByKey, DBFacade.getProjectsByName") {db.purge;
                                        val project = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
                                        val projectQuery = "Test"
                                        db.addProject(project);
                                        val dbResult = db.getProjectsByName(projectQuery).last; 
                                        assert (project == dbResult);
                                        db.delProjectByKey(1);
                                        var dbResult2 = db.getProjectsByName(projectQuery);
                                        assert (dbResult2.length == 0);}
  
  test("DBFacade.addProject, DBFacade.addTask, DBFacade.addTask, DBFacade.getProjectWithTasks") {db.purge;
    val project = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    db.addProject(project)
    db.addTask(task1)
    db.addTask(task2)
    val result = db.getProjectWithTasks(1).get
    assert((result.tasks.head.duration + result.tasks.last.duration) == (task1.duration + task2.duration))
   }

  test("DBFacade.getListOfProjects (different variants)") {db.purge;
    val project1 = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
    val project2 = ProjectFactory(key = 2, name = "Test", author = "Test", startTime = "2002-01-01T00:01:01").get;
    val project3 = ProjectFactory(key = 3, name = "Other", author = "Test", startTime = "2000-01-01T00:01:01").get;
    val project4 = ProjectFactory(key = 4, name = "Other", author = "Test", startTime = "2002-01-01T00:01:01").get;
    db.addProject(project1)
    db.addProject(project2)
    db.addProject(project3)
    db.addProject(project4)
    val selectByName = db.getListOfProjects(List("Test"))
    assert ((for (x <- selectByName) yield x.key).toSet == Set(1,2))
    val selectSinceMoment = db.getListOfProjects(moment = "2001-01-01T00:01:01")
    assert ((for (x <- selectSinceMoment) yield x.key).toSet == Set(2,4))
    val selectToMoment = db.getListOfProjects(since = false, moment = "2001-01-01T00:01:01")
    assert ((for (x <- selectToMoment) yield x.key).toSet == Set(1,3))
    db.delProjectByKey(1)
    db.delProjectByKey(4)
    val selectNonDeleted = db.getListOfProjects()
    assert ((for (x <- selectNonDeleted) yield x.key).toSet == Set(2,3))
    val selectDeleted = db.getListOfProjects(deleted = true)
    assert ((for (x <- selectDeleted) yield x.key).toSet == Set(1,4))
   }

  test("DBFacade - addTasksToProject") {
    val project = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
    val task = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    db.addProject(project)
    db.addTask(task)
    val result = db.addTasksToProject(List(project))
    assert ((result.head.key == project.key) && (result.head.startTime == project.startTime))
  }

  test("DBFacade.addProject, DBFacade.addNewTasks, DBFacade.getListOfProjects") {db.purge;
    val projectWithTasks = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
    val projectWithoutTasks = ProjectFactory(key = 2, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 2, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    db.addProject(projectWithTasks)
    db.addProject(projectWithoutTasks)
    db.addNewTasks(List(task1, task2))
    val result = db.getListOfProjects()
    assert(result.length == 2)
    assert(result.head.tasks.length == 2)
    assert(result.last.tasks.length == 0)
   }
  test("DBFacade.sortProjects") {
    val projectWithTasks = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
    val projectWithoutTasks = ProjectFactory(key = 2, name = "Test", author = "Test", startTime = "1999-01-01T00:01:01").get;
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2002-01-01T00:01:01", endTime = "2002-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 2, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    db.addProject(projectWithTasks)
    db.addProject(projectWithoutTasks)
    db.addNewTasks(List(task1, task2))
    val sortByUpdate = db.getListOfProjects(sortingFactor = "update")
    assert ((sortByUpdate.last.key == 1) && (sortByUpdate.head.key == 2))
    val sortByUpdateDesc = db.getListOfProjects(sortingFactor = "update", sortingAsc = false)
    assert ((sortByUpdate.last.key == 2) && (sortByUpdate.head.key == 1))
    // val sortByCreate = db.getListOfProjects(sortingFactor = "create")
    // assert ((sortByUpdate.last.key == 1) && (sortByUpdate.head.key == 2))
    // val sortByCreateDesc = db.getListOfProjects(sortingFactor = "create", sortingAsc = false)
    // assert ((sortByUpdate.last.key == 2) && (sortByUpdate.head.key == 1))
  }
}