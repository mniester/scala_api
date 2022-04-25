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

  test("UserFactory - fail; name too long") {assert (UserFactory(uuid = UUID.random.toString, name = "ab" * Settings.maxUserNameLength) == None)}
  test("ProjectFactory - fail; name too long") {assert (ProjectFactory(name = "abc" * Settings.maxProjectNameLength, author = "abc", startTime = "2000-01-01T00:01:01") == None)}
  test("ProjectFactory - fail; user name too long") {assert (ProjectFactory(name = "abc", author = "abc" * Settings.maxProjectNameLength, startTime = "2000-01-01T00:01:01") == None)}
  test("ProjectFactory - fail; datetime not ok") {assert (ProjectFactory(name = "abc" * Settings.maxProjectNameLength, author = "abc", startTime = "2000-13-01T00:01:01") == None)}
  test("TaskFactory - fail; comment too long") {assert (TaskFactory(name = "Test",
                                                        author = "Test",
                                                        startTime = "2000-01-01T00:01:01",
                                                        endTime = "2000-02-01T00:01:01",
                                                        project = 123, time = 1,
                                                        volume = 1, 
                                                        comment = "abc" * Settings.maxTaskCommentLength) == None)}
  
  test("TaskFactory - fail; endTime is earlier than startTime") {assert (TaskFactory(name = "Test",
                                                        author = "Test",
                                                        startTime = "2001-01-01T00:01:01",
                                                        endTime = "2000-02-01T00:01:01",
                                                        project = 123, time = 1,
                                                        volume = 1, 
                                                        comment = "abc") == None)}
  
  test("DB - add, get and remove user") {db.purge;
                                        val user = UserFactory(key = 1, uuid = UUID.random.toString, name = "Test").get;
                                        val userQuery = "Test"
                                        db.addUser(user);
                                        var dbResult = db.getUserByName(userQuery).last; 
                                        assert (user == dbResult);
                                        db.delUsersByName(userQuery);
                                        var dbResult2 = db.getUserByName(userQuery);
                                        assert (dbResult2.length == 0);
                                        }
  
  test("DB - add, get and remove project") {db.purge;
                                        val project = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
                                        val projectQuery = "Test"
                                        db.addProject(project);
                                        val dbResult = db.getProjectsByName(projectQuery).last; 
                                        assert (project == dbResult);
                                        db.delProjectByKey(1);
                                        var dbResult2 = db.getProjectsByName(projectQuery);
                                        assert (dbResult2.length == 0);
                                      }
  
  test("DB - add, get and remove task by name") {db.purge;
                                        val task = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
                                        val taskQuery = "Test"
                                        db.addTask(task);
                                        val dbResult = db.getTasksByName(taskQuery)(0);
                                        assert (task == dbResult);
                                        db.delTasksByName(taskQuery);
                                        var dbResult2 = db.getTasksByName(taskQuery);
                                        assert (dbResult2.isEmpty);
                                      }

  test("DB - add, get task by project") {db.purge;
                                        val task = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
                                        db.addTask(task);
                                        val dbResult = db.getTasksByProject(task.project).head;
                                        assert (task == dbResult);
                                      }

  test("DB - remove Project with Tasks") {db.purge;
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
  
  test("Task - checkLocalTimeDateOverlap true") {
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
    assert(task1.checkLocalTimeDateOverlap(task2))
  }

  test("Task - checkLocalTimeDateOverlap false") {
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2001-01-01T00:01:01", endTime = "2001-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
    assert(!task1.checkLocalTimeDateOverlap(task2))
  }

  test("DB - checkOverlappingTasksInProject") {db.purge;
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task3 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 2, time = 1, volume = -1, comment = "Test").get;
    db.addNewTasks(Seq(task2, task3))
    val result = db.checkOverlappingTasksInProject(task1)
    assert(result.length == 1)
  }

  test("DB - checkOverlappingTasksInProject - no found") {db.purge;
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2001-01-01T00:01:01", endTime = "2001-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task3 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
    db.addNewTasks(Seq(task2, task3))
    val result = db.checkOverlappingTasksInProject(task1)
    assert(result.length == 0)
  }

  test("DB - add, get and remove User using Facade OK") {db.purge;
                                        val user = UserFactory(key = 1, uuid = UUID.random.toString, name = "Test").get;
                                        val userQuery = "Test"
                                        db.addUser(user);
                                        val dbResult = db.getUserByName(userQuery).last; 
                                        assert (user == dbResult);
                                        db.delUsersByName(userQuery);
                                        var dbResult2 = db.getUserByName(userQuery);
                                        assert (dbResult2.length == 0);
                                        }
  
  test("DB - add User using Facade returns User with the same name") {db.purge;
                                        val user = UserFactory(key = 1, uuid = UUID.random.toString, name = "Test").get;
                                        val userQuery = "Test"
                                        db.addUser(user);
                                        val dbResult = db.addUser(user).get;
                                        }

  test("DB - add Task using Facade ok") {db.purge;
                                        val task = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
                                        val taskQuery = "Test"
                                        db.addTask(task);
                                        val dbResult = db.getTasksByName(taskQuery)(0);
                                        assert (task == dbResult);
                                        }

  test("DB - addTask Facade - return overlapping task ") {db.purge;
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task3 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 2, time = 1, volume = -1, comment = "Test").get;
    db.addNewTasks(Seq(task2, task3))
    val result = db.addTask(task1).head
    assert(result == task2)
  }

  test("DB - add, get and remove Project using Facade OK") {db.purge;
                                        val project = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
                                        val projectQuery = "Test"
                                        db.addProject(project);
                                        val dbResult = db.getProjectsByName(projectQuery).last; 
                                        assert (project == dbResult);
                                        db.delProjectByKey(1);
                                        var dbResult2 = db.getProjectsByName(projectQuery);
                                        assert (dbResult2.length == 0);}
  
  test("DB - add Project with the same name using Facade; returns Project with the same name") {db.purge;
                                        val project = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
                                        val projectQuery = "Test"
                                        db.addProject(project);
                                        val dbResult = db.getProjectsByName(projectQuery)
                                        }
  
  test("DB -  getProjectWithTasks") {db.purge;
    val project = ProjectFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01").get;
    val task1 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    val task2 = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 1, time = 1, volume = -1, comment = "Test").get;
    db.addProject(project)
    db.addTask(task1)
    db.addTask(task2)
    val result = db.getProjectWithTasks(1).get
    assert((result.tasks.head.duration + result.tasks.last.duration) == (task1.duration + task2.duration))
   }

   test("DB - filtering") {db.purge;
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
}