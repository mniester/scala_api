import slick.jdbc.SQLiteProfile.api._


import slick.jdbc.SQLiteProfile.api._
import slick.lifted.TableQuery

import DBs._
import Factories._

object Dev extends App {
  val db = SQLite
  db.setup()
  val task = TaskFactory(key = 1, name = "Test", author = "Test", startTime = "2000-01-01T00:01:01", endTime = "2000-02-01T00:01:01", project = 123, time = 1, volume = -1, comment = "Test").get;
  val taskQuery = "Test"
  db.addTask(task);
  val dbResult = db.getTasksByName(taskQuery)(0);
  assert (task == dbResult);
}