package object Settings {
  import scala.concurrent.duration.Duration
  import java.io.File
  import com.typesafe.config.{Config, ConfigFactory}
  import slick.basic.BasicBackend
  import slick.jdbc.SQLiteProfile.api._

  import os._
  
  val source: Config = ConfigFactory.parseFile(new File(s"${os.pwd}/src/resources/application.conf"))

  val sqlite = Database.forConfig(path = "", config = source.getConfig("db.sqlite3"))
  
  val minUserNameLength = source.getConfig("length").getInt("minUserNameLength")
  val maxUserNameLength = source.getConfig("length").getInt("maxUserNameLength")
  
  val minProjectNameLength = source.getConfig("length").getInt("minProjectNameLength")
  val maxProjectNameLength = source.getConfig("length").getInt("maxProjectNameLength")
  
  val maxTaskNameLength = source.getConfig("length").getInt("maxTaskNameLength")
  val minTaskNameLength = source.getConfig("length").getInt("minTaskNameLength")
  val maxTaskCommentLength = source.getConfig("length").getInt("maxTaskCommentLength")
  val minTaskCommentLength = source.getConfig("length").getInt("minTaskCommentLength")
  val maxCharsInPage = source.getConfig("length").getInt("maxCharsInPage") 
  
  val JwtKey = source.getConfig("secrets").getString("JwtKey")
  val dbWaitingDuration = Duration(source.getConfig("wait").getInt("db.quantity"), source.getConfig("wait").getString("db.unit"))

  val localHostName = source.getConfig("server").getString("localHostName")
  val port = source.getConfig("server").getInt("port")

  val userRoute = source.getConfig("routes").getString("user")
  val projectRoute = source.getConfig("routes").getString("project")
  val taskRoute = source.getConfig("routes").getString("task")
  val projectsListsRoute = source.getConfig("routes").getString("projectsLists")
}