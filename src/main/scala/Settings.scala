package object Settings {
  import scala.concurrent.duration.Duration
  import java.io.File
  import com.typesafe.config.{Config, ConfigFactory}

  import os._
  
  val source: Config = ConfigFactory.parseFile(new File(s"${os.pwd}/src/resources/application.conf"))
  
  val minUserNameLength = source.getConfig("length").getInt("minUserNameLength")
  val maxUserNameLength = source.getConfig("length").getInt("maxUserNameLength")
  
  val minProjectNameLength = source.getConfig("length").getInt("minProjectNameLength")
  val maxProjectNameLength = source.getConfig("length").getInt("maxProjectNameLength")
  
  val maxTaskCommentLength = source.getConfig("length").getInt("maxTaskCommentLength")
  val minTaskCommentLength = source.getConfig("length").getInt("minTaskCommentLength")
  val maxCharsInPage = source.getConfig("length").getInt("maxCharsInPage") 
  
  val JWTKey = source.getConfig("secrets").getString("JWTKey")
  val dbWaitingDuration = Duration(source.getConfig("wait").getInt("db.quantity"), source.getConfig("wait").getString("db.unit"))
}