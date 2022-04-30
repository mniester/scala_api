import DBs.SQLite

object Main extends App {
  val db = SQLite
  db.setup()
  println("Hello!")
}