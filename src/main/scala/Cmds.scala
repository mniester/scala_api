package Cmds

case class IntQuery(number: Int, uuid: String)

case class DelData(dataKey: Int, userKey: Int, userUuid: String)

case class FullProjectQuery (
  searchedPage: Int,
  listOfNames: List[String],
  moment: String, // ISO timedate
  since: Boolean,
  deleted: Boolean,
  sortingFactor: String, 
  sortingAsc: Boolean
)