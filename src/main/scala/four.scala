package four
import scala.io.Source
import scala.collection.immutable.NumericRange
import scala.collection.immutable.Range

object Four {

  def readFileToSTring(fileName: String): String =
    Source.fromFile(fileName).mkString

  val data = readFileToSTring("/Users/hasnatswaleheen/clones/advent-2022/data/four.txt")

  def fullyContains(record: String): Boolean  =
    val intervalTuple = record.split(",")
    val res1 = intervalTuple.map(_.split("-"))
    val interval1 = (res1(0)(0).toInt, res1(0)(1).toInt)
    val interval2 = (res1(1)(0).toInt, res1(1)(1).toInt)
    (interval1(0) <= interval2(0) && interval1(1) >= interval2(1)) || (interval2(0) <= interval1(0) && interval2(1) >= interval1(1))

  def partiallyContains(record: String): Boolean =
    val intervalTuple = record.split(",")
    val res1 = intervalTuple.map(_.split("-"))
    val interval1 = (res1(0)(0).toInt, res1(0)(1).toInt)
    val interval2 = (res1(1)(0).toInt, res1(1)(1).toInt)
    if interval1(0) < interval2(0)
      then interval1(1) >= interval2(0)
    else if interval1(1) > interval2(1)
      then interval1(0) <= interval2(1)
    else fullyContains(record)


  val mainA: Int =
    data.split("\n") // array of strings
      .map(fullyContains(_))
      .map(if _ then 1 else 0)
      .sum
    
  val mainB: Int =
    data.split("\n")
      .map(partiallyContains(_))
      .map(if _ then 1 else 0)
      .sum
}