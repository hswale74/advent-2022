package three
import scala.io.Source
import scala.collection.immutable.NumericRange

object Three {

  def read_file_to_string(filePath: String): String =
    Source.fromFile(filePath).mkString

  val lowerCase: NumericRange.Inclusive[Char] = 'a' to 'z'
  val upperCase: NumericRange.Inclusive[Char] = 'A' to 'Z'

  def computePriority(c: Char): Int =
    if lowerCase.indexOf(c) >= 0 then lowerCase.indexOf(c) + 1 else upperCase.indexOf(c) + 27

  val data: String = read_file_to_string("/Users/hasnatswaleheen/clones/advent-2022/data/three.txt")
  val mainA: Int =
    data.split("\n")
      .map((record: String) => (record, record.length))
      .map((record: String, length: Int) => (record.slice(0, length/2), record.slice(length/2, length)))
      .flatMap((h1: String, h2: String) => h1.toSet.intersect(h2.toSet))
      .map((c: Char) => computePriority(c))
      .sum

  val mainADebug: Unit =
    data.split("\n")
      .map((record: String) => (record, record.length))
      .map((record: String, length: Int) => (record, record.slice(0, length / 2), record.slice(length / 2, length)))
      .map((record: String, h1: String, h2: String) => (record, h1, h2, h1.toSet.intersect(h2.toSet).mkString("|")))
      .map((record: String, h1: String, h2: String, item: String) => (record, h1, h2, item, item.map(computePriority(_)).sum))
      .map(println(_))

  val mainB: Int =
    data.split("\n")
      .grouped(3)
      .flatMap((arr: Array[String]) => arr(0).toSet intersect arr(1).toSet intersect arr(2).toSet)
      .map(computePriority(_))
      .sum

}
