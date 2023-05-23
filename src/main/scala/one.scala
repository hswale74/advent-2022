package one
import scala.io.Source

object One {


  def read_file_to_string(filepath: String): String =
    Source.fromFile(filepath).mkString

  val inventory = read_file_to_string("/Users/hasnatswaleheen/clones/advent-2022/data/one.txt")

  def richest_elf(inventory: String): Int =
    val split = inventory.split("\n\n")
    split.map(_.split("\n").flatMap(_.toIntOption).sum).reduce(_ max _)

  def richest_3_elves(inventory: String): Int =
    val split = inventory.split("\n\n")
    val sortedElves = split.map(_.split("\n").flatMap(_.toIntOption).sum).sorted(Ordering.Int.reverse)
    sortedElves.slice(0,3).sum

  def mainA: Int =
    richest_elf(inventory)

  def mainB: Int =
    richest_3_elves(inventory)
}
