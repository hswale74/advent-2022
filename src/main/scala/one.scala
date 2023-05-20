package one
import scala.io.Source

object One {

  def read_file_to_string(filepath: String): String =
    Source.fromFile(filepath).mkString

  def richest_elf(inventory: String): Int =
    val split = inventory.split(" ")
    val nested = split.map(_.split("\n").flatMap(_.toIntOption))
    nested.foldLeft(0)((acc: Int, arr: Array[Int]) => arr.sum.max(acc))

  def main1: Int =
    richest_elf(read_file_to_string("/Users/hasnatswaleheen/clones/advent-2022/data/one.txt"))
}
