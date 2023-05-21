package two
import scala.io.Source
import scala.collection.mutable.Map
object Two {

  def read_data_from_file(filename: String): String =
    Source.fromFile(filename).mkString

  val data = read_data_from_file("/Users/hasnatswaleheen/clones/advent-2022/data/two.txt")

  val mySubstances = Map[String, Int]("X" -> 1, "Y" -> 2, "Z" -> 3)
  val enemySubstances = Map[String, Int]("A" -> 1, "B" -> 2, "C" -> 3)
  val outcomes = Map[String, Int]("X" -> -1, "Y" -> 0, "Z" -> 1)

  def fight(me: Int, enemy: Int): Int =
    if (me - enemy == -1 || me - enemy == 2) {
          me
        }
        else if (me - enemy == 1 || me - enemy == -2) {
          me + 6
        }
        else {
          me + 3
        }

  def fight2(enemy: String, outcome: String): Int =
    val enemyInt = enemySubstances(enemy)
    val outcomeInt = outcomes(outcome)
    val meInt = (enemyInt - 1 + outcomeInt + 3) % 3 + 1

    fight(meInt, enemyInt)

  val mainA: Int =
    data.split("\n").map(_.strip().split(" "))
      .map((r:Array[String]) => (enemySubstances(r(0)), mySubstances(r(1))))
      .map((enemy, me) => fight(me, enemy))
      .sum

  val mainB: Int =
    data.split("\n").map(_.strip().split(" "))
      .map((r: Array[String]) => fight2(r(0), r(1)))
      .sum




}
