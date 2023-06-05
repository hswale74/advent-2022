package eight
import scala.io.Source

object Eight {
  
  val data = Source.fromFile("/Users/hasnatswaleheen/clones/advent-2022/data/eight.txt")
    .mkString
    .split("\n")
    .map(_.toArray.map(_.toString.toInt))

  def isVisible(row: Int, column: Int): Int =
    val left = (0 until column map data(row)).maxOption
    val up = (0 until row).map(data).map(_(column)).maxOption
    val right = (column + 1 until data(0).length map data(row)).maxOption
    val down = (row + 1 until data.length).map(data).map(_(column)).maxOption

    if List(left, up, right, down).flatten.min < data(row)(column) then 1 else 0

  val mainA = (
    for
      row <- 1 until data.length - 1
      column <- 1 until data(0).length - 1
    yield
      isVisible(row, column)).sum + (data.length * 4 - 4)

  def foldLeftWhile(a: Int)(height: Int)(lst: List[Int]): Int =
    lst match
      case head :: tail =>
        if head >= height
        then a
        else
          foldLeftWhile(a + 1)(height)(tail)
      case _ => a - 1


  val mainB =
    (
      for
        row <- 0 until data.length
        column <- 0 until data(0).length
        left = foldLeftWhile(1)(data(row)(column))((column - 1 to 0 by -1 map data(row)).toList)
        up = foldLeftWhile(1)(data(row)(column))((row - 1 to 0 by -1).map(data).map(_(column)).toList)
        right = foldLeftWhile(1)(data(row)(column))((column + 1 until data(0).length map data(row)).toList)
        down = foldLeftWhile(1)(data(row)(column))((row + 1 until data.length).map(data).map(_(column)).toList)
      yield
        left * up * right * down
      )
      .max
}
