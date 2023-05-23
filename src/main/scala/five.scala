package five
import scala.io.Source

object Five {

  val five = "/Users/hasnatswaleheen/clones/advent-2022/data/five.txt"
  val fiveTxt = Source.fromFile(five).mkString.split("\n\n")

  val stacks = fiveTxt(0)
  val instructions: Array[String] = fiveTxt(1).split("\n")

  val crates: Array[String] = stacks.split("\n").slice(0, stacks.length-1)
  crates.last
  val numStacks = crates
    .last
    .filterNot(_.isWhitespace)
    .grouped(1)
    .map(_.toInt)
    .max


  val crateLineLength = (numStacks - 1) * 4 + 3
  def padStringRight(s: String, exp: Int): String =
    val diff: Int = exp - s.length
    s.concat(" " * diff)

  padStringRight("foo", crateLineLength)

  def scanCrates(layer: Iterator[List[Char]], acc: Map[Int, List[Char]], stackNum: Int): Map[Int, List[Char]] =
    val ignoreChars: List[Char] = List(' ', '[', ']')
    if layer.hasNext
    then {
      val nxt = layer.next()(1)
      if !ignoreChars.contains(nxt)
      then scanCrates(layer,
        acc.updated(stackNum, nxt :: acc(stackNum)),
        stackNum + 1)
      else scanCrates(layer, acc, stackNum + 1)
    }
    else acc

  val ret = scanCrates(layer = crates(0).toList.grouped(4),
    acc = Map[Int, List[Char]](
      1 -> List[Char](),
      2 -> List[Char](),
      3 -> List[Char](),
      4 -> List[Char](),
      5 -> List[Char](),
      6 -> List[Char](),
      7 -> List[Char](),
      8 -> List[Char](),
      9 -> List[Char]()
    ),
    stackNum = 1
  )


  val acc = Map[Int, List[Char]](
    1 -> List[Char](),
    2 -> List[Char](),
    3 -> List[Char](),
    4 -> List[Char](),
    5 -> List[Char](),
    6 -> List[Char](),
    7 -> List[Char](),
    8 -> List[Char](),
    9 -> List[Char]()
  )

  val exampleAcc = Map[Int, List[Char]](
    1 -> List[Char](),
    2 -> List[Char](),
    3 -> List[Char]()
  )

  val stackMap: Map[Int, List[Char]] = crates
    .reverse
    .slice(1, crates.length)
    .foldLeft(acc)(
      (acc: Map[Int, List[Char]], layer: String)
      => scanCrates(layer.toList.grouped(4), acc, 1)
    )


  def applyInstruction(initial: Map[Int, List[Char]], instruction: String): Map[Int, List[Char]] =
    println("*"*50)
    (1 to initial.size).map(initial(_)).foreach(println)
    println("-"*10)
    println(instruction)
    println("-"*10)
    val parsed = instruction.split(" ")
    val quantity = parsed(1).toInt
    val giver = parsed(3).toInt
    val receiver = parsed(5).toInt

    val afterTake = initial.updated(
      giver,
      initial(giver).drop(quantity)
    )

    val afterGive = afterTake.updated(
      receiver,
      initial(giver).take(quantity).reverse ++ initial(receiver)
    )

    (1 to afterGive.size).map(afterGive(_)).foreach(println)

    afterGive

  def applyInstructionB(initial: Map[Int, List[Char]], instruction: String): Map[Int, List[Char]] =
    println("*"*50)
    (1 to initial.size).map(initial(_)).foreach(println)
    println("-"*10)
    println(instruction)
    println("-"*10)
    val parsed = instruction.split(" ")
    val quantity = parsed(1).toInt
    val giver = parsed(3).toInt
    val receiver = parsed(5).toInt

    val afterTake = initial.updated(
      giver,
      initial(giver).drop(quantity)
    )

    val afterGive = afterTake.updated(
      receiver,
      initial(giver).take(quantity) ++ initial(receiver)
    )

    (1 to afterGive.size).map(afterGive(_)).foreach(println)

    afterGive


  val stackMap2 = instructions
    .foldLeft(stackMap)(
      applyInstruction
    )


  val mainA = (1 to 9 map (stackMap2(_).headOption)).filter(_.isDefined).flatten.mkString


  val stackMap2B = instructions.foldLeft(stackMap)(
    applyInstructionB
  )

  val mainB = (1 to 9 map (stackMap2B(_).headOption)).filter(_.isDefined).flatten.mkString


}
