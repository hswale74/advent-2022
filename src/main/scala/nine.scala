package nine

import scala.annotation.{tailrec, targetName}
import scala.io.Source
import scala.math.abs

object Nine {

  val data = Source.fromFile("/Users/hasnatswaleheen/clones/advent-2022/data/nine.txt")
    .mkString
    .split("\n")
    .map(_.split(" "))

  val exampleData = Source.fromFile("/Users/hasnatswaleheen/clones/advent-2022/data/nine_example.txt")
    .mkString
    .split("\n")
    .map(_.split(" "))

  type Pos = (Int, Int)

  extension (pos: Pos)
    @targetName("+")
    infix def +(pos2: Pos) =
      (pos._1 + pos2._1, pos._2 + pos2._2)

    @targetName("-")
    infix def -(pos2: Pos) =
      (pos._1 - pos2._1, pos._2 - pos2._2)

    def isLongDiag =
      abs(pos._1 min pos._2) > 0 && ((abs(pos._1) max abs(pos._2))) == 2

    def diagToward(pos2: Pos) =
      if List((2,1),(1,2)).contains(pos2 - pos) // Up and to the right
        then (1,1)
      else if List((-2,1),(-1,2)).contains(pos2 - pos) // Down and to the right
        then (-1,1)
      else if List((2,-1),(1,-2)).contains(pos2 - pos) // Up and to the left
        then (1, -1)
      else if List((-2,-1),(-1, -2)).contains(pos2 - pos) // Down and to the left
        then (-1, -1)
      else throw Exception(s"Something wrong with your diag ${pos2 - pos}")
    def acceptableDistance(pos2: Pos): Boolean =
      if pos._1 != pos2._1 && pos._2 != pos2._2
      then abs(pos._1 - pos2._1) + abs(pos._2 - pos2._2) <= 2
      else abs(pos._1 - pos2._1) + abs(pos._2 - pos2._2) <= 1

    def deltaForTail(tail: Pos): Pos =
      if pos acceptableDistance tail
      then (0,0)
      else if (pos - tail).isLongDiag
      then tail diagToward pos
      else pos - tail

  extension (i: Int)
    private def toString2: String =
      if i == 0 then "H" else i.toString

  type Instruction = List[Pos]

  type Laurels = Set[Pos]

  type State = (Pos, Pos, Laurels)

  type Snake = List[Pos]

  type State2 = (Snake, Laurels)

  def parseInstruction(arr: Array[String]): List[Pos] =
    val direction = arr(0)
    val steps = arr(1).toInt

    if direction == "R"
    then List.fill(steps)((1, 0))
    else if direction == "L"
    then List.fill(steps)(-1, 0)
    else if direction == "U"
    then List.fill(steps)(0, 1)
    else List.fill(steps)(0, -1)

  def parseInstructionT(arr: Array[String]): Instruction =
    val direction = arr(0)
    val steps = arr(1).toInt

    if direction == "R"
    then List.fill(steps)((0, 1))
    else if direction == "L"
    then List.fill(steps)(0, -1)
    else if direction == "U"
    then List.fill(steps)(1, 0)
    else List.fill(steps)(-1, -0)
  def go(state: State, instruction: Instruction): State =
    val (headPos, tailPos, laurels) = state

    instruction match
      case head :: tail => {
        val newHeadPos = headPos + head
        if newHeadPos acceptableDistance tailPos
        then go((newHeadPos, tailPos, laurels), tail)
        else go((newHeadPos, headPos, laurels.union(Set(headPos))), tail)
      }
      case _ => state

  def getNewSnake(snake: Snake, delta: Pos): Snake =
    snake match
      case head :: nxt :: tail =>
        if (head + delta) acceptableDistance nxt
        then
          (head + delta) :: nxt :: tail
        else
          if (head.+(delta) - nxt).isLongDiag
          then
            (head + delta) :: getNewSnake(nxt :: tail, nxt diagToward head.+(delta))
          else (head + delta) :: getNewSnake(nxt :: tail, head - nxt)

      case head :: Nil =>
        head + delta :: Nil
      case _ => snake

  def processIns(snake: Snake, instruction: Instruction): Snake =
    println(List.fill(50)("@").mkString)
    display(emptyBoard, snake).reverse.map(_.mkString(" ")).foreach(println)
    instruction match
      case head :: tail => processIns(getNewSnake(snake, head), tail)
      case _ => snake
  def wrap(state: State2, instruction: Instruction): State2 =
    println(s"Another instruction: $instruction")
    val (snake, laurels) = state
    val newSnake = processIns(snake, instruction)
    println(s"Another instruction complete. This time the new snake was ${newSnake}")
    (newSnake, laurels.union(Set(newSnake.last)))

  @tailrec
  private def display(board: Array[Array[String]], snake: Snake): Array[Array[String]] =
    snake match
      case head :: tail => {
        val newBoard = 
          (for
            row <- 0 until 20
            column <- 0 until 20
            yield
            if row == head(0) && column == head(1)
            then (10 - snake.length).toString2
            else board(row)(column)
          )
          .toArray
          .grouped(20)
          .toArray
        display(newBoard, tail)
      }
      case _ => board

  val startingPos: Pos = (0, 0)
  val startingState: State = (startingPos, startingPos, Set(startingPos))
//  val mainA = data
//    .map(parseInstruction)
//    .foldLeft(startingState)((state, instruction) => go(state, instruction))
//    ._3
//    .size

//  val mainAExample = exampleData
//    .map(parseInstruction)
//    .foldLeft(startingState)((state, instruction) => go(state, instruction))
//    ._3
//    .size

  val startingSnake: Snake = List.fill(10)((0, 0))
  val startingState2 = (startingSnake, Set[(Int, Int)]())
  val emptyBoard: Array[Array[String]] = Array.fill(20)(Array.fill(20)("*"))

  val mainB = exampleData
    .take(2)
    .map(parseInstructionT)
    .foldLeft(startingState2)((state: State2, instruction: Instruction) => wrap(state, instruction))
    ._2
    .size


//  val mainBExample = exampleData
//    .flatMap(parseInstruction)
//    .foldLeft(startingState2)((state: State2, delta: Pos) => wrap(state, delta))
//    ._2

}
