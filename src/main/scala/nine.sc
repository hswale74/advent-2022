import java.security.KeyStore.TrustedCertificateEntry
import scala.annotation.targetName
import scala.io.Source
import scala.math.abs

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

  def acceptableDistance(pos2: Pos): Boolean =
    if pos._1 != pos2._1 && pos._2 != pos2._2
    then abs(pos._1 - pos2._1) + abs(pos._2 - pos2._2) <= 2
    else abs(pos._1 - pos2._1) + abs(pos._2 - pos2._2) <= 1

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
  println(s"getting new snake for delta $delta")
  snake match
    case head :: nxt :: tail =>
      println(s"$head,$nxt,$tail")
      println(s"$head going to ${head + delta}")
      if (head + delta) acceptableDistance nxt
        then
        println(s"${head  + delta} and $nxt are close enough")
        (head + delta) :: nxt :: tail
      else
        println(s"${head  + delta} and $nxt are too far apart")
        (head + delta)
          :: (getNewSnake(nxt :: tail, head - nxt))
    case head :: Nil =>
      head + delta :: Nil
    case _ => snake

def wrap(state: State2, delta: Pos): State2 =
  println("Another instruction")
  val (snake, laurels) = state
  val newSnake = getNewSnake(snake, delta)
  println(s"Another instruction complete. This time tail was at ${newSnake.last}")
  (newSnake, laurels.union(Set(newSnake.last)))


val startingPos: Pos = (0, 0)
val startingState: State = (startingPos, startingPos, Set(startingPos))
val mainA = data
  .map(parseInstruction)
  .foldLeft(startingState)((state, instruction) => go(state, instruction))
  ._3
  .size

val mainAExample = exampleData
  .map(parseInstruction)
  .foldLeft(startingState)((state, instruction) => go(state, instruction))
  ._3
  .size

val startingSnake: Snake = List.fill(10)((0, 0))
val startingState2 = (startingSnake, Set[(Int, Int)]())

val mainB = data
  .flatMap(parseInstruction)
  .foldLeft(startingState2)((state: State2, delta: Pos) => wrap(state, delta))
  ._2
  .size

val mainBExample = exampleData
  .flatMap(parseInstruction)
  .foldLeft(startingState2)((state: State2, delta: Pos) => wrap(state, delta))
  ._2


val board = Array.fill(20)(Array.fill(20)("*"))
board.map(_.mkString(" ")).foreach(println)

val snakeRow = 1
val snakeCol = 1

(for
  row <- 0 until 20
  column <- 0 until 20
yield
  if row == snakeRow && column == snakeCol then "O" else board(row)(column)
  )
  .toArray
  .grouped(20)
  .toArray
  .reverse
  .map(_.mkString(" "))


