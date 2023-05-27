import scala.annotation.targetName
import scala.io.Source
import scala.math.abs

val data = Source.fromFile("/Users/hasnatswaleheen/clones/advent-2022/data/nine.txt")
  .mkString
  .split("\n")
  .map(_.split(" "))

type Pos = (Int, Int)
extension (pos: Pos)
  @targetName("+")
  infix def + (pos2: Pos) =
    (pos._1 + pos2._1, pos._2 + pos2._2)

  def acceptableDistance(pos2: Pos): Boolean =
    if pos._1 != pos2._1 && pos._2 != pos2._2
      then abs(pos._1 - pos2._1) + abs(pos._2 - pos2._2) <= 2
    else abs(pos._1 - pos2._1) + abs(pos._2 - pos2._2) <= 1

type Instruction = List[Pos]

type Laurels = Set[Pos]

type State = (Pos, Pos, Laurels)

def parseInstruction(arr: Array[String]): List[Pos] =
  val direction = arr(0)
  val steps  = arr(1).toInt

  if direction == "R"
  then List.fill(steps)((1, 0))
  else if direction == "L"
  then List.fill(steps)(-1, 0)
  else if direction == "U"
  then List.fill(steps)(0, -1)
  else List.fill(steps)(0, 1)

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

val startingPos: Pos = (0,0)
val startingState: State = (startingPos, startingPos, Set(startingPos))
val mainA = data
  .map(parseInstruction)
  .foldLeft(startingState)((state, instruction)=>go(state, instruction))
  ._3
  .size

data map parseInstruction foreach println






