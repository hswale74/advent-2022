package nine

import scala.annotation.targetName
import scala.io.Source
import scala.math.abs

object Nine {

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

  type Snake = List[Pos]

  type State2 = (Snake, Laurels)

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

  def applyInsToSnk(upStreamPos: Pos, snake: Snake, delta: Pos, acc: Snake): Snake =
    snake match
      case head :: Nil =>
        if upStreamPos acceptableDistance head
        then acc ++ snake
        else acc ++ head + delta :: Nil
      case head :: tail =>
        if upStreamPos acceptableDistance head
        then acc ++ snake
        else applyInsToSnk(head + delta, tail, delta, acc ++ ((head + delta) :: Nil))

//    def go2(state: State2, instruction: Instruction): State2 =
//      val (snake, laurels) = state
//
//      instruction match
//        case head :: tail =>




  val startingPos: Pos = (0,0)
  val startingState: State = (startingPos, startingPos, Set(startingPos))
  val mainA = data
    .map(parseInstruction)
    .foldLeft(startingState)((state, instruction)=>go(state, instruction))
    ._3
    .size

  println("foo")







}
