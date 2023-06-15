package snake

import scala.annotation.targetName
import scala.io.Source

trait snakeTypes {
  type Pos = (Int, Int)
  extension (pos: Pos)

    @targetName("+")
    infix def +(delta: Delta): Pos =
      ((pos._1 + delta._1), (pos._2 + delta._2))

    infix def isDiag(pos2: Pos): Boolean =
      pos._1 != pos2._1 && pos._1 != pos2._2

    infix def diagTo(head: Pos): Delta =
      if (pos acceptableDistance head) || !pos.isDiag(head)
      then throw Exception(s"Incorrect use of diagTo between $pos and $head")
      else
        val delta = pos.pathTo(head)
        if delta._1 > 0 && delta._2 > 0 //up and to the right
        then (1, 1)
        else if delta._1 > 0 && delta._2 < 0 //up and to the left
        then (1, -1)
        else if delta._1 < 0 && delta._2 > 0 //down and to the right
        then (-1, 1)
        else (-1, -1) //down and to the left

    infix def pathTo(pos2: Pos): Delta =
      (pos2._1 - pos._1, pos2._2 - pos._2)
    infix def acceptableDistance(tail: Pos): Boolean =
      if pos isDiag tail
      then (pos pathTo tail).distance <= 2
      else (pos pathTo tail).distance <= 1
  ////////////////////////////////////////////////////////
  type Delta = (Int, Int)
  extension (delta: Delta)
    def distance: Int = math.abs(delta._1) + math.abs(delta._2)
  ///////////////////////////////////////////////////////////
  type Instruction = List[Delta]

  ////////////////////////////////////////////////////////////////
  type Snake = List[Pos]
  ///////////////////////////////////////////////////////////


  extension (i: Int)
    private def toString2: String =
      if i == 0 then "H" else i.toString

  type Board = Array[Array[String]]

  extension (board: Board)

    def print =
      board.reverse.map(_.mkString(" ")).foreach(println)
    def printSnake(snake: Snake): Board =
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
          newBoard.printSnake(tail)
        }
        case _ => board
}

trait snakeTypes2 {
  case class Delta(upDown: Int, leftRight: Int){
    val distance = upDown + leftRight
  }

  case class centeredTuple(head: Pos){
    def
  }

  case class Pos(x: Int, y: Int){

    @targetName("+")
    infix def +(delta: Delta): Pos =
      Pos(x + delta.upDown, y + delta.leftRight)

    val center: Delta =
      Delta(-1*x, -1*y)
  }

  case class Snake(snake: List[Pos]){
    val processDelta(delta: Delta): Snake =
      snake match
        case head::nxt::tail =>


  }

}

trait snakeProgram extends snakeTypes2 {

  private def parseInstruction(arr: Array[String]): Instruction =
    val direction = arr(0)
    val steps = arr(1).toInt

    if direction == "R"
    then List.fill(steps)((0, 1))
    else if direction == "L"
    then List.fill(steps)(0, -1)
    else if direction == "U"
    then List.fill(steps)(1, 0)
    else List.fill(steps)(-1, 0) //otherwise must be D


  private def wrapProcessDelta(snake: Snake, delta: Delta): Snake =
    println(Array.fill(10)("7").mkString)
    emptyBoard.printSnake(snake).print
    processDelta(snake, delta)

  def processDelta(snake: Snake, delta: Delta): Snake =
    snake match
      case head :: nxt :: tail =>
        if head + delta acceptableDistance nxt
        then head + delta :: nxt :: tail
        else
          val nextHead = head + delta
          if nextHead isDiag nxt
          then nextHead :: processDelta(nxt :: tail, nxt diagTo nextHead)
          else nextHead :: processDelta(nxt :: tail, nxt pathTo head)
      case head :: Nil =>
        (head + delta) :: Nil
      case Nil =>
        Nil

  private def processInstruction(snake: Snake, instruction: Instruction): Snake =
    instruction.map(_.toString).foreach(println)
    val resultSnake = instruction.foldLeft(snake)((snake: Snake, delta: Delta) => wrapProcessDelta(snake, delta))

    resultSnake

  private val startingSnake: Snake = List.fill(10)((0, 0))
  private val emptyBoard: Board = Array.fill(20)(Array.fill(20)("*"))

  def main(file: String) =
    Source.fromFile(file)
      .mkString
      .split("\n")
      .map(_.split(" "))
      .map(parseInstruction(_))
      .foldLeft(startingSnake)((snake: Snake, instruction: Instruction) => processInstruction(snake, instruction))

}

trait Game extends snakeProgram {

}

