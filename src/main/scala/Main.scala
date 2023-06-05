import snake._

@main def hello: Unit =

  println("Hello")
//  object NineB extends NineB{
//
//  }
  object thisGame extends Game {}
  val answer = thisGame.main("/Users/hasnatswaleheen/clones/advent-2022/data/nine_example.txt")
  println(s"Answer = $answer")

