package seven
import scala.io.Source

object Seven {

  import scala.io.Source
  import scala.collection.immutable.Map.WithDefault

  enum Record:
    case cd(dirname: String)
    case ls
    case directory(dirname: String)
    case file(name: String, size: Int)


  def parseRecord(s: Array[String]): Record =
    if s(0) == "$"
    then {
      if s(1) == "cd"
      then Record.cd(s(2))
      else Record.ls
    }
    else if s(0) == "dir"
    then Record.directory(s(1))
    else Record.file(s(1), s(0).toInt)

  val records: List[Record] = Source.fromFile("/Users/hasnatswaleheen/clones/advent-2022/data/seven.txt")
    .mkString
    .split("\n")
    .map(_.split(" "))
    .map(parseRecord(_))
    .toList

  def cd(stack: List[String], dirname: String): List[String] =
    if dirname == "/"
    then "/" :: Nil
    else if dirname == ".."
    then stack.tail
    else dirname :: stack


  def processFile(dirSize: Map[String, Int],
                  stack: List[String],
                  file: Record.file): Map[String, Int] =
    stack match
      case Nil => dirSize
      case _ => processFile(
        dirSize.updated(stack.head,
          dirSize.getOrElse(stack.head, "0") + file.size
        ),
        stack.tail,
        file
      )


  def processRecord(dirSize: Map[String, Int],
                    records: List[Record],
                    stack: List[String]
                   ): Map[String, Int] =
    records match
      case Nil => dirSize
      case Record.cd(dirname) :: _ => processRecord(
        dirSize,
        records.tail,
        cd(dirname)
      )
      case Record.file(x, y) :: _ => processRecord(
        processFile(dirSize, stack, Record.file(x, y)),
        records.tail,
        stack
      )

  val maxDirs = processRecord(
    Map[String, Int](),
    records,
    List[String]()
  )


}