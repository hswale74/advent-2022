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

  def chgDir(stack: List[String], dirname: String): List[String] =
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
        dirSize.updated(stack.mkString("-"),
          dirSize.getOrElse(stack.mkString("-"), "0").toString.toInt + file.size
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
      case Record.cd(dirname) :: _ => {
        print(stack)
        print("-->")
        println(records.head)
        processRecord(
          dirSize,
          records.tail,
          chgDir(stack, dirname)
        )
      }
      case Record.file(x, y) :: _ => {
        print(x)
        print(" ")
        print(y)
        print(" ")
        println(processFile(dirSize, stack, Record.file(x, y)))
        processRecord(
          processFile(dirSize, stack, Record.file(x, y)),
          records.tail,
          stack
        )
      }
      case _ => {
        println(records.head)

        processRecord(
          dirSize,
          records.tail,
          stack
        )
      }


  val records: List[Record] = Source.fromFile("/Users/hasnatswaleheen/clones/advent-2022/data/seven.txt")
    .mkString
    .split("\n")
    .map(_.split(" "))
    .map(parseRecord(_))
    .toList

  val exampleRecords: List[Record] = Source.fromFile("/Users/hasnatswaleheen/clones/advent-2022/data/seven_example.txt")
    .mkString
    .split("\n")
    .map(_.split(" "))
    .map(parseRecord(_))
    .toList

  val maxDirs = processRecord(
    Map[String, Int](),
    records,
    List[String]()
  )

  val exampleMaxDirs = processRecord(
    Map[String, Int](),
    exampleRecords,
    List[String]()
  )

  val mainA =
    maxDirs
      .filter((_, size) => size <= 100000)
      .values
      .sum

  val mainAEample =
    exampleMaxDirs
      .filter((_, size) => size <= 100000)
      .values
      .sum

}