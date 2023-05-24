package six

object Six {
  import scala.io.Source

  val data = Source.fromFile("/Users/hasnatswaleheen/clones/advent-2022/data/six.txt").toList

  def scanForStart(data: List[Char],
                   buffer: Array[Char],
                   acc: Int,
                   msgSz: Int): Int =

    val hd = data.head
    if Set(hd).union(buffer.toSet).size < msgSz
    then scanForStart(data.tail, buffer.drop(1) :+ hd, acc + 1, msgSz)
    else acc + 1

  val messageSize = 14
  scanForStart(data.drop(messageSize - 1),
    data.take(messageSize - 1).toArray,
    messageSize - 1,
    messageSize)
  
  val mainA = scanForStart(data.drop(4 - 1),
    data.take(4 - 1).toArray,
    4 - 1,
    4)
  
  val mainB = scanForStart(data.drop(14 - 1),
    data.take(14 - 1).toArray,
    14 - 1,
    14)

}
