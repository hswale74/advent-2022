// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import snake.Game

class MySuite extends munit.FunSuite {

  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }

  test("test acceptable distance for non diag up"){

    new Game:
      val tail: Pos = (0,0)
      val headUp: Pos = (2,0)
      assertEquals(tail acceptableDistance headUp, false )
  }

  test("test acceptable distance for non diag down") {

    new Game:
      val tail: Pos = (0, 0)
      val headDown: Pos = (-2, 0)
      assertEquals(tail acceptableDistance headDown, false)
  }

  test("test acceptable distance for non diag left") {

    new Game:
      val tail: Pos = (0, 0)
      val headLeft: Pos = (0, -2)
      assertEquals(tail acceptableDistance headLeft, false)
  }

  test("test acceptable distance for non diag right") {

    new Game:
      val tail: Pos = (0, 0)
      val headRight: Pos = (0, 2)
      assertEquals(tail acceptableDistance headRight, false)
  }

  test("test process delta") {
    new Game:
      val tail = (0,0)
      val heads = List((1,1), (1,-1))

      def testThis(thisHead: Pos): Unit =
        val delta = (1,0)
        val actual = processDelta(List(thisHead, tail), delta)
        val expected = List(thisHead + delta, thisHead)
        assertEquals(actual, expected)

      heads.foreach(testThis(_))
  }
}
