package learn

import parlang._
import cats.implicits._
import utest._

object TestCases extends TestSuite {
  case class WrongResult(expect: Reduced, get: ReducedThunk, program: PExpr)
  type TestError = Either[TracedError, WrongResult]

  def checkResult(program: PExpr, expect: Reduced): Option[TestError] = {
    eval(StandardLib.all, program) match {
      case Left(error) => Some(error.asLeft)
      case Right(v) =>
        if (v.expr != expect) Some(WrongResult(expect, v, program).asRight)
        else None
    }
  }

  val tests = Tests {
    test("example programs") {
      import ParLangExample._

      def fact(n: Int): Int =
        if (n > 0) n * fact(n - 1) else 1

      test("inc") {
        (-10 to 10).foreach { i =>
          checkResult(incExpr.call(i), i + 1) ==> None
        }
      }

      test("fact") {
        List(-4, -1, 0, 1, 2, 5, 10, 20).foreach { i =>
          checkResult(factExpr.call(i), fact(i)) ==> None
        }
      }

      test("foldr") {
        val input = list(1, 2, 3, 4).asInstanceOf[Reduced]
        checkResult(
          "eager" call foldrPairExpr.call(input),
          list(4, 3, 2, 1).asInstanceOf[Reduced],
        ) ==> None
      }

      test("repeat 1 2") {

      }

      test("out of scope error") {
        assert(eval(StandardLib.all, let("x", "y")("y" ~> "y")).isLeft)
      }
      test("infinite loop error") {
        assert(eval(StandardLib.all, let("x", "x")("x")).isLeft)
      }
    }
  }
}
