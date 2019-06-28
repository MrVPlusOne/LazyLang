package vplusone

import lazylang._
import cats.implicits._
import utest._
import Evaluation.{ReducedThunk, TracedError}

object EvaluationTests extends TestSuite {

  case class WrongResult(expect: PExpr, get: ReducedThunk, program: PExpr)
  type TestError = Either[TracedError, WrongResult]

  def checkResult(program: PExpr, expect: PExpr): Option[TestError] = {
    eval(StandardLib.all, maxSteps = 4000)(program) match {
      case Left(error) => Some(error.asLeft)
      case Right(v) =>
        if (v.expr != expect) Some(WrongResult(expect, v, program).asRight)
        else None
    }
  }

  val tests = Tests {
    test("example programs") {
      import LazyLangExamples._

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
        val input = list(1, 2, 3, 4)
        checkResult(
          "get".call(3, foldrPairExpr.call(input)),
          4,
        ) ==> None
      }

      test("mutual isEven") {
        (0 to 9).foreach { i =>
          checkResult(isEven.call(i), i % 2 == 0) ==> None
        }
      }

      test("fib") {
        val program = parseExprGet(
          "fib 5 where fib n = " +
            "(if greater n 1 then plus n1 (fib (plus n -2)) else 1 where n1 = fib (plus n -1))",
        )
        checkResult(program, 8) ==> None
      }

      test("nats performances") {
        val program =
          parseExprGet("get 10 nats where nats = [0, map (plus 1) nats]")
        checkResult(program, 10) ==> None
      }

      test("fibs performances") {
        val program = parseExprGet(
          "get 3 fibs where fibs = [1, 1, zipWith plus fibs (snd fibs)]",
        )
        checkResult(program, 3) ==> None
      }

      test("out of scope error") {
        assert(eval(StandardLib.all)(let("x", "y")("y" ~> "y")).isLeft)
      }
      test("infinite loop error") {
        assert(eval(StandardLib.all)(let("x", "x")("x")).isLeft)
      }
    }
  }
}
