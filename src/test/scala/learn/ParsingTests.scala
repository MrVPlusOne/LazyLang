package learn

import learn.parlang._
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Prop, Test}
import org.scalacheck.util.Pretty
import utest._

object ParsingTests extends TestSuite with UTestScalaCheck {
  def result(expr: PExpr): Right[Error, PExpr] = Right(expr)

  val tests = Tests {

    test("random expr linear show parsing") {
      import learn.parlang.RandomExpr.exprGen

      (0 until 50).foreach { size =>
        val expr = exprGen.pureApply(
          Gen.Parameters.default.withSize(size),
          Seed.random(),
        )
        val got = parseExpr(expr.showLinear)
        got ==> result(expr)
      }
    }
    test("evaluate parsing") {
      parseEval("if greater x 1 then 5 else 6 where x = 10")
        .map(_.expr) ==> result(5)
    }
  }
}

trait UTestScalaCheck {

  protected[this] object UTestReporter extends Test.TestCallback {
    private val prettyParams = Pretty.defaultParams

    override def onTestResult(
                               name: String,
                               res: org.scalacheck.Test.Result,
                             ): Unit = {
      val scalaCheckResult =
        if (res.passed) "" else Pretty.pretty(res, prettyParams)
      assert(scalaCheckResult.isEmpty)
    }
  }

  implicit protected[this] class PropWrapper(prop: Prop) {
    def checkUTest(): Unit = {
      prop.check(Test.Parameters.default.withTestCallback(UTestReporter))
    }
  }

}
