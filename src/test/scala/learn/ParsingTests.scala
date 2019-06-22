package learn

import learn.parlang.PExpr
import learn.parlang.Parsing.ParsingError
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Prop, Test}
import org.scalacheck.util.Pretty
import utest._

object ParsingTests extends TestSuite with UTestScalaCheck {
  val tests = Tests{
    test("random expr linear show parsing"){
      import learn.parlang.RandomExpr.exprGen

      (0 until 50).foreach{ size =>
        val expr = exprGen.pureApply(Gen.Parameters.default.withSize(size), Seed.random())
        val got = parlang.parseExpr(expr.showLinear)
        println("equal?: " + (got equals Right(expr)))
        assert(got.toString == Right(expr).toString)
      }
    }
  }
}

trait UTestScalaCheck {

  protected[this] object UTestReporter extends Test.TestCallback {
    private val prettyParams = Pretty.defaultParams

    override def onTestResult(name: String, res: org.scalacheck.Test.Result): Unit = {
      val scalaCheckResult = if (res.passed) "" else Pretty.pretty(res, prettyParams)
      assert(scalaCheckResult.isEmpty)
    }
  }

  implicit protected[this] class PropWrapper(prop: Prop) {
    def checkUTest(): Unit = {
      prop.check(Test.Parameters.default.withTestCallback(UTestReporter))
    }
  }

}
