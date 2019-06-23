package learn.parlang

import learn.parlang.Evaluation.Binding
import org.scalacheck.Gen
import org.scalacheck.Gen._

object RandomExpr {

  val randomSplit: Gen[(Int, Int)] =
    for {
      s <- Gen.size
      s1 <- choose(0, s)
    } yield (s1, s - s1)

  def together[A, B](g1: Gen[A], g2: Gen[B]): Gen[(A, B)] = {
    for {
      (s1, s2) <- randomSplit
      e1 <- resize(s1, g1)
      e2 <- resize(s2, g2)
    } yield (e1, e2)
  }

  def sizedList[A](len: Int, gen: Gen[A]): Gen[Seq[A]] = {
    if (len <= 0) Gen.const(Seq())
    else
      for {
        s <- size
        avgSize = s / len
        each = for {
          eachSize <- choose(0, avgSize * 3 / 2)
          e <- resize(eachSize, gen)
        } yield e
        xs <- Gen.listOfN(len, each)
      } yield xs
  }

  val nameGen: Gen[Name] = {
    val randomName = for {
      h <- alphaChar
      len <- choose(0, 8)
      tail <- resize(len, alphaNumStr)
    } yield String.valueOf(h +: tail)
    randomName.retryUntil(!Parsing.keywordList.contains(_))
  }

  val varGen: Gen[PExpr] = nameGen.map(varFromString)

  val applyOrPairGen: Gen[PExpr] =
    for {
      (x, y) <- together(exprGen, exprGen)
      dice <- choose(0.0, 1.0)
    } yield {
      if (dice < 0.8) x.call(y) else pair(x, y)
    }

  val atomGen: Gen[PExpr] = {
    val intGen = Gen.choose(-100, 100).map(intValue)
    val boolGen = Gen.oneOf(true, false).map(boolValue)
    Gen.frequency((4, intGen), (1, boolGen))
  }

  val lambdaGen: Gen[PExpr] = for {
    vars <- listOfN(3, nameGen)
    body <- exprGen
  } yield {
    lambda(vars: _*)(body)
  }

  val bindingGen: Gen[Binding] =
    for {
      name <- nameGen
      e <- exprGen
    } yield (name, e)

  val whereGen: Gen[PExpr] = for {
    bindingNum <- choose(0, 3)
    bindingsGen = sizedList(bindingNum, bindingGen)
    (e, clauses) <- together(exprGen, bindingsGen)
  } yield e.where(clauses: _*)

  lazy val exprGen: Gen[PExpr] = Gen.sized { size =>
    if (size <= 1) oneOf(varGen, atomGen)
    else oneOf(varGen, applyOrPairGen, atomGen, lambdaGen, whereGen)
  }

}
