package learn.parlang

import fastparse.Parsed
import learn.parlang.PExpr.Binding

object Parsing {

  type ParsingError = String

  private object Impl {
    import fastparse._
    import fastparse.ScalaWhitespace._

    def letter[_: P]: P[Unit] = P(lowercase | uppercase)
    def lowercase[_: P]: P[Unit] = P(CharIn("a-z"))
    def uppercase[_: P]: P[Unit] = P(CharIn("A-Z"))
    def digit[_: P]: P[Unit] = P(CharIn("0-9"))
    def decimalinteger[_: P]: P[Int] =
      P(nonzerodigit ~ digit.rep | "0").!.map(_.toInt)
    def nonzerodigit[_: P]: P[Unit] = P(CharIn("1-9"))
    def pInt[_: P]: P[AtomValue] =
      pythonparse.Lexical.integer
        .map(_.toInt.pipe(intValue))
        .opaque("<integer>")

    def pBool[_: P]: P[AtomValue] =
      (P("True").map(_ => true) | P("False").map(_ => false))
        .map(boolValue)
        .opaque("<boolean>")

    def pAtom[_: P]: P[AtomValue] = P {
      pInt | pBool
    }

    val keywordList: Set[String] = Set(
      "where",
      "lam",
      "True", "False"
    )

    def pIdentifier[_: P]: P[String] = {
      import fastparse.NoWhitespace._
      P((letter | "_") ~ (letter | digit | "_").rep).!.filter(
        !keywordList.contains(_),
      )
    }

    def pVar[_: P]: P[PExpr] = P(pIdentifier).map(varFromString)

    def pWhere[_: P]: P[PExpr] =
      P(pApply ~ ("where" ~/ pClauses).?)
        .map {
          case (e, None) => e
          case (e, Some(clauses)) =>
            e.where(clauses: _*)
        }

    def pLambda[_: P]: P[PExpr] =
      P(("lam" | "λ") ~ pIdentifier.rep(1) ~ "." ~ pExpr)
        .map {
          case (vars, e) => lambda(vars: _*)(e)
        }

    def pPaired[_: P]: P[PExpr] =
      P("(" ~ pExpr.rep(0, ",") ~ ")")
        .map { xs =>
          ((xs: @unchecked) match {
            case Seq(e)    => e
            case Seq(a, b) => pair(a, b)
          }).asInstanceOf[PExpr]  // IDE bug
        }

    def pSeg[_: P]: P[PExpr] = P {
      pLambda | pList | pAtom | pVar | pPaired
    }

    def pApply[_: P]: P[PExpr] = P {
      pSeg.rep(1).map { es =>
        es.reduce(_ call _)
      }
    }

    def pList[_: P]: P[PExpr] =
      P("[" ~ pExpr.rep(sep = ","./) ~ "]").map(list(_: _*))

    def pClauses[_: P]: P[List[Binding]] = P {
      ("{" ~ pBinding.rep(sep = ";"./).map(_.toList) ~ "}") |
        pBinding.map(List(_))
    }

    def pBinding[_: P]: P[Binding] =
      P(pLhs ~ "=" ~ pExpr)
        .map {
          case (Right(v), e) => v -> e
          case (Left(vs), e) =>
            val f :: args = vs
            f -> lambda(args: _*)(e)
        }

    type LHS = Either[List[Name], Name]

    def pLhs[_: P]: P[LHS] =
      P(pIdentifier.rep(1)).map {
        case List(v) => Right(v)
        case more    => Left(more.toList)
      }

    def pExpr[_: P]: P[PExpr] = P {
      pWhere
    }

  }

  trait ParsingAPI {
    def parseExpr(text: String): Either[ParsingError, PExpr] = {
      fastparse.parse(text, Impl.pExpr(_)) match {
        case Parsed.Success(value, _) => Right(value)
        case f: Parsed.Failure =>
          Left(f.trace().longAggregateMsg + "\nText:\n" + text)
      }
    }
  }

  def main(args: Array[String]): Unit = {

    println {
      parseExpr(
        "(-96 where {gu = m; f = HQz; g4Nw = k}) 3",
      )
    }
  }
}
