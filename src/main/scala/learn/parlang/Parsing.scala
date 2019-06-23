package learn.parlang

import learn.parlang.Evaluation.Binding

object Parsing {

  case class ParsingError(message: String) extends Error {
    override def toString: Name = s"Parsing error: $message"
  }

  val keywordList: Set[String] = Set(
    "where",
    "lam",
    "True",
    "False",
    "unit",
    "if",
    "then",
    "else",
  )

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
      pInt | pBool | P("unit").map(_ => unit)
    }

    def pIdentifier[_: P]: P[String] = {
      import fastparse.NoWhitespace._
      P((letter | "_") ~ (letter | digit | "_").rep).!.filter(
        !keywordList.contains(_),
      )
    }

    def pVar[_: P]: P[PExpr] = P(pIdentifier).map(varFromString)

    def pWhere[_: P]: P[PExpr] =
      P(pIf ~ ("where" ~/ pClauses).?)
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

    def pIf[_: P]: P[PExpr] =
      P("if" ~ pExpr ~ "then" ~ pExpr ~ "else" ~ pIf)
        .map { case (e1, e2, e3) => "choose".call(e1, e2, e3) } | pApply

    def pTuple[_: P]: P[PExpr] =
      P("[" ~ pExpr.rep(0, ",") ~ "]")
        .map { xs =>
          list(xs: _*)
        }

    def pSeg[_: P]: P[PExpr] = P {
      pLambda | pList | pAtom | pVar | pTuple | P("(" ~ pExpr ~ ")")
    }

    def pApply[_: P]: P[PExpr] = P {
      pSeg.rep(1).map { es =>
        es.reduce(_ call _)
      }
    }

    def pList[_: P]: P[PExpr] =
      P("[" ~ pExpr.rep(sep = ","./) ~ "]")
        .map(_.toVector)
        .map {
          case Vector() => unit
          case Vector(a: PExpr) => a
          case other =>
            val Vector(a, b) = other.takeRight(2)
            other.dropRight(2).foldRight(pair(a, b))(pair)
        }

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

    def pExpr[_: P]: P[PExpr] = {
      pWhere
    }

    def pInput[_: P]: P[Either[Binding, PExpr]] = {
      (pBinding.map(Left(_)) | pExpr.map(Right(_))) ~ End
    }

  }

  trait ParsingAPI {

    import fastparse._

    private def parseWithError[A](
                                   text: String,
                                   parser: P[_] => P[A],
                                 ): Either[ParsingError, A] = {
      parse(text, parser) match {
        case Parsed.Success(value, _) => Right(value)
        case f: Parsed.Failure =>
          Left(ParsingError(f.trace().longAggregateMsg + "\nText:\n" + text))
      }
    }

    def parseConsoleInput(
                           text: String,
                         ): Either[ParsingError, Either[Binding, PExpr]] =
      parseWithError(text, Impl.pInput(_))

    def parseExpr(text: String): Either[ParsingError, PExpr] =
      parseWithError(text, Impl.pExpr(_))


    def parseExprGet(text: String): PExpr = {
      parseExpr(text) match {
        case Right(v) => v
        case Left(e) => throw e
      }
    }
  }

  def main(args: Array[String]): Unit = {

    println {
      parseExpr(
        "if greater x 1 then 5 else 6 where x = 10",
      )
    }
  }
}
