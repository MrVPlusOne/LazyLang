package learn.parlang

import learn.parlang.PExpr.Pair
import learn.parlang.Reduced.func

//noinspection TypeAnnotation
object StandardLib {
  import AtomValue._

  val plus = func("plus") {
    case IntValue(x) =>
      Result(func(s"plus $x") {
        case IntValue(y) =>
          Result(intValue(x + y))
      })
  }

  val times = func("times") {
    case IntValue(x) =>
      Result(func(s"times $x") {
        case IntValue(y) =>
          Result(intValue(x * y))
      })
  }

  val fst = func("fst") {
    case Pair(left, _) => Result(left)
  }

  val snd = func("snd") {
    case Pair(_, right) => Result(right)
  }

  val choose = func("choose") {
    case BoolValue(b) =>
      Result {
        if (b) "x" ~> ("y" ~> "x")
        else "x" ~> ("y" ~> "y")
      }
  }

  val isZero = func("isZero") {
    case IntValue(x) => Result(BoolValue(x == 0))
  }

  val greater = func("greater") {
    case IntValue(x) =>
      Result(func(s"greater $x") {
        case IntValue(y) => Result(BoolValue(x > y))
      })
  }

  val all: PContext = Seq(plus, fst, snd, choose, isZero, greater).map { p =>
    p.name -> p
  }.toMap
}
