package learn.parlang

import learn.parlang.PExpr.Pair
import learn.parlang.Reduced.{EagerFunc, func}

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
    case IntValue(x) => Result(x == 0)
  }

  val isUnit = func("isUnit") {
    case UnitValue => Result(true)
    case _         => Result(false)
  }

  val greater = func("greater") {
    case IntValue(x) =>
      Result(func(s"greater $x") {
        case IntValue(y) => Result(BoolValue(x > y))
      })
  }

  val eagerPair = func("eagerPair") {
    case x =>
      Result {
        func("eager1") {
          case y => Result(pair(x, y))
        }
      }
  }

  val eager: EagerFunc = func("eager") {
    case Pair(left, right) =>
      Result {
        eagerPair.call(eager call left).call(eager call right)
      }
    case other => Result(other)
  }

  val map = let(
    "map",
    "f" ~> ("xs" ~>
      choose
        .call(isUnit.call("xs"))
        .call(unit)
        .call(let("x1", "f".call(fst.call("xs"))) {
          let("rest", snd.call("xs")) {
            pair("x1", "map".call("f").call("rest"))
          }
        })),
  ) { "map" }

  val all: PContext =
    (Seq(
      plus,
      times,
      fst,
      snd,
      choose,
      isZero,
      isUnit,
      greater,
      eagerPair,
      eager,
    ).map(p => p.name -> p)
      ++ Map("map" -> map)).map {
      case (n, f) =>
        n -> thunk(Map(), f)
    }.toMap
}
