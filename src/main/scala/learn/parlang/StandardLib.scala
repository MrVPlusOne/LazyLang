package learn.parlang

import learn.parlang.PExpr.Pair
import learn.parlang.Reduced.{EagerFunc, func}

//noinspection TypeAnnotation
object StandardLib {
  import AtomValue._
  import Evaluation._

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

  val or = func("or") {
    case BoolValue(x) =>
      Result(func(s"or $x") {
        case BoolValue(y) => Result(x || y)
      })
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

  val mkPair = "x" ~> ("y" ~> pair("x", "y"))

  /* foldr f z []     = z
   * foldr f z (x:xs) = f x (foldr f z xs)
   *  */
  val foldr =
    "foldr".where(
      "foldr" ->
        "f" ~>
          ("x0" ~>
            ("xs" ~> {
              choose.call(
                isUnit.call("xs"),
                "x0",
                "f".call(
                  fst.call("xs"),
                  "foldr".call("f", "x0", snd.call("xs")),
                ),
              )
            })),
    )

  val take = let(
    "take",
    "n" ~>
      ("xs" ~>
        choose
          .call(or.call(isUnit.call("xs"), isZero.call("n")))
          .call(unit)
          .call(
            pair(
              fst.call("xs"),
              "take".call(
                plus.call("n", -1),
                snd.call("xs"),
              ),
            ),
          )),
  )("take")

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
      ++ Map(
      "map" -> map,
      "foldr" -> foldr,
      "mkPair" -> mkPair,
      "take" -> take,
    )).map {
      case (n, f) =>
        n -> thunk(Map(), f)
    }.toMap
}
