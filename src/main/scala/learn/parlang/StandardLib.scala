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

  val not = func("not") {
    case BoolValue(x) => Result(!x)
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


  val basicCtx: PContext =
    Seq(
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
      or,
      not,
    ).map { p =>
      p.name -> thunk(Map(), p)
    }.toMap

  val all: PContext = {
    val defs = Seq(
      "map f xs = if isUnit xs then unit else [x1, map f rest] " +
        "where {x1 = f (fst xs); rest = snd xs}",
      "take n xs = if or (isUnit xs) (isZero n) then unit else [fst xs, take (plus n -1) (snd xs)]",
      "nats = [1, map (plus 1) nats]"
    )
    val bindings = defs.map { s =>
      parseConsoleInput(s)
        .getOrElse(throw new Error(s"Failed to parse lib def: $s"))
        .left.getOrElse(throw new Error(s"Parsed as expression in lib def: $s"))
    }
    newCtxFromBindings(basicCtx, bindings)
  }
}
