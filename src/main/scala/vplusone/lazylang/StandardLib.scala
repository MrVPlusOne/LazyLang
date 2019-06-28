package vplusone.lazylang

import vplusone.lazylang.Reduced.{EagerFunc, func}

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

  val showAtom: EagerFunc = func("showAtom") {
    case a: AtomValue => Result(stringValue(a.show))
  }

  val isAtom = func("isAtom") {
    case x => Result(x.isInstanceOf[AtomValue])
  }

  val strConcat = func("strConcat") {
    case a: StrValue =>
      Result(func(s"strConcat $a") {
        case b: StrValue => Result(a.concat(b))
      })
  }

  val basicCtx: PContext =
    Seq(
      plus,
      times,
      choose,
      isZero,
      isUnit,
      greater,
      showAtom,
      isAtom,
      strConcat,
      or,
      not,
    ).map { p =>
      p.name -> thunk(Map(), p)
    }.toMap

  val all: PContext = {
    val defs = Seq(
      s"$pairName = lam x y f. f x y",
      "left x y = x",
      "right x y = y",
      "fst p = p left",
      "snd p = p right",
      "showList x = if isUnit x then 'unit' else wrap (strConcat (strConcat head ', ') tail) " +
        "where {head = showAtom (fst x); tail = showList (snd x); " +
        "  wrap s = strConcat (strConcat '[' s) ']'}",
      "map f xs = if isUnit xs then unit else [x1, map f rest] " +
        "where {x1 = f (fst xs); rest = snd xs}",
      "take n xs = if or (isUnit xs) (isZero n) then unit else [fst xs, take (plus n -1) (snd xs)]",
      "foldr f z xs = if isUnit xs then unit else f (fst xs) (foldr f z (snd xs))",
      "foldl f z xs = if isUnit xs then unit else foldl f (f z (fst xs)) (snd xs)",
      "nats = [1, map (plus 1) nats]",
      "get n xs = if (isZero n) then fst xs else get (plus n -1) (snd xs)",
      "zipWith f xs ys = if or (isUnit xs) (isUnit ys) then unit else " +
        "[f (fst xs) (fst ys), zipWith f (snd xs) (snd ys)]",
    )
    val bindings = defs.map { s =>
      parseConsoleInput(s)
        .getOrElse(throw new Error(s"Failed to parse lib def: $s"))
        .left
        .getOrElse(throw new Error(s"Parsed as expression in lib def: $s"))
    }
    newCtxFromBindings(basicCtx, bindings)
  }
}
