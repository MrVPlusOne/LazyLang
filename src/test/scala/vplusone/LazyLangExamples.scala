package vplusone

import vplusone.lazylang._

//noinspection TypeAnnotation
object LazyLangExamples {
  import StandardLib._

  def cond(b: PExpr, e1: PExpr, e2: PExpr): PExpr =
    choose.call(b, e1, e2)

  val incExpr = "x" ~> "plus".call(1).call("x")

  val factExpr =
    parseExprGet(
      "fact where fact x = if greater x 0 " +
        "then times x (fact (plus x -1)) else 1",
    )

  val foldrPairExpr = {
    "xs" ~> "foldr".call("Pair", unit, "xs")
  }

  val listExample = let("x", "plus".call(1).call(5))("print".call(pair("x", 2)))

  val isEven = lets(
    "isEven" -> ("x" ~> cond(
      isZero.call("x"),
      true,
      cond(
        isZero.call(plus.call("x", -1)),
        false,
        "isOdd".call(plus.call("x", -1)),
      ),
    )),
    "isOdd" -> ("x" ~> cond(
      isZero.call("x"),
      false,
      "isEven".call(plus.call("x", -1)),
    )),
  )("isEven")

  def main(args: Array[String]): Unit = {
    val longList = list((0 to 20).map(intValue): _*)

    println {
      eval(StandardLib.all)("showList" call "take".call(8, longList))
    }
  }

}
