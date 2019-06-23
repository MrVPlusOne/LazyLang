package learn

import parlang._

//noinspection TypeAnnotation
object ParLangExample {
  import StandardLib._

  def cond(b: PExpr, e1: PExpr, e2: PExpr): PExpr =
    choose.call(b, e1, e2)

  val incExpr = "x" ~> "plus".call(1).call("x")

  val factExpr = {
    val factBody = "x" ~>
      "choose"
        .call("greater".call("x").call(0))
        .call(
          "times"
            .call("x")
            .call(
              "fact"
                .call("plus".call("x").call(-1)),
            ),
        )
        .call(1)
    let("fact", factBody)("fact")
  }

  val foldrPairExpr = {
    "xs" ~> "foldr".call("mkPair", unit, "xs")
  }

  val listExample = let("x", "plus".call(1).call(5))("print".call(pair("x", 2)))

  val isEven = lets(
    "isEven" -> ("x" ~> cond(
      isZero.call("x"),
      true,
      cond(
        isZero.call(
          plus.call("x", -1)),
        false,
        "isOdd".call(plus.call("x", -1))
      )
    )),
    "isOdd" -> ("x" ~> cond(
      isZero.call("x"),
      false,
      "isEven".call(plus.call("x", -1))
    ))
  )("isEven")

  def main(args: Array[String]): Unit = {
    val longList = list((0 to 20).map(intValue): _*)

    println {
      eval(StandardLib.all)(eager call "take".call(8, longList))
    }
  }

}
