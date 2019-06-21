package learn

import parlang._

//noinspection TypeAnnotation
object ParLangExample {
  import StandardLib._

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

  val listExample = let("x", "plus".call(1).call(5))("print".call(pair("x", 2)))

  def main(args: Array[String]): Unit = {
    println {
      eval(
        StandardLib.all,
        "eager" call "map".call(incExpr).call(list(1, 2, 3, 4)),
      )
    }
  }

}
