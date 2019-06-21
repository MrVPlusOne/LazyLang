package learn

import parlang._

//noinspection TypeAnnotation
object ParLangExample {

  val incLambda = "x" ~> "plus".call(1).call("x")

  val incExample = let("f", incLambda)("f".call(4))

  val factExample = {
    val factBody = "x" ~>
      "choose"
        .call("greater".call("x").call(0))
        .call(
          "times"
            .call("x")
            .call(
              "fact"
                .call("plus".call("x").call(-1))
            )
        )
        .call(1)
    let("fact", "choose".call(true).call(5)) {
      "fact".call(3)
    }
  }

  val listExample = let("x", "plus".call(1).call(5))("print".call(pair("x", 2)))

  def main(args: Array[String]): Unit = {
    println {
      eval(factExample, StandardLib.all)
    }
  }

}
