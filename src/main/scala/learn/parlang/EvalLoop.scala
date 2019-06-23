package learn.parlang

object EvalLoop {

  def input(code: String): Unit = {
    parseExpr(code).flatMap(eval(StandardLib.all)) match {
      case Right(r) =>
        println(r.expr)
      case Left(e) =>
        Console.err.println(e)
    }

  }

  def main(args: Array[String]): Unit = {
    while (true) {
      print("λ> ")
      val code = Console.in.readLine()
      input(code)
    }
  }
}
