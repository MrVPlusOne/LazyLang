package learn.parlang

object EvalLoop {

  private val example =
    """
      |eager (take 10 xs) where xs = [1,[2,xs]]
      |eager (take 10 xs) where xs = [1,2,xs]
    """.stripMargin

  def main(args: Array[String]): Unit = {

    while (true) {
      print(Console.BLACK + "λ> ")
      Console.flush()

      val code = Console.in.readLine()
      parseExpr(code).flatMap { p =>
        println(Console.BLUE + "(parsed) " + p)
        eval(StandardLib.all)(p)
      } match {
        case Right(r) =>
          println(Console.GREEN + r.expr)
        case Left(e) =>
          Console.println(Console.RED + e)
      }
    }
  }
}
