package vplusone.lazylang

object REPL {

  def main(args: Array[String]): Unit = {

    var ctx = StandardLib.all
    var resultId = 0

    while (true) {
      print(Console.BLACK + "λ> ")
      Console.flush()

      val code = Console.in.readLine()
      parseConsoleInput(code).flatMap {
        case Left(name -> expr) =>
          println(Console.BLUE + s"(binding parsed) $name := $expr")
          ctx = newCtxFromBindings(ctx, Seq(name -> expr))
          println(Console.YELLOW + s"$name defined.")
          Right(())
        case Right(e) =>
          println(Console.BLUE + s"(expr parsed) $e")
          eval(
            ctx,
            evalCallback =
              n => println(Console.BLUE + s"(evaluated in $n steps)"),
          )(e).map { r =>
            val resultName = s"res$resultId"
            ctx = ctx.updated(resultName, Evaluation.thunk(r.ctx, r.expr))
            println(Console.GREEN + s"$resultName = " + r.expr)
            resultId += 1
          }
      } match {
        case Right(_) =>
        case Left(e) =>
          Console.println(Console.RED + e)
      }
    }
  }
}
