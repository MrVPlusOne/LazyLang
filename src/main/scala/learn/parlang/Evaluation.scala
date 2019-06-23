package learn.parlang

import cats.implicits._

import scala.util.chaining._
import cats.data.{EitherT, ReaderT}
import cats.Eval

object Evaluation {

  type StackTrace = List[ThunkValue[PExpr]]

  type WithTrace[T] = ReaderT[Eval, StackTrace, T]

  val maxTraceToShow = 20

  case class TracedError(trace: StackTrace, message: String) extends Error {
    override def toString: String = {
      val exceed = trace.length - maxTraceToShow
      val traceStr =
        if (exceed <= 0) trace.mkString("\n")
        else {
          trace
            .take(maxTraceToShow)
            .mkString("\n") + s"\n ... ($exceed more hidden) ..."
        }
      "Reduction error: " + message + "\n" + traceStr
    }
  }

  type Result[T] = EitherT[WithTrace, TracedError, T]

  private[parlang] object Result {
    def fail[T](message: String): Result[T] =
      EitherT(ReaderT { trace =>
        Eval.now(TracedError(trace, message).asLeft)
      })

    def apply[T](v: T): Result[T] = v.pure[Result]

    def defer[T](f: => Result[T]): Result[T] = {
      Result(()).flatMap(_ => f)
    }

    def addToTrace[T](expr: ThunkValue[PExpr])(r: Result[T]): Result[T] = {
      EitherT(ReaderT { trace =>
        r.value.run(expr :: trace)
      })
    }
  }

  type PContext = Map[Name, Thunk]

  type Binding = (Name, PExpr)

  type ReducedThunk = ThunkValue[Reduced]

  private[parlang] case class ThunkValue[+V](ctx: PContext, expr: V) {
    override def toString: Name = {
      s"$expr     -| ctx ${ctx.keySet}"
    }
  }

  private[parlang] class Thunk(var value: ThunkValue[PExpr]) {
    def expr: PExpr = value.expr
    def ctx: PContext = value.ctx
  }

  private[parlang] def thunk(ctx: PContext, expr: PExpr) =
    new Thunk(ThunkValue(ctx, expr))

  trait EvaluationAPI {
    import PExpr.{Apply, Lambda, Var, Where}
    import Reduced.{Applicable, EagerFunc}

    def parseEval(code: String): Either[Error, ReducedThunk] = {
      parseExpr(code).flatMap(eval(StandardLib.all))
    }

    def eval(
              ctx: PContext,
              maxSteps: Int = 10000,
              memoizing: Boolean = true,
              evalCallback: Int => Unit = _ => (),
            )(
        e: PExpr,
    ): Either[TracedError, ReducedThunk] = {
      var steps = 0

      def reduce(t: Thunk): Result[ReducedThunk] = {
        steps += 1
        if (steps > maxSteps)
          Result.fail(s"Max steps ($maxSteps) hit.")
        else
          Result.defer {
            val result = Result.addToTrace[ReducedThunk](t.value) {
              val (e, ctx) = (t.expr, t.ctx)
              e match {
                case r: Reduced => Result(ThunkValue(ctx, r))
                case Var(id) =>
                  ctx.get(id) match {
                    case Some(t1) => reduce(t1)
                    case None     => Result.fail(s"Undefined var: $id.")
                  }
                case Apply(f, x) =>
                  def asFunc(r: Reduced): Result[Applicable] = r match {
                    case f: Applicable => Result(f)
                    case _             => Result.fail(s"Term $r used as function.")
                  }

                  def substitute(
                      f: Applicable,
                      x: Thunk,
                      ctx: PContext,
                  ): Result[ReducedThunk] = {
                    f match {
                      case Lambda(v, expr) =>
                        reduce(thunk(ctx.updated(v, x), expr))
                      case a: EagerFunc =>
                        for {
                          ThunkValue(xCtx, xV) <- reduce(x)
                          e <- a.f(xV)
                          r <- reduce(thunk(xCtx, e))
                        } yield r
                    }
                  }

                  for {
                    ThunkValue(ctx1, f1) <- reduce(thunk(ctx, f))
                    f2 <- asFunc(f1)
                    r <- substitute(f2, thunk(ctx, x), ctx1)
                  } yield r
                case Where(body, bindings) =>
                  val emptyThunks = bindings.map {
                    case (v, _) =>
                      v -> new Thunk(null) // initialize thunks later
                  }
                  val ctx1: PContext = ctx ++ emptyThunks
                  bindings.foreach {
                    case (v, e1) =>
                      ctx1(v).value = ThunkValue(ctx1, e1)
                  }
                  reduce(thunk(ctx1, body))
              }
            }
            if (memoizing)
              result.map { r =>
                t.value = r
                r
              } else result
          }
      }

      val undefinedVars = e.freeVars -- ctx.keySet
      if (undefinedVars.nonEmpty)
        Left(
          TracedError(
            List(),
            s"Checking failed, undefined vars: ${undefinedVars.mkString(", ")}",
          ),
        )
      else {
        val ctx1 = e.freeVars.map(k => k -> ctx(k)).toMap
        reduce(thunk(ctx1, e)).value
          .run(List())
          .value
          .tap { _ =>
            evalCallback(steps)
          }
      }
    }

    /** also useful for REPL's let bindings */
    def newCtxFromBindings(ctx: PContext, bindings: Seq[Binding]): PContext = {
      val emptyThunks = bindings.map {
        case (v, _) =>
          v -> new Thunk(null) // initialize thunks later
      }
      val ctx1: PContext = ctx ++ emptyThunks
      bindings.foreach {
        case (v, e1) =>
          ctx1(v).value = ThunkValue(ctx1, e1)
      }
      ctx1
    }

  }
}
