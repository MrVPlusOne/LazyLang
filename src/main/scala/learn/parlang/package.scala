package learn

import cats.Eval
import cats.data.{Chain, EitherT, Reader, ReaderT}
import cats.implicits._

import scala.util.chaining._
import scala.language.implicitConversions

package object parlang {
  import parlang.PExpr._
  import learn.parlang.Reduced.EagerFunc

  type Name = String

  sealed trait PExpr {

    def call(xs: PExpr*): PExpr = xs.foldLeft(this)(Apply)

    def where(bindings: (Name, PExpr)*): PExpr =
      Where(this, bindings.toList)

    override def toString: String = this match {
      case Var(id) => id
      case Where(body, bindings) =>
        val bindingList = {
          val clauses = bindings.map { case (v, e) => s"$v = $e" }
          if (clauses.length > 1)
            clauses.mkString("{", "; ", "}")
          else clauses.mkString("; ")
        }
        s"($body where $bindingList)"
      case Lambda(v, expr) =>
        s"(λ $v. $expr)"
      case Apply(f, x) =>
        s"$f($x)"
      case Pair(x, y) =>
        s"($x, $y)"
      case at: AtomValue => at.show
      case f: EagerFunc  => f.toString
    }

    def freeVars: Set[Name]
  }

  sealed trait Reduced extends PExpr

  sealed trait Applicable extends Reduced

  private[parlang] object PExpr {
    case class Var(id: Name) extends PExpr {
      lazy val freeVars: Set[Name] = Set(id)
    }

    case class Where(body: PExpr, bindings: List[(Name, PExpr)]) extends PExpr {
      val freeVars: Set[Name] = {
        val (names, exprs) = bindings.unzip
        (exprs.foldMap(_.freeVars) ++ body.freeVars) -- names.toSet
      }
    }

    case class Lambda(v: Name, expr: PExpr) extends Applicable {
      lazy val freeVars: Set[Name] = expr.freeVars - v
    }

    case class Apply(f: PExpr, x: PExpr) extends PExpr {
      lazy val freeVars: Set[Name] = f.freeVars ++ x.freeVars
    }

    case class Pair(left: PExpr, right: PExpr) extends Reduced {
      lazy val freeVars: Set[Name] = left.freeVars ++ right.freeVars
    }
  }

  private[parlang] object Reduced {
    case class EagerFunc(name: String, f: Reduced => Result[PExpr])
        extends Applicable {
      override def toString: Name = s"<function: $name>"

      def freeVars: Set[Name] = Set()
    }

    def func(
        name: String,
    )(f: PartialFunction[Reduced, Result[PExpr]]): EagerFunc =
      EagerFunc(
        name,
        x =>
          if (f.isDefinedAt(x))
            addToTrace(ThunkValue(Map(), name.call(x)))(f(x))
          else Result.fail("Function undefined on value."),
      )
  }

  sealed trait AtomValue extends Reduced {
    def show: String

    def freeVars: Set[Name] = Set()
  }

  private[parlang] object AtomValue {
    case class IntValue(v: Int) extends AtomValue {
      def show: String = v.toString
    }

    case class BoolValue(b: Boolean) extends AtomValue {
      def show: String = b.toString
    }

    case object UnitValue extends AtomValue {
      def show = "unit"
    }
  }

  implicit def varFromString(name: Name): PExpr = Var(name)

  implicit def intValue(v: Int): AtomValue = AtomValue.IntValue(v)

  implicit def boolValue(v: Boolean): AtomValue = AtomValue.BoolValue(v)

  implicit class LambdaBuilder(v: Name) {
    def ~>(e: PExpr): PExpr = Lambda(v, e)
  }

  def let(v: Name, e: PExpr)(body: PExpr): PExpr = Where(body, List(v -> e))

  def lets(bindings: (Name, PExpr)*)(body: PExpr): PExpr =
    Where(body, bindings.toList)

  def pair(x: PExpr, y: PExpr): PExpr = Pair(x, y)

  val unit: AtomValue = AtomValue.UnitValue

  def list(xs: PExpr*): PExpr = xs.foldRight(unit: PExpr)(pair)

  type StackTrace = List[ThunkValue[PExpr]]

  def addToTrace[T](expr: ThunkValue[PExpr])(r: Result[T]): Result[T] = {
    EitherT(ReaderT { trace =>
      r.value.run(expr :: trace)
    })
  }

  type WithTrace[T] = ReaderT[Eval, StackTrace, T]

  case class TracedError(trace: StackTrace, message: String) {
    override def toString: Name = {
      message + "\n" + trace.mkString("\n")
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
  }

  type PContext = Map[Name, Thunk]

  case class ThunkValue[+V](ctx: PContext, expr: V) {
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

  type ReducedThunk = ThunkValue[Reduced]

  var memoizing = true

  def eval(ctx: PContext, maxSteps: Int = 5000)(
      e: PExpr,
  ): Either[TracedError, ReducedThunk] = {
    var steps = 0

    def reduce(t: Thunk): Result[ReducedThunk] = {
      steps += 1
      if (steps > maxSteps)
        Result.fail("Max steps hit.")
      else
        Result.defer {
          val result = addToTrace[ReducedThunk](t.value) {
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
        TracedError(List(), s"Undefined vars: ${undefinedVars.mkString(", ")}"),
      )
    else {
      val ctx1 = e.freeVars.map(k => k -> ctx(k)).toMap
      reduce(thunk(ctx1, e)).value
        .run(List())
        .value
        .tap { _ =>
          println(s"reduction steps: $steps")
        }
    }
  }

}
