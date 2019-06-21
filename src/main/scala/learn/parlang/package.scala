package learn

import cats.data.{Chain, EitherT, Reader}
import cats.implicits._

import scala.language.implicitConversions

package object parlang {
  import parlang.PExpr._
  import learn.parlang.Reduced.EagerFunc

  type Name = String

  sealed trait PExpr {
    def call(x: PExpr): PExpr = Apply(this, x)

    override def toString: String = this match {
      case Var(id) => id
      case Let(v, expr, body) =>
        s"let $v = $expr in $body"
      case Lambda(v, expr) =>
        s"(λ $v. $expr)"
      case Apply(f, x) =>
        s"$f($x)"
      case Pair(x, y) =>
        s"($x, $y)"
      case at: AtomValue => at.show
      case f: EagerFunc  => f.toString
    }
  }

  sealed trait Reduced extends PExpr

  sealed trait Applicable extends Reduced

  private[parlang] object PExpr {
    case class Var(id: Name) extends PExpr

    case class Let(v: Name, expr: PExpr, body: PExpr) extends PExpr

    case class Lambda(v: Name, expr: PExpr) extends Applicable

    case class Apply(f: PExpr, x: PExpr) extends PExpr

    case class Pair(left: PExpr, right: PExpr) extends Reduced
  }

  private[parlang] object Reduced {
    case class EagerFunc(name: String, f: Reduced => Result[PExpr])
        extends Applicable {
      override def toString: Name = s"<function: $name>"
    }

    def func(
        name: String,
    )(f: PartialFunction[Reduced, Result[PExpr]]): EagerFunc =
      EagerFunc(
        name,
        x =>
          if (f.isDefinedAt(x))
            addToTrace(ThunkValue(() => Map(), name.call(x)))(f(x))
          else Result.fail("Function undefined on value."),
      )
  }

  sealed trait AtomValue extends Reduced {
    def show: String

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

  def let(v: Name, e: PExpr)(body: PExpr): PExpr = Let(v, e, body)

  def pair(x: PExpr, y: PExpr): PExpr = Pair(x, y)

  val unit: AtomValue = AtomValue.UnitValue

  def list(xs: PExpr*): PExpr = xs.foldRight(unit: PExpr)(pair)

  type StackTrace = List[ThunkValue[PExpr]]

  def addToTrace[T](expr: ThunkValue[PExpr])(r: Result[T]): Result[T] = {
    EitherT(Reader { trace =>
      r.value(expr :: trace)
    })
  }

  type WithTrace[T] = Reader[StackTrace, T]

  case class TracedError(trace: StackTrace, message: String) {
    override def toString: Name = {
      message + "\n" + trace.mkString("\n")
    }
  }

  type Result[T] = EitherT[WithTrace, TracedError, T]

  private[parlang] object Result {
    def fail[T](message: String): Result[T] =
      EitherT(Reader { trace =>
        TracedError(trace, message).asLeft
      })

    def apply[T](v: T): Result[T] = v.pure[Result]
  }

  type PContext = Map[Name, Thunk]

  case class ThunkValue[+V](ctx: () => PContext, expr: V) {
    override def toString: Name = {
      s"$expr     -| ctx ${ctx().keySet}"
    }
  }

  private[parlang] class Thunk(var value: ThunkValue[PExpr]) {
    def expr: PExpr = value.expr
    def ctx: PContext = value.ctx()
  }

  private[parlang] def thunk(ctx: => PContext, expr: PExpr) =
    new Thunk(ThunkValue(() => ctx, expr))

  type ReducedThunk = ThunkValue[Reduced]

  val memoizing = true

  private def reduce(t: Thunk): Result[ReducedThunk] = {
    val (e, ctx) = (t.expr, t.ctx)
    addToTrace(t.value) {
      e match {
        case r: Reduced => Result(ThunkValue(() => ctx, r))
        case Var(id) =>
          ctx.get(id) match {
            case Some(t1) => reduce(t1)
            case None     => Result.fail(s"Undefined var: $id")
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
                  r <- reduce(thunk(xCtx(), e))
                } yield r
            }
          }

          for {
            ThunkValue(ctx1, f1) <- reduce(thunk(ctx, f))
            f2 <- asFunc(f1)
            r <- substitute(f2, thunk(ctx, x), ctx1())
          } yield r
        case Let(v, expr, body) =>
          lazy val exprThunk: Thunk = thunk(ctx1, expr)
          lazy val ctx1: PContext = ctx.updated(v, exprThunk)
          reduce(thunk(ctx1, body))
      }
    }
  }

  def eval(ctx: PContext, e: PExpr): Either[TracedError, ReducedThunk] = {
    reduce(thunk(ctx, e)).value.run(List())
  }

}
