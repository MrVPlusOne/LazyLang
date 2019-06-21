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
          if (f.isDefinedAt(x)) f(x)
          else Result.fail("Function undefined on value."),
      )
  }

  sealed trait AtomValue extends Reduced

  private[parlang] object AtomValue {
    case class IntValue(v: Int) extends AtomValue

    case class BoolValue(b: Boolean) extends AtomValue

    case object UnitValue extends AtomValue
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

  type StackTrace = List[PExpr]

  def addToTrace[T](expr: PExpr)(r: Result[T]): Result[T] = {
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

  type PContext = Map[Name, PExpr]

  private def reduce(e: PExpr, ctx: PContext): Result[Reduced] =
    addToTrace(e) {
      e match {
        case r: Reduced => Result(r)
        case Var(id) =>
          ctx.get(id) match {
            case Some(v) => reduce(v, ctx)
            case None    => Result.fail(s"Undefined var: $id")
          }
        case l: Lambda => Result(l)
        case Apply(f, x) =>
          def asFunc(r: Reduced): Result[Applicable] = r match {
            case f: Applicable => Result(f)
            case _             => Result.fail(s"Term $r used as function.")
          }

          def substitute(
              f: Applicable,
              x: PExpr,
              ctx: PContext,
          ): Result[Reduced] = {
            f match {
              case Lambda(v, expr) =>
                reduce(expr, ctx.updated(v, x))
              case a: EagerFunc =>
                for {
                  xV <- reduce(x, ctx)
                  e <- a.f(xV)
                  r <- reduce(e, ctx)
                } yield r
            }
          }

          for {
            f1 <- reduce(f, ctx) >>= asFunc
            r <- substitute(f1, x, ctx)
          } yield r
        case Let(v, expr, body) =>
          reduce(body, ctx.updated(v, expr))
        case p: Pair => Result(p)
      }
    }

  def eval(e: PExpr, ctx: PContext): Either[TracedError, Reduced] = {
    reduce(e, ctx).value.run(List())
  }

}
