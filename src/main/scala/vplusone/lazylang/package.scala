package vplusone

import scala.language.implicitConversions
import cats.data.Chain
import cats.implicits._
import vplusone.lazylang.Evaluation._
import vplusone.lazylang.Parsing.ParsingAPI

package object lazylang
    extends EvaluationAPI
    with util.ChainingSyntax
    with ParsingAPI {
  import lazylang.PExpr._
  import lazylang.Reduced._

  type Name = String

  sealed trait PExpr {

    def call(xs: PExpr*): PExpr = xs.foldLeft(this)(Apply)

    def where(bindings: (Name, PExpr)*): PExpr =
      Where(this, bindings.toList)

    override def toString: Name = showLinear

    def showLinear: String = this match {
      case Var(id) =>
        assert(!id.contains(" "), s"Var name contains whitespace: '$id'")
        id
      case Where(body, bindings) =>
        val bindingList = {
          val clauses = bindings.map { case (v, e) => s"$v = $e" }
          if (clauses.length == 1)
            clauses.head.toString
          else clauses.mkString("{", "; ", "}")
        }
        s"($body where $bindingList)"
      case Lambda(v, expr) =>
        s"(λ $v. $expr)"
      case Apply(f, x) =>
        s"($f $x)"
      case at: AtomValue => at.show
      case f: EagerFunc  => f.toString
    }

    def freeVars: Set[Name]
  }

  sealed trait Reduced extends PExpr

  private[lazylang] object PExpr {

    case class Var(id: Name) extends PExpr {
      lazy val freeVars: Set[Name] = Set(id)
    }

    case class Where(body: PExpr, bindings: List[Binding]) extends PExpr {
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
  }

  private[lazylang] object Reduced {

    /** Either a lambda or an external function */
    sealed trait Applicable extends Reduced

    /** A function whose argument must be reduced first */
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
            Result.addToTrace(ThunkValue(Map(), name.call(x)))(f(x))
          else Result.fail("Function undefined on value."),
      )
  }

  sealed trait AtomValue extends Reduced {
    def show: String

    def freeVars: Set[Name] = Set()
  }

  private[lazylang] object AtomValue {
    case class IntValue(v: Int) extends AtomValue {
      def show: String = v.toString
    }

    case class BoolValue(b: Boolean) extends AtomValue {
      def show: String = if (b) "True" else "False"
    }

    case object UnitValue extends AtomValue {
      def show = "unit"
    }

    case class StrValue(repl: Chain[String]) extends AtomValue {
      lazy val show: String = repl.mkString_("'", "", "'")

      def concat(b: StrValue): StrValue = StrValue(repl ++ b.repl)
    }
  }

  // constructors
  implicit def varFromString(name: Name): PExpr = Var(name)

  implicit def intValue(v: Int): AtomValue = AtomValue.IntValue(v)

  implicit def boolValue(v: Boolean): AtomValue = AtomValue.BoolValue(v)

  def stringValue(v: String): AtomValue = AtomValue.StrValue(Chain(v))

  implicit class LambdaBuilder(v: Name) {
    def ~>(e: PExpr): PExpr = Lambda(v, e)
  }

  def lambda(vars: Name*)(body: PExpr): PExpr =
    vars.foldRight(body)((l, r) => l ~> r)

  def let(v: Name, e: PExpr)(body: PExpr): PExpr = Where(body, List(v -> e))

  def lets(bindings: (Name, PExpr)*)(body: PExpr): PExpr =
    Where(body, bindings.toList)

  val pairName = "Pair"

  def pair(x: PExpr, y: PExpr): PExpr = pairName.call(x, y)

  val unit: AtomValue = AtomValue.UnitValue

  def list(xs: PExpr*): PExpr = xs.foldRight(unit: PExpr)(pair)

}
