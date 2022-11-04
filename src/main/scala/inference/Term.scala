package inference

sealed trait Term extends Product with Serializable { self =>
  import Term._

  def prettyPrint: String =
    self match {
      case Var(name)            => name
      case App(e1, e2)          => s"(${e1.prettyPrint} ${e2.prettyPrint})"
      case Lam(v, body)         => s"(λ${v.prettyPrint} → ${body.prettyPrint})"
      case Literal.LInt(value)  => value.toString
      case Literal.LBool(value) => value.toString
    }
}

object Term {
  final case class Var(name: String)       extends Term
  final case class App(e1: Term, e2: Term) extends Term
  final case class Lam(v: Var, e: Term)    extends Term

  sealed trait Literal extends Term

  object Literal {
    final case class LInt(value: Int)      extends Literal
    final case class LBool(value: Boolean) extends Literal
  }
}
