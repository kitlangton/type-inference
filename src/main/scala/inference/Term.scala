package inference

// Zymposium — Implementing Hindley-Milner-Langton!
// Today's Topic: Writing a type-inference algorithm from scratch!
//
// - abstruse, technical.
// - haskell

// - Type
// - Term
// infer: Term -> Option[Type]
// "well typed"
//
// eval: Term -> Value
sealed trait Term extends Product with Serializable { self =>
  import Term._

  def prettyPrint: String = self match {
    case Var(name)            => name
    case Apply(e1, e2)        => s"(${e1.prettyPrint} ${e2.prettyPrint})"
    case Lambda(v, body)      => s"(λ${v.prettyPrint} → ${body.prettyPrint})"
    case Literal.LInt(value)  => value.toString
    case Literal.LBool(value) => value.toString
    case IfThenElse(cond, thenBranch, elseBranch) =>
      s"(if ${cond.prettyPrint} then ${thenBranch.prettyPrint} else ${elseBranch.prettyPrint})"
  }
}

object Term {
  final case class Var(name: String)         extends Term
  final case class Apply(e1: Term, e2: Term) extends Term
  final case class Lambda(v: Var, e: Term)   extends Term
  // x -> (y -> x + y)
  // (x -> (y -> x + y)) 10
  // (y -> x + y) (x = 10)
  final case class IfThenElse(cond: Term, thenBranch: Term, elseBranch: Term) extends Term

  sealed trait Literal extends Term

  object Literal {
    final case class LInt(value: Int)      extends Literal
    final case class LBool(value: Boolean) extends Literal
  }
}
