package inference

import inference.Term.{Literal, Var}

sealed trait Value extends Product with Serializable

object Value {
  final case class VInt(int: Int)                        extends Value
  final case class VBool(boolean: Boolean)               extends Value
  final case class Closure(v: Var, body: Term, env: Env) extends Value
}

final case class Env(map: Map[Var, Value]) {
  def ++(env2: Env) = Env(map ++ env2.map)

  def extend(x: Var, v: Value): Env =
    Env(map + (x -> v))

  def apply(v: Var): Value =
    map.getOrElse(v, throw new RuntimeException(s"Variable not found: ${v.name}"))
}

object Env {
  val empty: Env = Env(Map.empty)
}

object Eval extends App {
  import Value._

  def eval(term: Term): Value =
    eval(term, Env.empty)

  def eval(term: Term, env: Env): Value =
    term match {
      case v: Var =>
        env(v)

      case Term.Apply(e1, e2) =>
        // TODO: Flush out bugs. Write some tests
        val Closure(x, e, env2) = eval(e1, env)
        val v                   = eval(e2, env)
        eval(e, env.extend(x, v) ++ env2)

      case Term.Lambda(v, e) =>
        Closure(v, e, env)

      case Term.IfThenElse(cond, thenBranch, elseBranch) =>
        val VBool(bool) = eval(cond, env)
        if (bool) eval(thenBranch, env)
        else eval(elseBranch, env)

      case literal: Term.Literal =>
        literal match {
          case Literal.LInt(value)  => VInt(value)
          case Literal.LBool(value) => VBool(value)
        }
    }

}
