package inference

import inference.Term.{Literal, Var}
import inference.Type.{TArr, TCon, TVar}

sealed trait Type extends Product with Serializable { self =>
  override def toString: String =
    self match {
      case Type.TCon(name)   => name
      case Type.TArr(t1, t2) => s"${t1.withParens} -> $t2"
      case TVar(name)        => name
    }

  def withParens: String =
    self match {
      case Type.TCon(name)   => name
      case Type.TArr(t1, t2) => s"(${t1.withParens} -> $t2)"
      case TVar(name)        => name
    }
}

object Type {
  // Int or Bool
  final case class TCon(name: String) extends Type
  // a -> Bool
  final case class TArr(t1: Type, t2: Type) extends Type
  // a
  final case class TVar(name: String) extends Type

  object TVar {
    var char = 'a'

    def fresh: TVar = {
      val tv = TVar(char.toString)
      char = (char + 1).toChar
      tv
    }
  }

  val intType: Type  = TCon("Int")
  val boolType: Type = TCon("Bool")
}

object Infer extends App {

  final case class Context(map: Map[Var, Type]) {
    def extend(x: Var, tv: TVar): Context =
      Context(map + (x -> tv))

    def apply(v: Var): Type =
      map.getOrElse(v, throw new RuntimeException(s"Variable not found: ${v.name}"))
  }

  object Context {
    val empty: Context = Context(Map.empty)
  }

  final case class Knowledge(map: Map[TVar, Type]) {
    def ++(that: Knowledge): Knowledge =
      Knowledge(that.map.map { case (tv, ty) =>
        tv -> apply(ty)
      } ++ map)

    def apply(t: Type): Type =
      t match {
        case con: Type.TCon    => con
        case Type.TArr(t1, t2) => Type.TArr(apply(t1), apply(t2))
        case tVar: TVar        => map.getOrElse(tVar, tVar)
      }

    def apply(ctx: Context): Context =
      Context(ctx.map.map { case (v, t) =>
        v -> apply(t)
      })
  }

  object Knowledge {
    val empty: Knowledge = Knowledge(Map.empty)
  }

  def inferTop(term: Term): Type = {
    Type.TVar.char = 'a'
    infer(term, Context.empty)._2
  }

  def unify(t1: Type, t2: Type): Knowledge =
    (t1, t2) match {
      case (tv1: TVar, tv2: TVar) if tv1 == tv2 =>
        Knowledge.empty

      case (tv: TVar, t) if !occurs(tv, t) => Knowledge(Map(tv -> t))
      case (t, tv: TVar) if !occurs(tv, t) => Knowledge(Map(tv -> t))

      case (TArr(a, b), TArr(c, d)) =>
        val k1 = unify(a, c)
        val k2 = unify(k1(b), k1(d))
        k2 ++ k1

      case (t1: TCon, t2: TCon) if t1 == t2 =>
        Knowledge.empty

      case (t1, t2) =>
        throw new RuntimeException(s"Cannot unify $t1 with $t2")
    }

  def occurs(tvar: TVar, t: Type): Boolean =
    t match {
      case `tvar`       => true
      case TArr(t1, t2) => occurs(tvar, t1) || occurs(tvar, t2)
      case _            => false
    }

  def infer(term: Term, ctx: Context): (Knowledge, Type) =
    term match {
      case literal: Term.Literal =>
        val resultType = literal match {
          case Literal.LInt(_)  => Type.intType
          case Literal.LBool(_) => Type.boolType
        }
        Knowledge.empty -> resultType

      case v: Term.Var =>
        Knowledge.empty -> ctx(v)

      // x -> if x then 10 else 20
      // x -> e
      case Term.Lambda(x, e) =>
        val tv       = Type.TVar.fresh
        val (k1, t1) = infer(e, ctx.extend(x, tv))
        k1 -> Type.TArr(k1(tv), k1(t1))

      // (\id -> if true then id 10 else id 20) (\x -> x)
      case Term.Apply(f, e) =>
        val (k1, t1) = infer(f, ctx)
        val (k2, t2) = infer(e, k1(ctx))
        val tv       = TVar.fresh
        val k3       = unify(k2(t1), TArr(t2, tv))
        k3 ++ k2 ++ k1 -> k3(tv)

      case Term.IfThenElse(cond, thenBranch, elseBranch) =>
        val (k1, condType) = infer(cond, ctx)
        val (k2, thenType) = infer(thenBranch, k1(ctx))
        val (k3, elseType) = infer(elseBranch, k2(ctx))
        val k4             = unify(k3(condType), Type.boolType)
        val k5             = unify(k4(thenType), k4(elseType))
        k5 ++ k4 ++ k3 ++ k2 ++ k1 -> k5(thenType)
    }

  val boolTerm = Parsers.parse("true")
//  println(inferTop(boolTerm))

  val intTerm = Parsers.parse("10")
//  println(inferTop(intTerm))

  val lambdaTerm = Parsers.parse("""\x -> (\y -> x)""")
//  println(inferTop(lambdaTerm))

  def identity[A](x: A): A = x
  // Lambda
  // term: \x -> x
  // type:  a -> a
  val t1 = Parsers.parse("""\x -> x""") // a -> a
//  println("")
//  println(Eval.eval(t1))
//  println(inferTop(t1))

  // Application
  // term: (\x -> x) (if true then 10 else 20)
  val t2 = Parsers.parse("""(\x -> true) 10""") // a
//  println(Eval.eval(t2))

  // \x -> if x then 10 else 20
  // type: Bool -> Int
  //  x: a
  //
  // \x -> if x then 10 else 20
  val t20 = Parsers.parse("""\x -> if x then 10 else 20""") // Bool -> Int
//  println(Eval.eval(t20))
//  println(inferTop(t20))

  // if 50 then 10 else 20
  // String -> Term -> (Term, Type) -> Value
  val ex100: Term = Parsers.parse("""if 50 then 10 else 20""")
//  println(Eval.eval(ex100))

  // term: \x -> \y -> x
  // type: a -> b -> a
  val t3 = Parsers.parse("""(\x -> \y -> x)""") // 10

  // term: (\x -> \y -> \x)(true)(10)
  // type: Boolean
//  val t4 = Parsers.parse("""(\x -> (\y -> \x)) true 10""") // Boolean

  // undecidable!
  // (\id -> if true then id 10 else id 20) (\x -> x)
  val t4 = Parsers.parse("""\id -> id 10""")
  println(t4)
//  val t4 = Parsers.parse("""(\id -> if true then id 10 else id 20)""")
  val t5 = Parsers.parse("""(\id -> if true then (id 10) else 20)""") // 10
//  println("wacky")
  println(inferTop(t5))
  println(Eval.eval(t5))

}
