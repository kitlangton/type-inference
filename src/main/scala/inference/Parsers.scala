package inference

import zio.parser.Parser

object Parsers extends App {

  def literal(str: String): Parser[String, Char, Unit] =
    Parser.string(str, ())

  lazy val reserved = Set("if", "then", "else")

  lazy val ident: Parser[String, Char, String] =
    Parser.alphaNumeric.repeat.string
      .filter((res: String) => !reserved.contains(res), "reserved word")

  private lazy val startLam = Parser.char('\\') | Parser.char('λ')
  private lazy val lamArrow = Parser.char('→') | literal("->")
  lazy val lam =
    (startLam ~ variable ~ lamArrow ~ spaces ~ term)
      .map { case (v, term) => Term.Lambda(v, term) }

  lazy val spaces =
    Parser.char(' ').repeat0.unit

  lazy val variable: Parser[String, Char, Term.Var] =
    ident
      .map(Term.Var(_))
      .surroundedBy(spaces)

  lazy val literalInt: Parser[String, Char, Term.Literal.LInt] =
    Parser.digit.repeat.string
      .map(_.toInt)
      .map(Term.Literal.LInt(_))
      .surroundedBy(spaces)

  lazy val literalBool: Parser[String, Char, Term.Literal.LBool] =
    (literal("true").as(true) | literal("false").as(false))
      .map(Term.Literal.LBool(_))
      .surroundedBy(spaces)

  lazy val parenthesizedTerm: Parser[String, Char, Term] =
    Parser.char('(') ~ term ~ Parser.char(')')

  lazy val literal = literalInt | literalBool

  lazy val ifThenElse =
    (spaces ~ literal("if") ~ spaces ~ term
      ~ spaces ~ literal("then") ~ spaces ~ term ~ spaces ~ literal(
        "else"
      ) ~ spaces ~ term ~ spaces)
      .map { case (cond, thenBranch, elseBranch) => Term.IfThenElse(cond, thenBranch, elseBranch) }

  lazy val term0: Parser[String, Char, Term] =
    parenthesizedTerm | ifThenElse | lam | literal | variable

  lazy val term: Parser[String, Char, Term] =
    term0.flatMap { t =>
      term0.repeat
        .map { ts =>
          ts.foldLeft(t) { case (acc, t) => Term.Apply(acc, t) }
        }
        .orElse(Parser.succeed(t))
    }

  val example = """\x -> (if true then 10 else 10)"""

  term.parseString(example) match {
    case Left(error) => println(error)
    case Right(result) =>
      println(result)
      println(result.prettyPrint)
  }

  def parse(str: String): Term =
    term.parseString(str).toOption.get
}
