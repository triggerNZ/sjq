package au.com.simplemachines.sjq

import au.com.simplemachines.sjq.Syntax._
import au.com.simplemachines.sjq.Tokens.StringLiteral
import fastparse.WhitespaceApi


object Parser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" \t\n".rep)
  }

  import fastparse.noApi._
  import White._

  import scalaz.syntax.std.boolean._

  object Tok {
    import Tokens._

    val neq          = P("!==")
    val eq           = P("==")
    val as           = P("as")
    val import_      = P("import")
    val include      = P("include")
    val module       = P("module")
    val def_         = P("def")
    val if_          = P("if")
    val then_        = P("then")
    val else_        = P("else")
    val elif         = P("elif")
    val and          = P("and")
    val or           = P("or")
    val end          = P("end")
    val reduce       = P("reduce")
    val foreach      = P("foreach")
    val definedor    = P("//")
    val try_         = P("try")
    val catch_       = P("catch")
    val label        = P("label")
    val break        = P("break")
    val loc          = P("__loc__")
    val setpipe      = P("|=")
    val setplus      = P("+=")
    val setminus     = P("-=")
    val setmult      = P("*=")
    val setdiv       = P("/=")
    val setmod       = P("%=")
    val setdefinedor = P("//=")
    val lesseq       = P("<=")
    val greatereq    = P(">=")
    val rec          = P("..")
    val alternation  = P("?//")

    val op           = CharIn(".?=;,:|+-*/%$<>").!
    val enterBracket = CharIn("[{(").!
    val exitBracket  = CharIn("]})").!
    val format: P[Token]      = "@" ~ CharIn('a' to 'z', 'A' to 'Z', '0' to '9').rep(1).!.map(Format)

    val numLiteral: P[NumLiteral] = {
      val digits        = P( CharIn('0' to '9').rep(1))
      val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
      val fractional    = P( "." ~ digits )
      val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

      P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map { str =>
        val numValue = str.toDouble
        val isInt = numValue == numValue.toInt.toDouble
        val isDouble = !isInt
        NumLiteral(
          isDouble.either(numValue) or (numValue.toInt)
        )
      }
    }

    val stringLiteral: P[StringLiteral] = {
      val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
      val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
      val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )
      val strChars = P( CharsWhile(!"\"\\".contains(_: Char)) )
      val string =
        P( "\"" ~/ (strChars | escape).rep.! ~ "\"")
      string
    }.map(StringLiteral)

    //TODO: Identifiers with modules
    val ident: P[Token] = P((CharIn('a' to 'z', 'A' to 'Z', "_") ~ CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_").rep).!).map(Identifier)
    val field: P[Field] = P("." ~ (CharIn('a' to 'z', 'A' to 'Z', "_") ~ CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_").rep).!).map(Field)

    val true_ = P("true").map(_ => BooleanLiteral(true))
    val false_ = P("false").map(_ => BooleanLiteral(false))
    val null_ = P("null")
  }


  val stringLiteralTerm: P[Term] = Tok.stringLiteral.map(lit => StringLiteralTerm(lit.value))
  val numberLiteralTerm: P[Term] = Tok.numLiteral.map(nl => NumberLiteralTerm(nl))

  val indexTerm: P[Term] = stringLiteralTerm | numberLiteralTerm

  //TODO handle than just empty dicts
  def mkDict: P[MkDict] = P("").map(_ => MkDict(List()))

  def term: P[Term] =
    stringLiteralTerm |
      (Tok.true_ | Tok.false_).map(BooleanLiteralTerm) |
      Tok.null_.map(_ => NullLiteralTerm) |
      ("{" ~ mkDict ~ "}").map(DictTerm) |
      ("[" ~ "]").map(_ => ArrayTerm())  |
      (Tok.field ~ "?".!.?).rep(1).map(fs  => FieldTerm(fs.toList.map {case (f, o) => (f, o.isDefined)})) |
    Tok.rec          .map(_   => RecTerm) |
//    P(".") ~ sliceExpr.? .map { ie => wrapSliceTerm(IdentityTerm, ie)} |
    P(".") ~ indexExpr.? .map { ie => wrapIndexTerm(IdentityTerm, ie)} |
    numberLiteralTerm

  def wrapIndexTerm(t: Term, idx: Option[IndexExpr]): Term = idx match {
    case None => t
    case Some(IndexExpr(e)) => IndexTerm(t, e)
  }

  def wrapSliceTerm(t: Term, idx: Option[SliceExpr]): Term = idx match {
    case None => t
    case Some(SliceExpr(e1, e2)) => SliceTerm(t, e1, e2)
  }

  //TODO this is a horrible hack
  def indexExpr: P[IndexExpr] = ("[" ~ indexTerm ~ "]").map {term =>
    IndexExpr(TermExp(term))
  }

  def sliceExpr: P[SliceExpr] = ("[" ~ numberLiteralTerm.? ~ ":" ~ numberLiteralTerm.? ~ "]").map { case (t1o, t2o) =>
    SliceExpr(t1o.map(TermExp), t2o.map(TermExp))
  }

  def exp: P[Exp] = term.map(TermExp)
}
