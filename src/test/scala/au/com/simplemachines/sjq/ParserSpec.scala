package au.com.simplemachines.sjq

import au.com.simplemachines.sjq.Syntax.IndexTerm
import org.specs2.Specification
import fastparse.all._

import scalaz._
import syntax.either._

object ParserSpec extends Specification {
  import Parser._
  import Tokens._

  def is =
    s2"""
       Tokens
         format           ${ "@uri".p(Tok.format) === Format("uri") }
         float literals   $numLiterals
         identifiers      ${"foo".p(Tok.ident) === Identifier("foo")}
         strings          ${"\"hello world\"".p(Tok.stringLiteral) === StringLiteral("hello world")}
       Slices and indices
         String index     ${".[\"Hello\"]".p(Parser.term).isInstanceOf[IndexTerm] === true}
    """


  def numLiterals = {
    Seq(
      "3".p(Tok.numLiteral) === NumLiteral(3.left),
      "3.14159".p(Tok.numLiteral) === NumLiteral(3.14159.right),
      "6.67408e-11".p(Tok.numLiteral) === NumLiteral(6.67408e-11.right)
    )
  }


  implicit class UnsafeParse(self: String) {
    def p[T](parser: P[T]): T = parser.parse(self).get.value
    def isFailed[T](parser:P[T]) = parser.parse(self) match {
      case Parsed.Success(_, _) => false
      case Parsed.Failure(_, _, _) => true
    }
  }
}
