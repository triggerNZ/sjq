package au.com.simplemachines.sjq

import argonaut._, Argonaut._

import scalaz._

object Tokens {
  sealed trait Token
  case class Format(name: String) extends Token
  case class NumLiteral(value: Int \/ Double) extends Token {
    import argonaut._
    def toJsNumber: Json = value.fold(jNumber(_), jNumber(_))
  }
  case class StringLiteral(value: String) extends Token
  case class Identifier(name: String) extends Token
  case class Field(name: String) extends Token
}

object Syntax {
  sealed trait TopLevel
  case class TopLevelModImpExp(module: Module, imports: Imports, exp: Exp) extends TopLevel
  case class TopLevelModImpFunc(module: Module, imports: Imports, funcDefs: FuncDefs) extends TopLevel

  sealed trait Module
  case object EmptyModule extends Module
  case class ModuleExp(exp: Exp) extends Module

  sealed trait Imports
  case object EmptyImports extends Imports
  case class ImportImports(import_ : Import, imports: Imports) extends Imports

  sealed trait FuncDefs
  case object EmptyFuncDefs extends FuncDefs
  case class FuncDefFuncDefs(funcDef: FuncDef, funcDefs: FuncDefs) extends FuncDefs

  sealed trait Exp
  case class FuncDefExp(exp: Exp) extends Exp
  case class AsExp(term: Term, patterns: Patterns, exp: Exp) extends Exp
  case class ReduceExp(term: Term, patterns: Patterns, exp1: Exp, exp2:Exp) extends Exp
  case class Foreach3ExpExp(term: Term, patterns: Patterns, exp1: Exp, exp2: Exp, exp3: Exp) extends Exp
  case class Foreach2ExpExp(term: Term, patterns: Patterns, exp1: Exp, exp2: Exp) extends Exp

  case class TermExp(term: Term) extends Exp

  sealed trait Term
  case object IdentityTerm extends Term
  case object RecTerm extends Term
  case class FieldTerm(fields: List[(Tokens.Field, Boolean)]) extends Term
  case class StringLiteralTerm(value: String) extends Term
  case class NumberLiteralTerm(value: Tokens.NumLiteral) extends Term
  case class IndexTerm(term: Term, idxExpr: Exp) extends Term
  case class SliceTerm(term: Term, fromIdx: Option[Exp], toIdx: Option[Exp]) extends Term



  sealed trait Import
  sealed trait FuncDef

  sealed trait Patterns


  // The below are incomplete things for holding stuff
  case class IndexExpr(e: Exp)
  case class SliceExpr(e1: Option[Exp], e2: Option[Exp])
}