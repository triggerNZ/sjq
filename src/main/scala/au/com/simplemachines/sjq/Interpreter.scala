package au.com.simplemachines.sjq

import Syntax._
import argonaut._
import Argonaut._

import scalaz._
import Scalaz._

object Interpreter {
  def runExp(exp: Exp, input: Json): Json = {
    exp match {
      case TermExp (t) =>   runTerm(t, input)

      case e => sys.error(s"Don't know how to handle expression $e")
    }
  }

  def runTerm(term: Term, input:Json): Json =
   termToFunction(term)(input)


  def termToFunction(term: Term): JqFunction = {
    term match {
      case IdentityTerm => identityF
      case FieldTerm(fields) => composeList(fields.map {
        case (Tokens.Field(name), optional) =>
          if (optional) optionalField(jString(name)) else strictField(jString(name))
      })
      case StringLiteralTerm(str) => constant(jString(str))
      case IndexTerm(term, index) => indexTerm(term, index)
      case SliceTerm(term, i1o, i2o) => sliceTerm(term, i1o, i2o)
      case NumberLiteralTerm(n) => constant(n.toJsNumber)
      case BooleanLiteralTerm(b) => constant(jBool(b.value))
      case NullLiteralTerm => constant(jNull)
      case DictTerm(MkDict(List())) => constant(Json())  //TODO extend with actual dicts
      case ArrayTerm() => constant(Json.array())         //TODO handle data in arrays
      case t => sys.error(s"Don't know how to handle term $t")
    }
  }

  //TODO: Do we need this?
  def expToFunction(exp: Exp): JqFunction = exp match {
    case FuncDefExp(exp) => ???
    case AsExp(term, patterns, exp) => ???
    case ReduceExp(term, patterns, exp1, exp2) => ???
    case Foreach3ExpExp(term, patterns, exp1, exp2, exp3) => ???
    case Foreach2ExpExp(term, patterns, exp1, exp2) => ???
    case TermExp(term) => termToFunction(term)
  }

  case class JqError(message: String)

  //A normal Jq Function
  case class JqFunction(fn: Json => Json) {
    def apply(in: Json): Json = fn(in)
  }

  def seqLength(input: Json): Int = {
    input.string.map(_.length) orElse input.array.map(_.length) getOrElse(sys.error("Can only get length of strings and arrays"))
  }

  def sliceTerm(trm: Term, startExp: Option[Exp], endExp: Option[Exp]) = JqFunction { input =>
    val len = seqLength(input)

    val termFunction = termToFunction(trm)

    val startFunction: JqFunction = startExp.fold(constant(jNumber(0)))(idx => expToFunction(idx))
    val endFunction  : JqFunction = endExp.fold(constant(jNumber(len)))(idx => expToFunction(idx))

    val term  = termFunction(input)
    val startIdx  =  startFunction(term).numberOr(sys.error("Number expected")).toInt.getOrElse(sys.error("Int expected"))
    val endIdx  =  endFunction(term).numberOr(sys.error("Number expected")).toInt.getOrElse(sys.error("Int expected"))
    val indices = startIdx until endIdx

    val functions = indices.map(idx => indexTerm(trm, TermExp(NumberLiteralTerm(Tokens.NumLiteral(idx.left)))))

    Json.array(functions.map(_(input)): _*)
  }

  def indexTerm(term: Term, index: Exp) = JqFunction { input =>
    //This is weird: we use input twice. Does it make sense?
    val termFunction = termToFunction(term)
    val indexFunction = expToFunction(index)

    val trm = termFunction(input)
    val idx = indexFunction(trm)

    val value = strictField(idx)(input)
    value
  }

  val identityF = JqFunction( input => input )

  def strictField(field: Json) = JqFunction { input =>
    val result: Json =
      if (field.isString) {
        input.fieldOr(field.stringOrEmpty, jNull)
      } else if (field.isNumber) {
        val jNum = field.numberOr(JsonLong(0))
        val suppliedIndex = jNum.toInt.getOrElse(sys.error("Integer index expected"))
        val array = input.arrayOrEmpty

        if (suppliedIndex >= 0 && suppliedIndex < array.length) {
          array(suppliedIndex)
        } else if (suppliedIndex < 0) {
          val inversedIndex = array.length + suppliedIndex  //the number is negative so we add instead of subtracting
          if (inversedIndex >= 0) {
            array(inversedIndex)
          } else jNull
        } else jNull
      } else {
        sys.error("String or Integer index expected")
      }

    result
  }

  def optionalField(field: Json) = strictField(field)   //this is horribly horribly wrong

  def constant(value: Json) = JqFunction { _ => value }

  def composeList(lst: List[JqFunction]): JqFunction = lst.foldLeft(identityF) { (acc, next) => JqFunction { input =>
      val previousResult = acc(input)
      val nextResult = next(previousResult)
    nextResult
    }
  }
}
