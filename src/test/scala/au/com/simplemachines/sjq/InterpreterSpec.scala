package au.com.simplemachines.sjq

import argonaut._, Argonaut._

import org.specs2.Specification

object InterpreterSpec extends Specification {
  def is = s2"""
      Identity                    $identityS
      Object identifier index     $objectIdentifierIndex
      Optional object index       $optionalObjectIndex
      Recursive descent           $recursiveDescent
      Literals                    $literals
      Array index                 $arrayIndex
      Slice                       $slice
      Dictionaries                $dicts
      Arrays                     $arrays
    """

  def identityS = Seq(
    ".".on(jString("Hello, World")) === jString("Hello, World"),
    ".".on(jString(0xFEFF.toChar + "Hello, World")) === jString(0xFEFF.toChar + "Hello, World")

  )

  def objectIdentifierIndex = Seq(
    ".foo".on(Json("foo" := 42, "bar" := "less interesting data")) === jNumber(42),

    ".foo".on(Json("notfoo" := true, "bar" := "less interesting data")) === jNull,

    ".[\"foo\"]".on(Json("foo" := 42, "apple" := "A")) === jNumber(42),

    ".foo.bar".on(Json("foo" := Json("bar" := "a"))) === jString("a")
  )

  def optionalObjectIndex = Seq(
    ".foo?".on(Json("foo" := 42, "bar" := "less interesting data")) ===  jNumber(42),
    ".foo?".on(Json("notFoo" := 42, "bar" := "less interesting data")) ===  jNull,
    ".[\"foo\"]?".on(Json("foo" := 42)) === jNumber(42)
  )

  def recursiveDescent = Seq(
    //TODO
    1 === 1
  )


  def literals = Seq(
    "\"hello world\"".ignoreInput === jString("hello world"),
    "true".ignoreInput === jTrue,
    "false".ignoreInput === jFalse,
    "null".ignoreInput === jNull
  )

  def dicts = Seq(
    "{}".ignoreInput === Json()
  )

  def arrays = Seq(
    "[]".ignoreInput === Json.array()
  )


  def arrayIndex = Seq(
    ".[0]".on(Json.array(Json("name" := "JSON", "good" := true), Json("name" := "XML", "good" := false))) === Json("name" := "JSON", "good" := true),
    ".[2]".on(Json.array(Json("name" := "JSON", "good" := true), Json("name" := "XML", "good" := false))) === jNull,
    ".[-2]".on(Json.array(jNumber(1), jNumber(2), jNumber(3))) === jNumber(2)
  )

  def slice = skipped {
    Seq(
      ".[2:4]".on(Json.array(jString("a"), jString("b"), jString("c"), jString("d"), jString("e"))) === Json.array(jString("c"), jString("d"))
    )
  }

  implicit class MagicString(self: String) {
    def on(js:Json) = {
      val parsed = Parser.exp.parse(self).get.value
      Interpreter.runExp(parsed, js)
    }
    def ignoreInput = on(jNull)
  }
}
