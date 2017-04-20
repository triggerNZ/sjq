package au.com.simplemachines.sjq

import scalaz.Isomorphism._

import argonaut.{Json => ArgJson}

object SJQ {
  def query[JS](q: String, js: JS)(implicit iso: JS <=> ArgJson): JS = {
    val parsed = Parser.exp.parse(q).get.value
    val input: ArgJson = iso.to(js)
    val output = Interpreter.runExp(parsed, input)
    iso.from(output)
  }
}
