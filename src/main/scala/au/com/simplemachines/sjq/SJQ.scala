package au.com.simplemachines.sjq

import scalaz.Isomorphism._
import argonaut.{Argonaut, Json => ArgJson, Parse}

object SJQ {
  def query[JS](q: String, js: JS)(implicit iso: JS <=> ArgJson): JS = {
    val parsed = Parser.exp.parse(q).get.value
    val input: ArgJson = iso.to(js)
    val output = Interpreter.runExp(parsed, input)
    iso.from(output)
  }

  def main(args: Array[String]): Unit = {
    val query = args(0)
    val fileContents = io.Source.fromInputStream(System.in, "UTF-8").mkString
    Parse.parseWith(fileContents, { success =>
      val output = SJQ.query(query, success)
      println(output.spaces4)
    }, { error =>
      System.err.println(s"Failed to parse: $error")
    })
  }
}
