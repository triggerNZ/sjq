package au.com.simplemachines.sjq

import org.specs2._
import org.specs2.specification.core._

//
class JqTestFileSpecification(path: String) extends Specification{
  def is = {
    println(s"Reading $path")
    val source = scala.io.Source.fromFile(path)
    val testLines = source.getLines().filterNot(_.startsWith("#"))
    val fragments: List[Fragment] = iterativeSplit2(testLines)(_.isEmpty).map { case List(program, inputText, expectedOutputText) =>
//      println(program)
//      println(inputText)
//      println(expectedOutputText)
//      println()

      Fragment(
        description = Text(s"\n$program on $inputText should be $expectedOutputText"),
        execution = Execution.result {
          val input = argonaut.Parse.parseOption(inputText).get
          val expOutput = argonaut.Parse.parseOption(expectedOutputText).get

          SJQ.query(program, input) === expOutput
        }
      )
    }.take(8).toList //TODO: take more

    Fragments(fragments: _*)
  }


  //this breaks iterator laws but whatever, seems to work and it's only a test
  def iterativeSplit2[T](iter: Iterator[T])(breakOn: T => Boolean): Iterator[List[T]] =
    new Iterator[List[T]] {
      def hasNext = iter.hasNext

      def next = {
        val cur = iter.takeWhile(!breakOn(_)).toList
        iter.dropWhile(breakOn)
        cur
      }
    }.withFilter(l => l.nonEmpty)
}

object JqTestFileSpec extends JqTestFileSpecification("/Users/tinpavlinic/tin/jq/tests/jq.test") {

}