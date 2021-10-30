package org.enso.syntax

import org.enso.syntax.text.DocParser
import org.enso.syntax.text.DocParser.Result
import org.enso.syntax.text.ast.Doc
import org.scalameter.api._
import org.scalameter.execution.LocalExecutor
import org.scalameter.picklers.Implicits._

import scala.annotation.nowarn
import scala.math.pow

object DocParserBench extends Bench.OfflineRegressionReport {

  override def executor = new LocalExecutor(warmer, aggregator, measurer)

  val range = 0
  @nowarn("cat=w-flag-numeric-widen")
  def exp(i: Int): Gen[Int] =
    Gen.exponential("size")(pow(2, i - range).toInt, pow(2, i).toInt, 2)

  def gen(range: Gen[Int], f: Int => String): Gen[String] =
    for { i <- range } yield f(i)

  val tests = List(
    "formatters" -> gen(exp(14), i => "*foo bar*\n" * i),
    "unclosed"   -> gen(exp(14), i => "*_foobar*\n" * i),
    "combined"   -> gen(exp(14), i => "*_~fo0~_*\n" * i),
    "normal"     -> gen(exp(14), i => "test12345\n" * i),
    "link"       -> gen(exp(14), i => "[foo](bo)\n" * i),
    "tags"       -> gen(exp(14), i => "ADDED\nfoo\n" * i),
    "list" -> gen(
      exp(13),
      i => """foo
             |  - A
             |  - B
             |  - C
             |""".stripMargin * i
    ),
    "list_nested" -> gen(
      exp(12),
      i => """foo
             |  - A
             |  - B
             |    * CA
             |    * CB
             |  - D
             |""".stripMargin * i
    ),
    "sections" -> gen(
      exp(13),
      i => "Foo\n\n!B\n\n?C \n\n>D \n\n" * i
    )
  )

  def run(str: String): Result[Doc] = DocParser.run(str)
  performance of "DocParser" in {
    tests.foreach { case (name, gen) =>
      measure method name in (using(gen) in run)
    }
  }
}
