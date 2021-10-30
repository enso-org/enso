package org.enso.docs.generator

import org.enso.docs.generator.DocsGenerator
import org.scalameter.api._
import org.scalameter.execution.LocalExecutor
import org.scalameter.picklers.Implicits._
import scala.math.pow

object DocsGeneratorBench extends Bench.OfflineRegressionReport {

  override def executor = new LocalExecutor(warmer, aggregator, measurer)

  val range = 0

  def exp(i: Int): Gen[Int] =
    Gen.exponential("size")(
      pow(2.0, (i - range).toDouble).toInt,
      pow(2.0, i.toDouble).toInt,
      2
    )

  def gen(range: Gen[Int], f: Int => List[String]): Gen[String] =
    for { i <- range } yield f(i).toString()

  val tests = List(
    "formatters" -> gen(exp(14), i => List.fill(i)("*foo bar*\n")),
    "unclosed"   -> gen(exp(14), i => List.fill(i)("*_foobar*\n")),
    "combined"   -> gen(exp(14), i => List.fill(i)("*_~fo0~_*\n")),
    "normal"     -> gen(exp(14), i => List.fill(i)("test12345\n")),
    "link"       -> gen(exp(14), i => List.fill(i)("[foo](bo)\n")),
    "tags"       -> gen(exp(14), i => List.fill(i)("ADDED\nfoo\n")),
    "list" -> gen(
      exp(13),
      i => List.fill(i)("""foo
                          |  - A
                          |  - B
                          |  - C
                          |""".stripMargin)
    ),
    "list_nested" -> gen(
      exp(12),
      i => List.fill(i)("""foo
                          |  - A
                          |  - B
                          |    * CA
                          |    * CB
                          |  - D
                          |""".stripMargin)
    ),
    "sections" -> gen(exp(13), i => List.fill(i)("Foo\n\n!B\n\n?C \n\n>D \n\n"))
  )

  def run(str: String): List[String] = DocsGenerator.generate(List(str))
  performance of "Docs Generator" in {
    tests.foreach { case (name, gen) =>
      measure method name in (using(gen) in run)
    }
  }
}
