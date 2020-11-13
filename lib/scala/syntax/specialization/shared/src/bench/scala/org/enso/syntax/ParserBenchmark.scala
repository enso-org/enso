package org.enso.syntax

import java.io.BufferedReader
import java.io.File
import java.io.FileReader
import java.io.PrintWriter

import org.enso.syntax.text.AST
import org.enso.flexer
import org.enso.syntax.text.Parser
import org.scalameter.api._
import org.enso.syntax.text.ast.DSL._
import org.scalameter.Measurer
import org.scalameter.execution.LocalExecutor
import org.scalameter.picklers.Implicits._

import scala.annotation.nowarn
import scala.math.pow

object ParserBenchmark extends Bench.OfflineRegressionReport {

  override def executor =
    new LocalExecutor(warmer, aggregator, new Measurer.Default)

  val range = 0
  @nowarn("cat=w-flag-numeric-widen")
  def exp(i: Int) =
    Gen.exponential("size")(pow(2, i - range).toInt, pow(2, i).toInt, 2)

  def gen(range: Gen[Int], f: Int => String): Gen[String] =
    for { i <- range } yield f(i)

  def gen(range: Gen[Int]): Gen[Int] =
    for { i <- range } yield i

  def patternMatching0(i: Int): Unit = {
    val n1     = "foo" + i.toString
    val n2     = n1 + "!"
    var v: AST = AST.Var(n1)
    for { j <- 0 to i } {
      if (j % 2 == 0) v = n2 $_ v
      else v = n1 $__ v
    }
    for { _ <- 0 to i } v = v match {
      case AST.Var(_)             => v
      case AST.App.Prefix(_, arg) => arg
    }
  }

  def patternMatching1(i: Int): Unit = {
    val n1     = "foo" + i.toString
    val n2     = n1 + "!"
    var v: AST = AST.Var(n1)
    for { _ <- 0 to i } v = n2 $_ v
    for { _ <- 0 to i } v = v match {
      case AST.App.Prefix(_, arg) => arg
    }
  }

  val parserInput = List(
    "text"                  -> gen(exp(10), i => "'ab #$ 60' " * i),
    "number"                -> gen(exp(10), i => "123456789 " * i),
    "codeBlock"             -> gen(exp(10), i => "foo0\nBar2\n" * i),
    "codeBlock with macros" -> gen(exp(10), i => "a = x\nb++\n" * i),
    "allRules" -> gen(
      exp(7),
      i => """
             | string = "ABCD"
             | number = 123_4.67
             | fib   : Int -> Int
             | fib n = fib n-1 + fib n-2
             |""".stripMargin * i
    )
  )

  val filename = "target/parser-bench-input.txt"

  if (!new File(filename).exists()) {
    val file = new PrintWriter(new File(filename))
    for (_ <- 1 to 10000) {
      file.print("rewuipf\uD800\uDF1Edsahkjlzcvxm,/\uD800\uDF1E.m,';k]o1&**&$")
      file.print("6!@#&*()+|{}QWERYUI\uD800\uDF1EOIO\uD800\uDF1E}ASFDJKM>>?\n")
    }
  }

  def runReader()    = new flexer.Reader(new File(filename)).toString()
  def runReaderUTF() = new flexer.ReaderUTF(new File(filename)).toString()
  def runBufferedReader() = {
    val reader  = new BufferedReader(new FileReader(filename))
    val builder = new java.lang.StringBuilder()
    var char    = 0
    while ({ char = reader.read(); char != -1 }) {
      builder.append(char.toChar)
      if (char.toChar.isHighSurrogate)
        builder.append(reader.read().toChar)
    }
    builder.toString
  }

  ///////////////////////////////////////
  //// Tests ////////////////////////////
  ///////////////////////////////////////

  val exp18 = gen(exp(18))

  performance of "pattern match" in {
    measure method "Var and App" in (using(exp18) in patternMatching0)
    measure method "Var" in (using(exp18) in patternMatching1)
  }

  val dummy = gen(exp(0))

  performance of "reader" in {
    measure method "Buffered" in  { using(dummy) in (_ => runBufferedReader()) }
    measure method "FlexerUTF" in { using(dummy) in (_ => runReaderUTF())      }
    measure method "Flexer" in    { using(dummy) in (_ => runReader())         }
  }

  def run(str: String) = Parser().run(str)

  performance of "parser" in {
    parserInput.foreach {
      case (name, gen) => measure method name in (using(gen) in run)
    }
  }

}
