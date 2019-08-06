package org.enso.syntax.text.lexer

import org.scalameter.api._
import scala.math.pow
import org.enso.syntax.text.parser.Parser
import org.enso.syntax.text.parser.BParser
import java.io.StringReader

object RangeBenchmark extends Bench.OfflineReport {
  val exp14 = Gen.exponential("size")(pow(2, 14).toInt, pow(2, 16).toInt, 2)
  val exp15 = Gen.exponential("size")(pow(2, 15).toInt, pow(2, 17).toInt, 2)
  val exp16 = Gen.exponential("size")(pow(2, 16).toInt, pow(2, 18).toInt, 2)

  val longVars     = for { i <- exp16 } yield "test12" * i
  val multipleVars = for { i <- exp16 } yield "test1 (A " * i
  val exprs1       = for { i <- exp14 } yield "a += 1\nb == '\n'\n" * i
  val exprs2       = for { i <- exp14 } yield "a += 1\nb == '`a`!'\n" * i

  performance of "Range" in {
    // measure method "exprs1" in {
    //   using(exprs1) in {
    //     input => new Lexer(input).lexAll()
    //   }
    // }
    // measure method "exprs2" in {
    //   using(exprs2) in {
    //     input => new Lexer(input).lexAll()
    //   }
    // }
    // measure method "longVar" in {
    //   using(longVars) in {
    //     input => new Lexer(input).lexAll()
    //   }
    // }
    // measure method "multipleVars" in {
    //   using(multipleVars) in {
    //     input => new Lexer(input).lexAll()
    //   }
    // }

//    measure method "Parser-multipleVars" in {
//      using(multipleVars) in { input =>
//        {
//          (new Parser(new StringReader(input))).parse
//        }
//      }
//    }

    measure method "BParser-multipleVars" in {
      using(multipleVars) in { input =>
        {
          (new BParser(new StringReader(input))).parse
        }
      }
    }

    measure method "SParser-multipleVars" in {
      using(multipleVars) in { input =>
        {
          (new SParser(new StringReader(input))).parse
        }
      }
    }
  }
}
