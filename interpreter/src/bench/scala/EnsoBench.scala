import org.enso.interpreter.Constants
import org.enso.interpreter.LanguageRunner
import org.graalvm.polyglot.Context
import org.scalameter.api._

class EnsoBench extends Bench.LocalTime with LanguageRunner {
  val gen = Gen.unit("")

  val sumTCOCode =
    """
      |{ |sumTo|
      |  summator = { |acc, current|
      |      ifZero: [current, acc, @summator [acc + current, current - 1]]
      |  };
      |  res = @summator [0, sumTo];
      |  res
      |}
    """.stripMargin

  val sumTCO = ctx.eval(Constants.LANGUAGE_ID, sumTCOCode)

  val sumRecursiveCode =
    """
      |{ |sumTo|
      |  summator = { |i| ifZero: [i, 0, i + (@summator [i - 1])] };
      |  res = @summator [sumTo];
      |  res
      |}
    """.stripMargin

  val sumRecursive = ctx.eval(Constants.LANGUAGE_ID, sumRecursiveCode)

  performance of "Enso TCO" in {
    measure method "sum numbers upto a million" in {
      using(gen) in { _ =>
        sumTCO.call(100000000)
      }
    }
  }

  performance of "Enso Recursive" in {
    measure method "sum numbers upto 100" in {
      using(gen) in { _ =>
        sumRecursive.call(100)
      }
    }
  }
}
