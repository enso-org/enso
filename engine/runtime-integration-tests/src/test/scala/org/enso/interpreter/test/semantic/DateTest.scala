package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class DateTest extends InterpreterTest {
  override def subject: String = "LocalDate"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "evaluate a date expression" in {
      val code =
        s"""import Standard.Base.IO
           |
           |import Standard.Base.Data.Time.Date
           |
           |main =
           |    IO.println (Date.new_builtin 2022 04 01)
           |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("2022-04-01")
    }

    "print out java date" in {
      val code =
        s"""import Standard.Base.IO
           |polyglot java import java.time.LocalDate
           |
           |main =
           |    IO.println (LocalDate.of 2022 04 01)
           |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("2022-04-01")
    }

    "check java date has enso methods" in {
      val code =
        s"""import Standard.Base.IO
           |polyglot java import java.time.LocalDate
           |import Standard.Base.Data.Time.Date
           |
           |main =
           |    javadate = LocalDate.of 2022 4 1
           |    ensoyear = javadate.year
           |    ensomonth = javadate.month
           |    ensoday = javadate.day
           |    ensotext = javadate.to_text
           |    IO.println ensoyear
           |    IO.println ensomonth
           |    IO.println ensoday
           |    IO.println ensotext
           |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("2022", "4", "1", "2022-04-01")
    }

    "check enso date has enso methods" in {
      val code =
        s"""import Standard.Base.IO
           |import Standard.Base.Data.Time.Date
           |
           |main =
           |    ensodate = Date.new_builtin 2022 4 1
           |    ensoyear = ensodate.year
           |    ensomonth = ensodate.month
           |    ensoday = ensodate.day
           |    ensotext = ensodate.to_text
           |    IO.println ensoyear
           |    IO.println ensomonth
           |    IO.println ensoday
           |    IO.println ensotext
           |    ensodate
           |""".stripMargin
      val ensoDate = eval(code)
      consumeOut shouldEqual List("2022", "4", "1", "2022-04-01")
      ensoDate.getMemberKeys().size() shouldEqual 0
    }
  }
}
