package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}
import org.enso.polyglot.RuntimeOptions
import org.graalvm.polyglot.Context

class ConversionMethodsTest extends InterpreterTest {
  override def subject: String = "Conversion methods"


  override def contextModifiers: Option[Context#Builder => Context#Builder] =
    Some(_
      .option(RuntimeOptions.LANGUAGE_HOME_OVERRIDE, "../../distribution/component")
      .option(RuntimeOptions.DISABLE_IR_CACHES, "false")
    )

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "be defined in the global scope and dispatched to" in {
      val code =
        """
          |type Foo
          |    Mk_Foo foo
          |type Bar
          |    Mk_Bar bar
          |type Baz
          |    Mk_Baz baz
          |
          |Foo.from (that:Bar) = Foo.Mk_Foo that.bar
          |Foo.from (that:Baz) = Foo.Mk_Foo that.baz
          |
          |main = (Foo.from (Baz.Mk_Baz 10)).foo + (Foo.from (Bar.Mk_Bar 20)).foo
          |""".stripMargin
      eval(code) shouldEqual 30
    }

    "dispatch on polyglot map value" in {
      val code =
        """
          |polyglot java import java.util.Map as Java_Map
          |import Standard.Base.Data.Map.Map
          |
          |type Foo
          |   Mk_Foo map
          |
          |Foo.from (that:Map) = Foo.Mk_Foo that
          |
          |main =
          |   m = Java_Map.of "A" 1 "B" 2
          |   Foo.from m . map . size
          |""".stripMargin
      eval(code) shouldEqual 2
    }

    "dispatch on polyglot date value" in {
      val code =
        """
          |polyglot java import java.time.LocalDate as Java_Date
          |import Standard.Base.Data.Time.Date.Date
          |
          |type Foo
          |   Mk_Foo date
          |
          |Foo.from (that:Date) = Foo.Mk_Foo that
          |
          |main =
          |   jdate = Java_Date.of 2023 2 2
          |   Foo.from jdate . date . quarter
          |""".stripMargin
      eval(code) shouldEqual 1
    }

    "dispatch on polyglot time value" in {
      val code =
        """
          |polyglot java import java.time.LocalTime as Java_Time
          |import Standard.Base.Data.Time.Time_Of_Day.Time_Of_Day
          |
          |type Foo
          |   Mk_Foo foo
          |
          |Foo.from (that:Time_Of_Day) = Foo.Mk_Foo that
          |
          |main = Foo.from (Java_Time.of 23 59) . foo . minute
          |""".stripMargin
      eval(code) shouldEqual 59
    }
  }
}
