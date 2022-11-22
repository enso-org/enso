package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest, Metadata}

class ExpressionIdTest extends InterpreterTest {
  override def subject: String = "Expression IDs"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "be correct in simple arithmetic expressions" in
    withIdsInstrumenter { instrumenter =>
      val code = "main = 2 + 45 * 20"
      val meta = new Metadata
      val id1  = meta.addItem(7, 11)
      val id2  = meta.addItem(11, 7)
      val id3  = meta.addItem(11, 2)

      instrumenter.assertNodeExists(id1, "902")
      instrumenter.assertNodeExists(id2, "900")
      instrumenter.assertNodeExists(id3, "45")

      eval(meta.appendToCode(code))
    }

    "be correct with parenthesized expressions" in
    withIdsInstrumenter { instrumenter =>
      val code = "main = (2 + 45) * 20"
      val meta = new Metadata
      val id1  = meta.addItem(7, 13)
      val id2  = meta.addItem(8, 6)

      instrumenter.assertNodeExists(id1, "940")
      instrumenter.assertNodeExists(id2, "47")
      eval(meta.appendToCode(code))
    }

    "be correct in applications and method calls" in
    withIdsInstrumenter { instrumenter =>
      val code =
        """from Standard.Base import all
          |
          |main = (2-2 == 0).if_then_else (List.Cons 5 6) 0
          |""".stripMargin.linesIterator.mkString("\n")
      val meta = new Metadata
      val id1  = meta.addItem(38, 41)
      val id2  = meta.addItem(63, 13)

      instrumenter.assertNodeExists(id1, "Cons 5 6")
      instrumenter.assertNodeExists(id2, "Cons 5 6")
      eval(meta.appendToCode(code))
    }

    "be correct for deeply nested functions" in
    withIdsInstrumenter { instrumenter =>
      val code =
        """
          |import Standard.Base.Nothing
          |import Standard.Base.IO
          |
          |Nothing.method =
          |    foo = a -> b ->
          |        IO.println a
          |        add = a -> b -> a + b
          |        add a b
          |    foo 10 20
          |
          |main = Nothing.method
          |""".stripMargin.linesIterator.mkString("\n")
      val meta = new Metadata
      val id1  = meta.addItem(137, 5)
      val id2  = meta.addItem(155, 1)
      val id3  = meta.addItem(151, 7)
      val id4  = meta.addItem(163, 9)

      instrumenter.assertNodeExists(id1, "30")
      instrumenter.assertNodeExists(id2, "10")
      instrumenter.assertNodeExists(id3, "30")
      instrumenter.assertNodeExists(id4, "30")
      eval(meta.appendToCode(code))
    }

    "be correct inside pattern matches" in
    withIdsInstrumenter { instrumenter =>
      val code =
        """
          |import Standard.Base.Data.List.List
          |
          |main =
          |    x = List.Cons 1 2
          |    y = List.Nil
          |
          |    add = a -> b -> a + b
          |
          |    foo = x -> case x of
          |        List.Cons a b ->
          |            z = add a b
          |            x = z * z
          |            x
          |        _ -> 5 * 5
          |
          |    foo x + foo y
          |""".stripMargin.linesIterator.mkString("\n")
      val meta = new Metadata
      val id1  = meta.addItem(127, 113, "1111")
      val id2  = meta.addItem(178, 7)
      val id3  = meta.addItem(198, 9)
      val id4  = meta.addItem(235, 5)

      instrumenter.assertNodeExists(id1, "9")
      instrumenter.assertNodeExists(id2, "3")
      instrumenter.assertNodeExists(id3, "Nothing")
      instrumenter.assertNodeExists(id4, "25")
      eval(meta.appendToCode(code))
    }

    "be correct for defaulted arguments" in
    withIdsInstrumenter { instrumenter =>
      val code =
        """
          |main =
          |    bar = x -> x + x * x
          |    foo = x -> (y = bar x) -> x + y
          |    foo 3
          |""".stripMargin.linesIterator.mkString("\n")
      val meta = new Metadata
      val id1  = meta.addItem(53, 5)
      val id2  = meta.addItem(57, 1)

      instrumenter.assertNodeExists(id1, "12")
      instrumenter.assertNodeExists(id2, "3")
      eval(meta.appendToCode(code))
    }

    "be correct for lazy arguments" in
    withIdsInstrumenter { instrumenter =>
      val code =
        """
          |main =
          |    bar = a -> ~b -> ~c -> b
          |
          |    bar 0 10 0
          |""".stripMargin.linesIterator.mkString("\n")
      val meta = new Metadata
      val id   = meta.addItem(35, 1)

      instrumenter.assertNodeExists(id, "10")
      eval(meta.appendToCode(code))
    }

    "annotate the body of a lambda" in
    withIdsInstrumenter { instrumenter =>
      val code =
        """
          |main =
          |    x = 1
          |""".stripMargin.linesIterator.mkString("\n")
      val meta = new Metadata
      val id   = meta.addItem(7, 10)

      instrumenter.assertNodeExists(id, "Nothing")
      eval(meta.appendToCode(code))
    }
  }
}
