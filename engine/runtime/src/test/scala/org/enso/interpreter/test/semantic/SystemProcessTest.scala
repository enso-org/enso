package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}

import scala.util.Random

class SystemProcessTest extends InterpreterTest {
  override def subject: String = "System.create_process"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "return success exit code" in {
      val code =
        """main =
          |    result = System.create_process "echo" [] "" False False False
          |    result.exit_code
          |""".stripMargin
      eval(code) shouldEqual 0
      consumeOut shouldEqual List()
      consumeErr shouldEqual List()
    }

    "return error when creating nonexistent command" in {
      val code =
        """main = System.create_process "nonexistentcommandxyz" [] "" False False False"""

      val error = the[InterpreterException] thrownBy eval(code)
      error.getMessage should include("nonexistentcommandxyz")
      consumeOut shouldEqual List()
      consumeErr shouldEqual List()
    }

    "return custom exit code" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "exit 23"] "" False False False
          |    result.exit_code
          |""".stripMargin

      eval(code) shouldEqual 23
      consumeOut shouldEqual List()
      consumeErr shouldEqual List()
    }

    "redirect stdin chars" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "read line; echo $line"] "" True True True
          |    result.exit_code
          |""".stripMargin

      feedInput("hello")
      eval(code) shouldEqual 0
      consumeOut shouldEqual List("hello")
      consumeErr shouldEqual List()
    }

    "redirect stdin bytes" in {
      val input = Random.nextBytes(Byte.MaxValue)
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "wc --bytes"] "" True True True
          |    result.exit_code
          |""".stripMargin

      feedBytes(input)
      eval(code) shouldEqual 0
      consumeOut shouldEqual List(input.length.toString)
      consumeErr shouldEqual List()
    }

    "redirect stdin unused" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "echo 42"] "" True True True
          |    result.exit_code
          |""".stripMargin

      feedInput("unused input")
      eval(code) shouldEqual 0
      consumeOut shouldEqual List("42")
      consumeErr shouldEqual List()
    }

    "redirect stdin empty" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "echo 9"] "" True True True
          |    result.exit_code
          |""".stripMargin

      eval(code) shouldEqual 0
      consumeOut shouldEqual List("9")
      consumeErr shouldEqual List()
    }

    "provide stdin string" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "read line; echo -n $line"] "hello" False False False
          |    result.stdout
          |""".stripMargin

      eval(code) shouldEqual "hello"
      consumeOut shouldEqual List()
      consumeErr shouldEqual List()
    }

    "redirect stdout chars" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "echo foobar"] "" False True True
          |    result.exit_code
          |""".stripMargin

      eval(code) shouldEqual 0
      consumeOut shouldEqual List("foobar")
      consumeErr shouldEqual List()
    }

    "redirect stdout binary" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "printf '\\x01\\x0F\\x10'"] "" False True True
          |    result.exit_code
          |""".stripMargin

      eval(code) shouldEqual 0
      consumeOutBytes shouldEqual Array(1, 15, 16)
      consumeErrBytes shouldEqual Array()
    }

    "return stdout string" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "echo -n foobar"] "" False False False
          |    result.stdout
          |""".stripMargin

      eval(code) shouldEqual "foobar"
      consumeOut shouldEqual List()
      consumeErr shouldEqual List()
    }

    "redirect stderr chars" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "echo err 1>&2"] "" False True True
          |    result.exit_code
          |""".stripMargin

      eval(code) shouldEqual 0
      consumeOut shouldEqual List()
      consumeErr shouldEqual List("err")
    }

    "redirect stderr binary" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "printf '\\xCA\\xFE\\xBA\\xBE' 1>&2"] "" False True True
          |    result.exit_code
          |""".stripMargin

      eval(code) shouldEqual 0
      consumeOutBytes shouldEqual Array()
      consumeErrBytes shouldEqual Array(-54, -2, -70, -66)
    }

    "return stderr string" in {
      val code =
        """main =
          |    result = System.create_process "bash" ["-c", "echo -n err 1>&2"] "" False False False
          |    result.stderr
          |""".stripMargin

      eval(code) shouldEqual "err"
      consumeOut shouldEqual List()
      consumeErr shouldEqual List()
    }

  }
}
