package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}

import scala.util.Random

class SystemProcessTest extends InterpreterTest {
  override def subject: String = "System_Process"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "return exit code 0 on success" in {
      val code = """main = Process.create ["echo"] False False False"""

      eval(code) shouldEqual 0
      consumeOut shouldEqual List()
      consumeErr shouldEqual List()
    }

    "return error when creating nonexistent command" in {
      val code =
        """main = Process.create ["nonexistentcommandxyz"] False False False"""

      val error = the[InterpreterException] thrownBy eval(code)
      error.getMessage should include("nonexistentcommandxyz")
      consumeOut shouldEqual List()
      consumeErr shouldEqual List()
    }

    "return exit code custom" in {
      val code =
        """main = Process.create ["bash", "-c", "exit 23"] False False False"""

      eval(code) shouldEqual 23
      consumeOut shouldEqual List()
      consumeErr shouldEqual List()
    }

    "redirect stdin chars" in {
      val code =
        """main = Process.create ["bash", "-c", "read line; echo $line"] True True True"""

      feedInput("hello")
      eval(code) shouldEqual 0
      consumeOut shouldEqual List("hello")
      consumeErr shouldEqual List()
    }

    "redirect stdin bytes" in {
      val input = Random.nextBytes(Byte.MaxValue)
      val code =
        """main = Process.create ["bash", "-c", "wc --bytes"] True True True"""

      feedBytes(input)
      eval(code) shouldEqual 0
      consumeOut shouldEqual List(input.length.toString)
      consumeErr shouldEqual List()
    }

    "redirect stdin unused" in {
      val code =
        """main = Process.create ["bash", "-c", "echo 42"] True True True"""

      feedInput("unused input")
      eval(code) shouldEqual 0
      consumeOut shouldEqual List("42")
      consumeErr shouldEqual List()
    }

    "redirect stdin empty" in {
      val code =
        """main = Process.create ["bash", "-c", "echo 9"] True True True"""

      eval(code) shouldEqual 0
      consumeOut shouldEqual List("9")
      consumeErr shouldEqual List()
    }

    "redirect stdout chars" in {
      val code =
        """main = Process.create ["bash", "-c", "echo foobar"] False True True"""

      eval(code) shouldEqual 0
      consumeOut shouldEqual List("foobar")
      consumeErr shouldEqual List()
    }

    "redirect stdout binary" in {
      val code =
        """main = Process.create ["bash", "-c", "printf '\\x01\\x0F\\x10'"] False True True"""

      eval(code) shouldEqual 0
      consumeOutBytes shouldEqual Array(1, 15, 16)
      consumeErrBytes shouldEqual Array()
    }

    "redirect stderr chars" in {
      val code =
        """main = Process.create ["bash", "-c", "echo err 1>&2"] False True True"""

      eval(code) shouldEqual 0
      consumeOut shouldEqual List()
      consumeErr shouldEqual List("err")
    }

    "redirect stderr binary" in {
      val code =
        """main = Process.create ["bash", "-c", "printf '\\xCA\\xFE\\xBA\\xBE' 1>&2"] False True True"""

      eval(code) shouldEqual 0
      consumeOutBytes shouldEqual Array()
      consumeErrBytes shouldEqual Array(-54, -2, -70, -66)
    }

  }
}
