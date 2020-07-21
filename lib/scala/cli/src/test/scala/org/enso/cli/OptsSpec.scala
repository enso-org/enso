package org.enso.cli

import cats.implicits._
import org.enso.cli.Opts.implicits._
import org.enso.cli.internal.Parser
import org.scalactic.source
import org.scalatest.exceptions.{StackDepthException, TestFailedException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{EitherValues, OptionValues}

class OptsSpec
    extends AnyWordSpec
    with Matchers
    with EitherValues
    with OptionValues {
  implicit class ParseSyntax[A](val opts: Opts[A]) {
    private def splitLine(line: String): Seq[String] =
      if (line.nonEmpty) line.split(' ').toSeq else Seq()

    def parse(line: String): Either[List[String], A] = parse(splitLine(line))

    def parse(args: Seq[String]): Either[List[String], A] = {
      val (tokens, additionalArguments) = Parser.tokenize(args)
      Parser
        .parseOpts(opts, tokens, additionalArguments, isTopLevel = false, Seq("???"))
        .map(_._1)
    }

    def parseSuccessfully(line: String)(implicit pos: source.Position): A =
      parseSuccessfully(splitLine(line))

    def parseSuccessfully(
      args: Seq[String]
    )(implicit pos: source.Position): A = {
      val result = parse(args)
      result match {
        case Left(errors) =>
          throw new TestFailedException(
            { _: StackDepthException =>
              Some(
                "Parse should succeed, but it failed with:\n" + errors
                  .mkString("\n")
              )
            },
            None,
            pos
          )
        case Right(value) => value
      }
    }

    def parseFailing(
      line: String
    )(implicit pos: source.Position): List[String] =
      parseFailing(splitLine(line))

    def parseFailing(
      args: Seq[String]
    )(implicit pos: source.Position): List[String] = {
      val result = parse(args)
      result match {
        case Right(value) =>
          throw new TestFailedException(
            { _: StackDepthException =>
              Some(
                s"Expected parse to fail, but it succeeded, returning $value"
              )
            },
            None,
            pos
          )
        case Left(errors) => errors
      }
    }
  }

  "positionalArgument" should {
    val opt = Opts.positionalArgument[Int]("arg")
    "be required" in {
      opt.parseFailing("").head should include("Missing")
    }

    "report parse errors" in {
      opt.parseFailing("NaN").head should include("Invalid number")
    }

    "return parsed value" in {
      opt.parseSuccessfully("42") shouldEqual 42
    }

    "handle spaces in arguments" in {
      val opt = Opts.positionalArgument[String]("arg")
      opt.parseSuccessfully(Seq("a b c")) shouldEqual "a b c"
    }

    "show in the usage" in {
      opt.commandLines().head should include("arg")
    }
  }

  "optionalArgument" should {
    val opt = Opts.optionalArgument[Int]("arg")
    "be optional" in {
      opt.parseSuccessfully("") shouldEqual None
    }

    "return parsed value" in {
      opt.parseSuccessfully("42").value shouldEqual 42
    }

    "show in the usage" in {
      opt.commandLines().head should include("[arg]")
    }
  }

  "trailingPositionalArguments" should {
    val opt = Opts.trailingPositionalArguments[Int]("args")
    "be optional" in {
      opt.parseSuccessfully("") shouldEqual Seq()
    }

    "consume arbitrary amount of values" in {
      opt.parseSuccessfully("42") shouldEqual Seq(42)
      opt.parseSuccessfully("1 2 3 4") shouldEqual Seq(1, 2, 3, 4)
    }

    "show in the usage" in {
      opt.commandLines().head should include("[args...]")
    }
  }

  "flag" should {
    val opt = Opts.flag("flag", 'f', "help", showInUsage = false)
    "set value if it is present" in {
      opt.parseSuccessfully("") shouldEqual false
      opt.parseSuccessfully("-f") shouldEqual true
      opt.parseSuccessfully("--flag") shouldEqual true
    }

    "show in usage if specified" in {
      Opts.flag("a", "help", showInUsage = true).commandLines().head should
      include("--a")
      Opts.flag("a", "help", showInUsage = false).commandLines().head should not
      include("--a")
    }
  }

  "parameter" should {
    val opt = Opts.parameter[String]("param", "STR", "help")
    "be required" in {
      opt.parseFailing("").head should include("Missing")
    }

    "handle both notations" in {
      opt.parseSuccessfully("--param abc") shouldEqual "abc"
      opt.parseSuccessfully("--param=abc") shouldEqual "abc"
      opt.parseSuccessfully(Seq("--param", "a b c")) shouldEqual "a b c"
      opt.parseSuccessfully(Seq("--param=a b c")) shouldEqual "a b c"
    }

    "show in the usage" in {
      opt.commandLines().head should include("--param STR")
    }

    "parse when put anywhere between arguments" in {
      val arg1 = Opts.positionalArgument[String]("arg1")
      val arg2 = Opts.positionalArgument[String]("arg2")
      val param = Opts.parameter[String]("p", "x", "help")
      val opts = (arg1, arg2, param) mapN { (_, _, p) => p }
      opts.parseSuccessfully("arg1 arg2 --p value") shouldEqual "value"
      opts.parseSuccessfully("arg1 --p value arg2") shouldEqual "value"
      opts.parseSuccessfully("--p value arg1 arg2") shouldEqual "value"
    }
  }

  "optionalParameter" should {
    val opt =
      Opts.optionalParameter[Int]("param", "INT", "help", showInUsage = true)
    "be required" in {
      opt.parseSuccessfully("") shouldEqual None
    }

    "handle both notations" in {
      opt.parseSuccessfully("--param 42").value shouldEqual 42
      opt.parseSuccessfully("--param=42").value shouldEqual 42
    }

    "show in the usage" in {
      opt.commandLines().head should include("[--param INT]")
    }
  }

  "prefixedParameters" should {
    "handle arbitrary amount of occurences" in {
      val opt = Opts.prefixedParameters("prefix", "help")
      opt.parseSuccessfully("") shouldEqual Seq()
      opt.parseSuccessfully("--prefix.k=v") shouldEqual Seq(("k", "v"))
      opt.parseSuccessfully("--prefix.k v") shouldEqual Seq(("k", "v"))

      opt.parseSuccessfully("--prefix.k v --prefix.k v") shouldEqual
      Seq(("k", "v"), ("k", "v"))

      opt.parseSuccessfully(
        "--prefix.key1=value1 --prefix.key2 value2"
      ) shouldEqual Seq(("key1", "value1"), ("key2", "value2"))
    }
  }

  "additionalArguments" should {
    "handle arbitrary combinations of arguments" in {
      val opt = Opts.additionalArguments()
      opt.parseSuccessfully("") shouldEqual Seq()
      opt.parseSuccessfully("--") shouldEqual Seq()
      opt.parseSuccessfully("-- arg1 arg2") shouldEqual Seq("arg1", "arg2")
      opt.parseSuccessfully("-- --prefix.k=v") shouldEqual Seq("--prefix.k=v")

      opt.parseSuccessfully("-- arg1 --flag arg2 -some-thing-") shouldEqual
      Seq("arg1", "--flag", "arg2", "-some-thing-")
    }
  }

  "pure" should {
    "return its value" in {
      Opts.pure("A").parseSuccessfully("") shouldEqual("A")
    }

    "not consume any options" in {
      Opts.pure("A").parseFailing("arg").head should include("Unexpected")
      Opts.pure("A").parseFailing("--flag").head should include("Unknown")
      Opts.pure("A").parseFailing("--parameter=x").head should include("Unknown")
    }
  }

  "subcommands" should {
    val opt = Opts.subcommands(
      Subcommand("cmd1") {
        Opts.flag("flag1", "", showInUsage = true).map((1, _))
      },
      Subcommand("cmd2") {
        Opts.flag("flag2", "", showInUsage = true).map((2, _))
      }
    )

    "delegate to right subcommand" in {
      opt.parseSuccessfully("cmd1")._1 shouldEqual 1
      opt.parseSuccessfully("cmd2")._1 shouldEqual 2
    }

    "handle subcommand options correctly" in {
      opt.parseSuccessfully("cmd1 --flag1") shouldEqual ((1, true))
      opt.parseSuccessfully("cmd1") shouldEqual ((1, false))
      opt.parseSuccessfully("cmd2 --flag2") shouldEqual ((2, true))
      opt.parseSuccessfully("cmd2") shouldEqual ((2, false))
      opt.parseFailing("cmd1 --flag2").head should include("Unknown")
      opt.parseFailing("cmd2 --flag1").head should include("Unknown")
    }

    "handle parent options" in {
      val flag = Opts.flag("flag3", "", showInUsage = true)
      val opts: Opts[(Boolean, (Int, Boolean))] =
        (flag, opt) mapN { (flag, rest) => (flag, rest) }

      opts.parseSuccessfully("--flag3 cmd1") shouldEqual ((true, (1, false)))
      opts.parseSuccessfully("cmd1") shouldEqual ((false, (1, false)))

      opts.parseSuccessfully("--flag3 cmd1 --flag1") shouldEqual
        ((true, (1, true)))
      opts.parseSuccessfully("cmd1 --flag3 --flag1") shouldEqual
        ((true, (1, true)))

      opts.parseFailing("--flag1 cmd1")
    }
  }

  "withDefault" should {
    "return the default value if the result is missing" in {
      Opts.optionalArgument[Int]("arg")
        .withDefault(1)
        .parseSuccessfully("") shouldEqual 1
    }

    "return the original value if provided" in {
      Opts.optionalArgument[Int]("arg")
        .withDefault(1)
        .parseSuccessfully("0") shouldEqual 0
    }
  }
}
