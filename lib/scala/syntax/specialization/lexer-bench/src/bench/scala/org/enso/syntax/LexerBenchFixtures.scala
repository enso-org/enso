package org.enso.syntax

import java.nio.charset.StandardCharsets

import org.enso.flexer
import org.enso.flexer.Reader
import org.enso.syntax.text.AST
import org.enso.syntax.text.spec.{ParserDef, ParserDef2}

object LexerBenchFixtures {

  private val newEngine = flexer.Parser.compile(ParserDef())

  // === Lexer Runner ===

  /** Execute the lexer on the provided `input`.
    *
    * @param input the source code
    * @return the result of lexing `input`
    */
  def runLexer(input: String): ParserDef2.Result[AST.Module] = {
    val engine = newEngine()
    val reader = new Reader(input)
    engine.run(reader)
  }

  // === Utilities ===

  /** Replicate the provided `input` out to the provided `size` in bytes
    * (according to utf-8).
    *
    * @param input the text to replicate
    * @param size the size to replicate `input` to
    * @param addNewline whether or not a newline should be added after each
    *                   repetition of `input`
    * @return `input`, repeated enough times to make the size >= `size`
    */
  def replicate(
    input: String,
    size: Int,
    addNewline: Boolean = false
  ): String = {
    val inputSize    = input.getBytes(StandardCharsets.UTF_8).length
    val times        = 1 + size / inputSize
    val inputNewline = if (addNewline) input + "\n" else input + " "
    inputNewline.repeat(times)
  }

  /** Replace all CRLF line endings in the input by LF.
    *
    * @param input the input text
    * @return `input` with all `\r\n` replaced by `\n`
    */
  def preprocess(input: String): String = {
    input.replace("\r\n", "\n")
  }

  // === Benchmarks ===

  val benchmarks: Map[String, String] = Map(
    // Literals
    ("literalNumberInteger", Literal.Number.integer),
    ("literalNumberIntegerExplicitBase", Literal.Number.integerExplicitBase),
    ("literalNumberDecimal", Literal.Number.decimal),
    ("literalNumberDecimalExplictBase", Literal.Number.decimalExplicitBase),
    ("literalNumberErrorBase", Literal.Number.errorBase),
    ("literalTextFormatLine", Literal.Text.formatLine),
    ("literalTextFormatInlineBlock", Literal.Text.formatInlineBlock),
    ("literalTextFormatBlock", Literal.Text.formatBlock),
    ("literalTextRawLine", Literal.Text.rawLine),
    ("literalTextRawInlineBlock", Literal.Text.rawInlineBlock),
    ("literalTextRawBlock", Literal.Text.rawBlock),
    // Names
    ("nameLineOf", Name.lineOf),
    ("nameInvalidSuffix", Name.invalidSuffix),
    // Operators
    ("operatorLineOf", Operator.lineOf),
    ("operatorDotCall", Operator.dotCall),
    ("operatorInvalidSuffix", Operator.invalidSuffix),
    // Blocks
    ("blockTopLevel", Block.topLevel),
    ("blockNested", Block.nested),
    ("blockDeeplyNested", Block.deeplyNested),
    // Comments
    ("commentLine", Comment.line),
    ("commentInLine", Comment.inLine),
    ("commentDoc", Comment.doc),
    // Combined
    ("combinedSimple", Combined.simple),
    ("combinedComplex", Combined.complex)
  )

  // === Inputs ===

  object Literal {
    object Number {
      val integer: String             = preprocess("12345")
      val integerExplicitBase: String = preprocess("16_a4fd31")
      val decimal: String             = preprocess("1.3141")
      val decimalExplicitBase: String = preprocess("10_1.000999")
      val errorBase: String           = preprocess("10.2_2")
    }

    object Text {
      val formatLine: String =
        "'dearest creature in \\n creation studying english pronunciation'"

      val formatInlineBlock: String =
        "''' An inline block. It's a very good inline block carl \\u{AB}"

      val formatBlock: String =
        """''' Here is my block of format text. I can `interpolate + things` like that.
          |    It goes on and on and on for `times` times because I feel like it.
          |
          |    Complex interpolated expression `x -> y ~> x | y` woo!
          |""".stripMargin

      val rawLine: String =
        "\"dearest creature in '''' creation studying english pronunciation\""

      val rawInlineBlock: String =
        "\"\"\" An inline block. It's a very good inline block carl "

      val tQuote = "\"\"\""
      val rawBlock: String =
        s"""$tQuote Here is my block of raw text. `Interpolations` are nothing special here.
           |    It goes on and on and on for I can escape \" though.
           |
           |    It also supports blank lines!
           |""".stripMargin
    }
  }

  object Name {
    val lineOf: String =
      "Referent_Ident var_ident JavaType _ @annotation ticked_ident' number_1"

    val invalidSuffix: String = "some_var'iable some_varД"
  }

  object Operator {
    val lineOf: String        = "+ - * -> ~> <~ <- ! & | /"
    val dotCall: String       = ".== . != .<*> .*> .|>"
    val invalidSuffix: String = ".... +=="
  }

  object Block {
    val topLevel: String = "foo\nbar\nbaz"
    val nested: String   = "foo\\nbar\\n    baz\\n    quux"
    val deeplyNested: String =
      """foo
        |bar
        |    baz
        |    quux
        |        bim
        |            bam
        |    oh
        |no
        |""".stripMargin
  }

  object Comment {
    val line: String =
      "# foo bar baz I have a really long line comment here that goes on and on"

    val inLine: String = "a + b # A useless comment: add a to b"

    val doc: String =
      """##  I have a really big doc comment here
        |    That just keeps prattling on and on and on.
        |
        |    With blank lines
        |
        |    Forever
        |
        |    and
        |    ever
        |
        |    and
        |
        |
        |
        |
        |    ever
        |documented
        |""".stripMargin
  }

  object Combined {
    val simple: String =
      """
        |import Base.Meta
        |
        |##  Decompose the value using runtime reflection and print its decomposition.
        |Main.print_decomp a b =
        |    y      = a + b
        |    decomp = Meta.decompose y
        |    Io.println decomp
        |""".stripMargin

    val complex: String =
      """import Base.Meta
        |
        |##  Frobnicate the doodads by constructing a new type operator through runtime reflection such that
        |    it can be passed to another language.
        |
        |    ! WARNING
        |    Type-checking code like this is virtually impossible, and it is treated as `Dynamic` inside
        |    Enso code.
        |Main.foo a b =
        |    y = x -> z ->
        |        ty = a.gen_type (~>) (<-) b
        |        ty (z x)
        |    decomp = Meta.decompose (y a b)
        |    Io.println decomp
        |
        |##  Execute the main function of this project.
        |main =
        |    func = Meta.reify (here.foo "My_Name" "my_field")
        |    Io.println(func)
        |""".stripMargin
  }
}
