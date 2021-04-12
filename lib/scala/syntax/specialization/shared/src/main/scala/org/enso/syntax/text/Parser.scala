package org.enso.syntax.text

import java.util.UUID

import cats.Foldable
import org.enso.data.List1
import org.enso.data.Span
import org.enso.flexer
import org.enso.flexer.Reader
import org.enso.syntax.text.AST.Block.Line
import org.enso.syntax.text.AST.Block.OptLine
import org.enso.syntax.text.AST.Macro.Match.SegmentOps
import org.enso.syntax.text.AST.App
import org.enso.syntax.text.ast.meta.Builtin
import org.enso.syntax.text.prec.Macro
import org.enso.syntax.text.spec.ParserDef
import cats.implicits._
import io.circe
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser._

////////////////////////////////

class InternalError(reason: String, cause: Throwable = None.orNull)
    extends Exception(s"Internal error $reason", cause)

////////////////
//// Parser ////
////////////////

/** This is the main Parser class.
  *
  * ==The Macro System==
  *
  * The parser bases on a very sophisticated Macro mechanism, allowing users for
  * unparalleled control and flexibility. The macro systems allows advanced
  * users to create new syntax or new domain-specific languages. In a similar
  * fashion to Lisp, Enso macros can freely transform the syntactic structure of
  * the program. In short, anything that Enso can do to a data structure, Enso
  * macros can do to code. In contrast, in most other languages, the parser's
  * output is purely internal to the language implementation and cannot be
  * manipulated by the programmer.
  *
  * Macro resolution steps:
  *
  * 1. Parser is executed by using the [[Parser#run]] function. It reads source
  * code and outputs a token stream [[AST.Stream]]. The token stream contains a
  * very narrow range of possible elements: [[AST.Blank]], [[AST.Var]],
  * [[AST.Cons]], [[AST.Opr]], [[AST.Number]], [[AST.Text]], and [[AST.Block]],
  * which contains lines of streams of these elements. Every other AST structure
  * is build by the macro system. Please note that the stream in this step is
  * encoded by using [[AST.App]] on subsequent elements.
  *
  * 2. Parser prepares [[Builtin.registry]] containing predefined set of macro
  * [[AST.Macro.Definition]], which define such constructs as comments, parensed
  * expressions, imports, new data definitions, if-then-else mixfix functions,
  * or even foreign languages support. During this step parser will be also
  * asking interpreter to fill the registry with definitions from other modules.
  * Each [[AST.Macro.Definition]] contains macro segment descriptions and a
  * finalizer, a function transforming matched tokens to final AST. Finalizer is
  * used only if all macro segments were matched correctly.
  *
  * 3. The token stream is partitioned according to registered macros segments.
  * Each macro contains multiple segments. A segment contains of an identifier,
  * like "if" or "then" and a macro [[Pattern]]. Patterns are not used in this
  * step. The AST stream is partitioned solely by segment identifiers. Macros
  * can overlap, for example, [[Builtin.registry]] contains both "if-then" and
  * "if-then-else" macro. When it is impossible to decide which macro to choose,
  * like for the input "(if a) b", [[AST.Macro.Ambiguous]] is returned.
  * Otherwise, each macro segment is matched against corresponding [[Pattern]]
  * and [[AST.Macro.Match]] is returned and stored back in the [[AST.Stream]].
  * Please note, that even if pattern matching fails, the [[AST.Macro.Match]]
  * will be the result. It will contain information about failed patterns.
  *
  * 4. The segment [[Pattern]] is similar to regular expression. It contains
  * around 10 building blocks, such as [[Pattern.Nothing]], which does not
  * consume any input, or [[Pattern.Tok]], allowing matching a specific token,
  * like the "if" keyword. The result, [[Pattern.Match]] is stored in
  * [[AST.Macro.Match]]. The [[Pattern.Match.Err]] is used to mark unsuccessful
  * pattern match fragments, while the [[Pattern.Match.Tok]] is used to provide
  * additional help messages to the end-user. Please note that it is impossible
  * for the pattern match mechanism to break even on malformed user
  * [[AST.Macro.Definition]]. Each definition contains a pre-process step inside
  * of [[AST.Macro.Definition]] constructor, which modifies the user provided
  * rules with checks if the pattern succeed and in case the pattern was used
  * between segments, if it consumed all tokens. In case either of validators
  * fail, all tokens are consumed and marked as an invalid match.
  *
  * 5. A very special pattern is the [[Pattern.Build]] construction, which tells
  * the pattern match mechanism that it should build a single [[AST]] expression
  * out of matched tokens. For example, a pattern
  * [[Pattern.Build(Pattern.Cls[AST.Opr])]] will match an operator token and
  * build a side-section AST from it. The [[Pattern.Build]] blocks are resolved
  * during the pattern match step. After this step is finished and
  * [[AST.Macro.Match]] or [[AST.Macro.Ambiguous]] is stored back in the
  * [[AST.Stream]], nothing more happens, parsing is done! It is important to
  * note, that there is a special module parsing macro, which runs
  * [[Pattern.Build]] on every line.
  *
  * ==Pattern Build Mechanism==
  *
  * The resolution of [[Pattern.Build]] is as interesting as the macro system.
  * It contains of the following stages:
  *
  * 1. First, the [[AST.Stream]] is partitioned byt the [[Distance]] processor
  * according to the spacing information. All non-spaced tokens are grouped
  * together and processed first. After their processing is done and each group
  * will be transformed to a single [[AST]], it is put back to the original
  * [[AST.Stream]] and the whole stream is processed the same way (described in
  * the following points).
  *
  * 2. Each token of a chosen stream is then processed by the
  * [[https://en.wikipedia.org/wiki/Shunting-yard_algorithm Shunting-yard
  * algorithm]]. Basically, it re-shuffles the [[AST]] stream to combination of
  * [[AST.App]], [[AST.App.Left]], [[AST.App.Right]], and [[AST.App.Sides]],
  * according to the operator precedence. Please note that the precedence of
  * user defined operators is fixed in Enso and depends on the shape of the
  * operator. For example, all "arrows" like "<-", "<-<", or "<=<", have the
  * same precedence. The associativity is inferred by the operator direction,
  * where both "=" and "," operators are considered right-associative. See
  * [[Operator]] and [[Prec]] for more information.
  *
  * ==Finalizers==
  *
  * A careful reader will notice that there was no description of how finalizers
  * (mentioned in the first section) are used. Finalizers are user-provided AST
  * transformations which are applied to valid AST Macro matches. After
  * finalizer is applied, the spacing information might be lost.
  *
  * ==Space-unaware AST===
  *
  * That's because they are NOT used during parsing. A very important design
  * decision is that Enso AST contains all information allowing for printing the
  * code back from the AST, while keeping all whitespaces as they were before
  * parsing. This is why each space-aware AST, like [[AST.App]] records all
  * positional information. For convenient usage, all space-aware [[AST]]
  * definitions end with "Of", like [[AST.App.PrefixOf]] and have a counterpart
  * without "Of" allowing for pattern matching without thinking about the
  * spacing information. Because macro system is end-user extensible, we cannot
  * assume that the end-user will care about recording valid spacing when
  * transforming [[AST]] to another form. That's why there are also
  * space-unaware [[AST]] structures, which are handy to work with by automated
  * tools like the interpreter, while all the spacing information is stored only
  * in the basic set of tokens and [[AST.Macro]] tokens. Each AST node has a
  * [[AST.map]] function for mapping over sub-nodes, which allows easy building
  * of AST traversals. The [[Parser#resolveMacros]] is such a traversal, which
  * applies [[AST.Macro.Definition.Resolver]] to each [[AST.Macro.Match]] found
  * in the AST, while loosing a lot of positional information.
  */
case class SourceFile(ast: AST, metadata: Json)

object SourceFile {
  val METATAG = "\n\n\n#### METADATA ####\n"

  implicit def MWMEncoder: Encoder[SourceFile] =
    module =>
      Json.obj("ast" -> module.ast.toJson(), "metadata" -> module.metadata)
}

class Parser {
  import Parser._
  private val engine = newEngine()

  def splitMeta(code: String): (String, IDMap, Json) = {
    import SourceFile._
    code.split(METATAG) match {
      case Array(input) => (input, Seq(), Json.obj())
      case Array(input, rest) =>
        val meta = rest.split('\n')
        if (meta.length < 2) {
          throw new ParserError(s"Expected two lines after METADATA.")
        }
        val idmap = idMapFromJson(meta(0)).left.map { error =>
          throw new ParserError("Could not deserialize idmap.", error)
        }.merge
        val metadata = decode[Json](meta(1)).left.map { error =>
          throw new ParserError("Could not deserialize metadata.", error)
        }.merge
        (input, idmap, metadata)
    }
  }

  /** Parse contents of the program source file,
    * where program code may be followed by idmap and metadata.
    */
  def runWithMetadata(program: String): SourceFile = {
    val (input, idmap, metadata) = splitMeta(program)
    SourceFile(run(new Reader(input), idmap), metadata)
  }

  /** Parse contents of the program source file, attaching any IDs defined
    * in the metadata section and dropping macros resolution data.
    *
    * @param input the code parse.
    * @return the AST resulting from parsing input.
    */
  def runWithIds(input: String): AST.Module = {
    val (code, idmap, _) = splitMeta(input)
    val ast              = run(code)
    val noMacros         = dropMacroMeta(ast)
    attachIds(noMacros, idmap)
  }

  private def attachIds(module: AST.Module, ids: IDMap): AST.Module = {
    val idMap: Map[Location, AST.ID] = ids.map { case (span, id) =>
      (Location(span.index.value, span.index.value + span.size.value), id)
    }.toMap

    def go(ast: AST): AST = {
      val id = ast.location.flatMap(idMap.get)
      ast.setID(id).map(go)
    }

    module.map(go)
  }

  /** Parse simple string with empty IdMap into AST. */
  def run(input: String): AST.Module =
    run(new Reader(new CommentRemover(input).run), Nil)

  /** Parse input with provided IdMap into AST */
  def run(input: Reader, idMap: IDMap): AST.Module = {
    val tokenStream = engine.run(input).map(InHoisting.run)

    val spanned = tokenStream.map(attachModuleLocations)
    val resolved = spanned.map(Macro.run) match {
      case flexer.Parser.Result(_, flexer.Parser.Result.Success(mod)) =>
        val mod2 = annotateModule(idMap, mod)
        resolveMacros(mod2).asInstanceOf[AST.Module]
      case _ => throw ParsingFailed
    }
    resolved
  }

  /** Processes an input [[AST.Module]], attaching absolute span information
    * to it and all its children.
    */
  def attachModuleLocations(ast: AST.Module): AST.Module = {
    val toplevelOffset = 0
    var currentOffset  = toplevelOffset
    val newLines: List1[OptLine] = ast.lines.map { line =>
      val locatedElem    = line.elem.map(attachLocations(_, currentOffset))
      val locatedLine    = Line(locatedElem, line.off)
      val expressionSpan = line.elem.map(_.span).getOrElse(0)
      val lineOffset     = line.off
      val newLineOffset  = 1
      currentOffset += expressionSpan + lineOffset + newLineOffset
      locatedLine
    }
    val unspannedModule = ast.setLines(newLines)
    unspannedModule.setLocation(Location(toplevelOffset, ast.span))
  }

  /** Processes an AST block, attaching absolute span information to it
    * and all children.
    *
    * @param ast the AST block to mark with absolute positions
    * @param startOffset the position in the file this AST is located at
    * @return an AST properly marked with absolute span information
    */
  def attachBlockLocations(ast: AST.Block, startOffset: Int): AST.Block = {
    val blockBeginOffset         = 1
    val newLineOffset            = 1
    val emptyLinesNewLinesOffset = ast.emptyLines.length
    val emptyLinesSpacingOffset  = ast.emptyLines.sum
    val firstLineOffset = startOffset + blockBeginOffset +
      emptyLinesNewLinesOffset + emptyLinesSpacingOffset
    var currentOffset = firstLineOffset
    currentOffset += ast.indent
    val locatedFirstLine: AST.Block.Line =
      ast.firstLine.map(attachLocations(_, currentOffset))
    currentOffset += locatedFirstLine.elem.span + locatedFirstLine.off + newLineOffset
    val locatedLines = ast.lines.map { line =>
      if (line.elem.isDefined) {
        currentOffset += ast.indent
      }
      val locatedLine = line.map(_.map(attachLocations(_, currentOffset)))
      val elemSpan    = locatedLine.elem.map(_.span).getOrElse(0)
      currentOffset += elemSpan + locatedLine.off + newLineOffset
      locatedLine
    }
    val unspannedBlock = ast
      .replaceFirstLine(locatedFirstLine)
      .replaceLines(locatedLines)
    unspannedBlock.setLocation(
      Location(startOffset, startOffset + ast.span)
    )
  }

  /** Attaches absolute span information to arbitrary AST.
    *
    * [[App.Prefix]] nodes are treated specially, since at the stage this
    * method is run, we expect a token-stream-like AST, where App nodes act as
    * list conses.
    *
    * @param ast the AST to attach span information to
    * @param startOffset the absolute offset this AST was encountered at
    * @return a version of the input AST with the absolute positioning info
    *         populated
    */
  def attachLocations(ast: AST, startOffset: Int): AST =
    ast match {
      case App.Prefix.any(app) =>
        val locatedFn = attachLocations(app.func, startOffset)
        val locatedArg =
          attachLocations(app.arg, startOffset + locatedFn.span + app.off)
        App.Prefix(locatedFn, app.off, locatedArg)
      case AST.Block.any(block) => attachBlockLocations(block, startOffset)
      case _ =>
        ast.setLocation(Location(startOffset, startOffset + ast.span))
    }

  def annotateModule(
    idMap: IDMap,
    mod: AST.Module
  ): AST.Module = {
    var ids = idMap.sorted.toList
    mod.traverseWithOff { (off, ast) =>
      val key = Span(off, ast)

      while (ids.nonEmpty && ids.head._1 < key) ids = ids.tail

      ids match {
        case (k, id) :: _ if k == key =>
          ids = ids.tail
          ast.setID(id)
        case _ =>
          ast.withNewID()
      }
    }
  }

  /** Although this function does not use any Parser-specific API now, it will
    * use such in the future when the interpreter will provide information about
    * defined macros other than [[Builtin.registry]].
    */
  def resolveMacros(ast: AST): AST =
    ast match {
      case AST.Macro.Match.any(ast) =>
        val resolvedAST = ast.map(resolveMacros)
        Builtin.registry.get(resolvedAST.path) match {
          case None => throw MissingMacroDefinition
          case Some(spec) =>
            val id       = resolvedAST.id.getOrElse(UUID.randomUUID)
            val segments = resolvedAST.segs.toList().map(_.wrapped)
            val ctx      = AST.Macro.Resolver.Context(resolvedAST.pfx, segments, id)
            resolvedAST.copy(shape = resolvedAST.shape.copy[AST](resolved = {
              Option(resolveMacros(spec.resolver(ctx)))
            }))
        }
      case _ => ast.map(resolveMacros)
    }

  /* Note: [Type safety]
   * ~~~~~~~~~~~~~~~~~~~
   * This function promises to return AST with the same shape as it
   * received, however compiler cannot verify this due to type erasure.
   *
   * As we are only using copy/map function and never change shape to use
   * different variants, we can say it is safe and coerce the types.
   */

  /** Automatically derives source location for an AST node, based on its
    * children's locations
    *
    * @param ast the AST to derive location for
    * @return AST with derived location
    */
  def deriveLocation(ast: AST): AST =
    if (ast.location.isEmpty) {
      val location = ast match {
        case AST.App.Section.Right(opr, right) =>
          opr.location |+| right.location
        case _ => ast.foldMap(_.location)
      }
      ast.setLocation(location)
    } else {
      ast
    }

  /** Derives location (see [[deriveLocation]]) for a node and all its
    * descendants.
    *
    * @param ast the AST to derive location for
    * @return AST with derived location
    */
  def deriveLocations(ast: AST): AST = {
    val withLocatedChildren = ast.map(deriveLocations)
    deriveLocation(withLocatedChildren)
  }

  /** Drops macros metadata keeping only resolved macros in the AST.
    * WARNING: this transformation drops the relative information about AST
    * spacing, while the absolute positioning is preserved.
    */
  def dropMacroMeta(ast: AST.Module): AST.Module = {
    def go: AST => AST = {
      case AST.Macro.Match.any(t) => {
        val prefix = t.pfx.toList.flatMap(_.toStream.map(_.wrapped))
        val segments =
          t.segs.toList().flatMap(_.wrapped.toStream.map(_.wrapped))
        val originalSegments = (prefix ++ segments).map(deriveLocations)
        val originalSpan =
          Foldable[List].foldMap(originalSegments)(_.location)
        val resolved = t.resolved.map(_.setLocation(originalSpan))
        go(resolved.orNull)
      }
      case t => deriveLocation(t.map(go))
    }
    ast.map(go)
  }

}

object Parser {

  type IDMap = Seq[(Span, AST.ID)]

  private val newEngine = flexer.Parser.compile(ParserDef())

  def apply(): Parser = new Parser()

  def idMapFromJson(json: String): Either[circe.Error, IDMap] =
    decode[IDMap](json)

  //// Exceptions ////

  case object ParsingFailed extends ParserError("parsing failed")
  case object MissingMacroDefinition
      extends ParserError("macro definition not found")
  class ParserError(reason: String, cause: Throwable = None.orNull)
      extends InternalError(s"in parser $reason", cause)
}

////////////////////////////////////////////////////////////////////////////////
//// Interactive Testing Utilities /////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

//////////////
//// Main ////
//////////////

object Main extends scala.App {

  println("--- START ---")

  val parser = new Parser()

  val in_def_maybe =
    """## Foo bar baz
      |   bax
      |def Maybe a
      |    ## test
      |    def Just val:a
      |    def Nothing
    """.stripMargin

  val in_arr1 = "a = b -> c d"

  val in3  = "(a) b = c"
  val in4  = "if a then (b)"
  val in2  = "(a) b = c]"
  val inp2 = "a (b (c)) x"

  val inp =
    """
      |##
      |  DEPRECATED
      |  REMOVED - replaced by Foo Bar
      |  ADDED
      |  MODIFIED
      |  UPCOMING
      |  ALAMAKOTA a kot ma Ale
      |  This is a test of Enso Documentation Parser. This is a short synopsis.
      |
      |  Here you can write the body of documentation. On top you can see tags
      |  added to this piece of code. You can customise your text with _Italic_
      |  ~Strikethrough~ or *Bold*. ~_*Combined*_~ is funny
      |
      |
      |  There are 3 kinds of sections
      |    - Important
      |    - Info
      |    - Example
      |      * You can use example to add multiline code to your documentation
      |
      |  ! Important
      |    Here is a small test of Important Section
      |
      |  ? Info
      |    Here is a small test of Info Section
      |
      |  > Example
      |    Here is a small test of Example Section
      |        Import Foo
      |        def Bar a
      |type Maybe a
      |    ## test attached to Just
      |    type Just val:a
      |    ##DEPRECATED
      |      foo bar baz
      |    type Nothing
      |
      |    ## The pow function calculates power of integers.
      |    pow x y = x ** y
      |""".stripMargin
  val inC =
    """
      |from Standard.Base import all
      |
      |polyglot java import java.util.Locale as JavaLocale
      |
      |# Constants
      |# These are chosen as the union of the top 10 countries by population, and the
      |# top ten countries by total (nominal, not per-capita) GDP.
      |
      |## The default locale.
      |
      |   The default locale is a locale that does not specify any language, country,
      |   or variant and is used as the language/country-neutral setting for locale
      |   sensitive operations.
      |default : Locale
      |default = here.from_java JavaLocale.ROOT
      |
      |## A locale representing Bangladesh.
      |
      |   > Example
      |     Get the Bangladeshi locale.
      |         Locale.bangladesh
      |bangladesh : Locale
      |bangladesh = here.from_language_tag "bn-BD"
      |
      |## A locale representing Brazil.
      |
      |   > Example
      |     Get the Brazilian locale.
      |         Locale.brazil
      |brazil : Locale
      |brazil = here.from_language_tag "pt-BR"
      |
      |## A locale representing Canada with language English.
      |
      |   > Example
      |     Get the Canadian english locale.
      |         Locale.canada_english
      |canada_english : Locale
      |canada_english = here.from_language_tag "en-CA"
      |
      |## A locale representing Canada with language French.
      |
      |   > Example
      |     Get the Canadian french locale.
      |         Locale.canada_french
      |canada_french : Locale
      |canada_french = here.from_language_tag "fr-CA"
      |
      |## A locale representing the PRC.
      |
      |   > Example
      |     Get the PRC locale.
      |         Locale.china
      |china : Locale
      |china = here.from_language_tag "zh-CN"
      |
      |## A locale representing France.
      |
      |   > Example
      |     Get the French locale.
      |         Locale.france
      |france : Locale
      |france = here.from_language_tag "fr-FR"
      |
      |## A locale representing Germany.
      |
      |   > Example
      |     Get the German locale.
      |         Locale.germany
      |germany : Locale
      |germany = here.from_language_tag "de-DE"
      |
      |## A locale representing India with language Hindi.
      |
      |   > Example
      |     Get the Indian hindi locale.
      |         Locale.india_hindi
      |india_hindi : Locale
      |india_hindi = here.from_language_tag "hi-IN"
      |
      |## A locale representing India with language English.
      |
      |   > Example
      |     Get the Indian english locale.
      |         Locale.indian_english
      |india_english : Locale
      |india_english = here.from_language_tag "en-IN"
      |
      |## A locale representing Indonesia.
      |
      |   > Example
      |     Get the Indonesian locale.
      |         Locale.indonesia
      |indonesia : Locale
      |indonesia = here.from_language_tag "id-ID"
      |
      |## A locale representing Italy.
      |
      |   > Example
      |     Get the Italian locale.
      |         Locale.italy
      |italy : Locale
      |italy = here.from_language_tag "it-IT"
      |
      |## A locale representing Japan.
      |
      |   > Example
      |     Get the Japanese locale.
      |         Locale.japan
      |japan : Locale
      |japan = here.from_language_tag "jp-JP"
      |
      |## A locale representing Mexico.
      |
      |   > Example
      |     Get the Mexican locale.
      |         Locale.mexico
      |mexico : Locale
      |mexico = here.from_language_tag "es-MX"
      |
      |## A locale representing Nigeria.
      |
      |   > Example
      |     Get the Nigerian locale.
      |         Locale.nigeria
      |nigeria : Locale
      |nigeria = here.from_language_tag "en-NG"
      |
      |## A locale representing paksitan with language Urdu.
      |
      |   > Example
      |     Get the Pakistani urdu locale.
      |         Locale.pakistan_urdu
      |pakistan_urdu : Locale
      |pakistan_urdu = here.from_language_tag "ur-PK"
      |
      |## A locale representing paksitan with language English.
      |
      |   > Example
      |     Get the Pakistani english locale.
      |         Locale.bangladesh
      |pakistan_english : Locale
      |pakistan_english = here.from_language_tag "en-PK"
      |
      |## A locale representing Russia.
      |
      |   > Example
      |     Get the Russian locale.
      |         Locale.russia
      |russia : Locale
      |russia = here.from_language_tag "ru-RU"
      |
      |## A locale representing South Korea.
      |
      |   > Example
      |     Get the South Korean locale.
      |         Locale.south_korea
      |south_korea : Locale
      |south_korea = here.from_language_tag "ko-KR"
      |
      |## A locale representing the UK.
      |
      |   > Example
      |     Get the british locale.
      |         Locale.uk
      |uk : Locale
      |uk = here.from_language_tag "en-GB"
      |
      |## A locale representing the United States.
      |
      |   > Example
      |     Get the US locale.
      |         Locale.us
      |us : Locale
      |us = here.from_language_tag "en-US"
      |
      |## Construct a new locale.
      |
      |   Arguments:
      |   - language: The language tag for the locale.
      |   - country: The country tag for the locale.
      |   - variant: The variant for the locale.
      |
      |   > Example
      |     A locale representing en-GB.UTF-8.
      |         Locale.new "en" "GB" "UTF-8"
      |new : Text -> Text | Nothing -> Text | Nothing -> Locale
      |new language country=Nothing variant=Nothing =
      |    country_text = if country.is_nothing then "" else country
      |    variant_text = if variant.is_nothing then "" else variant
      |    java_locale = JavaLocale.new language country_text variant_text
      |    here.from_java java_locale
      |
      |## Returns the locale specified by the provided IETF BCP47 language tag string.
      |
      |   ? Language Tag Syntax
      |     If the specified language tag contains any ill-formed subtags, the first
      |     such subtag and all following subtags are ignored.
      |
      |     The following conversions are performed:
      |     - The language code "und" is mapped to language "".
      |     - The language codes "he", "yi", and "id" are mapped to "iw", "ji", and
      |       "in" respectively.
      |     - The portion of a private use subtag prefixed by "lvariant", if any, is
      |       removed and appended to the variant field in the result locale (without
      |       case normalization).
      |     - When the languageTag argument contains an extlang subtag, the first such
      |       subtag is used as the language, and the primary language subtag and other
      |       extlang subtags are ignored.
      |     - Case is normalized except for variant tags, which are left unchanged.
      |       Language is normalized to lower case, script to title case, country to
      |       upper case, and extensions to lower case.
      |     - If, after processing, the locale would exactly match either ja_JP_JP or
      |       th_TH_TH with no extensions, the appropriate extensions are added.
      |
      |     This implements the 'Language-Tag' production of BCP47, and so supports
      |     grandfathered (regular and irregular) as well as private use language tags.
      |
      |   > Example
      |     Creating the locale en_US.
      |         Locale.from_language_tag "en_US"
      |from_language_tag : Text -> Locale
      |from_language_tag tag =
      |    java_locale = JavaLocale.forLanguageTag tag
      |    here.from_java java_locale
      |
      |## A type representing a locale.
      |
      |   A locale consists of three parts:
      |   - A language, which is mandatory.
      |   - A country code, which is optional.
      |   - A variant, which is optional.
      |type Locale
      |
      |    ## A type representing a locale.
      |
      |       Arguments:
      |       - java_locale: The Java locale representation used internally.
      |    type Locale java_locale
      |
      |    ## Gets the language from the locale.
      |
      |       > Example
      |         Get the language tag from the default locale.
      |             Locale.default.language
      |    language : Text | Nothing
      |    language =
      |        lang = this.java_locale.getLanguage
      |        if lang.is_empty then Nothing else lang
      |
      |    ## Gets the country from the locale.
      |
      |       > Example
      |         Get the country tag from the default locale.
      |             Locale.default.country
      |    country : Text | Nothing
      |    country =
      |        place = this.java_locale.getCountry
      |        if place.is_empty then Nothing else place
      |
      |    ## Gets the variant from the locale.
      |
      |       > Example
      |         Get the variant tag from the default locale.
      |             Locale.default.variant
      |    variant : Text | Nothing
      |    variant =
      |        var = this.java_locale.getVariant
      |        if var.is_empty then Nothing else var
      |
      |    ## Gets a representation of the language in the locale that can be shown to
      |       the user.
      |
      |       > Example
      |         Get the display language tag from the default locale.
      |             Locale.default.display_language
      |    display_language : Text | Nothing
      |    display_language =
      |        disp = this.java_locale.getDisplayLanguage
      |        if disp.is_empty then Nothing else disp
      |
      |    ## Gets a representation of the country in the locale that can be shown to
      |       the user.
      |
      |       > Example
      |         Get the display country tag from the default locale.
      |             Locale.default.display_country
      |    display_country : Text | Nothing
      |    display_country =
      |        disp = this.java_locale.getDisplayCountry
      |        if disp.is_empty then Nothing else disp
      |
      |    ## Gets a representation of the variant in the locale that can be shown to
      |       the user.
      |
      |       > Example
      |         Get the display variant tag from the default locale.
      |             Locale.default.display_variant
      |    display_variant : Text | Nothing
      |    display_variant =
      |        disp = this.java_locale.getDisplayVariant
      |        if disp.is_empty then Nothing else disp
      |
      |    ## Converts the locale to text.
      |
      |       > Example
      |         Convert the default locale to text.
      |             Locale.default.to_text
      |    to_text : Text | Nothing
      |    to_text = this.java_locale.toLanguageTag
      |
      |    ## A Locale to Json conversion
      |
      |       > Example
      |         Convert the default locale to JSON.
      |             Locale.default.to_json
      |    to_json : Json.Object
      |    to_json =
      |        b = Vector.new_builder
      |        b.append ["type", "Locale"]
      |        if this.language.is_nothing.not then b.append ["language", this.language]
      |        if this.country.is_nothing.not then b.append ["country", this.country]
      |        if this.variant.is_nothing.not then b.append ["variant", this.variant]
      |        Json.from_pairs b.to_vector
      |
      |## PRIVATE
      |
      |   Convert a java locale to an Enso locale.
      |
      |   Arguments:
      |   - java: The java locale value.
      |from_java : JavaLocale -> Locale
      |from_java java = Locale java
      |
      |## PRIVATE
      |javaLocaleBuilder = JavaLocale.Builder
      |""".stripMargin

  println("--- PARSING ---")

  val mod = parser.run(inC)

  println(Debug.pretty(mod.toString))

  println("=========================")
  println(Debug.pretty(parser.dropMacroMeta(mod).toString))
  val rmod = parser.resolveMacros(mod)
  if (mod != rmod) {
    println("\n---\n")
    println(Debug.pretty(rmod.toString))
  }

  println("------")
  println(mod.show() == inC)
  println("------")
  println(mod.show())
  println("------")

  /** Invoking the Enso Documentation Parser */
  println("===== DOCUMENTATION =====")
  val droppedMeta = parser.dropMacroMeta(mod)
  val doc         = DocParserRunner.createDocs(droppedMeta)

  println(Debug.pretty(doc.toString))
  println("------")
  println(doc.show())
  println("=========================")
  val htmlCode = DocParserHTMLGenerator.generateHTMLForEveryDocumented(doc)
  println("========== HTML ===========")
  println(htmlCode)
  println("=========================")

  println("===== PURE DOCUMENTATION PARSER AND GENERATOR (W/O AST CONN) =====")
  val inpOnlyDoc =
    """DEPRECATED
      |REMOVED - replaced by Foo Bar
      |ADDED
      |MODIFIED
      |UPCOMING
      |ALAMAKOTA a kot ma Ale
      |This is a test of Enso Documentation Parser. This is a short synopsis.
      |
      |Here you can write the body of documentation. On top you can see tags
      |added to this piece of code. You can customise your text with _Italic_
      |~Strikethrough~ or *Bold*. ~_*Combined*_~ is funny
      |
      |
      |There are 3 kinds of sections
      |  - Important
      |  - Info
      |  - Example
      |    * You can use example to add multiline code to your documentation
      |
      |! Important
      |  Here is a small test of Important Section
      |
      |? Info
      |  Here is a small test of Info Section
      |
      |> Example
      |  Here is a small test of Example Section
      |      Import Foo
      |      def Bar a
      |          Foo x y
      |""".stripMargin
  val doc2      = DocParser.runMatched(inpOnlyDoc)
  val htmlCode2 = DocParserHTMLGenerator.generateHTMLPureDoc(doc2)
  println(htmlCode2)

  AST.main()

}
