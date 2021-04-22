package org.enso.syntax.text

import org.enso.flexer
import org.enso.flexer.Reader
import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.spec.DocParserDef
import scalatags.Text.TypedTag
import scalatags.Text.{all => HTML}
import HTML._
import flexer.Parser.{Result => res}
import org.enso.data.List1
import org.enso.syntax.text.Shape.Block.Line

////////////////////////////////////////////////////////////////////////////////
//// Doc Parser ////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/** This is the class used to invoke Documentation Parser.
  *
  * It is used to create structured documentation from the blocks of commented
  * text created by the main Enso parser.
  *
  * It has been built on the same foundation as Parser, so in order not to
  * duplicate information, please refer to Parser documentation.
  */
class DocParser {
  import DocParser._
  private val engine = newEngine()
  private val errMsg = "Internal Documentation Parser Error"

  /** Used to match result of [[run]] function to possibly retrieve Doc
    *
    * @param input - input string to Doc Parser
    * @return - If it was able to retrieve Doc, then retrieved data, else
    *           exception with error message [[errMsg]]
    */
  def runMatched(input: String): Doc =
    run(input) match {
      case res(_, res.Success(v)) => v
      case _                      => throw new Exception(errMsg)
    }

  /** Used to initialize Doc Parser with input string to get parsed Doc
    *
    * @param input - input string to Doc Parser
    * @return - unmatched result possibly containing Doc
    */
  def run(input: String): Result[Doc] = engine.run(new Reader(input))
}

object DocParser {
  type Result[T] = flexer.Parser.Result[T]
  private val newEngine = flexer.Parser.compile(DocParserDef())

  /** Doc Parser running methods, as described above, in class [[DocParser]]
    */
  def runMatched(input: String): Doc = {
    val lines            = input.split("\n")
    var indentSecondLine = 0
    if (lines.tail.nonEmpty) {
      val s = lines.tail.head
      indentSecondLine = s.indexOf(s.trim())
    }
    val in = " " * indentSecondLine + lines.mkString("\n")
    new DocParser().runMatched(in)
  }
  def run(input: String): Result[Doc] = new DocParser().run(input)
}

////////////////////////////////////////////////////////////////////////////////
//// Doc Parser Runner /////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/** This is Doc Parser Runner.
  *
  * Essentially it binds together Enso Parser with Doc Parser.
  * When Parser finishes its job it invokes runner with AST created by it after
  * resolving macros. Then Runner does it's job - running Doc Parser on every
  * [[AST.Comment]], combined with connecting [[Doc]] with AST in
  * [[AST.Documented]] node, which gets AST from [[AST.Def]] and
  * [[AST.App.Infix]]
  */
object DocParserRunner {
  //////////////////////////////////////////////////////////////////////////////
  //// Created Doc's in right places with appropriate AST //////////////////////
  //////////////////////////////////////////////////////////////////////////////
  /** This function invokes the documentation parser on every instance of
    * [[AST.Comment]].
    *
    * It matches on [[AST.Module]] or [[AST.Def]] in order to traverse
    * through their lines to create [[AST.Documented]] from [[AST.Comment]] if
    * found, with AST from [[AST.App.Infix]] or [[AST.Def]]
    *
    * @param ast - module with possibility to create Documentation from comments
    * @return - modified data containing possibly Documentation(s) with AST
    */
  def createDocs(ast: AST): AST = {
    ast match {
      case AST.Module.any(m) => createDocsFromModule(m)
      case AST.Def.any(d) =>
        d.body match {
          case Some(body) =>
            body match {
              case AST.Block.any(b) => createDocsFromDefBody(d.name, d.args, b)
              case _                => d
            }
          case None => d
        }
      case other => other
    }
  }

  /** This is a helper function for [[createDocs]] to traverse through
    * [[AST.Module]] and create Docs from comments with appropriate [[AST]]
    */
  def createDocsFromModule(m: AST.Module): AST.Module = {
    val emptyLine = List1(AST.Block.OptLine())
    val transformedLines =
      List1(attachDocToSubsequentAST(m.lines.toList)).getOrElse(emptyLine)
    AST.Module(transformedLines)
  }

  /** This is a helper function for [[createDocs]] to traverse through
    * [[AST.Def]] and create Docs from comments inside [[AST.Def]] with
    * appropriate [[AST]]
    */
  def createDocsFromDefBody(
    name: AST.Cons,
    args: List[AST],
    b: AST.Block
  ): AST.Def = {
    val firstLine        = Line(Option(b.firstLine.elem), b.firstLine.off)
    val linesToTransform = firstLine :: b.lines
    val transformedLines = attachDocToSubsequentAST(linesToTransform)
    val TLHeadElem       = transformedLines.head.elem.get
    val TLHeadOff        = transformedLines.head.off
    val head             = AST.Block.Line(TLHeadElem, TLHeadOff)
    val lines            = transformedLines.tail
    val body             = AST.Block(b.ty, b.indent, b.emptyLines, head, lines)
    AST.Def(name, args, Some(body))
  }

  /** This is a helper function for creating docs with AST.
    * Essentially it traverses through lines and tries to find a pattern on them
    *
    * @param lines - AST lines
    * @return - lines with possibly Doc with added AST
    */
  def attachDocToSubsequentAST(
    lines: List[AST.Block.OptLine]
  ): List[AST.Block.OptLine] =
    lines match {
      case line1 :: tail =>
        line1 match {
          case Line(Some(AST.Comment.any(com)), off) =>
            tail match {
              case line2 :: rest =>
                line2 match {
                  case Line(Some(AST.App.Infix.any(ast)), _) =>
                    commentWithInfixForDocumented(
                      com,
                      off,
                      ast,
                      rest
                    )
                  case Line(Some(AST.Def.any(ast)), _) =>
                    commentWithDefForDocumented(com, off, ast, rest)
                  case Line(None, _) =>
                    var restTrav  = rest
                    var emp       = 1
                    val emptyLine = Line(None, 0)
                    while (restTrav.nonEmpty && restTrav.head == emptyLine) {
                      emp += 1
                      restTrav = restTrav.tail
                    }
                    val rTail = restTrav.tail
                    val rHead = restTrav.head
                    rHead match {
                      case Line(Some(AST.App.Infix.any(ast)), _) =>
                        commentWithInfixForDocumented(
                          com,
                          off,
                          ast,
                          rTail,
                          emp
                        )
                      case Line(Some(AST.Def.any(ast)), _) =>
                        commentWithDefForDocumented(
                          com,
                          off,
                          ast,
                          rTail,
                          emp
                        )
                      case _ =>
                        line1 :: line2 :: attachDocToSubsequentAST(rest)
                    }
                  case other =>
                    line1 :: attachDocToSubsequentAST(other :: rest)
                }
              case Nil => line1 :: Nil
            }
          case other => other :: attachDocToSubsequentAST(tail)
        }
      case Nil => Nil
    }

  /** Creates Docs from comments found in parsed data
    *
    * @param comment - Comment found in AST.
    * @return - Documentation.
    */
  def createDocFromComment(comment: AST.Comment): Doc = {
    val in = comment.lines.mkString("\n")
    DocParser.runMatched(in)
  }

  /** Function for creating documented lines in [[attachDocToSubsequentAST]]
    * method with [[AST.App.Infix]] as Documented AST
    *
    * @param com - comment found in AST
    * @param off - line offset
    * @param ast - [[AST.App.Infix]] to go with comment into Documented
    * @param rest - lines after documented
    * @param emptyLines - Empty lines in between Doc and AST
    * @return - [[AST.Documented]]
    */
  def commentWithDefForDocumented(
    com: AST.Comment,
    off: Int,
    ast: AST.Def,
    rest: List[AST.Block.OptLine],
    emptyLines: Int = 0
  ): List[AST.Block.OptLine] = {
    val docFromAst = createDocs(ast)
    val docLine =
      createDocumentedLine(com, emptyLines, docFromAst, off)
    docLine :: attachDocToSubsequentAST(rest)
  }

  /** Function for creating documented lines in [[attachDocToSubsequentAST]]
    * method with [[AST.Def]] as Documented AST
    *
    * @param com - comment found in AST
    * @param off - line offset
    * @param ast - [[AST.Def]] to go with comment into Documented
    * @param rest - lines after documented
    * @param emptyLines - Empty lines in between Doc and AST
    * @return - [[AST.Documented]]
    */
  def commentWithInfixForDocumented(
    com: AST.Comment,
    off: Int,
    ast: AST.App.Infix,
    rest: List[AST.Block.OptLine],
    emptyLines: Int = 0
  ): List[AST.Block.OptLine] = {
    val docLine = createDocumentedLine(com, emptyLines, ast, off)
    docLine :: attachDocToSubsequentAST(rest)
  }

  /** Function for creating documented lines in [[attachDocToSubsequentAST]]
    * method
    *
    * @param comment - comment found in AST
    * @param emptyLines - Empty lines in between Doc and AST
    * @param off - line offset
    * @param ast - AST to go with comment into Documented
    * @return - [[AST.Documented]]
    */
  def createDocumentedLine(
    comment: AST.Comment,
    emptyLines: Int,
    ast: AST,
    off: Int
  ): Line[Some[AST.Documented]] = {
    val doc        = createDocFromComment(comment)
    val documented = Some(AST.Documented(doc, emptyLines, ast))
    Line(documented, off)
  }
}

////////////////////////////////////////////////////////////////////////////////
//// Doc Parser HTML Generator /////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/** This is Doc Parser HTML Generator.
  *
  * Essentially it enables Doc Parser to create pretty HTML files from
  * documented code.
  */
object DocParserHTMLGenerator {

  /** This method is used for generation of HTML files from parsed and
    * reformatted [[AST.Documented]]
    *
    * @param ast - parsed AST.Module and reformatted using Doc Parser
    */
  def generateHTMLForEveryDocumented(ast: AST): String = {
    var allDocs = new String
    val extMethodsHeader = HTML.h2(
      HTML.div(HTML.`class` := "ml-20 flex")(
        HTML.raw(
          "<MethodsIcon className=\"-ml-16 -mb-3 mr-4 self-center h-12 p-2 text-content-title-on-dark bg-accent-important fill-current rounded-xl\" />"
        ),
        HTML.p("Extension Methods")
      )
    )
    var extensionMethods = new String
    ast.map { elem =>
      elem match {
        case AST.Documented.any(documented) =>
          val file = onHTMLRendering(documented)
          documented.ast match {
            case AST.App.Infix.any(_) =>
              extensionMethods += HTML
                .div(HTML.`class` := "ml-20 mb-20")(file.code)
                .toString()
            case _ =>
              allDocs += HTML
                .div(HTML.`class` := "mb-20")(file.code)
                .toString()
          }
        case AST.Def.any(tp) =>
          tp.body match {
            case Some(body) => allDocs += generateHTMLForEveryDocumented(body)
            case None       => ()
          }
        case _ =>
          allDocs += generateHTMLForEveryDocumented(elem)
      }
      elem
    }
    if (extensionMethods.length > 0) {
      extensionMethods = extMethodsHeader.render + extensionMethods
    }
    "<div>" + allDocs + extensionMethods + "</div>"
  }

  /** Function to generate HTML File from pure doc comment w/o connection to AST
    *
    * @param doc - Doc from Doc Parser
    * @return - HTML Code from Doc
    */
  def generateHTMLPureDoc(doc: Doc): String =
    HTML
      .html(
        HTML.body(
          HTML.div(HTML.`class` := "doc")(HTML.style := "font-size: 13px;")(
            doc.html
          )
        )
      )
      .toString()

  //////////////////////////////////////////////////////////////////////////////
  //// HTML Rendering of Documentation /////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** Used to create HTML files from Doc with or without title after Doc Parser
    * Runner finished it's job
    *
    * @param documented - documented made by Doc Parser Runner from AST and Doc
    * @return - HTML code with file name
    */
  def onHTMLRendering(documented: AST.Documented): htmlFile = {
    val htmlCode = DocumentedToHtml(documented.ast, documented.doc)
    val astLines = documented.ast.show().split("\n")
    val fileName =
      astLines.head
        .replaceAll("/", "")
        .replaceAll(" ", "_")
        .split("=")
        .head
    htmlFile(htmlCode, fileName)
  }
  case class htmlFile(code: TypedTag[String], name: String)

  /** This function is used to get HTML content of Doc and try to render AST,
    * by finding if it also contains Documented to retrieve Doc and it's AST,
    * or simply call show() method on other element of AST.
    *
    * @param ast - AST from Parser
    * @param doc - Doc from Doc Parser
    * @return -  HTML Code from Doc and contents of [[AST.Def]] or
    *            [[AST.App.Infix]]
    */
  def DocumentedToHtml(ast: AST, doc: Doc): TypedTag[String] = {
    val astHTML = createHTMLFromAST(ast, doc.tags)
    val astName = HTML.div(astHTML.header)
    astHTML.body match {
      case Some(body) =>
        // Case when producing main page
        HTML.div(HTML.`class` := "main ml-20")(
          astName,
          doc.htmlWoTagsMain,
          body
        )
      case None =>
        // Case when listing atoms or methods
        HTML.div(
          astName,
          doc.htmlWoTags
        )
    }
  }

  /** This case class is used to hold HTML-rendered AST
    *
    * @param header - header of AST - name of module/method with parameters
    * @param body - body of AST - All of AST's documented submodules/methods
    */
  case class astHtmlRepr(
    header: TypedTag[String],
    body: Option[TypedTag[String]]
  )
  object astHtmlRepr {
    def apply(header: TypedTag[String], body: TypedTag[String]): astHtmlRepr =
      new astHtmlRepr(header, Some(body))
    def apply(header: TypedTag[String]): astHtmlRepr =
      new astHtmlRepr(header, None)
    def apply(): astHtmlRepr =
      new astHtmlRepr(HTML.div(), None)
  }

  /** Function invoked by [[DocumentedToHtml]] to create HTML from AST in
    * [[AST.Documented]] on every matching element
    *
    * @param ast - AST
    * @return - HTML Code
    */
  def createHTMLFromAST(
    ast: AST,
    tags: Option[Doc.Tags] = None
  ): astHtmlRepr = {
    ast match {
      case AST.Def.any(d) =>
        d.body match {
          case Some(body) =>
            body match {
              case AST.Block.any(b) =>
                createDefWithBody(d.name, d.args, b, tags)
              case _ =>
                astHtmlRepr(createAtomHtmlRepr(d.name, d.args, tags))
            }
          case None => astHtmlRepr(createAtomHtmlRepr(d.name, d.args, tags))
        }
      case AST.App.Infix.any(i) =>
        if (i.larg.show().split(" ").nonEmpty) {
          astHtmlRepr(createInfixHtmlRepr(i, tags))
        } else {
          astHtmlRepr()
        }
      case _ => astHtmlRepr()
    }
  }

  /** Helper function for [[createHTMLFromAST]] to generate appropriate code
    * from [[AST.Def]] with traversing through body and creating HTML code
    * on elements in it
    *
    * @param name - Def Name
    * @param args - Def Arguments
    * @param body - Def body
    * @return - HTML code generated from Def
    */
  def createDefWithBody(
    name: AST.Cons,
    args: List[AST],
    body: AST.Block,
    tags: Option[Doc.Tags]
  ): astHtmlRepr = {
    val firstLine = Line(Option(body.firstLine.elem), body.firstLine.off)
    val atomsHeader = HTML.h2(
      HTML.div(HTML.`class` := "flex")(
        HTML.raw(
          "<AtomsIcon className=\"-ml-16 -mb-3 mr-4 self-center h-12 p-2 text-content-title-on-dark bg-accent-important fill-current rounded-xl\" />"
        ),
        HTML.p("Atoms")
      )
    )

    val extMethodsHeader = HTML.h2(
      HTML.div(HTML.`class` := "flex")(
        HTML.raw(
          "<MethodsIcon className=\"-ml-16 -mb-3 mr-4 self-center h-12 p-2 text-content-title-on-dark bg-accent-important fill-current rounded-xl\" />"
        ),
        HTML.p("Methods")
      )
    )

    val allLines      = firstLine :: body.lines
    val generatedCode = renderHTMLOnLine(allLines)
    val atoms = generatedCode.filter(
      _.toString().contains("class=\"atom flex\"")
    )
    val methods = generatedCode.filter(
      _.toString().contains("class=\"method flex\"")
    )

    val head         = createDocTitle(name, args, tags)
    var methodsLines = HTML.div()
    var atomsLines   = HTML.div()
    if (methods.nonEmpty) {
      methodsLines =
        HTML.div(HTML.`class` := "methods")(extMethodsHeader, methods)
    }
    if (atoms.nonEmpty) {
      atomsLines = HTML.div(HTML.`class` := "atoms")(atomsHeader, atoms)
    }
    val lines = HTML.div(atomsLines, methodsLines)
    astHtmlRepr(head, lines)
  }

  def createDocTitle(
    name: AST.Cons,
    args: List[AST],
    tags: Option[Doc.Tags]
  ): TypedTag[String] = {
    val nameStr  = name.show()
    val argsStr  = args.map(_.show())
    val tagsHtml = tags.getOrElse(Doc.Elem.Text("")).html

    HTML.h1(
      HTML.p(
        HTML.span(HTML.`class` := "name")(nameStr),
        " ",
        HTML.span(HTML.`class` := "parameter")(argsStr)
      ),
      tagsHtml
    )
  }

  /** Function to generate Atoms from it's name and tags.
    *
    * @param name - Atom Name
    * @param args - Atom Arguments
    * @param tags - Atom Tags
    * @return - HTMl doc of atom.
    */
  def createAtomHtmlRepr(
    name: AST.Cons,
    args: List[AST],
    tags: Option[Doc.Tags]
  ): TypedTag[String] = {
    val nameStr  = name.show()
    val argsStr  = args.map(_.show() + " ")
    val tagsHtml = tags.getOrElse(Doc.Elem.Text("")).html

    HTML.div(HTML.`class` := "atom flex")(
      HTML.p(
        HTML.span(HTML.`class` := "name")(nameStr),
        " ",
        HTML.span(HTML.`class` := "parameter")(argsStr)
      ),
      tagsHtml
    )
  }

  /** Helper function for [[createHTMLFromAST]] to generate appropriate HTML
    * code from [[AST.App.Infix]]
    *
    * @param infix - AST Infix
    * @return - HTML code generated from Infix
    */
  def createInfixHtmlRepr(
    infix: AST.App.Infix,
    tags: Option[Doc.Tags]
  ): TypedTag[String] = {
    val nameStr  = infix.larg.show().split(" ").head
    val argsStr  = infix.larg.show().split(" ").tail.mkString(" ")
    val tagsHtml = tags.getOrElse(Doc.Elem.Text("")).html

    HTML.div(HTML.`class` := "method flex")(
      HTML.p(
        HTML.span(HTML.`class` := "name")(nameStr),
        " ",
        HTML.span(HTML.`class` := "argument")(argsStr)
      ),
      tagsHtml
    )
  }

  /** Helper function for [[createDefWithBody]] to traverse through body's lines
    * and try to generate HTML code from [[AST.Documented]] parts of it. It also
    * tries to find nested [[AST.Def]] and [[AST.App.Infix]] inside of body
    *
    * @param lines - lines inside of Def body
    * @return - HTML code generated from contents of lines
    */
  def renderHTMLOnLine(lines: List[AST.Block.OptLine]): List[TypedTag[String]] =
    lines match {
      case Line(Some(AST.Documented.any(doc)), _) :: rest =>
        val docHtml = DocumentedToHtml(doc.ast, doc.doc)
        HTML.div(docHtml) :: renderHTMLOnLine(rest)
      case _ :: rest => renderHTMLOnLine(rest)
      case other =>
        other match {
          case Nil       => List()
          case _ :: rest => renderHTMLOnLine(rest)
        }
    }

  /** Function invoked by [[DocumentedToHtml]] to create HTML.Head part of file
    *
    * @param title - HTML page title
    * @return - HTML Head Code
    */
  def createHTMLHead(title: String): TypedTag[String] = {
    val metaEquiv = HTML.httpEquiv := "Content-Type"
    val metaCont  = HTML.content := "text/html"
    val metaChar  = HTML.charset := "UTF-8"
    val meta      = HTML.meta(metaEquiv)(metaCont)(metaChar)
    val fileTitle = scalatags.Text.tags2.title(title)
    HTML.head(meta)(fileTitle)
  }
}
