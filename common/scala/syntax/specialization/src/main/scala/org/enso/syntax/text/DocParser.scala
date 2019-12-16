package org.enso.syntax.text

import java.io.{File, PrintWriter}
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

/**
  * This is the class used to invoke Documentation Parser.
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

  /**
    * Used to match result of [[run]] function to possibly retrieve Doc
    *
    * @param input - input string to Doc Parser
    * @return - If it was able to retrieve Doc, then retrieved data, else
    *           exception with error message [[errMsg]]
    */
  def runMatched(input: String): Doc = run(input) match {
    case res(_, res.Success(v)) => v
    case _                      => throw new Exception(errMsg)
  }

  /**
    * Used to initialize Doc Parser with input string to get parsed Doc
    *
    * @param input - input string to Doc Parser
    * @return - unmatched result possibly containing Doc
    */
  def run(input: String): Result[Doc] = engine.run(new Reader(input))
}

object DocParser {
  type Result[T] = flexer.Parser.Result[T]
  private val newEngine = flexer.Parser.compile(DocParserDef())

  /**
    * Doc Parser running methods, as described above, in class [[DocParser]]
    */
  def runMatched(input: String): Doc  = new DocParser().runMatched(input)
  def run(input: String): Result[Doc] = new DocParser().run(input)
}

////////////////////////////////////////////////////////////////////////////////
//// Doc Parser Runner /////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/**
  * This is Doc Parser Runner.
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
  /**
    * This function invokes the documentation parser on every instance of
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

  /**
    * This is a helper function for [[createDocs]] to traverse through
    * [[AST.Module]] and create Docs from comments with appropriate [[AST]]
    */
  def createDocsFromModule(m: AST.Module): AST.Module = {
    val emptyLine = List1(AST.Block.OptLine())
    val transformedLines =
      List1(attachDocToSubsequentAST(m.lines.toList)).getOrElse(emptyLine)
    AST.Module(transformedLines)
  }

  /**
    * This is a helper function for [[createDocs]] to traverse through
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

  /**
    * This is a helper function for creating docs with AST.
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
                    commentWithInfixForDocumented(com, off, ast, rest)
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
                        commentWithInfixForDocumented(com, off, ast, rTail, emp)
                      case Line(Some(AST.Def.any(ast)), _) =>
                        commentWithDefForDocumented(com, off, ast, rTail, emp)
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

  /**
    * Creates Docs from comments found in parsed data
    *
    * @param comment - comment found in AST
    * @return - Documentation
    */
  def createDocFromComment(comment: AST.Comment): Doc = {
    val in = comment.lines.mkString("\n")
    DocParser.runMatched(in)
  }

  /**
    * Function for creating documented lines in [[attachDocToSubsequentAST]]
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
    val docLine    = createDocumentedLine(com, emptyLines, docFromAst, off)
    docLine :: attachDocToSubsequentAST(rest)
  }

  /**
    * Function for creating documented lines in [[attachDocToSubsequentAST]]
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

  /**
    * Function for creating documented lines in [[attachDocToSubsequentAST]]
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

/**
  * This is Doc Parser HTML Generator.
  *
  * Essentially it enables Doc Parser to create pretty HTML files from
  * documented code.
  *
  * When Doc Parser finishes its job user can invoke DocParserHTMLGenerator by
  * simply passing the output of Doc Parser onto function called
  * [[DocParserHTMLGenerator.generateHTMLForEveryDocumented]], and it will
  * automatically traverse through AST prepared by Doc Parser and generate
  * HTML files in all appropriate places.
  */
object DocParserHTMLGenerator {

  /**
    * This method is used for generation of HTML files from parsed and
    * reformatted [[AST.Documented]]
    *
    * @param ast - parsed AST.Module and reformatted using Doc Parser
    * @param path - path to save file
    * @param cssFileName - name of file containing stylesheets for the HTML code
    */
  def generateHTMLForEveryDocumented(
    ast: AST,
    path: String,
    cssFileName: String
  ): Unit = {
    ast.map { elem =>
      elem match {
        case AST.Documented.any(d) =>
          val file = onHTMLRendering(d, cssFileName)
          saveHTMLToFile(path, file.name, file.code)
      }
      elem
    }
  }

  /**
    * Saves HTML code to file
    *
    * @param path - path to file
    * @param name - file name
    * @param code - HTML code generated with Doc Parser
    */
  def saveHTMLToFile(
    path: String,
    name: String,
    code: TypedTag[String]
  ): Unit = {
    val writer = new PrintWriter(new File(path + name + ".html"))
    writer.write(code.toString)
    writer.close()
  }

  //////////////////////////////////////////////////////////////////////////////
  //// HTML Rendering of Documentation /////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /**
    * Used to create HTML files from Doc with or without title after Doc Parser
    * Runner finished it's job
    *
    * @param documented - documented made by Doc Parser Runner from AST and Doc
    * @param cssFileName - name of file containing stylesheets for the HTML code
    * @return - HTML code with file name
    */
  def onHTMLRendering(
    documented: AST.Documented,
    cssFileName: String
  ): htmlFile = {
    val htmlCode = renderHTML(documented.ast, documented.doc, cssFileName)
    val astLines = documented.ast.show().split("\n")
    val fileName = astLines.head.replaceAll("/", "")
    htmlFile(htmlCode, fileName)
  }
  case class htmlFile(code: TypedTag[String], name: String)

  /**
    * Function invoked by [[onHTMLRendering]] to render HTML File
    *
    * @param ast - AST from Parser
    * @param doc - Doc from Doc Parser
    * @param cssLink - string containing CSS file name
    * @return - HTML Code from Doc and contents of [[AST.Def]] or
    *           [[AST.App.Infix]], with optional title made from AST
    */
  def renderHTML(
    ast: AST,
    doc: Doc,
    cssLink: String = "style.css"
  ): TypedTag[String] = {
    val title         = ast.show().split("\n").head
    val documentation = DocumentedToHtml(ast, doc)
    HTML.html(createHTMLHead(title, cssLink), HTML.body(documentation))
  }

  /**
    * This function is used to get HTML content of Doc and try to render AST,
    * by finding if it also contains Documented to retrieve Doc and it's AST,
    * or simply call show() method on other element of AST.
    *
    * @param ast - AST from Parser
    * @param doc - Doc from Doc Parser
    * @return -  HTML Code from Doc and contents of [[AST.Def]] or
    *            [[AST.App.Infix]]
    */
  def DocumentedToHtml(
    ast: AST,
    doc: Doc
  ): TypedTag[String] = {
    val astHeadCls = HTML.`class` := "ASTHead"
    val astBodyCls = HTML.`class` := "ASTData"
    val astHTML    = createHTMLFromAST(ast)
    val astName    = Seq(HTML.div(astHeadCls)(astHTML.header))
    val astBody    = Seq(HTML.div(astBodyCls)(astHTML.body))
    val docClass   = HTML.`class` := "Documentation"
    HTML.div(docClass)(astName, doc.html, astBody)
  }

  /**
    * This case class is used to hold HTML-rendered AST
    *
    * @param header - header of AST - name of module/method with parameters
    * @param body - body of AST - All of AST's documented submodules/methods
    */
  case class astHtmlRepr(header: TypedTag[String], body: TypedTag[String])
  object astHtmlRepr {
    def apply(header: TypedTag[String]): astHtmlRepr =
      new astHtmlRepr(header, HTML.div())
    def apply(): astHtmlRepr =
      new astHtmlRepr(HTML.div(), HTML.div())
  }

  /**
    * Function invoked by [[DocumentedToHtml]] to create HTML from AST in
    * [[AST.Documented]] on every matching element
    *
    * @param ast - AST
    * @return - HTML Code
    */
  def createHTMLFromAST(ast: AST): astHtmlRepr = {
    ast match {
      case AST.Def.any(d) =>
        d.body match {
          case Some(body) =>
            body match {
              case AST.Block.any(b) => createDefWithBody(d.name, d.args, b)
              case _ =>
                astHtmlRepr(createDefWithoutBody(d.name, d.args))
            }
          case None => astHtmlRepr(createDefWithoutBody(d.name, d.args))
        }
      case AST.App.Infix.any(i) => astHtmlRepr(createInfixHtmlRepr(i))
      case _                    => astHtmlRepr()
    }
  }

  /**
    * Helper function for [[createHTMLFromAST]] to generate appropriate code
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
    body: AST.Block
  ): astHtmlRepr = {
    val firstLine     = Line(Option(body.firstLine.elem), body.firstLine.off)
    val constructors  = HTML.h2(`class` := "constr")("Constructors")
    val allLines      = firstLine :: body.lines
    val generatedCode = renderHTMLOnLine(allLines)
    val head          = createDefTitle(name, args)
    val clsBody       = HTML.`class` := "DefBody"
    val lines         = HTML.div(clsBody)(constructors, generatedCode)
    val cls           = HTML.`class` := "Def"
    astHtmlRepr(HTML.div(cls)(head), HTML.div(cls)(lines))
  }

  /**
    * Helper function for [[createHTMLFromAST]] to generate appropriate code
    * from [[AST.Def]] when it doesn't contain anything in it's body
    *
    * @param name - Def Name
    * @param args - Def Arguments
    * @return - HTML code generated from Def
    */
  def createDefWithoutBody(
    name: AST.Cons,
    args: List[AST]
  ): TypedTag[String] = {
    val cls = HTML.`class` := "DefNoBody"
    HTML.div(cls)(createDefTitle(name, args))
  }

  /**
    * Helper function for [[createDefWithBody]] or [[createDefWithoutBody]]
    * to generate [[AST.Def]] title form it's name and args
    *
    * @param name - Def Name
    * @param args - Def Arguments
    * @return - Def title in HTML
    */
  def createDefTitle(name: AST.Cons, args: List[AST]): TypedTag[String] = {
    val clsTitle = HTML.`class` := "DefTitle"
    val clsArgs  = HTML.`class` := "DefArgs"
    HTML.div(clsTitle)(name.show(), HTML.div(clsArgs)(args.map(_.show())))
  }

  /**
    * Helper function for [[createHTMLFromAST]] to generate appropriate HTML
    * code from [[AST.App.Infix]]
    *
    * @param infix - AST Infix
    * @return - HTML code generated from Infix
    */
  def createInfixHtmlRepr(infix: AST.App.Infix): TypedTag[String] = {
    val cls = HTML.`class` := "Infix"
    HTML.div(cls)(infix.larg.show())
  }

  /**
    * Helper function for [[createDefWithBody]] to traverse through body's lines
    * and try to generate HTML code from [[AST.Documented]] parts of it. It also
    * tries to find nested [[AST.Def]] and [[AST.App.Infix]] inside of body
    *
    * @param lines - lines inside of Def body
    * @return - HTML code generated from contents of lines
    */
  def renderHTMLOnLine(lines: List[AST.Block.OptLine]): List[TypedTag[String]] =
    lines match {
      case Line(Some(AST.Documented.any(doc)), _) :: rest =>
        val cls     = HTML.`class` := "DefDoc"
        val docHtml = DocumentedToHtml(doc.ast, doc.doc)
        HTML.div(cls)(docHtml) :: renderHTMLOnLine(rest)
      case x :: rest =>
        x match {
          case Line(Some(d), _) =>
            val cls     = HTML.`class` := "DefNoDoc"
            val astHtml = createHTMLFromAST(d)
            val div     = HTML.div(cls)(astHtml.header, astHtml.body)
            div :: renderHTMLOnLine(rest)
          case _ => renderHTMLOnLine(rest)
        }
      case other =>
        other match {
          case Nil       => List()
          case _ :: rest => renderHTMLOnLine(rest)
        }
    }

  /**
    * Function invoked by [[DocumentedToHtml]] to create HTML.Head part of file
    *
    * @param title - HTML page title
    * @param cssLink - string containing CSS file name
    * @return - HTML Head Code
    */
  def createHTMLHead(title: String, cssLink: String): TypedTag[String] = {
    val metaEquiv = HTML.httpEquiv := "Content-Type"
    val metaCont  = HTML.content := "text/html"
    val metaChar  = HTML.charset := "UTF-8"
    val meta      = HTML.meta(metaEquiv)(metaCont)(metaChar)
    val cssRel    = HTML.rel := "stylesheet"
    val cssHref   = HTML.href := cssLink
    val css       = HTML.link(cssRel)(cssHref)
    val fileTitle = scalatags.Text.tags2.title(title)
    HTML.head(meta, css)(fileTitle)
  }
}
