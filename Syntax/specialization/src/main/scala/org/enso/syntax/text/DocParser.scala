package org.enso.syntax.text

import org.enso.flexer
import org.enso.flexer.Reader
import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.spec.DocParserDef
import scalatags.Text.TypedTag
import scalatags.Text.{all => HTML}
import HTML._
import java.io.File
import java.io.PrintWriter
import flexer.Parser.{Result => res}
import org.enso.data.List1
import org.enso.syntax.text.AST.Block.{LineOf => Line}

////////////////////////////////////////////////////////////////////////////////
//// Doc Parser ////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/**
  * This is the class used to invoke Documentation Parser.
  *
  * It is used to create structured documentation from the blocks of commented
  * text created by the main Enso parser.
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

  //////////////////////////////////////////////////////////////////////////////
  //// HTML Rendering of Documentation /////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /**
    * Used to create HTML files from Doc with or without title after Doc Parser
    * Runner finished it's job
    *
    * @param documented - documented made by Doc Parser Runner from AST and Doc
    */
  def onHTMLRendering(documented: AST.Documented): Unit = {
    val path        = "syntax/specialization/target/"
    val cssFileName = "style.css"
    val htmlCode    = renderHTML(documented.ast, documented.doc, cssFileName)
    val astLines    = documented.ast.show().split("\n")
    val fileName    = astLines.head.replaceAll("/", "")
    saveHTMLToFile(path, fileName, htmlCode)
  }

  /**
    * Function invoked by [[onHTMLRendering]] to render HTML File
    *
    * @param ast - ast from Doc Parser Runner
    * @param doc - Doc from Doc Parser
    * @param cssLink - string containing CSS file name
    * @return - HTML Code from Doc with optional title from AST
    */
  def renderHTML(
    ast: AST,
    doc: Doc,
    cssLink: String = "style.css"
  ): TypedTag[String] = {
    val title         = ast.show().split("\n").head
    val astHtml       = Seq(HTML.div(HTML.`class` := "ASTData")(ast.show()))
    val docClass      = HTML.`class` := "Documentation"
    val documentation = Seq(HTML.div(docClass)(doc.html, astHtml))
    HTML.html(createHTMLHead(title, cssLink), HTML.body(documentation))
  }

  /**
    * Function invoked by [[renderHTML]] to create HTML.Head part of file
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

object DocParser {
  type Result[T] = flexer.Parser.Result[T]
  private val newEngine = flexer.Parser.compile(DocParserDef())

  /**
    * Doc Parser running methods, as described above, in class [[DocParser]]
    */
  def runMatched(input: String): Doc         = new DocParser().runMatched(input)
  def run(input: String):        Result[Doc] = new DocParser().run(input)

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
  * [[AST.Comment]], combined with connecting [[Doc]] with AST in [[AST.Documented]]
  * node, which gets AST from [[AST.Def]] and [[AST.App.Infix]]
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
    * Helper functions for [[createDocs]]
    * to traverse through Module and Def body
    */
  def createDocsFromModule(m: AST.Module): AST.Module = {
    val emptyLine = List1(AST.Block.OptLine())
    val transformedLines =
      List1(attachDocToSubsequentAST(m.lines.toList)).getOrElse(emptyLine)
    AST.Module(transformedLines)
  }

  def createDocsFromDefBody(
    name: AST.Cons,
    args: List[AST],
    b: AST.Block
  ): AST.Def = {
    val firstLine        = Line(Option(b.firstLine.elem), b.firstLine.off)
    val linesToTransform = firstLine :: b.lines
    val transforemdLines = attachDocToSubsequentAST(linesToTransform)
    val TLHeadElem       = transforemdLines.head.elem.get
    val TLHeadOff        = transforemdLines.head.off
    val head             = AST.Block.Line(TLHeadElem, TLHeadOff)
    val lines            = transforemdLines.tail
    val body             = AST.Block(b.typ, b.indent, b.emptyLines, head, lines)
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
    * @param off - line offset
    * @param ast - AST to go with comment into Documented
    * @return - [[AST.Documented]]
    */
  def createDocumentedLine(
    comment: AST.Comment,
    emptyLines: Int,
    ast: AST,
    off: Int
  ): AST.Block.LineOf[Some[AST.Documented]] = {
    val doc        = createDocFromComment(comment)
    val documented = Some(AST.Documented(doc, emptyLines, ast))
    Line(documented, off)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Generating HTML for created Doc's ///////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /**
    * This method is used for generation of HTML files from parsed and
    * reformatted [[AST.Documented]]
    *
    * @param ast - parsed AST.Module and reformatted using Doc Parser
    */
  def generateHTMLForEveryDocumented(ast: AST): Unit = {
    ast.map { elem =>
      elem match {
        case AST.Documented.any(d) => new DocParser().onHTMLRendering(d)
        case _                     => generateHTMLForEveryDocumented(elem)
      }
      elem
    }
  }
}
