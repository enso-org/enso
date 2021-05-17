package org.enso.syntax.text.docparser

import org.enso.syntax.text.AST
import org.enso.syntax.text.Shape.Block.Line
import org.enso.syntax.text.ast.Doc
import scalatags.Text.TypedTag
import scalatags.Text.{all => HTML}
import HTML._

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
