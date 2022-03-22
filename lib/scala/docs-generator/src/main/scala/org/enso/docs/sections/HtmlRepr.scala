package org.enso.docs.sections

import org.enso.syntax.text.ast.Doc
import scalatags.Text.{all => HTML}
import scalatags.Text.all._

trait HtmlRepr[A] {

  def toHtml(a: A): Doc.HTML
}
object HtmlRepr {

  def apply[A: HtmlRepr]: HtmlRepr[A] =
    implicitly[HtmlRepr[A]]

  val newlineRepr: String = " "

  implicit val newlineHtmlRepr: HtmlRepr[Doc.Elem.Newline.type] = { _ =>
    Seq(newlineRepr)
  }

  implicit val textHtmlRepr: HtmlRepr[Doc.Elem.Text] = { elem =>
    Seq(elem.text)
  }

  implicit val unclosedHtmlRep: HtmlRepr[Doc.Elem.Formatter.Unclosed] = {
    elem => Seq(elem.repr.build())
  }

  implicit val codeBlockHtmlRepr: HtmlRepr[Doc.Elem.CodeBlock] = { elem =>
    val firstIndent = elem.elems.head.indent
    val elemsHtml   = elem.elems.toList.map(_.htmlOffset(firstIndent))
    Seq(HTML.pre(elemsHtml))
  }

  implicit val inlineCodeBlockHtmlRepr: HtmlRepr[Doc.Elem.CodeBlock.Inline] = {
    elem => Seq(HTML.code(elem.str))
  }

  implicit val codeBlockLineHtmlRepr: HtmlRepr[Doc.Elem.CodeBlock.Line] = {
    elem => Seq(HTML.code(" " * elem.indent + elem.elem), HTML.br)
  }

  implicit val formatterHtmlRepr: HtmlRepr[Doc.Elem.Formatter] = { elem =>
    Seq(elem.typ.htmlMarker(elem.elems.map(htmlRepr.toHtml)))
  }

  implicit val headerHtmlRepr: HtmlRepr[Doc.Section.Header] = { elem =>
    Seq(HTML.h1(elem.elems.map(htmlRepr.toHtml)))
  }

  implicit val imageHtmlRepr: HtmlRepr[Doc.Elem.Link.Image] = { elem =>
    Seq(HTML.img(HTML.src := elem.url), elem.name)
  }

  implicit val urlHtmlRepr: HtmlRepr[Doc.Elem.Link.URL] = { elem =>
    Seq(HTML.a(HTML.href := elem.url)(elem.name))
  }

  implicit val invalidLinkHtmlRepr: HtmlRepr[Doc.Elem.Link.Invalid] = { elem =>
    Seq(elem.repr.build())
  }

  implicit val tagHtmlRepr: HtmlRepr[Doc.Tags.Tag] = { elem =>
    Seq(elem.details.fold(elem.name)(elem.name + " " + _))
  }

  implicit val listItemHtmlRepr: HtmlRepr[Doc.Elem.ListItem] = { elem =>
    Seq(elem.elems.map(htmlRepr.toHtml))
  }

  implicit val listMisalignedItemHtmlRepr: HtmlRepr[Doc.Elem.MisalignedItem] = {
    elem => Seq(elem.elems.map(htmlRepr.toHtml))
  }

  implicit val listHtmlRepr: HtmlRepr[Doc.Elem.List] = { elem =>
    val elemsHTML = elem.elems.reverse.toList.map {
      case elem: Doc.Elem.List     => elem.html
      case elem: Doc.Elem.ListElem => Seq(HTML.li(elem.html))
    }
    Seq(elem.typ.HTMLMarker(elemsHTML))
  }

  implicit val htmlRepr: HtmlRepr[Doc.Elem] = {
    case Doc.Elem.Newline =>
      newlineHtmlRepr.toHtml(Doc.Elem.Newline)
    case elem: Doc.Elem.Text =>
      HtmlRepr[Doc.Elem.Text].toHtml(elem)
    case elem: Doc.Elem.Formatter.Unclosed =>
      HtmlRepr[Doc.Elem.Formatter.Unclosed].toHtml(elem)
    case elem: Doc.Elem.CodeBlock =>
      HtmlRepr[Doc.Elem.CodeBlock].toHtml(elem)
    case elem: Doc.Elem.CodeBlock.Inline =>
      HtmlRepr[Doc.Elem.CodeBlock.Inline].toHtml(elem)
    case elem: Doc.Elem.CodeBlock.Line =>
      HtmlRepr[Doc.Elem.CodeBlock.Line].toHtml(elem)
    case elem: Doc.Elem.Formatter =>
      HtmlRepr[Doc.Elem.Formatter].toHtml(elem)
    case elem: Doc.Section.Header =>
      HtmlRepr[Doc.Section.Header].toHtml(elem)
    case elem: Doc.Elem.Link.Invalid =>
      HtmlRepr[Doc.Elem.Link.Invalid].toHtml(elem)
    case elem: Doc.Tags.Tag =>
      HtmlRepr[Doc.Tags.Tag].toHtml(elem)
    case elem: Doc.Elem.ListItem =>
      HtmlRepr[Doc.Elem.ListItem].toHtml(elem)
    case elem: Doc.Elem.MisalignedItem =>
      HtmlRepr[Doc.Elem.MisalignedItem].toHtml(elem)
    case elem: Doc.Elem.List =>
      HtmlRepr[Doc.Elem.List].toHtml(elem)
    case link: Doc.Elem.Link =>
      link match {
        case elem: Doc.Elem.Link.URL =>
          HtmlRepr[Doc.Elem.Link.URL].toHtml(elem)
        case elem: Doc.Elem.Link.Image =>
          HtmlRepr[Doc.Elem.Link.Image].toHtml(elem)
      }
  }

}
