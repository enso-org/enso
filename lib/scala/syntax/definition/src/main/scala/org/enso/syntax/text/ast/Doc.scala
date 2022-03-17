package org.enso.syntax.text.ast

import org.enso.data.List1
import org.enso.flexer.ADT
import org.enso.syntax.text.ast.Repr.R
import scalatags.Text.all._
import scalatags.Text.TypedTag
import scalatags.Text.{all => HTML}
import scalatags.generic
import scalatags.text.Builder

import scala.util.Random

////////////////////////////////////////////////////////////////////////////////
//// Doc ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/** Doc - The highest level container, the output of Doc Parser
  *
  * Doc can be made of up to 3 elements:
  *
  * @param tags     - If exists, holds [[Doc#Tags]] to documented text
  * @param synopsis - If exists, holds [[Doc#Synopsis]] of documented text
  * @param body     - If exists, holds [[Doc#Body]] of documented text
  */
final case class Doc(
  tags: Option[Doc.Tags],
  synopsis: Option[Doc.Synopsis],
  body: Option[Doc.Body]
) extends Doc.Symbol {
  val repr: Repr.Builder = R + tags + synopsis + body
  val htmlWoTags: Doc.HTML = Seq(
    HTML.div(synopsis.html)(body.html)
  )
  val htmlWoTagsMain: Doc.HTML = synopsis match {
    case Some(s) => Seq(HTML.div(s.htmlBig)(body.html))
    case None    => Seq(HTML.div(body.html))
  }
  val html: Doc.HTML = Seq(
    HTML.div(tags.html)(synopsis.html)(body.html)
  )

  def htmlWithTitle(title: String): Doc.HTML = {
    if (title != "") {
      Seq(
        HTML.div(
          HTML.div(HTML.`class` := "doc-title-container")(
            HTML.div(HTML.`class` := "doc-title-name")(title),
            tags.html
          )
        )(synopsis.html)(body.html)
      )
    } else {
      Seq(HTML.div(tags.html)(synopsis.html)(body.html))
    }
  }
}

object Doc {
  def apply(): Doc           = Doc(None, None, None)
  def apply(tags: Tags): Doc = Doc(Some(tags), None, None)
  def apply(synopsis: Synopsis): Doc =
    Doc(None, Some(synopsis), None)
  def apply(synopsis: Synopsis, body: Body): Doc =
    Doc(None, Some(synopsis), Some(body))
  def apply(tags: Tags, synopsis: Synopsis): Doc =
    Doc(Some(tags), Some(synopsis), None)
  def apply(tags: Tags, synopsis: Synopsis, body: Body): Doc =
    Doc(Some(tags), Some(synopsis), Some(body))

  type HTML    = Seq[Modifier]
  type HTMLTag = TypedTag[String]

  //////////////////////////////////////////////////////////////////////////////
  //// Symbol //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** Symbol - the most low-level element, on top of which every other element
    * is built
    *
    * It extends Repr.Provider, so it also contain repr method, as well as
    * span and show values. In addition to that it specifies html method for
    * extending tokens and getting HTML file out of Doc Parser
    */
  sealed trait Symbol extends Repr.Provider {
    def show(): String = repr.build()
    def html: HTML

    def htmlCls(): generic.AttrPair[Builder, String] =
      HTML.`class` := getClass.toString.split('$').last.split('.').last
  }

  implicit final class ExtForSymbol[T <: Symbol](val self: Option[T]) {
    val dummyText  = Elem.Text("")
    val html: HTML = self.getOrElse(dummyText).html
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Elem ////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** Elem - the trait for proper element of Doc, which elements can be used in
    * higher level elements
    * Invalid - trait for invalid element of Doc, which elements can be used in
    * higher level elements
    */
  sealed trait Elem extends Symbol
  object Elem {
    sealed trait Invalid extends Elem

    ////////////////////////////////////////////////////////////////////////////
    //// Normal text & Newline /////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    /** Text - used to hold normal string as Elem
      * Newline - used to hold newline ('\n') as elem
      */
    final case class Text(text: String) extends Elem {
      val repr: Repr.Builder = text
      val html: HTML         = Seq(text)
    }

    implicit def stringToText(str: String): Elem.Text = Elem.Text(str)

    case object Newline extends Elem {
      val repr: Repr.Builder = R + "\n"
      val html: HTML         = Seq(" ")
    }

    ////////////////////////////////////////////////////////////////////////////
    //// Text Formatter - Bold, Italic, Strikeout //////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    /** Formatter - element used to hold formatted text
      *
      * @param typ - specifies type of formatter (Bold, Italic, Strikeout)
      * @param elems - elems which make up formatter
      */
    final case class Formatter(typ: Formatter.Type, elems: scala.List[Elem])
        extends Elem {
      val repr: Repr.Builder = R + typ.marker + elems + typ.marker
      val html: HTML         = Seq(typ.htmlMarker(elems.html))
    }

    object Formatter {
      def apply(typ: Type): Formatter = Formatter(typ, Nil)
      def apply(typ: Type, elem: Elem): Formatter =
        Formatter(typ, elem :: Nil)
      def apply(typ: Type, elems: Elem*): Formatter =
        Formatter(typ, elems.toList)

      abstract class Type(val marker: Char, val htmlMarker: HTMLTag)
      case object Bold      extends Type('*', HTML.b)
      case object Italic    extends Type('_', HTML.i)
      case object Strikeout extends Type('~', HTML.s)

      /** Unclosed - Invalid formatter made by parser if user has invoked
        * formatter but hasn't ended it
        *
        * @param typ - specifies type of formatter (Bold, Italic, Strikeout)
        * @param elems - elems which make up unclosed formatter
        */
      final case class Unclosed(typ: Type, elems: scala.List[Elem])
          extends Elem.Invalid {
        val repr: Repr.Builder = R + typ.marker + elems
        val html: HTML         = Seq(HTML.div(htmlCls())(typ.htmlMarker(elems.html)))
      }

      object Unclosed {
        def apply(typ: Type): Unclosed             = Unclosed(typ, Nil)
        def apply(typ: Type, elem: Elem): Unclosed = Unclosed(typ, elem :: Nil)
        def apply(typ: Type, elems: Elem*): Unclosed =
          Unclosed(typ, elems.toList)
      }
    }

    implicit final class ExtForListOfElem(val self: scala.List[Elem])
        extends Symbol {
      val repr: Repr.Builder = R + self.map(_.repr)
      val html: HTML         = Seq(self.map(_.html))
    }

    ////////////////////////////////////////////////////////////////////////////
    //// Code //////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    /** Code - block used to hold lines of code in Documentation
      *
      * @param elems - lines of code
      */
    final case class CodeBlock(elems: List1[CodeBlock.Line]) extends Elem {
      val newLn: Elem        = Elem.Newline
      val repr: Repr.Builder = R + elems.head + elems.tail.map(R + newLn + _)
      val html: HTML = {
        val uniqueIDCode = Random.alphanumeric.take(8).mkString("")
        val uniqueIDBtn  = Random.alphanumeric.take(8).mkString("")
        val htmlIdCode   = HTML.`id` := uniqueIDCode
        val htmlIdBtn    = HTML.`id` := uniqueIDBtn
        val firstIndent  = elems.head.indent
        val elemsHTML    = elems.toList.map(_.htmlOffset(firstIndent))
        val copyClass    = HTML.`class` := "doc-copy-btn flex"
        val codeClass    = HTML.`class` := "doc-code-container"
        val copyBtn      = HTML.button(htmlIdBtn)(copyClass)("Copy")
        Seq(
          HTML.div(
            HTML.div(codeClass)(htmlIdCode)(HTML.pre(elemsHTML)),
            copyBtn
          )
        )
      }
    }
    object CodeBlock {
      def apply(elem: CodeBlock.Line): CodeBlock = CodeBlock(List1(elem))
      def apply(elems: CodeBlock.Line*): CodeBlock =
        CodeBlock(
          List1(elems.head, elems.tail.toList)
        )

      /** Inline - line of code which is in line with other elements
        * Line - elem which is a part of Code Block
        */
      final case class Inline(str: String) extends Elem {
        val marker             = '`'
        val repr: Repr.Builder = R + marker + str + marker
        val html: HTML         = Seq(HTML.code(str))
      }
      final case class Line(indent: Int, elem: String) extends Elem {
        val repr: Repr.Builder = R + indent + elem
        val html: HTML         = Seq(HTML.code(" " * indent + elem), HTML.br)
        def htmlOffset(off: Int): HTML =
          Seq(HTML.code(" " * (indent - off) + elem), HTML.br)
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    //// Link - URL & Image ////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    /** Link - element used to hold links
      *
      * @param name - specifies where does the link take us
      * @param url - specifies address
      *
      * there are two kinds of links - normal URL and Image embedded in text
      *
      * Link.Invalid - something that couldn't be pattern matched to create link
      */
    abstract class Link(name: String, url: String, val marker: Option[String])
        extends Elem {
      val repr: Repr.Builder = R + marker + "[" + name + "](" + url + ")"
      val html: HTML = this match {
        case _: Link.URL   => Seq(HTML.a(HTML.href := url)(name))
        case _: Link.Image => Seq(HTML.img(HTML.src := url), name)
      }
    }

    object Link {
      final case class URL(name: String, url: String)
          extends Link(name, url, None)
      object URL {
        def apply(): URL = URL("", "")
      }

      final case class Image(name: String, url: String)
          extends Link(name, url, Some("!"))
      object Image {
        def apply(): Image = Image("", "")
      }

      final case class Invalid(elem: String) extends Elem {
        val repr: Repr.Builder = R + elem
        val html: HTML = {
          val htmlClass = HTML.`class` := this.productPrefix + getObjectName
          Seq(HTML.div(htmlClass)(elem.html))
        }
      }

      def getObjectName: String = {
        getClass.toString.split('$').last
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    //// List - Ordered & Unordered, Invalid Indent ////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    /** An element of the list.
      *
      * The list can contain following elements:
      * - [[ListItem]] a plain list element
      * - [[List]] a sublist
      * - [[MisalignedItem]] a list item that is not aligned correctly with the
      *   previous list item
      */
    sealed trait ListElem extends Elem {

      /** Append elements to this list item.
        *
        * @param xs elements to append
        */
      def append(xs: scala.List[Elem]): ListElem
    }

    /** A list item that can hold a complex element structure.
      *
      * @param elems the elements that make up this list item
      */
    final case class ListItem(elems: scala.List[Elem]) extends ListElem {

      override val repr: Repr.Builder = R + elems

      override def html: HTML = elems.map(_.html)

      /** @inheritdoc */
      override def append(xs: scala.List[Elem]): ListItem =
        copy(elems = elems :++ xs)
    }
    object ListItem {

      /** Create a list item from the provided elemenets
        *
        * @param elems the elements that make up the list item
        * @return the list item
        */
      def apply(elems: Elem*): ListItem =
        ListItem(elems.toList)
    }

    /** The list item that is not aligned correctly with the previous list item.
      *
      * @param indent the indentation of this list item
      * @param typ the list type
      * @param elems the elements that make up this list item
      */
    final case class MisalignedItem(
      indent: Int,
      typ: List.Type,
      elems: scala.List[Elem]
    ) extends ListElem {

      override val repr: Repr.Builder =
        R + indent + typ.marker + List.ElemIndent + elems

      override def html: HTML =
        elems.map(_.html)

      /** @inheritdoc */
      override def append(xs: scala.List[Elem]): MisalignedItem =
        copy(elems = elems :++ xs)
    }
    object MisalignedItem {

      /** Create a misaligned item from the provided elements.
        *
        * @param indent the indentation of this list item
        * @param typ the list type
        * @param elems the elements that make up the list item
        * @return the new misaligned item
        */
      def apply(indent: Int, typ: List.Type, elems: Elem*): MisalignedItem =
        new MisalignedItem(indent, typ, elems.toList)
    }

    /** List - block used to hold ordered and unordered lists
      *
      * Indent.Invalid - holds list element with invalid indent
      *
      * @param indent specifies indentation of list
      * @param typ the list type
      * @param elems the elements that make up this list
      */
    final case class List(indent: Int, typ: List.Type, elems: List1[ListElem])
        extends ListElem {

      val repr: Repr.Builder = {
        val listElems = elems.reverse
        R + indent + typ.marker + List.ElemIndent + listElems.head +
        listElems.tail.map {
          case elem: List =>
            R + Newline + elem
          case elem: ListItem =>
            R + Newline + indent + typ.marker + List.ElemIndent + elem
          case elem: MisalignedItem =>
            R + Newline + elem
        }
      }

      val html: HTML = {
        val elemsHTML = elems.reverse.toList.map {
          case elem: List     => elem.html
          case elem: ListElem => Seq(HTML.li(elem.html))
        }
        Seq(typ.HTMLMarker(elemsHTML))
      }

      /** @inheritdoc */
      override def append(xs: scala.List[Elem]): List = {
        val newElems = List1(elems.head.append(xs), elems.tail)
        this.copy(elems = newElems)
      }

      /** Add a new list item.
        *
        * @param item the list item to add
        * @return the new list with this item added
        */
      def addItem(item: ListElem): List = {
        val newElems = elems.prepend(item)
        this.copy(elems = newElems)
      }

      /** Add an empty list item.
        *
        * @return the new list with an empty list item added
        */
      def addItem(): List =
        this.copy(elems = elems.prepend(ListItem(Nil)))
    }

    object List {

      val ElemIndent: Int = 1

      /** Create an empty list.
        *
        * @param indent the list indentation
        * @param typ the list type
        * @return the new list
        */
      def empty(indent: Int, typ: Type): List =
        new List(indent, typ, List1(ListItem(Nil)))

      /** Create a new list.
        *
        * @param indent the list indentation
        * @param typ the list type
        * @param elem the first elements of this list
        * @param elems the rest of the list elements
        * @return the new list
        */
      def apply(indent: Int, typ: Type, elem: Elem, elems: Elem*): List = {
        val listItems = (elem :: elems.toList).reverse.map {
          case list: List           => list
          case elem: ListItem       => elem
          case elem: MisalignedItem => elem
          case elem                 => ListItem(elem)
        }
        new List(
          indent,
          typ,
          List1.fromListOption(listItems).get
        )
      }

      /** The list type. */
      abstract class Type(val marker: Char, val HTMLMarker: HTMLTag)
      final case object Unordered extends Type('-', HTML.ul)
      final case object Ordered   extends Type('*', HTML.ol)

    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Sections - Raw & Marked /////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** Section - block used to hold one section of text
    *
    * indent - specifies indentation of section
    * elems - elements which make up section
    *
    * Marked - Section which is marked as Important, Info or Example
    * Raw - normal, unmarked block of text
    */
  sealed trait Section extends Symbol {
    def indent: Int
    def elems:  List[Elem]

    def reprOfNormalText(elem: Elem, prevElem: Elem): Repr.Builder = {
      prevElem match {
        case Elem.Newline => R + indent + elem
        case _            => R + elem
      }
    }

    val html: HTML = Seq(HTML.div(htmlCls())(elems.map(_.html)))
  }

  object Section {

    /** Header - element used to hold header for section
      *
      * @param elems - elements which make up header
      */
    final case class Header(elems: List[Elem]) extends Elem {
      val repr: Repr.Builder = R + elems.map(_.repr)
      val html: HTML = Seq(
        HTML.div(HTML.`class` := "summary")(
          elems.map(_.html)
        )
      )
    }
    object Header {
      def apply(elem: Elem): Header   = Header(elem :: Nil)
      def apply(elems: Elem*): Header = Header(elems.toList)
    }

    final case class Marked(
      indentBeforeMarker: Int,
      indentAfterMarker: Int,
      typ: Marked.Type,
      elems: List[Elem]
    ) extends Section {
      val marker: String = typ.marker.toString
      val firstIndentRepr: Repr.Builder =
        R + indentBeforeMarker + marker + indentAfterMarker

      val dummyElem = Elem.Text("")
      val elemsRepr: List[Repr.Builder] = elems.zip(dummyElem :: elems).map {
        case (elem @ (_: Elem.List), _)      => R + elem
        case (elem @ (_: Elem.CodeBlock), _) => R + elem
        case (elem, prevElem)                => reprOfNormalText(elem, prevElem)
      }

      val repr: Repr.Builder = R + firstIndentRepr + elemsRepr
      override def htmlCls(): generic.AttrPair[Builder, String] = {
        HTML.`class` := typ.toString.toLowerCase
      }

      override def indent: Int =
        indentBeforeMarker + marker.length + indentAfterMarker
    }

    object Marked {
      def apply(
        indentBeforeMarker: Int,
        indentAfterMarker: Int,
        typ: Type
      ): Marked = Marked(indentBeforeMarker, indentAfterMarker, typ, Nil)
      def apply(
        indentBeforeMarker: Int,
        indentAfterMarker: Int,
        typ: Type,
        elem: Elem
      ): Marked =
        Marked(indentBeforeMarker, indentAfterMarker, typ, elem :: Nil)
      def apply(
        indentBeforeMarker: Int,
        indentAfterMarker: Int,
        typ: Type,
        elems: Elem*
      ): Marked =
        Marked(indentBeforeMarker, indentAfterMarker, typ, elems.toList)
      val defaultIndent = 0
      def apply(typ: Type): Marked =
        Marked(defaultIndent, defaultIndent, typ, Nil)
      def apply(typ: Type, elem: Elem): Marked =
        Marked(defaultIndent, defaultIndent, typ, elem :: Nil)
      def apply(typ: Type, elems: Elem*): Marked =
        Marked(defaultIndent, defaultIndent, typ, elems.toList)

      abstract class Type(val marker: Char)
      case object Important extends Type('!')
      case object Info      extends Type('?')
      case object Example   extends Type('>')
    }

    final case class Raw(indent: Int, elems: List[Elem]) extends Section {
      val dummyElem   = Elem.Text("")
      val newLn: Elem = Elem.Newline
      val elemsRepr: List[Repr.Builder] = elems.zip(dummyElem :: elems).map {
        case (elem @ (_: Section.Header), _) => R + newLn + indent + elem
        case (elem @ (_: Elem.List), _)      => R + elem
        case (elem @ (_: Elem.CodeBlock), _) => R + elem
        case (elem, prevElem)                => reprOfNormalText(elem, prevElem)
      }

      val repr: Repr.Builder = R + indent + elemsRepr

      override def htmlCls(): generic.AttrPair[Builder, String] = {
        HTML.`class` := "raw"
      }

      override val html: HTML = Seq(HTML.p(elems.map(_.html)))
    }

    object Raw {
      def apply(indent: Int): Raw               = Raw(indent, Nil)
      def apply(indent: Int, elem: Elem): Raw   = Raw(indent, elem :: Nil)
      def apply(indent: Int, elems: Elem*): Raw = Raw(indent, elems.toList)
      val defaultIndent                         = 0
      def apply(): Raw                          = Raw(defaultIndent, Nil)
      def apply(elem: Elem): Raw                = Raw(defaultIndent, elem :: Nil)
      def apply(elems: Elem*): Raw              = Raw(defaultIndent, elems.toList)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Synopsis ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** Synopsis - block used to hold section as a synopsis of documentation
    *
    * @param elems - sections which make up synopsis
    */
  final case class Synopsis(elems: List1[Section]) extends Symbol {
    val newLn: Elem        = Elem.Newline
    val repr: Repr.Builder = R + elems.head + elems.tail.map(R + newLn + _)
    val html: HTML = {
      Seq(
        HTML.div(HTML.`class` := "synopsis")(
          elems.toList.map(_.html)
        )
      )
    }
    val htmlBig: HTML = {
      Seq(
        HTML.div(HTML.`class` := "summary")(
          elems.toList.map(_.html)
        )
      )
    }
  }
  object Synopsis {
    def apply(elem: Section): Synopsis = Synopsis(List1(elem))
    def apply(elems: Section*): Synopsis =
      Synopsis(List1(elems.head, elems.tail.toList))
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Body ////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** Body - block used to hold proper body of documentation
    *
    * @param elems - sections which make up body
    */
  final case class Body(elems: List1[Section]) extends Symbol {
    val newLn: Elem = Elem.Newline
    val repr: Repr.Builder = R + newLn + elems.head + elems.tail.map(
      R + newLn + _
    )
    val html: HTML = Seq(
      HTML.div(HTML.`class` := "body")(elems.toList.map(_.html))
    )
  }

  object Body {
    def apply(elem: Section): Body = Body(List1(elem))
    def apply(elems: Section*): Body =
      Body(List1(elems.head, elems.tail.toList))
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Tags ////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** Tags - block used to hold tags for documentation
    *
    * @param elems - list of Tag of which Tags is made of
    */
  final case class Tags(elems: List1[Tags.Tag]) extends Symbol {
    val newLn: Elem = Elem.Newline
    val repr: Repr.Builder =
      R + elems.head + elems.tail.map(R + newLn + _) + newLn
    val html: HTML = Seq(
      HTML.div(HTML.`class` := "tags")(
        elems.toList.map(_.html)
      )
    )
  }
  object Tags {
    def apply(elem: Tag): Tags   = Tags(List1(elem))
    def apply(elems: Tag*): Tags = Tags(List1(elems.head, elems.tail.toList))

    /** Tag - one single tag for Tags
      *
      * @param indent - indent of tag
      * @param typ - type of tag, which can be one of listed in object `Type`
      * @param details - optional information for tag
      */
    final case class Tag(indent: Int, typ: Tag.Type, details: Option[String])
        extends Elem {
      val name: String  = typ.toString.toUpperCase
      val cName: String = typ.toString.toLowerCase
      val repr: Repr.Builder = typ match {
        case Tag.Unrecognized => R + indent + details
        case _                => R + indent + name + details
      }
      val html: HTML = {
        val htmlClass = HTML.`class` := "tag"
        typ match {
          case Tag.Unrecognized =>
            Seq(
              HTML.p(htmlClass)(
                HTML.span(HTML.`class` := cName)(details.html)
              )
            )
          case Tag.Type.TextOnly =>
            Seq(
              HTML.p(htmlClass)(
                HTML.span(HTML.`class` := cName)("TEXT ONLY")(
                  details.html
                )
              )
            )
          case _ =>
            Seq(
              HTML.p(htmlClass)(
                HTML.span(HTML.`class` := cName)(name)(details.html)
              )
            )
        }
      }
    }
    object Tag {
      val defaultIndent         = 0
      def apply(typ: Type): Tag = Tag(defaultIndent, typ, None)
      def apply(typ: Type, details: String): Tag =
        Tag(defaultIndent, typ, Some(details))
      def apply(indent: Int, typ: Type): Tag = Tag(indent, typ, None)
      def apply(indent: Int, typ: Type, details: String): Tag =
        Tag(indent, typ, Some(details))

      sealed trait Type
      object Type {
        case object Added      extends Type
        case object Advanced   extends Type
        case object Alias      extends Type
        case object Deprecated extends Type
        case object Modified   extends Type
        case object Private    extends Type
        case object Removed    extends Type
        case object TextOnly   extends Type
        case object Unstable   extends Type
        case object Upcoming   extends Type
        val codes = ADT.constructors[Type]
      }
      case object Unrecognized extends Type

    }

    implicit final class ExtForTagDetails(val self: Option[String]) {
      val html: HTML = Seq(self.map(HTML.span(HTML.`class` := "details")(_)))
    }
  }
}
