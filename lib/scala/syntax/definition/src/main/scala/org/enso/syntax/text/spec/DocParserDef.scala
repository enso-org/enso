package org.enso.syntax.text.spec

import org.enso.flexer._
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
import org.enso.data.List1
import org.enso.syntax.text.ast.Doc._
import org.enso.syntax.text.ast.Doc

case class DocParserDef() extends Parser[Doc] {

  //////////////////////////////////////////////////////////////////////////////
  //// Result //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  override def getResult(): Option[Doc] = result.doc

  /** result - used to manage result from Doc Parser.
    *
    * current - used to hold elem parser works on.
    * doc - used to hold ready to get Doc after parsing.
    * stack - used to hold stack of elems.
    */
  final object result {
    var current: Option[Elem] = None
    var doc: Option[Doc]      = None
    var stack: List[Elem]     = Nil

    def push(): Unit =
      logger.trace {
        if (current.isDefined) {
          logger.log(s"Pushed: $current")
          stack +:= current.get
          current = None
        } else {
          logger.err("Undefined current")
        }
      }

    def pop(): Unit =
      logger.trace {
        if (stack.nonEmpty) {
          current = Some(stack.head)
          stack   = stack.tail
          logger.log(s"New result: $current")
        } else {
          logger.err("Trying to pop empty AST stack")
        }
      }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Basic Char Classification ///////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  val lowerChar: Pattern  = range('a', 'z')
  val upperChar: Pattern  = range('A', 'Z')
  val digit: Pattern      = range('0', '9')
  val space: Pattern      = ' '
  val whitespace: Pattern = space.many1
  val newline: Char       = '\n'

  val char: Pattern = lowerChar | upperChar
  val specialChars: Pattern =
    "," | "." | ":" | ";" | "/" | "\\" | "’" | "=" | "'" | "|" | "+" | "-" | "#" | "\"" | "(" | ")"
  val possibleChars: Pattern = char | digit | whitespace | specialChars

  val normalText: Pattern = possibleChars.many1

  //////////////////////////////////////////////////////////////////////////////
  //// Text ////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** text - used to manage normal text, made of Strings. */
  final object text {
    def onPushing(in: String): Unit =
      logger.trace {
        if (documentation.isBeginning()) {
          if (!tags.checkIfTagExistInPushedText(in)) {
            val text = removeWhitespaces(in)
            push(text)
          }
        } else if (section.isBeginning()) {
          val text = removeWhitespaces(in)
          push(text)
        } else {
          push(in)
        }
      }

    def removeWhitespaces(in: String): String =
      logger.trace {
        var text = in
        if (text.nonEmpty) {
          while (text.head == ' ' && text.length > 1) {
            text = text.tail
          }
        }
        text
      }

    def push(in: String): Unit =
      logger.trace {
        result.current = Some(Elem.Text(in))
        result.push()
      }
  }

  ROOT || normalText || text.onPushing(currentMatch)

  //////////////////////////////////////////////////////////////////////////////
  //// Tags ////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** tags - used to manage potentially tagged documentation.
    *
    * possibleTagsList - holds every correct tag possible to create.
    * stack - holds applied tags.
    */
  final object tags {
    val possibleTagsList: List[Tags.Tag.Type] =
      Tags.Tag.Type.codes.-(Tags.Tag.Unrecognized).toList
    var stack: List[Tags.Tag] = Nil

    def pushTag(indent: Int, tagType: Tags.Tag.Type, details: String): Unit =
      logger.trace {
        if (details.replaceAll("\\s", "").length == 0) {
          stack +:= Tags.Tag(indent, tagType)
        } else {
          if (details.nonEmpty) {
            var det = text.removeWhitespaces(details)
            if (tagType != Tags.Tag.Unrecognized) {
              det = s" $det"
            }
            stack +:= Tags.Tag(indent, tagType, Some(det))
          } else {
            Tags.Tag(indent, tagType, None)
          }
        }
        result.current = None
      }

    def checkIfTagExistInPushedText(in: String): Boolean =
      logger.trace {
        var inArray     = in.split(" ")
        var containsTag = false

        def tryFindingTagInAvailableTags(elem: String): Unit =
          logger.trace {
            for (tagType <- possibleTagsList) {
              if (elem == tagType.toString.toUpperCase) {
                containsTag = true
                val tagDet = inArray.tail.mkString(" ")
                pushTag(section.currentIndentRaw, tagType, tagDet)
              }
            }
            if (
              !containsTag && !elem.contains(newline) && inArray.tail.isEmpty
            ) {
              pushTag(section.currentIndentRaw, Tags.Tag.Unrecognized, in)
              containsTag = true
            }
          }

        if (inArray.nonEmpty) {
          while (inArray.nonEmpty && inArray.head.isEmpty) {
            section.currentIndentRaw += 1
            inArray = inArray.tail
          }
          val elem = inArray.head
          if (elem.matches("^\\b[A-Z]{2,}\\b")) {
            tryFindingTagInAvailableTags(elem)
          }
        }

        containsTag
      }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Code ////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** code - used to manage code in documentation. */
  final object code {
    def onPushingInline(in: String): Unit =
      logger.trace {
        val code = in.substring(1).dropRight(1)
        result.current = Some(Elem.CodeBlock.Inline(code))
        result.push()
      }

    def onPushingMultiline(in: String): Unit =
      logger.trace {
        val dummyLine = Elem.CodeBlock.Line(0, "")
        do {
          result.pop()
        } while (result.current.get == Elem.Newline)
        result.current match {
          case Some(code @ (_: Elem.CodeBlock)) =>
            val newElem = Elem.CodeBlock.Line(indent.current, in)
            if (code.elems.head == dummyLine) {
              result.current = Some(Elem.CodeBlock(newElem))
            } else {
              result.current = Some(Elem.CodeBlock(code.elems.append(newElem)))
            }
          case Some(_) | None => result.push()
        }
        result.push()
      }

    val inlineCodeTrigger = '`'
    val inlinePattern: Pattern =
      inlineCodeTrigger >> not(inlineCodeTrigger).many >> inlineCodeTrigger
  }

  val notNewLine: Pattern = not(newline).many1
  lazy val CODE: State    = state.define("Code")

  ROOT || code.inlinePattern || code.onPushingInline(currentMatch)
  CODE || newline            || { state.end(); state.begin(NEWLINE)  }
  CODE || notNewLine         || code.onPushingMultiline(currentMatch)
  CODE || eof                || { state.end(); documentation.onEOF() }

  //////////////////////////////////////////////////////////////////////////////
  //// Formatter ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** formatter - used to manage text formatters.
    *
    * stack - holds applied formatters until they're closed.
    */
  final object formatter {
    var stack: List[Elem.Formatter.Type] = Nil

    def onPushing(typ: Elem.Formatter.Type): Unit =
      logger.trace {
        val unclosedFormattersToCheck = decideWhichToCheckIfUnclosed(typ)
        if (stack.contains(typ)) {
          unclosedFormattersToCheck.foreach(checkForUnclosed)
          val listOfFormattedAST: List[Elem] = getElemsFromStack(typ)
          result.pop()
          result.current = Some(Elem.Formatter(typ, listOfFormattedAST))
          stack          = stack.tail
          result.push()
        } else {
          addEmptyToStack(typ)
        }
      }

    def getElemsFromStack(typ: Elem.Formatter.Type): List[Elem] =
      logger.trace {
        var listOfFormattedAST: List[Elem] = Nil
        while (
          result.stack.nonEmpty && result.stack.head != Elem.Formatter(typ)
        ) {
          result.pop()
          result.current match {
            case Some(value) => listOfFormattedAST +:= value
            case _           =>
          }
        }
        listOfFormattedAST
      }

    def addEmptyToStack(typ: Elem.Formatter.Type): Unit =
      logger.trace {
        stack +:= typ
        result.current = Some(Elem.Formatter(typ))
        result.push()
      }

    def decideWhichToCheckIfUnclosed(
      typ: Elem.Formatter.Type
    ): List[Elem.Formatter.Type] =
      logger.trace {
        typ match {
          case Elem.Formatter.Strikeout =>
            List(Elem.Formatter.Bold, Elem.Formatter.Italic)
          case Elem.Formatter.Italic =>
            List(Elem.Formatter.Bold, Elem.Formatter.Strikeout)
          case Elem.Formatter.Bold =>
            List(Elem.Formatter.Italic, Elem.Formatter.Strikeout)
          case _ => throw new Error("Trying to use non-existing formatter")
        }
      }

    def checkForUnclosed(typ: Elem.Formatter.Type): Unit =
      logger.trace {
        if (stack.nonEmpty) {
          if (stack.head == typ) {
            val listOfFormattedAST: List[Elem] = getElemsFromStack(typ)
            result.pop()
            result.current =
              Some(Elem.Formatter.Unclosed(typ, listOfFormattedAST))
            stack = stack.tail
            result.push()
          }
        }
      }

    val boldTrigger: Char      = Elem.Formatter.Bold.marker
    val italicTrigger: Char    = Elem.Formatter.Italic.marker
    val strikeoutTrigger: Char = Elem.Formatter.Strikeout.marker
  }

  ROOT || formatter.boldTrigger || formatter
    .onPushing(Elem.Formatter.Bold)

  ROOT || formatter.italicTrigger || formatter
    .onPushing(Elem.Formatter.Italic)

  ROOT || formatter.strikeoutTrigger || formatter
    .onPushing(Elem.Formatter.Strikeout)

  //////////////////////////////////////////////////////////////////////////////
  //// Header //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** header - used to create section headers in Documentation. */
  final object header {
    def create(): Unit =
      logger.trace {
        section.current match {
          case Some(_) => loopThroughStackToFindHeader()
          case None =>
            result.pop()
            result.current match {
              case Some(_: Section.Header) => loopThroughStackToFindHeader()
              case _                       => result.push()
            }
        }
      }

    def loopThroughStackToFindHeader(): Unit =
      logger.trace {
        var listForHeader: List[Elem] = Nil
        do {
          result.pop()
          listForHeader +:= result.current.get
        } while (result.current.get != Elem.Newline && result.stack.nonEmpty)
        if (result.current.get == Elem.Newline) {
          result.push()
          listForHeader = listForHeader.tail
        }
        result.current = Some(Section.Header(listForHeader.reverse))
        result.push()
      }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Links ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** link - used to create links in Documentation.
    *
    * there are 2 possible link types - Image and normal URL.
    */
  final object link {
    def onCreatingURL(): Unit =
      logger.trace {
        if (currentMatch.contains("]") && currentMatch.contains("(")) {
          val in   = currentMatch.substring(1).dropRight(1).split(']')
          val name = in(0)
          val url  = in(1).substring(1)
          pushURL(name, url)
        } else {
          onInvalidLink()
        }
      }

    def pushURL(name: String, url: String): Unit =
      logger.trace {
        result.current = Some(Elem.Link.URL(name, url))
        result.push()
      }

    def onCreatingImage(): Unit =
      logger.trace {
        if (currentMatch.contains("]") && currentMatch.contains("(")) {
          val in   = currentMatch.substring(2).dropRight(1).split(']')
          val name = in(0)
          val url  = in(1).substring(1)
          pushImage(name, url)
        } else {
          onInvalidLink()
        }
      }

    def pushImage(name: String, url: String): Unit =
      logger.trace {
        result.current = Some(Elem.Link.Image(name, url))
        result.push()
      }

    def onInvalidLink(): Unit =
      logger.trace {
        result.current = Some(Elem.Link.Invalid(currentMatch))
        result.push()
      }

    def onInvalidLinkNewline(): Unit =
      logger.trace {
        result.current = Some(Elem.Link.Invalid(currentMatch.dropRight(1)))
        result.push()
        indent.onPushingNewLine()
      }

    def onInvalidLinkEOF(): Unit =
      logger.trace {
        onInvalidLink()
        documentation.onEOF()
      }

    val urlNameTrigger: String   = "["
    val imageNameTrigger: String = Elem.Link.Image().marker.get + urlNameTrigger
    val imagePattern: Pattern    = imageNameTrigger >> not(')').many1 >> ')'
    val urlPattern: Pattern      = urlNameTrigger >> not(')').many1 >> ')'
    val invalidPatternNewline: Pattern =
      (imageNameTrigger | urlNameTrigger) >> not(
        ')'
      ).many1 >> newline
    val invalidPatternEOF: Pattern = (imageNameTrigger | urlNameTrigger) >> not(
      ')'
    ).many1 >> eof
  }

  ROOT || link.imagePattern          || link.onCreatingImage()
  ROOT || link.urlPattern            || link.onCreatingURL()
  ROOT || link.invalidPatternNewline || link.onInvalidLinkNewline()
  ROOT || link.invalidPatternEOF     || link.onInvalidLinkEOF()

  //////////////////////////////////////////////////////////////////////////////
  //// Indent Management & New line ////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** indent - used to manage text and block indentation.
    *
    * stack - holds indents for code blocks and lists.
    */
  final object indent {
    var stack: List[Int] = Nil
    def current: Int =
      stack match {
        case h :: _ => h
        case Nil    => 0
      }

    def onIndent(): Unit =
      logger.trace {
        val diff = currentMatch.length - current
        if (list.isInList) {
          if (diff > list.minIndent) {
            tryToFindCodeInStack()
            stack +:= currentMatch.length
            state.begin(CODE)
          } else {
            text.push(currentMatch)
          }
        } else {
          if (
            currentMatch.length > section.currentIndentRaw && result.stack.nonEmpty
          ) {
            tryToFindCodeInStack()
            stack +:= currentMatch.length
            state.begin(CODE)
          } else {
            section.currentIndentRaw = currentMatch.length
          }
        }
      }

    def tryToFindCodeInStack(): Unit =
      logger.trace {
        result.pop()
        result.stack.head match {
          case _: Elem.CodeBlock =>
          case _ =>
            result.push()
            val dummyLine = Elem.CodeBlock.Line(0, "")
            result.current = Some(Elem.CodeBlock(dummyLine))
        }
        result.push()
      }

    def onIndentForListCreation(indent: Int, typ: Elem.List.Type): Unit =
      logger.trace {
        val diff = indent - list.current

        if (!list.isInList) {
          section.checkForUnclosedFormattersOnEOS()
          onPushingNewLine()

          stack +:= indent
          list.startNewList(indent, typ)
        } else if (diff == 0) {
          list.endListItem()
          list.startListItem()
        } else if (diff > 0) {
          stack +:= indent
          list.endListItem()
          list.startNewList(indent, typ)
        } else {
          if (indent > list.prev) {
            list.endListItem()
            list.startMisalignedListItem(indent, typ)
          } else {
            do {
              list.endListItem()
              list.endSublist()
            } while (indent < list.current)
            list.startListItem()
          }
        }
      }

    def onPushingNewLine(): Unit =
      logger.trace {
        result.current = Some(Elem.Newline)
        result.push()
      }

    def onEmptyLine(): Unit =
      logger.trace {
        if (list.isInList) {
          list.endListItem(endList = true)
        }
        onPushingNewLine()
        section.onEOS()
      }

    def onIndentPattern(): Unit =
      logger.trace {
        state.end()
        if (result.stack.nonEmpty) {
          indent.onPushingNewLine()
        }
        indent.onIndent()
      }

    def onEOFPattern(): Unit =
      logger.trace {
        state.end()
        indent.onPushingNewLine()
        documentation.onEOF()
      }

    val emptyLine: Pattern     = whitespace.opt >> newline
    val indentPattern: Pattern = whitespace.opt.many
    val EOFPattern: Pattern    = indentPattern >> eof
  }

  lazy val NEWLINE: State = state.define("Newline")

  ROOT    || newline              || state.begin(NEWLINE)
  NEWLINE || indent.EOFPattern    || indent.onEOFPattern()
  NEWLINE || indent.indentPattern || indent.onIndentPattern()

  //////////////////////////////////////////////////////////////////////////////
  //// Lists ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** list - used to create lists for documentation.
    *
    * there are 2 possible types of lists - ordered and unordered.
    */
  final object list {

    /** The minimum list indentation consisting of a list symbol and a space
      * character.
      */
    val minIndent: Int = 2

    var stack: List[Int] = Nil
    def current: Int =
      stack match {
        case h :: _ => h
        case Nil    => 0
      }
    def prev: Int =
      stack match {
        case _ :: p :: _ => p
        case _           => 0
      }

    def isInList: Boolean = stack.nonEmpty

    def startNewList(indent: Int, listType: Elem.List.Type): Unit =
      logger.trace {
        list.stack +:= indent
        result.current = Some(Elem.List.empty(indent, listType))
        result.push()
      }

    def startListItem(): Unit =
      logger.trace {
        result.pop()
        result.current match {
          case Some(l: Elem.List) =>
            result.current = Some(l.addItem())
            result.push()
          case elem =>
            throw new IllegalStateException(
              s"Illegal startListItem state [current=$elem]"
            )
        }
      }

    def startMisalignedListItem(indent: Int, typ: Elem.List.Type): Unit =
      logger.trace {
        result.pop()
        result.current match {
          case Some(l: Elem.List) =>
            val item = Elem.MisalignedItem(indent, typ, List())
            result.current = Some(l.addItem(item))
            result.push()
          case elem =>
            throw new IllegalStateException(
              s"Illegal startMisalignedListItem state [current=$elem]"
            )
        }
      }

    def endListItem(endList: Boolean = false): Unit =
      logger.trace {
        section.checkForUnclosedFormattersOnEOS()
        val elems = stackUnwind()
        result.current match {
          case Some(l: Elem.List) =>
            if (endList) {
              list.stack = list.stack.tail
            }
            result.current = Some(l.append(elems))
            result.push()
          case elem =>
            throw new IllegalStateException(
              s"Illegal endListItem state [current=$elem]"
            )
        }
      }

    def endSublist(): Unit =
      logger.trace {
        result.current match {
          case None =>
            result.pop()
            result.current match {
              case Some(sublist: Elem.List) =>
                result.pop()
                result.current match {
                  case Some(l: Elem.List) =>
                    list.stack     = list.stack.tail
                    result.current = Some(l.addItem(sublist))
                    result.push()
                  case elem =>
                    throw new IllegalStateException(
                      s"Illegal endSublist stack [List,$elem,...]"
                    )
                }
              case elem =>
                throw new IllegalStateException(
                  s"Illegal endSublist stack [$elem,...]"
                )
            }
          case elem =>
            throw new IllegalStateException(
              s"Illegal endSublist state [current=$elem]"
            )
        }
      }

    def addLastItem(): Unit =
      logger.trace {
        val elems = stackUnwind()
        result.current match {
          case Some(l: Elem.List) =>
            list.stack = list.stack.tail
            if (elems.last == Elem.Newline) {
              result.current = Some(l.append(elems.init))
              result.push()
              result.current = Some(Elem.Newline)
              result.push()
            } else {
              result.current = Some(l.append(elems))
              result.push()
            }
            while (stack.nonEmpty) {
              list.endListItem()
              list.endSublist()
            }
          case elem =>
            throw new IllegalStateException(
              s"Illegal addLastItem state [current=$elem]"
            )
        }
      }

    /** Get all elements from the stack that were added after the [[Elem.List]]
      * node was pushed.
      */
    def stackUnwind(): List[Elem] = {
      @scala.annotation.tailrec
      def go(elems: List[Elem]): List[Elem] = {
        result.pop()
        result.current match {
          case Some(_: Elem.List) | None =>
            elems
          case Some(elem) =>
            go(elem :: elems)
        }
      }

      val init = result.current.toList
      go(init)
    }

    def onOrdered(): Unit =
      logger.trace {
        state.end()
        val listIndent = currentMatch
          .takeWhile(_ != orderedListTrigger)
          .length
        indent.onIndentForListCreation(listIndent, Elem.List.Ordered)
      }

    def onUnordered(): Unit =
      logger.trace {
        state.end()
        val listIndent = currentMatch
          .takeWhile(_ != unorderedListTrigger)
          .length
        indent.onIndentForListCreation(listIndent, Elem.List.Unordered)
      }

    val orderedListTrigger: Char   = Elem.List.Ordered.marker
    val unorderedListTrigger: Char = Elem.List.Unordered.marker

    val orderedPattern: Pattern =
      indent.indentPattern >> orderedListTrigger >> space
    val unorderedPattern: Pattern =
      indent.indentPattern >> unorderedListTrigger >> space
  }

  NEWLINE || list.orderedPattern   || list.onOrdered()
  NEWLINE || list.unorderedPattern || list.onUnordered()

  //////////////////////////////////////////////////////////////////////////////
  //// Section /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** section - used to manage sections in Documentation.
    *
    * there are 2 possible types of sections - marked and raw.
    * there are 3 possible types of marked sections:
    *   - important
    *   - info
    *   - example
    *
    * stack - holds every section in document.
    * current - holds current section type.
    * currentIndentRaw - holds indent for Raw.
    * indentBeforeM & indentAfterM - holds appropriate indents for Marked.
    */
  final object section {
    var stack: List[Section]                 = Nil
    var current: Option[Section.Marked.Type] = None
    var currentIndentRaw: Int                = 0
    var indentBeforeMarker: Int              = 0
    var indentAfterMarker: Int               = 0

    //// Section Beginning /////
    def onNew(typ: Option[Section.Marked.Type]): Unit =
      logger.trace {
        result.pop()
        current = typ
      }

    def onNewMarked(typ: Section.Marked.Type): Unit =
      logger.trace {
        createMarkedSectionIndent(typ)
        onNew(Some(typ))
        currentIndentRaw += currentMatch.length
      }

    def createMarkedSectionIndent(typ: Section.Marked.Type): Unit =
      logger.trace {
        /* NOTE
         * We are adding here '_' in front and end in case there was no
         * indent on one side or another, and then remove this added char
         * from calculation.
         * We also add currentIndentRaw as for some reason
         * it may be the left indent
         */
        val in    = "_" + currentMatch + "_"
        val inArr = in.split(typ.marker)
        indentBeforeMarker = currentIndentRaw + inArr.head.length - 1
        indentAfterMarker  = inArr.tail.head.length - 1
      }

    def onNewRaw(): Unit =
      logger.trace {
        indent.onEmptyLine()
        onNew(None)
      }

    def onNewRawWithHeader(): Unit =
      logger.trace {
        state.end()
        onNewRaw()
        result.current = Some(Section.Header())
        result.push()
      }

    def isBeginning(): Boolean =
      logger.trace {
        result.stack.isEmpty || result.stack.head.isInstanceOf[Section.Header]
      }

    //// End of Section ////
    def checkForUnclosedFormattersOnEOS(): Unit =
      logger.trace {
        formatter.checkForUnclosed(Elem.Formatter.Bold)
        formatter.checkForUnclosed(Elem.Formatter.Italic)
        formatter.checkForUnclosed(Elem.Formatter.Strikeout)
      }

    def checkForUnclosedListsOnEOS(): Unit =
      if (list.isInList) {
        list.addLastItem()
      }

    def reverseStackOnEOS(): Unit =
      logger.trace {
        result.stack = result.stack.reverse
      }

    def push(): Unit =
      logger.trace {
        result.stack match {
          case Nil =>
          /* NOTE
           * We don't want to push an empty section into stack
           * in case of parsing for example empty file
           * Then we want to get back Doc(None) and not Doc(Section())
           */
          case _ =>
            section.current match {
              case Some(marker) =>
                section.stack +:= Section.Marked(
                  indentBeforeMarker,
                  indentAfterMarker,
                  marker,
                  result.stack
                )
              case None =>
                section.stack +:= Section.Raw(currentIndentRaw, result.stack)
            }
        }
      }

    def pushReadySection(s: Section): Unit = section.stack = section.stack :+ s

    def pop(): Option[Section] =
      logger.trace {
        section.stack match {
          case ::(head, next) => {
            section.stack = next
            Some(head)
          }
          case Nil => None
        }
      }

    def cleanupOnEOS(): Unit =
      logger.trace {
        result.current  = None
        result.stack    = Nil
        formatter.stack = Nil
      }

    def onEOS(): Unit =
      logger.trace {
        checkForUnclosedFormattersOnEOS()
        checkForUnclosedListsOnEOS()
        reverseStackOnEOS()
        header.create()
        push()
        cleanupOnEOS()
      }

    val importantTrigger: Char = Section.Marked.Important.marker
    val infoTrigger: Char      = Section.Marked.Info.marker
    val exampleTrigger: Char   = Section.Marked.Example.marker

    val importantPattern: Pattern =
      indent.indentPattern >> importantTrigger >> indent.indentPattern
    val infoPattern: Pattern =
      indent.indentPattern >> infoTrigger >> indent.indentPattern
    val examplePattern: Pattern =
      indent.indentPattern >> exampleTrigger >> indent.indentPattern
  }

  NEWLINE || indent.emptyLine || section.onNewRaw()

  NEWLINE || indent.emptyLine >> indent.emptyLine || section
    .onNewRawWithHeader()

  ROOT || section.importantPattern || section
    .onNewMarked(Section.Marked.Important)

  ROOT || section.infoPattern || section
    .onNewMarked(Section.Marked.Info)

  ROOT || section.examplePattern || section
    .onNewMarked(Section.Marked.Example)

  //////////////////////////////////////////////////////////////////////////////
  //// Documentation ///////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** documentation - used to manage every action in case of end of file.
    *
    * Prepares data to be ready to output to user, also depicts type of
    * documentation - is it invoked from Parser as Multi Line or Single Line or
    * is it just ran as DocParser - for example in test suite.
    */
  final object documentation {
    def reverseSectionsStackOnEOF(): Unit =
      logger.trace {
        section.stack = section.stack.reverse
        val baseIndent =
          if (section.stack.nonEmpty) section.stack.head.indent else 0
        if (section.stack.length > 2) {
          transformOverlyIndentedRawIntoCode(baseIndent)
        }
      }

    def transformOverlyIndentedRawIntoCode(
      baseIndent: Int
    ): Unit = {
      var newStack      = List[Section]()
      var currentIndent = baseIndent
      while (section.stack.nonEmpty) {
        var current = section.pop().get
        if (
          current.indent > currentIndent &&
          current.isInstanceOf[Section.Raw]
        ) {
          val codeIndent                         = current.indent
          var stackOfCodeSections: List[Section] = List(current)
          while (
            section.stack.nonEmpty &&
            section.stack.head.indent >= codeIndent &&
            section.stack.head.isInstanceOf[Section.Raw]
          ) {
            current = section.pop().get
            stackOfCodeSections :+= current
          }
          val codeLines = stackOfCodeSections.flatMap {
            _.repr
              .build()
              .split("\n")
              .map { line =>
                val (indent, text) = line.span(_ == ' ')
                Doc.Elem.CodeBlock.Line(indent.length, text)
              }
          }
          if (codeLines.nonEmpty) {
            val l1CodeLines = List1(codeLines.head, codeLines.tail)
            val codeBlock   = Doc.Elem.CodeBlock(l1CodeLines)
            val newElems    = newStack.head.elems :+ codeBlock
            val newSection = newStack.head match {
              case marked: Doc.Section.Marked =>
                marked.copy(elems = newElems)
              case raw: Doc.Section.Raw =>
                raw.copy(elems = newElems)
            }
            newStack = newStack.drop(1)
            newStack +:= newSection
          }
        } else {
          currentIndent = current.indent
          newStack +:= current
        }
      }
      section.stack = newStack.reverse
    }

    def reverseTagsStackOnEOF(): Unit =
      logger.trace {
        tags.stack = tags.stack.reverse
      }

    def createDoc(): Unit =
      logger.trace {
        val tags: Option[Tags]         = createTags()
        val synopsis: Option[Synopsis] = createSynopsis()
        val body: Option[Body]         = createBody()
        result.doc = Some(Doc(tags, synopsis, body))
      }

    def createTags(): Option[Tags] =
      logger.trace {
        tags.stack match {
          case Nil     => None
          case x :: xs => Some(Tags(List1(x, xs)))
        }
      }

    def createSynopsis(): Option[Synopsis] =
      logger.trace {
        section.stack match {
          case Nil    => None
          case x :: _ => Some(Synopsis(x))
        }
      }

    def createBody(): Option[Body] =
      logger.trace {
        section.stack match {
          case Nil => None
          case _ :: xs =>
            xs match {
              case Nil     => None
              case y :: ys => Some(Body(List1(y, ys)))
            }
        }
      }

    def onEOF(): Unit =
      logger.trace {
        section.onEOS()
        reverseSectionsStackOnEOF()
        reverseTagsStackOnEOF()
        createDoc()
      }

    def isBeginning(): Boolean =
      logger.trace {
        result.stack.isEmpty && section.stack.isEmpty
      }
  }

  ROOT || eof || documentation.onEOF()
  ROOT || any || text.push(currentMatch)
}
