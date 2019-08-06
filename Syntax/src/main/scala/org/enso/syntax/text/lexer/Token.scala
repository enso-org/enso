package org.enso.syntax.text.lexer

///////////
// Token //
///////////

case class Token(symbol: Symbol, offset: Int, span: Int)

////////////
// Symbol //
////////////

abstract class Symbol

// Identifiers
case class Var(name: String)  extends Symbol
case class Cons(name: String) extends Symbol
case object Wildcard          extends Symbol

// Operators
case class Operator(name: String) extends Symbol
case class Modifier(name: String) extends Symbol
case object DisabledAssignment    extends Symbol

// Layout
case object EOL          extends Symbol
case object BOF          extends Symbol
case object EOF          extends Symbol
case object BlockBegin   extends Symbol
case object BlockEnd     extends Symbol
case object BlockInvalid extends Symbol

case object GroupBegin  extends Symbol
case object GroupEnd    extends Symbol
case object ListBegin   extends Symbol
case object ListEnd     extends Symbol
case object RecordBegin extends Symbol
case object RecordEnd   extends Symbol

// Literals
case object TextBegin                      extends Symbol
case object TextEnd                        extends Symbol
case object TextRawBegin                   extends Symbol
case object TextRawEnd                     extends Symbol
case class Text(text: String)              extends Symbol
case class TextEscape(esc: TextEscapeType) extends Symbol
case object TextInterpolateBegin           extends Symbol
case object TextInterpolateEnd             extends Symbol
case class Number(base: Int, intPart: List[Int], fracPart: List[Int])
    extends Symbol

// Invalid
case class Invalid(reason: InvalidReason) extends Symbol
case class Unmatched(char: String)        extends Symbol

// Comments
case object Comment                  extends Symbol
case class CommentBody(text: String) extends Symbol

//////////////////
// Text Escapes //
//////////////////

abstract class TextEscapeType
case class CharEscape(code: Int)           extends TextEscapeType
case class CtrlEscape(code: Int)           extends TextEscapeType
case class IntEscape(code: Int)            extends TextEscapeType
case class Uni16Escape(code: Int)          extends TextEscapeType
case class Uni32Escape(code: Int)          extends TextEscapeType
case class Uni21Escape(code: Int)          extends TextEscapeType
case object QuoteEscape                    extends TextEscapeType
case object RawQuoteEscape                 extends TextEscapeType
case object SlashEscape                    extends TextEscapeType
case class InvalidCharEscape(char: Char)   extends TextEscapeType
case class InvalidUni32Escape(str: String) extends TextEscapeType
case class InvalidUni21Escape(str: String) extends TextEscapeType

/////////////
// Invalid //
/////////////

abstract class InvalidReason
case class UnexpectedSuffix(text: String) extends InvalidReason

////////////////
// Companions //
////////////////

object Number {

  def charToDigit(char: Char): Int = {
    val i = char.toInt
    if (i >= 48 && i <= 57) { return i - 48 } // 0 to 9
    if (i >= 65 && i <= 90) { return i - 55 } // A to Z
    if (i >= 97 && i <= 122) { return i - 87 } // a to z
    return -1
  }

  def stringToDigits(str: String): List[Int] = {
    str.toList.map(charToDigit)
  }

  def fromString(base: String, intPart: String, fracPart: String): Number = {
    val base2 = if (base == "") 10 else base.toInt
    return Number(base2, stringToDigits(intPart), stringToDigits(fracPart))
  }
}

object IntEscape {

  def fromString(code: String): IntEscape = {
    IntEscape(code.toInt)
  }
}

object CharEscape {

  def fromChar(c: Char): CharEscape = {
    CharEscape(c.toInt)
  }
}

object Uni32Escape {

  def fromString(str: String): TextEscapeType = {
    try {
      return Uni32Escape(Integer.parseInt(str, 16))
    } catch {
      case e: Exception => return InvalidUni32Escape(str)
    }
  }
}

object Uni21Escape {

  def fromString(str: String): TextEscapeType = {
    try {
      return Uni21Escape(Integer.parseInt(str, 16))
    } catch {
      case e: Exception => return InvalidUni21Escape(str)
    }
  }
}
