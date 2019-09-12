package org.enso.syntax.text.ast.text

import org.enso.data.ADT

sealed trait Escape {
  val repr: String
}

object Escape {


  final case class Invalid(str: String) extends Escape {
    val repr = str
  }

  final case class Number(int: Int) extends Escape {
    val repr = int.toString
  }

  // Reference: https://en.wikipedia.org/wiki/String_literal
  sealed trait Unicode extends Escape
  object Unicode {

    final case class Invalid(unicode: Unicode) extends Unicode {
      val repr = unicode.repr
    }

    abstract class U(pfx: String, sfx: String = "") extends Unicode {
      val digits: String
      val repr = pfx + digits + sfx
    }

    final case class U16 private (digits: String) extends U("u")
    final case class U32 private (digits: String) extends U("U")
    final case class U21 private (digits: String) extends U("u{", "}")

    object Validator {
      val hexChars =
        (('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9')).toSet
      def isHexChar(char: Char) =
        hexChars.contains(char)
    }

    object U16 {
      def apply(digits: String): Unicode =
        if (validate(digits)) U16(digits)
        else Invalid(U16(digits))
      def validate(digits: String) = {
        import Validator._
        val validLength = digits.length == 4
        val validChars  = digits.forall(isHexChar)
        validLength && validChars
      }
    }
    object U32 {
      def apply(digits: String): Unicode =
        if (validate(digits)) U32(digits)
        else Invalid(U32(digits))
      def validate(digits: String) = {
        import Validator._
        val validLength = digits.length == 8
        val validPrefix = digits.startsWith("00")
        val validChars  = digits.forall(isHexChar)
        validLength && validPrefix && validChars
      }
    }
    object U21 {
      def apply(digits: String): Unicode =
        if (validate(digits)) U21(digits)
        else Invalid(U21(digits))
      def validate(digits: String) = {
        import Validator._
        val validLength = digits.length >= 1 && digits.length <= 6
        val validChars  = digits.forall(isHexChar)
        validLength && validChars
      }
    }
  }


  abstract class Simple(val code: Int) extends Escape{
    def name = toString
    val repr = name
  }

  case object Slash    extends Simple('\\') { override val repr = "\\" }
  case object Quote    extends Simple('\'') { override val repr = "\'" }
  case object RawQuote extends Simple('"')  { override val repr = "\"" }

  // Reference: https://en.wikipedia.org/wiki/String_literal
  sealed trait Character extends Simple
  object Character {
    case object a extends Simple('\u0007') with Character
    case object b extends Simple('\u0008') with Character
    case object f extends Simple('\u000C') with Character
    case object n extends Simple('\n') with Character
    case object r extends Simple('\r') with Character
    case object t extends Simple('\u0009') with Character
    case object v extends Simple('\u000B') with Character
    case object e extends Simple('\u001B') with Character
    val codes = ADT.constructors[Character]
  }

  // Reference: https://en.wikipedia.org/wiki/Control_character
  sealed trait Control extends Simple
  object Control {
    case object NUL extends Simple(0x00) with Control
    case object SOH extends Simple(0x01) with Control
    case object STX extends Simple(0x02) with Control
    case object ETX extends Simple(0x03) with Control
    case object EOT extends Simple(0x04) with Control
    case object ENQ extends Simple(0x05) with Control
    case object ACK extends Simple(0x06) with Control
    case object BEL extends Simple(0x07) with Control
    case object BS  extends Simple(0x08) with Control
    case object TAB extends Simple(0x09) with Control
    case object LF  extends Simple(0x0A) with Control
    case object VT  extends Simple(0x0B) with Control
    case object FF  extends Simple(0x0C) with Control
    case object CR  extends Simple(0x0D) with Control
    case object SO  extends Simple(0x0E) with Control
    case object SI  extends Simple(0x0F) with Control
    case object DLE extends Simple(0x10) with Control
    case object DC1 extends Simple(0x11) with Control
    case object DC2 extends Simple(0x12) with Control
    case object DC3 extends Simple(0x13) with Control
    case object DC4 extends Simple(0x14) with Control
    case object NAK extends Simple(0x15) with Control
    case object SYN extends Simple(0x16) with Control
    case object ETB extends Simple(0x17) with Control
    case object CAN extends Simple(0x18) with Control
    case object EM  extends Simple(0x19) with Control
    case object SUB extends Simple(0x1A) with Control
    case object ESC extends Simple(0x1B) with Control
    case object FS  extends Simple(0x1C) with Control
    case object GS  extends Simple(0x1D) with Control
    case object RS  extends Simple(0x1E) with Control
    case object US  extends Simple(0x1F) with Control
    case object DEL extends Simple(0x7F) with Control
    val codes = ADT.constructors[Control]
  }
}