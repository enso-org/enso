package org.enso.syntax.text.ast.text

import org.enso.flexer.ADT
import org.enso.syntax.text.ast.text.Escape.Slash.toString

sealed trait Escape {
  val repr: String
}
sealed trait RawEscape {
  val repr: String
}

object Escape {

  final case object Unfinished extends RawEscape {
    val repr = ""
  }

  final case class Invalid(str: Char) extends RawEscape {
    val repr = str.toString
  }

  final case class Number(int: Int) extends Escape {
    val repr = int.toString
  }

  // Reference: https://en.wikipedia.org/wiki/String_literal
  sealed trait Unicode extends Escape
  object Unicode {

    /* Note [Circe and Naming] */
    type Invalid = InvalidUnicode

    /* Note [Circe and Naming] */
    val Invalid = InvalidUnicode

    /* Note [Circe and Naming] */
    final case class InvalidUnicode(unicode: Unicode) extends Unicode {
      val repr = unicode.repr
    }

    /* NOTE [Circe and Naming]
     * Name of the class above cannot be Invalid, as we already have
     * Escape.Invalid. And to be able to derive JSON serialization with circe
     * case class names within a trait subtree need to be unique.
     *
     * To keep this unpleasant detail hidden from library users, we introduce
     * aliases for type and object named `Invalid`.
     */

    type U16 = _U16
    final case class _U16(digits: String) extends Unicode {
      val pfx  = "u"
      val sfx  = ""
      val repr = pfx + digits + sfx
    }

    type U32 = _U32
    final case class _U32(digits: String) extends Unicode {
      val pfx  = "U"
      val sfx  = ""
      val repr = pfx + digits + sfx
    }

    type U21 = _U21
    final case class _U21(digits: String) extends Unicode {
      val pfx  = "u{"
      val sfx  = "}"
      val repr = pfx + digits + sfx
    }

    object Validator {
      val hexChars =
        (('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9')).toSet
      def isHexChar(char: Char) =
        hexChars.contains(char)
    }

    object U16 {
      def apply(digits: String): Unicode =
        if (validate(digits)) _U16(digits)
        else Invalid(_U16(digits))
      def validate(digits: String) = {
        import Validator._
        val validLength = digits.length == 4
        val validChars  = digits.forall(isHexChar)
        validLength && validChars
      }
    }
    object U32 {
      def apply(digits: String): Unicode =
        if (validate(digits)) _U32(digits)
        else Invalid(_U32(digits))
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
        if (validate(digits)) _U21(digits)
        else Invalid(_U21(digits))
      def validate(digits: String) = {
        import Validator._
        val validLength = digits.length >= 1 && digits.length <= 6
        val validChars  = digits.forall(isHexChar)
        validLength && validChars
      }
    }
  }

  case object Slash extends RawEscape {
    val code: Int     = '\\'
    def name: String  = toString
    override val repr = "\\"
  }
  case object Quote extends RawEscape {
    val code: Int     = '\''
    def name: String  = toString
    override val repr = "\'"
  }
  case object RawQuote extends RawEscape {
    val code: Int     = '"'
    def name: String  = toString
    override val repr = "\""
  }

  // Reference: https://en.wikipedia.org/wiki/String_literal
  sealed trait Character extends Escape
  object Character {
    case object a extends Character {
      val code: Int     = '\u0007'
      def name: String  = toString
      override val repr = name
    }
    case object b extends Character {
      val code: Int     = '\u0008'
      def name: String  = toString
      override val repr = name
    }
    case object f extends Character {
      val code: Int     = '\u000C'
      def name: String  = toString
      override val repr = name
    }
    case object n extends Character {
      val code: Int     = '\n'
      def name: String  = toString
      override val repr = name
    }
    case object r extends Character {
      val code: Int     = '\r'
      def name: String  = toString
      override val repr = name
    }
    case object t extends Character {
      val code: Int     = '\u0009'
      def name: String  = toString
      override val repr = name
    }
    case object v extends Character {
      val code: Int     = '\u000B'
      def name: String  = toString
      override val repr = name
    }
    case object e extends Character {
      val code: Int     = '\u001B'
      def name: String  = toString
      override val repr = name
    }
    val codes = ADT.constructors[Character]
  }

  // Reference: https://en.wikipedia.org/wiki/Control_character
  sealed trait Control extends Escape {
    val code: Int
  }
  object Control {
    case object NUL extends Control {
      val code: Int     = 0x00
      def name: String  = toString
      override val repr = name
    }
    case object SOH extends Control {
      val code: Int     = 0x01
      def name: String  = toString
      override val repr = name
    }
    case object STX extends Control {
      val code: Int     = 0x02
      def name: String  = toString
      override val repr = name
    }
    case object ETX extends Control {
      val code: Int     = 0x03
      def name: String  = toString
      override val repr = name
    }
    case object EOT extends Control {
      val code: Int     = 0x04
      def name: String  = toString
      override val repr = name
    }
    case object ENQ extends Control {
      val code: Int     = 0x05
      def name: String  = toString
      override val repr = name
    }
    case object ACK extends Control {
      val code: Int     = 0x06
      def name: String  = toString
      override val repr = name
    }
    case object BEL extends Control {
      val code: Int     = 0x07
      def name: String  = toString
      override val repr = name
    }
    case object BS extends Control {
      val code: Int     = 0x08
      def name: String  = toString
      override val repr = name
    }
    case object TAB extends Control {
      val code: Int     = 0x09
      def name: String  = toString
      override val repr = name
    }
    case object LF extends Control {
      val code: Int     = 0x0A
      def name: String  = toString
      override val repr = name
    }
    case object VT extends Control {
      val code: Int     = 0x0B
      def name: String  = toString
      override val repr = name
    }
    case object FF extends Control {
      val code: Int     = 0x0C
      def name: String  = toString
      override val repr = name
    }
    case object CR extends Control {
      val code: Int     = 0x0D
      def name: String  = toString
      override val repr = name
    }
    case object SO extends Control {
      val code: Int     = 0x0E
      def name: String  = toString
      override val repr = name
    }
    case object SI extends Control {
      val code: Int     = 0x0F
      def name: String  = toString
      override val repr = name
    }
    case object DLE extends Control {
      val code: Int     = 0x10
      def name: String  = toString
      override val repr = name
    }
    case object DC1 extends Control {
      val code: Int     = 0x11
      def name: String  = toString
      override val repr = name
    }
    case object DC2 extends Control {
      val code: Int     = 0x12
      def name: String  = toString
      override val repr = name
    }
    case object DC3 extends Control {
      val code: Int     = 0x13
      def name: String  = toString
      override val repr = name
    }
    case object DC4 extends Control {
      val code: Int     = 0x14
      def name: String  = toString
      override val repr = name
    }
    case object NAK extends Control {
      val code: Int     = 0x15
      def name: String  = toString
      override val repr = name
    }
    case object SYN extends Control {
      val code: Int     = 0x16
      def name: String  = toString
      override val repr = name
    }
    case object ETB extends Control {
      val code: Int     = 0x17
      def name: String  = toString
      override val repr = name
    }
    case object CAN extends Control {
      val code: Int     = 0x18
      def name: String  = toString
      override val repr = name
    }
    case object EM extends Control {
      val code: Int     = 0x19
      def name: String  = toString
      override val repr = name
    }
    case object SUB extends Control {
      val code: Int     = 0x1A
      def name: String  = toString
      override val repr = name
    }
    case object ESC extends Control {
      val code: Int     = 0x1B
      def name: String  = toString
      override val repr = name
    }
    case object FS extends Control {
      val code: Int     = 0x1C
      def name: String  = toString
      override val repr = name
    }
    case object GS extends Control {
      val code: Int     = 0x1D
      def name: String  = toString
      override val repr = name
    }
    case object RS extends Control {
      val code: Int     = 0x1E
      def name: String  = toString
      override val repr = name
    }
    case object US extends Control {
      val code: Int     = 0x1F
      def name: String  = toString
      override val repr = name
    }
    case object DEL extends Control {
      val code: Int     = 0x7F
      def name: String  = toString
      override val repr = name
    }
    val codes = ADT.constructors[Control]
  }
}
