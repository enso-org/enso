package org.enso.flexer

import java.io._
import java.nio.charset.StandardCharsets.UTF_8

/**  Fast UTF8 reader and preprocessor.
  *  It uses unboxed byte buffer under the hood,
  *  deals correctly with variable length UTF chars
  *  and replaces \r(\n) with \n and \t with 4 spaces.
  */
class ReaderUTF(val input: InputStream) {
  import ReaderUTF._

  // buffer will be unboxed as long as we don't use any fancy scala collection methods on it
  val buffer   = new Array[Byte](BUFFERSIZE)
  var offset   = 0
  var length   = BUFFERSIZE
  var charSize = 0
  var charCode = ENDOFINPUT

  def this(file: File)  = this(new FileInputStream(file))
  def this(str: String) = this(new ByteArrayInputStream(str.getBytes(UTF_8)))

  fill(0)

  protected def fill(off: Int): Unit = {
    length = off + input.read(buffer, off, BUFFERSIZE - off)
    offset = off
  }

  protected def nextByte(): Int = {
    if (offset >= length)
      if (!empty) fill(0)
      else return ENDOFINPUT
    val byte = buffer(offset)
    offset += 1
    byte.toInt
  }

  def empty: Boolean =
    offset >= length && length < BUFFERSIZE

  def nextChar(): Int = {
    charCode = nextByte()
    charSize = charLength(charCode.toByte.toInt)
    charCode = charCode & charMask(charSize)
    for (_ <- 1 until charSize)
      charCode = charCode << UTFBYTESIZE | nextByte() & charMask(-1)
    charCode
  }

  override def toString(): String = {
    val builder = new java.lang.StringBuilder()
    while (nextChar() != ENDOFINPUT) builder.appendCodePoint(charCode)
    builder.toString
  }

  final def currentStr: String =
    if (charCode < 0) "" else new String(Character.toChars(charCode))

}

object ReaderUTF {

  val ENDOFINPUT  = -1
  val BUFFERSIZE  = 32768
  val UTFBYTESIZE = 6

  /** For more info on UTF decoding look at: https://en.wikipedia.org/wiki/UTF-8 */
  def charLength(char: Int): Int =
    if (char == ENDOFINPUT) 0
    else
      ~char >> 4 match {
        case 0     => 4
        case 1     => 3
        case 2 | 3 => 2
        case _     => 1
      }

  def charMask(size: Int): Int = size match {
    case 0 => -1  // do not mask end of input
    case 1 => 127 // 0111 1111
    case 2 => 63  // 0011 1111
    case 3 => 31  // 0001 1111
    case 4 => 15  // 0000 1111
    case _ => 63  // 0011 1111
  }
}
