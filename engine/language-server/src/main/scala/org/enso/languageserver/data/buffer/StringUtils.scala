package org.enso.languageserver.data.buffer
import scala.collection.mutable.ArrayBuffer

object StringUtils {

  /** Splits a string into lines. Both `\r\n` and `\n` are considered line
    * breaks.
    *
    * @param string the string to split.
    * @return the list of lines in `string`
    */
  def getLines(string: String): List[String] = {
    var substringStart = 0
    var curIdx         = 0

    val lines: ArrayBuffer[String] = new ArrayBuffer[String]()

    def pushString(): Unit = {
      lines.addOne(string.substring(substringStart, curIdx + 1))
      substringStart = curIdx + 1
    }

    while (curIdx < string.length) {
      val currentChar = string.charAt(curIdx)
      if (currentChar == '\r') {
        if (curIdx + 1 < string.length && string.charAt(curIdx + 1) == '\n') {
          curIdx += 1
          pushString()
        }
      } else if (currentChar == '\n') {
        pushString()
      }
      curIdx += 1
    }

    val lastLine =
      if (substringStart < string.length)
        Some(string.substring(substringStart, string.length))
      else None

    lines.toList ++ lastLine
  }

  private def endsWithCrLf(string: String): Boolean = {
    string.length > 1 &&
    string.charAt(string.length - 2) == '\r' &&
    string.charAt(string.length - 1) == '\n'
  }

  private def endsWithLf(string: String): Boolean = {
    string.length > 0 && string.charAt(string.length - 1) == '\n'
  }

  /** Checks if a string ends in a newline.
    *
    * @param string the string to check.
    * @return `true` if the string ends in a newline, false otherwise.
    */
  def endsWithNewLine(string: String): Boolean = {
    endsWithLf(string) || endsWithCrLf(string)
  }

  /** Strips a trailing new line from the end of a string.
    *
    * @param string the string to strip a newline from.
    * @return the string resulting from removing a single newline group
    *         from the end of `string`
    */
  def stripNewline(string: String): String = {
    if (endsWithCrLf(string)) {
      string.substring(0, string.length - 2)
    } else if (endsWithLf(string)) {
      string.substring(0, string.length - 1)
    } else string
  }
}
