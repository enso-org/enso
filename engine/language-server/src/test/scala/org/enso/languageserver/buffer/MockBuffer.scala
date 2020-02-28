package org.enso.languageserver.buffer
import org.enso.languageserver.data.buffer.StringUtils

case class MockBuffer(lines: List[String]) {
  override def toString: String = lines.mkString("")

  def take(len: Int) = MockBuffer(lines.take(len))
  def drop(len: Int) = MockBuffer(lines.drop(len))
}

case object MockBuffer {
  def apply(str: String): MockBuffer = {
    val lines = StringUtils.getLines(str)
    MockBuffer(lines)
  }
}
