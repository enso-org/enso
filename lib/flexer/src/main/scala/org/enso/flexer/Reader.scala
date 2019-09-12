package org.enso.flexer

import java.io._
import java.nio.charset.StandardCharsets.UTF_8

import org.enso.Logger

class Reader(input: InputStream) extends ReaderUTF(input) {

  import org.enso.flexer.Reader._

  lazy val logger = new Logger()
  lazy val result = new java.lang.StringBuilder()

  def this(file: File)  = this(new FileInputStream(file))
  def this(str: String) = this(new ByteArrayInputStream(str.getBytes(UTF_8)))

  final override def empty: Boolean =
    copyByte == 0 && super.empty

  final override def fill(off: Int): Unit = {
    if (rewind.maxRewindOffset == 0)
      throw new OutOfMemoryError("Rewind is impossible. Buffer is too small.")
    val keepchars = length - rewind.maxRewindOffset
    rewind.decreaseOffset(length - keepchars)
    for (i <- 1 to keepchars)
      buffer(keepchars - i) = buffer(length - i)
    super.fill(keepchars)
  }

  final override def nextChar(): Int = {
    rewind.rewinded = false
    super.nextChar()
  }

  final override def nextByte(): Int = nextByteHelper() match {
    case '\r' => '\n'
    case '\t' => ' '
    case byte => byte
  }

  private var lastByte = 0
  private var copyByte = 0

  final private def nextByteHelper(): Int = {
    if (copyByte > 0) {
      copyByte -= 1
      return lastByte
    }
    lastByte = (lastByte, super.nextByte()) match {
      case ('\r', '\n') => nextByteHelper()
      case (_, '\t')    => copyByte = TABSIZE - 1; '\t'
      case (_, byte)    => byte
    }
    lastByte
  }

  final def charOffset: Int = offset - charSize

  // because of bug in macroContext.eval it cannot be part of object rewind
  class Rewinder(index: Int) {
    import rewind._

    final def set(): Unit = logger.trace {
      rewindBy(index)(0) = charOffset
      rewindBy(index)(1) = result.length
    }

    final def run(): Unit = logger.trace {
      result.setLength(rewindBy(index)(1))
      offset = rewindBy(index)(0)
      nextChar()
      rewinded = true
    }
  }

  final object rewind {
    var rewinded = false

    lazy val rewindBy = Array(Array(0, -1), Array(0, -1))
    lazy val matched  = new Rewinder(0)
    lazy val rule     = new Rewinder(1)

    def maxRewindOffset =
      if (rewindBy(0)(1) == -1) length else rewindBy(0)(0)

    def decreaseOffset(off: Int): Unit =
      for (i <- rewind.rewindBy.indices)
        rewind.rewindBy(i)(0) -= off
  }

}

object Reader {

  val TABSIZE = 4

}
