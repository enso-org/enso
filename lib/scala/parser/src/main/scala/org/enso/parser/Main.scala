package org.enso.parser

import ch.jodersky.jni.nativeLoader


@nativeLoader("demo0")
class Maain {
  @native def parse(input: String): String
}

object Main {
  def main(args: Array[String]): Unit = {
    val main = new Maain
    println(main.parse("HELLO"))
  }
}