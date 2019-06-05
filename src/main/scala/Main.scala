
package org.enso.syntax.text

import org.enso.syntax.text.parser.{Parser}
import java.io.{StringReader, StringWriter}

object Main extends App {
  val reader = new StringReader("a =\n  b")
  val parser = new Parser(reader)
  val ast = parser.parse
  pprint.pprintln(ast,width=3,height=1000)
}