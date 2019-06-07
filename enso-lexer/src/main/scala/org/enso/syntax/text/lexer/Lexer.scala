package org.enso.syntax.text.lexer

import java.io.{StringReader, Reader}
import scala.collection.immutable.Vector

// class Lexer (reader:Reader) {
//   val scanner       = new Scanner(reader)
//   private var _done = false

//   def this(str:String) {
//     this(new StringReader(str))
//   }

//   def lex(): Token = {
//     if (done) {
//       return Token(EOF,0,0)
//     }
//     if (scanner.done) {
//       _done = true
//       return lex
//     }
//     val token = scanner.lex
//     if (token == null) {
//       _done = true
//       return lex
//     } else {
//       return token
//     }
//   }

//   def lexAll(): Vector[Token] = {
//     var builder = Vector.newBuilder[Token]
//     do {
//       builder += lex
//     } while (!done)
//     builder.result
//   }

//   def done(): Boolean = {
//     return _done;
//   }
// }