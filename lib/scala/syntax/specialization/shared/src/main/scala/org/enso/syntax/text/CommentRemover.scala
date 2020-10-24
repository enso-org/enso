package org.enso.syntax.text

/** A dirty hack that replaces all disable comments with whitespace.
  * Documentation comments are left untouched.
  *
  * @param input the original input file
  */
class CommentRemover(input: String) {

  def run: String = {
    var currentState: CommentRemover.State = CommentRemover.Base
    val it                                 = input.iterator.buffered
    val result                             = new StringBuilder(input.length)

    while (it.hasNext) {
      val char: Char = it.next()
      if (char != '\n' && currentState == CommentRemover.InComment) {
        result.addOne(' ')
      } else if (currentState == CommentRemover.Base && char == '#') {
        if (it.hasNext && it.head == '#') {
          result.addOne('#')
          currentState = CommentRemover.InDocComment
        } else {
          result.addOne(' ')
          currentState = CommentRemover.InComment
        }
      } else if (char == '\n') {
        result.addOne('\n')
        currentState = CommentRemover.Base
      } else {
        result.addOne(char)
      }
    }
    result.toString()
  }
}

object CommentRemover {
  sealed trait State
  case object Base         extends State
  case object InDocComment extends State
  case object InComment    extends State
}
