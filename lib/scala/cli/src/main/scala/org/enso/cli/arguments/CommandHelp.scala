package org.enso.cli.arguments

/** A help entry used in the top-level help text.
  *
  * @param name    name of a command
  * @param comment a short description of that command
  */
case class CommandHelp(name: String, comment: String) {
  override def toString: String = s"$name\t$comment"
}
