package org.enso.cli.internal

/**
  * A mutable stream of tokens.
  * @param initialTokens initial sequence of tokens
  * @param errorReporter a function used for reporting errors
  */
class TokenStream(initialTokens: Seq[Token], errorReporter: String => Unit) {
  var tokens: List[Token] = initialTokens.toList

  /**
    * Returns true if there are more tokens available.
    */
  def hasTokens: Boolean = tokens.nonEmpty

  /**
    * Returns the next token. Cannot be called if [[hasTokens]] is false.
    */
  def consumeToken(): Token = {
    val token = tokens.head
    tokens = tokens.tail
    token
  }

  /**
    * Returns the next token, but does not remove it from the stream yet. Cannot
    * be called if [[hasTokens]] is false.
    */
  def peekToken(): Token = tokens.head

  /**
    * If the next available token is an argument, returns it. Otherwise returns
    * None and reports a specified error message.
    */
  def tryConsumeArgument(errorMessage: String): Option[String] = {
    tokens.headOption match {
      case Some(PlainToken(arg)) =>
        tokens = tokens.tail
        Some(arg)
      case _ =>
        errorReporter(errorMessage)
        None
    }
  }

  /**
    * Returns a sequence of remaining tokens.
    */
  def remaining(): Seq[Token] = tokens
}
