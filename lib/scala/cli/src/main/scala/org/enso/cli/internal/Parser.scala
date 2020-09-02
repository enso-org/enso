package org.enso.cli.internal

import org.enso.cli.{CLIOutput, Opts, OptsParseError, Spelling}

sealed trait ParserContinuation
object ParserContinuation {
  case object ContinueNormally extends ParserContinuation
  case class Escape(
    continuation: (Seq[Token], Seq[String]) => Nothing
  ) extends ParserContinuation
}

/**
  * A token used in the parser.
  */
sealed trait Token {

  /**
    * The original value that this token has been created from.
    *
    * Used to reverse the tokenization process.
    */
  def originalValue: String
}
case class PlainToken(override val originalValue: String) extends Token
case class ParameterOrFlag(parameter: String)(
  override val originalValue: String
) extends Token
case class MistypedParameter(parameter: String)(
  override val originalValue: String
) extends Token
case class ParameterWithValue(parameter: String, value: String)(
  override val originalValue: String
) extends Token

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

object Parser {

  /**
    * Parses an option set.
    *
    * @param opts the option set that defines the parsing logic
    * @param tokens the sequence of tokens to parse
    * @param additionalArguments additional arguments that may be needed by the
    *                            [[Opts.additionalArguments]] option
    * @param commandPrefix the sequence of subcommand names, used for
    *                      displaying help messages
    * @return returns either the result value of `opts` and remaining tokens or
    *         a list of errors on failure; the remaining tokens are non-empty
    *         only if `isTopLevel` is true
    */
  def parseOpts[A](
    opts: Opts[A],
    tokens: Seq[Token],
    additionalArguments: Seq[String],
    commandPrefix: Seq[String]
  ): Either[OptsParseError, (A, Option[() => Nothing])] = {
    var parseErrors: List[String] = Nil
    def addError(error: String): Unit = {
      parseErrors = error :: parseErrors
    }
    var suppressUnexpectedArgument = false
    def reportUnknownParameter(
      parameter: String,
      original: String
    ): Unit = {
      val similar =
        Spelling
          .selectClosestMatchesWithMetadata(parameter, opts.gatherOptions)
          .map(_._2)
      val suggestions =
        if (similar.isEmpty) ""
        else
          "\n\nThe most similar options are\n" +
          similar.map(CLIOutput.indent + _ + "\n").mkString
      val additional =
        if (opts.additionalArguments.isDefined)
          "\nIf the parameter is for a newer version, " +
          "you may have to include it after --."
        else ""

      suppressUnexpectedArgument = true
      addError(
        s"Unknown option `$original`." + suggestions + additional
      )
    }
    def reportUnknownPrefix(prefix: String, original: String): Unit = {
      val similar = Spelling
        .selectClosestMatchesWithMetadata(prefix, opts.gatherPrefixedParameters)
        .map(_._2)
      val suggestions =
        if (similar.isEmpty) ""
        else
          "\n\nThe most similar prefixed parameters are\n" +
          similar.map(CLIOutput.indent + _ + "\n").mkString

      addError(
        s"Unknown parameter prefix `$original`." + suggestions
      )
    }

    opts.reset()
    val tokenProvider = new TokenStream(tokens, addError)

    var escapeParsing: Option[(Seq[Token], Seq[String]) => Nothing] = None

    while (escapeParsing.isEmpty && tokenProvider.hasTokens) {
      tokenProvider.consumeToken() match {
        case PlainToken(value) =>
          if (opts.wantsArgument()) {
            val continuation = opts.consumeArgument(value, commandPrefix)
            continuation match {
              case ParserContinuation.ContinueNormally =>
              case ParserContinuation.Escape(cont) =>
                escapeParsing = Some(cont)
            }
          } else if (!suppressUnexpectedArgument) {
            addError(s"Unexpected argument `$value`.")
          }
          suppressUnexpectedArgument = false
        case token @ MistypedParameter(parameter) =>
          if (hasPrefix(parameter)) {
            val (prefix, _) = splitPrefix(parameter)
            reportUnknownPrefix(prefix, token.originalValue)
          } else {
            reportUnknownParameter(parameter, token.originalValue)
          }
          suppressUnexpectedArgument = false
        case token @ ParameterOrFlag(parameter) =>
          suppressUnexpectedArgument = false
          if (opts.flags.contains(parameter)) {
            opts.flags(parameter)()
          } else if (opts.parameters.contains(parameter)) {
            for (
              value <- tokenProvider.tryConsumeArgument(
                s"Expected a value for parameter $parameter."
              )
            ) opts.parameters(parameter)(value)
          } else if (hasPrefix(parameter)) {
            val (prefix, rest) = splitPrefix(parameter)
            if (opts.prefixedParameters.contains(prefix)) {
              for (
                value <- tokenProvider.tryConsumeArgument(
                  s"Expected a value for parameter $parameter."
                )
              ) opts.prefixedParameters(prefix)(rest, value)
            } else {
              reportUnknownPrefix(prefix, token.originalValue)
            }
          } else {
            reportUnknownParameter(parameter, token.originalValue)
          }
        case token @ ParameterWithValue(parameter, value) =>
          suppressUnexpectedArgument = false
          if (opts.parameters.contains(parameter)) {
            opts.parameters(parameter)(value)
          } else if (hasPrefix(parameter)) {
            val (prefix, rest) = splitPrefix(parameter)
            if (opts.prefixedParameters.contains(prefix)) {
              opts.prefixedParameters(prefix)(rest, value)
            } else {
              reportUnknownPrefix(prefix, token.originalValue)
            }
          } else {
            reportUnknownParameter(parameter, token.originalValue)
          }
      }
    }

    if (escapeParsing.isEmpty) {
      opts.additionalArguments match {
        case Some(additionalArgumentsHandler) =>
          additionalArgumentsHandler(additionalArguments)
        case None =>
          if (additionalArguments.nonEmpty) {
            addError("Additional arguments (after --) were not expected.")
          }
      }
    }

    val result = OptsParseError.addErrors(
      opts.result(commandPrefix),
      parseErrors.reverse
    )

    val finalResult = (escapeParsing, result) match {
      case (Some(cont), Right(_)) =>
        val pluginHandler =
          () => cont(tokenProvider.remaining(), additionalArguments)
        result.map((_, Some(pluginHandler)))
      case _ => result.map((_, None))
    }

    OptsParseError.appendHelp(finalResult)(
      s"See `${commandPrefix.mkString(" ")} --help` for usage explanation."
    )
  }

  def wantsHelp(args: Seq[Token]): Boolean =
    args.exists {
      case ParameterOrFlag("help") => true
      case ParameterOrFlag("h")    => true
      case _                       => false
    }

  private def splitAdditionalArguments(
    args: Seq[String]
  ): (Seq[String], Seq[String]) =
    args.indexOf("--") match {
      case -1 => (args, Seq())
      case dividerPos if dividerPos >= 0 =>
        (args.take(dividerPos), args.drop(dividerPos + 1))
    }

  private def hasPrefix(parameter: String): Boolean = parameter.contains('.')
  private def splitPrefix(parameter: String): (String, String) = {
    val dotPosition = parameter.indexOf('.')
    (parameter.take(dotPosition), parameter.drop(dotPosition + 1))
  }

  private val shortParam             = """-(\w)""".r
  private val longParam              = """--([\w-.]+)""".r
  private val paramWithValue         = """--([\w-.]+)=(.*)""".r
  private val mistypedLongParam      = """-([\w-.]+)""".r
  private val mistypedParamWithValue = """-([\w-.]+)=(.*)""".r
  def tokenize(args: Seq[String]): (Seq[Token], Seq[String]) = {
    def toToken(arg: String): Token =
      arg match {
        case paramWithValue(name, value) => ParameterWithValue(name, value)(arg)
        case longParam(name)             => ParameterOrFlag(name)(arg)
        case shortParam(name)            => ParameterOrFlag(name)(arg)

        case mistypedLongParam(name)         => MistypedParameter(name)(arg)
        case mistypedParamWithValue(name, _) => MistypedParameter(name)(arg)

        case _ => PlainToken(arg)
      }

    val (localArgs, rest) = splitAdditionalArguments(args)
    (localArgs.map(toToken), rest)
  }

  /**
    * The inverse of [[tokenize]]. Used when the tokens have to be parsed to an
    * external plugin.
    */
  def untokenize(tokens: Seq[Token]): Seq[String] =
    tokens.map(_.originalValue)
}
