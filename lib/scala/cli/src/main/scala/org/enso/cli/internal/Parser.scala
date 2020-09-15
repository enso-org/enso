package org.enso.cli.internal

import org.enso.cli.arguments.{Opts, OptsParseError}
import org.enso.cli.{CLIOutput, Spelling}

object Parser {

  /**
    * Parses an option set.
    *
    * @param opts the option set that defines the parsing logic
    * @param tokens the sequence of tokens to parse
    * @param additionalArguments additional arguments that may be needed by the
    *                            [[Opts.additionalArguments]] option
    * @param applicationName application name, used for displaying help
    *                        messages
    * @return returns either the result value of `opts` and an optional closure
    *         that defines the plugin handler behaviour or a parse error
    */
  def parseOpts[A](
    opts: Opts[A],
    tokens: Seq[Token],
    additionalArguments: Seq[String],
    applicationName: String
  ): Either[OptsParseError, (A, Option[() => Int])] = {
    var parseErrors: List[String] = Nil
    def addError(error: String): Unit = {
      parseErrors = error :: parseErrors
    }

    /**
      * Flag used to avoid issuing an 'unexpected argument' error if it was
      * preceded by a potential parameter.
      *
      * It is used because if we find an unknown parameter, we do not know if it
      * is a parameter or flag and cannot make any assumptions. The argument
      * coming after it can be a plain argument ora value to the parameter - to
      * avoid confusion in the latter case, we skip the unexpected argument
      * error.
      */
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

    var escapeParsing: Option[(Seq[Token], Seq[String]) => Int] = None
    var parsingStopped: Boolean                                 = false

    while (!parsingStopped && tokenProvider.hasTokens) {
      tokenProvider.consumeToken() match {
        case PlainToken(value) =>
          if (opts.wantsArgument()) {
            val continuation = opts.consumeArgument(value, Seq(applicationName))
            continuation match {
              case ParserContinuation.ContinueNormally =>
              case ParserContinuation.Stop =>
                parsingStopped = true
              case ParserContinuation.Escape(cont) =>
                escapeParsing  = Some(cont)
                parsingStopped = true
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
                s"Expected a value for parameter `$parameter`."
              )
            ) opts.parameters(parameter)(value)
          } else if (hasPrefix(parameter)) {
            val (prefix, rest) = splitPrefix(parameter)
            if (opts.prefixedParameters.contains(prefix)) {
              for (
                value <- tokenProvider.tryConsumeArgument(
                  s"Expected a value for parameter `$parameter`."
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

    val result =
      opts.result(Seq(applicationName)).addErrors(parseErrors.reverse)

    val finalResult = (escapeParsing, result) match {
      case (Some(cont), Right(_)) =>
        val pluginHandler =
          () => cont(tokenProvider.remaining(), additionalArguments)
        result.map((_, Some(pluginHandler)))
      case _ => result.map((_, None))
    }

    finalResult
      .appendFullHelp {
        opts.help(Seq(applicationName))
      }
      .appendShortHelp {
        opts.shortHelp(Seq(applicationName))
      }
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

  /**
    * Converts a sequence of arguments into a sequence of tokens and additional
    * arguments.
    *
    * All arguments are converted into tokens until a `--` argument is
    * encountered. Any further arguments (including additional instances of
    * `--`) are treated as additional arguments.
    */
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
