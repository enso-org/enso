package org.enso.cli.internal

import org.enso.cli.{
  Application,
  CLIOutput,
  Command,
  Opts,
  PluginInterceptedFlow,
  PluginNotFound,
  Spelling
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
    * @param isTopLevel determines if `opts` are top-level options or options
    *                   for a command; when an argument is encountered when
    *                   parsing top-level options, parsing is stopped to return
    *                   the top-level options and possibly continue it with
    *                   command options
    * @param command the sequance of subcommand names, used for displaying help
    *                messages
    * @return returns either the result value of `opts` and remaining tokens or
    *         a list of errors on failure; the remaining tokens are non-empty
    *         only if `isTopLevel` is true
    */
  def parseOpts[A](
    opts: Opts[A],
    tokens: Seq[Token],
    additionalArguments: Seq[String],
    isTopLevel: Boolean,
    command: Seq[String]
  ): Either[List[String], (A, Seq[Token])] = {
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

    /**
      * Specifies whether the parser should parse the next argument.
      * In top-level, we want to break when encountering the first positional
      * argument (which is the command).
      * Outside of top-level, we proceed always.
      */
    def shouldProceed(): Boolean =
      if (isTopLevel) !tokenProvider.peekToken().isInstanceOf[PlainToken]
      else true

    while (tokenProvider.hasTokens && shouldProceed()) {
      tokenProvider.consumeToken() match {
        case PlainToken(value) =>
          if (opts.wantsArgument()) {
            opts.consumeArgument(value)
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

    if (!isTopLevel) {
      opts.additionalArguments match {
        case Some(additionalArgumentsHandler) =>
          additionalArgumentsHandler(additionalArguments)
        case None =>
          if (additionalArguments.nonEmpty) {
            addError("Additional arguments (after --) were not expected.")
          }
      }
    } else if (additionalArguments.nonEmpty) {
      throw new IllegalArgumentException(
        "Additional arguments should only be provided for subcommand parsing," +
        " not at top level."
      )
    }

    def appendHelp[T](
      result: Either[List[String], T]
    ): Either[List[String], T] = {
      val help = s"See `${command.mkString(" ")} --help` for usage explanation."
      result match {
        case Left(errors) => Left(errors ++ Seq(help))
        case Right(value) => Right(value)
      }
    }

    appendHelp(
      appendErrors(
        opts.result().map((_, tokenProvider.remaining())),
        parseErrors.reverse
      )
    )
  }

  /**
    * Parses a command for the application.
    *
    * First tries to find a command in [[Application.commands]], if that fails,
    * it tries [[Command.related]] and later tries the
    * [[Application.pluginManager]] if available.
    */
  def parseCommand[Config](
    application: Application[Config],
    config: Config,
    tokens: Seq[Token],
    additionalArguments: Seq[String]
  ): Either[List[String], () => Unit] =
    tokens match {
      case Seq() =>
        singleError(
          s"Expected a command.\n\n" + application.renderHelp()
        )
      case Seq(PlainToken(commandName), commandArgs @ _*) =>
        application.commands.find(_.name == commandName) match {
          case Some(command) =>
            if (wantsHelp(commandArgs)) {
              Right(() => {
                CLIOutput.println(command.help(application.commandName))
              })
            } else {
              Parser
                .parseOpts(
                  command.opts,
                  commandArgs,
                  additionalArguments,
                  isTopLevel = false,
                  Seq(application.commandName, commandName)
                )
                .map(_._1)
                .map(runner => () => runner(config))
            }
          case None =>
            val possiblyRelated = Command.formatRelated(
              commandName,
              Seq(application.commandName),
              application.commands
            )

            possiblyRelated match {
              case Some(relatedCommandMessage) =>
                singleError(relatedCommandMessage)
              case None =>
                val additionalArgs =
                  if (additionalArguments.nonEmpty)
                    Seq("--") ++ additionalArguments
                  else Seq()
                val pluginBehaviour = application.pluginManager
                  .map(
                    _.tryRunningPlugin(
                      commandName,
                      untokenize(commandArgs) ++ additionalArgs
                    )
                  )
                  .getOrElse(PluginNotFound)
                pluginBehaviour match {
                  case PluginNotFound =>
                    singleError(application.commandSuggestions(commandName))
                  case PluginInterceptedFlow => Right(() => ())
                }
            }
        }
    }

  def wantsHelp(args: Seq[Token]): Boolean =
    args.exists {
      case ParameterOrFlag("help") => true
      case ParameterOrFlag("h")    => true
      case _                       => false
    }

  private def appendErrors[B](
    result: Either[List[String], B],
    errors: List[String]
  ): Either[List[String], B] =
    if (errors.isEmpty) result
    else
      result match {
        case Left(theirErrors) => Left(errors ++ theirErrors)
        case Right(_)          => Left(errors)
      }

  private def singleError[B](message: String): Either[List[String], B] =
    Left(List(message))

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
