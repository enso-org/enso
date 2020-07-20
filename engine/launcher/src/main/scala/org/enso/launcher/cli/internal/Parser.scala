package org.enso.launcher.cli.internal

import org.enso.launcher.cli.{
  Application,
  CLIOutput,
  Command,
  Opts,
  PluginInterceptedFlow,
  PluginNotFound,
  Spelling
}

sealed trait Token
case class PlainToken(value: String)                            extends Token
case class ParameterOrFlag(parameter: String)                   extends Token
case class ParameterWithValue(parameter: String, value: String) extends Token

class TokenProvider(initialTokens: Seq[Token], errorReporter: String => Unit) {
  var tokens: List[Token] = initialTokens.toList
  def hasTokens: Boolean  = tokens.nonEmpty

  def consumeToken(): Token = {
    val token = tokens.head
    tokens = tokens.tail
    token
  }

  def peekToken(): Token = tokens.head

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

  def rest(): Seq[Token] = tokens
}

object Parser {
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
    def unknownParameter(parameter: String): Unit = {
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
        s"Unknown option `$parameter`." + suggestions + additional
      )
    }
    def unknownPrefix(prefix: String): Unit = {
      val closestMatches =
        Spelling.selectClosestMatches(prefix, opts.gatherParameterPrefixes)
      val suggestions = closestMatches match {
        case Seq()    => ""
        case Seq(one) => s" Did you mean `$one`?"
        case seq =>
          val quoted = seq.map(s => s"`$s`")
          val inits  = quoted.init.mkString(", ")
          s" Did you mean $inits or ${quoted.last}?"
      }

      addError(
        s"Unknown parameter prefix $prefix." + suggestions
      )
    }

    opts.reset()
    val tokenProvider = new TokenProvider(tokens, addError)

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
        case ParameterOrFlag(parameter) =>
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
              unknownPrefix(prefix)
            }
          } else {
            unknownParameter(parameter)
          }
        case ParameterWithValue(parameter, value) =>
          suppressUnexpectedArgument = false
          if (opts.parameters.contains(parameter)) {
            opts.parameters(parameter)(value)
          } else if (hasPrefix(parameter)) {
            val (prefix, rest) = splitPrefix(parameter)
            if (opts.prefixedParameters.contains(prefix)) {
              opts.prefixedParameters(prefix)(rest, value)
            } else {
              unknownPrefix(prefix)
            }
          } else {
            unknownParameter(parameter)
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
        opts.result().map((_, tokenProvider.rest())),
        parseErrors.reverse
      )
    )
  }

  def parseSubcommand[Config](
    application: Application[Config],
    config: Config,
    tokens: Seq[Token],
    additionalArguments: Seq[String]
  ): Either[List[String], () => Unit] =
    tokens match {
      case Seq() =>
        singleError(
          s"Expected a command. " +
          s"See `${application.commandName} --help` " +
          s"for a list of available commands."
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
                val pluginBehaviour = application.pluginManager
                  .map(_.tryRunningPlugin(commandName, untokenize(commandArgs)))
                  .getOrElse(PluginNotFound)
                pluginBehaviour match {
                  case PluginNotFound =>
                    singleError(application.commandSuggestions(commandName))
                  case PluginInterceptedFlow(run) => Right(run)
                }
            }
        }
    }

  def wantsHelp(args: Seq[Token]): Boolean =
    args.contains(ParameterOrFlag("help")) ||
    args.contains(ParameterOrFlag("h"))

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

  private val shortParam     = """-(\w)""".r
  private val longParam      = """--([\w.]+)""".r
  private val paramWithValue = """--([\w.]+)=(.*)""".r
  def tokenize(args: Seq[String]): (Seq[Token], Seq[String]) = {
    def toToken(arg: String): Token =
      arg match {
        case paramWithValue(name, value) => ParameterWithValue(name, value)
        case longParam(name)             => ParameterOrFlag(name)
        case shortParam(name)            => ParameterOrFlag(name)
        case _                           => PlainToken(arg)
      }

    val (localArgs, rest) = splitAdditionalArguments(args)
    (localArgs.map(toToken), rest)
  }

  def untokenize(tokens: Seq[Token]): Seq[String] = {
    def fromToken(arg: Token): String =
      arg match {
        case PlainToken(value)                    => value
        case ParameterOrFlag(parameter)           => s"--$parameter"
        case ParameterWithValue(parameter, value) => s"--$parameter=$value"
      }

    tokens.map(fromToken)
  }
}
