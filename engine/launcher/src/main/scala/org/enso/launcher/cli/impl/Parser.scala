package org.enso.launcher.cli.impl

import org.enso.launcher.cli.{
  Application,
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

}

object Parser {
  def parseOpts[A](
    opts: Opts[A]
  )(args: Seq[String]): Either[List[String], A] = {
    var parseErrors: List[String] = Nil
    def addError(error: String): Unit = {
      parseErrors = error :: parseErrors
    }
    def unknownParameter(parameter: String): Unit = {
      val suggestions = Spelling
        .suggestClosestMatches(parameter, opts.gatherParameterNames)
      addError(s"Unknown parameter $parameter." + suggestions)
    }
    def unknownPrefix(prefix: String): Unit = {
      val suggestions =
        Spelling.suggestClosestMatches(prefix, opts.gatherParameterPrefixes)
      addError(s"Unknown parameter prefix $prefix." + suggestions)
    }

    opts.reset()
    val (tokens, additionalArguments) = tokenize(args)
    val tokenProvider                 = new TokenProvider(tokens, addError)

    while (tokenProvider.hasTokens) {
      tokenProvider.consumeToken() match {
        case PlainToken(value) =>
          if (opts.wantsArgument()) {
            opts.consumeArgument(value)
          } else {
            addError(s"Unexpected argument '$value'.")
          }
        case ParameterOrFlag(parameter) =>
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

    opts.additionalArguments match {
      case Some(additionalArgumentsHandler) =>
        additionalArgumentsHandler(additionalArguments)
      case None =>
        if (additionalArguments.nonEmpty) {
          addError("Additional arguments (after --) were not expected.")
        }
    }

    appendErrors(opts.result(), parseErrors)
  }

  def parseApplication(application: Application)(
    args: Seq[String]
  ): Either[List[String], () => Unit] =
    args match {
      case Seq() =>
        singleError(
          s"Expected a command. " +
          s"See ${application.name} --help for a list of available commands."
        )
      case Seq(commandName, commandArgs @ _*) =>
        application.commands.find(_.name == commandName) match {
          case Some(command) =>
            if (wantsHelp(commandArgs)) {
              Right(() => {
                println(command.help(application.name))
              })
            } else {
              Parser.parseOpts(command.opts)(commandArgs)
            }
          case None =>
            val pluginBehaviour = application.pluginManager
              .map(_.tryRunningPlugin(commandName, commandArgs))
              .getOrElse(PluginNotFound)
            pluginBehaviour match {
              case PluginNotFound =>
                val commandNames = application.gatherCommandNames()
                val suggestions =
                  Spelling.suggestClosestMatches(commandName, commandNames)
                singleError(
                  s"$commandName is not a known command." + suggestions
                )
              case PluginInterceptedFlow(run) => Right(run)
            }
        }
    }

  def wantsHelp(args: Seq[String]): Boolean =
    args.headOption.contains("help") || args.contains("--help")

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
  private def tokenize(args: Seq[String]): (Seq[Token], Seq[String]) = {
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
}
