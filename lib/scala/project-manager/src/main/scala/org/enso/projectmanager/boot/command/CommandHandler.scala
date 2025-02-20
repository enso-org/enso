package org.enso.projectmanager.boot.command

import org.enso.jsonrpc.{Id, JsonProtocol, Protocol}
import org.enso.logging.config.LoggerSetup
import org.enso.projectmanager.boot.Globals.SuccessExitCode
import org.enso.projectmanager.requesthandler.FailureMapper
import zio.{Console, ExitCode, ZAny, ZIO}

final class CommandHandler(protocol: Protocol) {

  /** Print the command result to the stdout.
    *
    * @param result the command result
    * @tparam E the error type
    * @return the program exit code
    */
  def printJson[E: FailureMapper](
    result: ZIO[ZAny, E, Any]
  ): ZIO[ZAny, Throwable, ExitCode] = {
    consoleLoggingOff *>
    result
      .foldZIO(
        e => {
          val error = FailureMapper[E].mapFailure(e)
          val errorData =
            JsonProtocol.ErrorData(error.code, error.message, error.payload)
          val response = JsonProtocol.ResponseError(None, errorData)
          Console.printLine(JsonProtocol.encode(response))
        },
        r => {
          val response =
            JsonProtocol.ResponseResult(
              Id.Number(0),
              protocol.payloadsEncoder(r)
            )
          Console.printLine(JsonProtocol.encode(response))
        }
      )
      .map(_ => SuccessExitCode)
  }

  /** Print only the error command result to the stdout suppressing the successful outcome.
    *
    * @param result the command result
    * @tparam E the error type
    * @return the program exit code
    */
  def printJsonErr[E: FailureMapper](
    result: ZIO[ZAny, E, Any]
  ): ZIO[ZAny, Throwable, ExitCode] = {
    consoleLoggingOff *>
    result
      .foldZIO(
        e => {
          val error = FailureMapper[E].mapFailure(e)
          val errorData =
            JsonProtocol.ErrorData(error.code, error.message, error.payload)
          val response = JsonProtocol.ResponseError(None, errorData)
          Console.printLine(JsonProtocol.encode(response))
        },
        _ => ZIO.succeed(())
      )
      .map(_ => SuccessExitCode)
  }

  private def consoleLoggingOff: ZIO[ZAny, Throwable, Unit] =
    ZIO.attempt {
      val loggerSetup = LoggerSetup.get()
      // sets up only the default (file) logger
      loggerSetup.setup()
    }

}
