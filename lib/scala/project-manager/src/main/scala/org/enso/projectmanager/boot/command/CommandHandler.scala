package org.enso.projectmanager.boot.command

import org.enso.jsonrpc.{Id, JsonProtocol, Protocol}
import org.enso.projectmanager.boot.Globals.SuccessExitCode
import org.enso.projectmanager.requesthandler.FailureMapper
import zio.{Console, ExitCode, ZAny, ZIO}

final class CommandHandler(protocol: Protocol) {

  def printJson[E: FailureMapper](
    result: ZIO[ZAny, E, Any]
  ): ZIO[ZAny, Throwable, ExitCode] =
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
