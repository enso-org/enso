package org.enso.gateway

import org.enso.gateway.JsonRpcController.jsonRpcVersion
import org.enso.gateway.protocol.request.Param.{ClientCapabilities, ClientInfo}
import org.enso.gateway.protocol.{Request, Requests, Response}
import org.enso.gateway.protocol.request.Params
import org.enso.gateway.protocol.request.Params.{InitializeParams, VoidParams}
import org.enso.gateway.protocol.response.Result.{InitializeResult, NullResult}
import org.enso.gateway.protocol.response.result.{
  ServerCapabilities,
  ServerInfo
}
import TestMessageDefinitions._

trait TestMessage[P <: Params] {
  def request: Request[P]

  def response: Response
}

object TestMessage {

  object Initialize extends TestMessage[InitializeParams] {
    val request = Request(
      jsonrpc = jsonRpcVersion,
      id      = id1,
      method  = Requests.Initialize.method,
      params = Some(
        InitializeParams(
          clientInfo = Some(
            ClientInfo(
              name    = clientName,
              version = Some(clientVersion)
            )
          ),
          capabilities = ClientCapabilities()
        )
      )
    )

    val response = Response.result(
      id = Some(id1),
      result = InitializeResult(
        capabilities = ServerCapabilities(),
        serverInfo = Some(
          ServerInfo(
            name    = serverName,
            version = Some(serverVersion)
          )
        )
      )
    )
  }

  object Shutdown extends TestMessage[VoidParams] {
    val request = Request(
      jsonrpc = jsonRpcVersion,
      id      = id2,
      method  = Requests.Shutdown.method,
      params  = Some(VoidParams())
    )

    val response = Response.result(
      id     = Some(id2),
      result = NullResult
    )
  }

}
