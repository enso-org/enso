package org.enso.languageserver.websocket.json

import io.circe.literal._

object ProfilingJsonMessages {

  def ok(reqId: Int) =
    json"""
        { "jsonrpc": "2.0",
          "id": $reqId,
          "result": null
        }"""

  def profilingStart(reqId: Int) =
    json"""
        { "jsonrpc": "2.0",
          "method": "profiling/start",
          "id": $reqId,
          "params": null
        }"""

  def profilingStop(reqId: Int) =
    json"""
        { "jsonrpc": "2.0",
          "method": "profiling/stop",
          "id": $reqId,
          "params": null
        }"""

  def profilingSnapshot(reqId: Int) =
    json"""
        { "jsonrpc": "2.0",
          "method": "profiling/snapshot",
          "id": $reqId,
          "params": null
        }"""

}
