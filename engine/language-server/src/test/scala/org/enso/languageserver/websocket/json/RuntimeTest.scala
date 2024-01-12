package org.enso.languageserver.websocket.json

import io.circe.literal._
import org.enso.languageserver.runtime.TestComponentGroups
import org.enso.logger.ReportLogsOnFailure
import org.enso.polyglot.runtime.Runtime.Api

class RuntimeTest extends BaseServerTest with ReportLogsOnFailure {

  "runtime/getComponentGroups" should {

    "return component groups successfully" in {
      val client = getInitialisedWsClient()

      // get component groups
      client.send(
        json"""
        { "jsonrpc": "2.0",
          "method": "runtime/getComponentGroups",
          "id": 1,
          "params": null
        }
        """
      )
      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.GetComponentGroupsRequest()) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.GetComponentGroupsResponse(
          TestComponentGroups.standardBase.toVector
        )
      )
      client.expectJson(json"""
        { "jsonrpc": "2.0",
          "id": 1,
          "result": {
            "componentGroups": [
              {
                "library" : "Standard.Base",
                "name" : "Input",
                "exports" : [
                  {
                    "name" : "Standard.Base.File.new"
                  },
                  {
                    "name" : "Standard.Database.Connection.Database.connect"
                  }
                ]
              }
            ]
          }
        }
        """)
    }
  }

}
