package org.enso.languageserver.websocket.json

import io.circe.literal._
import org.enso.logger.ReportLogsOnFailure
import org.enso.testkit.FlakySpec

class MonitoringTest
    extends BaseServerTest
    with FlakySpec
    with ReportLogsOnFailure {

  "Monitoring subsystem" must {

    "reply to ping requests" taggedAs Flaky in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "heartbeat/ping",
            "id": 1,
            "params": null
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)
    }
  }

}
