package org.enso.languageserver.websocket.json

import io.circe.literal._

class MonitoringTest extends BaseServerTest {

  "Monitoring subsystem" must {

    "reply to ping requests" in {
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
