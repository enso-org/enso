package org.enso.languageserver.websocket

import java.util.UUID
import io.circe.literal._

class CapabilitiesTest extends WebSocketServerTest {
  "Language Server" must {
    "be able to grant and release capabilities" in {
      val probe        = new WsTestClient(address)
      val capabilityId = UUID.randomUUID()
      probe.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 1,
            "params": {
              "id": $capabilityId,
              "method": "canEdit",
              "registerOptions": { "path": "Foo/bar" }
            }
          }
          """)
      probe.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)
      probe.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/release",
            "id": 2,
            "params": {
              "id": $capabilityId
            }
          }
          """)
      probe.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
    }

    "take canEdit capability away from clients when another client registers for it" in {
      val client1       = new WsTestClient(address)
      val client2       = new WsTestClient(address)
      val client3       = new WsTestClient(address)
      val capability1Id = UUID.randomUUID()
      val capability2Id = UUID.randomUUID()
      val capability3Id = UUID.randomUUID()

      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 1,
            "params": {
              "id": $capability1Id,
              "method": "canEdit",
              "registerOptions": { "path": "Foo/bar" }
            }
          }
          """)

      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)
      client2.expectNoMessage()
      client3.expectNoMessage()

      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 2,
            "params": {
              "id": $capability2Id,
              "method": "canEdit",
              "registerOptions": { "path": "Foo/bar" }
            }
          }
          """)

      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "capability/forceReleased",
            "params": {"id": $capability1Id}
          }
          """)
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
      client3.expectNoMessage()

      client3.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 3,
            "params": {
              "id": $capability3Id,
              "method": "canEdit",
              "registerOptions": { "path": "Foo/bar" }
            }
          }
          """)

      client1.expectNoMessage()
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "capability/forceReleased",
            "params": {"id": $capability2Id}
          }
          """)
      client3.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "result": null
          }
          """)
    }

    "implement the canEdit capability on a per-file basis" in {
      val client1       = new WsTestClient(address)
      val client2       = new WsTestClient(address)
      val capability1Id = UUID.randomUUID()
      val capability2Id = UUID.randomUUID()

      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 1,
            "params": {
              "id": $capability1Id,
              "method": "canEdit",
              "registerOptions": { "path": "Foo/bar" }
            }
          }
          """)

      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)
      client2.expectNoMessage()

      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 2,
            "params": {
              "id": $capability2Id,
              "method": "canEdit",
              "registerOptions": { "path": "Foo/baz" }
            }
          }
          """)

      client1.expectNoMessage()
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
    }
  }
}
