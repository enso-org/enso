package org.enso.languageserver.websocket
import io.circe.literal._

class SessionManagementTest extends BaseServerTest {

  "A server" when {

    "connection is not initialised" must {

      "reply with content roots if client initialise a session" in {
        val client = new WsTestClient(address)

        client.send(json"""
          { "jsonrpc": "2.0",
            "method": "session/initProtocolConnection",
            "id": 1,
            "params": {
              "clientId": "e3d99192-2edc-4613-bdf4-db35e4b9b956"
            }
          }
          """)
        client.expectJson(json"""
            { "jsonrpc":"2.0",
              "id":1,
              "result":{
                "contentRoots":[ $testContentRootId ]
              }
            }
              """)
      }

      "reply with SessionNotInitialisedError if client sends a request" in {
        val client = new WsTestClient(address)

        client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo", "bar", "baz.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
        client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": { "code": 6001, "message": "Session not initialised" }
          }
          """)
      }

    }

    "connection is initialised" must {

      "reply with an error if client tries initialise connection second time" in {
        val client = getInitialisedWsClient()
        client.send(json"""
          { "jsonrpc": "2.0",
            "method": "session/initProtocolConnection",
            "id": 1,
            "params": {
              "clientId": "e3d99192-2edc-4613-bdf4-db35e4b9b956"
            }
          }
          """)
        client.expectJson(json"""
            { "jsonrpc":"2.0",
              "id":1,
              "error": { "code": 6002, "message": "Session already initialised" }
            }
              """)
      }

    }

  }

}
