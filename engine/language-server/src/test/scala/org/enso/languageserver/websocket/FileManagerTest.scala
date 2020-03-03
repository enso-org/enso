package org.enso.languageserver.websocket

import java.nio.file.Paths
import java.util.UUID
import scala.io.{Source => IoSource}
import io.circe.literal._

class FileManagerTest extends WebSocketServerTest {
  "File Server" must {

    "write textual content to a file" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 3,
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
            "id": 3,
            "result": null
          }
          """)
      client.expectNoMessage()
      val path = Paths.get(testContentRoot.toString, "foo", "bar", "baz.txt")
      IoSource.fromFile(path.toFile).getLines().mkString shouldBe "123456789"
    }

    "return failure when a content root cannot be found" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 3,
            "params": {
              "path": {
                "rootId": ${UUID.randomUUID()},
                "segments": [ "foo", "bar", "baz.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "error" : {
              "code" : 1001,
              "message" : "Content root not found"
            }
          }
          """)
      client.expectNoMessage()
    }

    "read a file content" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 4,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 4,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/read",
            "id": 5,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 5,
            "result": { "contents": "123456789" }
          }
          """)
    }

    "return FileNotFoundError if a file doesn't exist" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/read",
            "id": 6,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "bar.txt" ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 6,
            "error" : {
              "code" : 1003,
              "message" : "File not found"
            }
          }
          """)
    }

    "create a file" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 7,
            "params": {
              "object": {
                "type": "File",
                "name": "bar.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo1" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 7,
            "result": null
          }
          """)

      val file = Paths.get(testContentRoot.toString, "foo1", "bar.txt").toFile
      file.isFile shouldBe true
    }

    "create a directory" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 7,
            "params": {
              "object": {
                "type": "Directory",
                "name": "baz",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo1" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 7,
            "result": null
          }
          """)

      val file = Paths.get(testContentRoot.toString, "foo1", "baz").toFile
      file.isDirectory shouldBe true
    }
  }
}
