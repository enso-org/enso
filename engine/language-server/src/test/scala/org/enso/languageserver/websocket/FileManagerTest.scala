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

    "delete a file" in {
      val client = new WsTestClient(address)

      // create a file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 8,
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
            "id": 8,
            "result": null
          }
          """)

      val file = Paths.get(testContentRoot.toString, "foo1", "bar.txt").toFile
      file.isFile shouldBe true

      // delete a file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/delete",
            "id": 9,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo1", "bar.txt" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 9,
            "result": null
          }
          """)

      file.exists shouldBe false
      file.getParentFile.isDirectory shouldBe true
    }

    "delete a directory" in {
      val client = new WsTestClient(address)

      // create a directory
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 10,
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
            "id": 10,
            "result": null
          }
          """)

      val file = Paths.get(testContentRoot.toString, "foo1", "baz").toFile
      file.isDirectory shouldBe true

      // delete a directory
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/delete",
            "id": 11,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo1", "baz" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 11,
            "result": null
          }
          """)

      file.exists shouldBe false
      file.getParentFile.isDirectory shouldBe true
    }

    "return FileNotFound when deleting nonexistent file" in {
      val client = new WsTestClient(address)
      val file   = Paths.get(testContentRoot.toString, "foo1", "bar.txt").toFile
      file.isFile shouldBe false

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/delete",
            "id": 12,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo1", "bar.txt" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 12,
            "error": {
              "code": 1003,
              "message": "File not found"
            }
          }
          """)

      file.exists shouldBe false
      file.getParentFile.isDirectory shouldBe true
    }

    "return FileNotFound when deleting nonexistent directory" in {
      val client = new WsTestClient(address)
      val file   = Paths.get(testContentRoot.toString, "foo1", "baz").toFile
      file.isDirectory shouldBe false

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/delete",
            "id": 13,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo1", "baz" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 13,
            "error": {
              "code": 1003,
              "message": "File not found"
            }
          }
          """)

      file.exists shouldBe false
      file.getParentFile.isDirectory shouldBe true
    }

    "copy a file" in {
      val client = new WsTestClient(address)

      // create a file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 14,
            "params": {
              "object": {
                "type": "File",
                "name": "test.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "a" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 14,
            "result": null
          }
          """)
      val from = Paths.get(testContentRoot.toString, "a", "test.txt")
      from.toFile.isFile shouldBe true

      // copy a file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/copy",
            "id": 15,
            "params": {
              "from": {
                "rootId": $testContentRootId,
                "segments": [ "a", "test.txt" ]
              },
              "to": {
                "rootId": $testContentRootId,
                "segments": [ "a", "test1.txt" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 15,
            "result": null
          }
          """)

      val to = Paths.get(testContentRoot.toString, "a", "test1.txt")
      to.toFile.isFile shouldBe true
    }

    "copy a directory" in {
      val client = new WsTestClient(address)

      // create a file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 16,
            "params": {
              "object": {
                "type": "File",
                "name": "test.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "b" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 16,
            "result": null
          }
          """)
      val from = Paths.get(testContentRoot.toString, "b", "test.txt")
      from.toFile.isFile shouldBe true

      // copy a directory
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/copy",
            "id": 17,
            "params": {
              "from": {
                "rootId": $testContentRootId,
                "segments": [ "b" ]
              },
              "to": {
                "rootId": $testContentRootId,
                "segments": [ "c" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 17,
            "result": null
          }
          """)

      val to = Paths.get(testContentRoot.toString, "c", "test.txt")
      to.toFile.isFile shouldBe true
    }

    "return failure when copying nonexistent file" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/copy",
            "id": 18,
            "params": {
              "from": {
                "rootId": $testContentRootId,
                "segments": [ "nonexistent", "test.txt" ]
              },
              "to": {
                "rootId": $testContentRootId,
                "segments": [ "some", "test.txt" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 18,
            "error": {
              "code": 1003,
              "message": "File not found"
            }
          }
          """)

      val to = Paths.get(testContentRoot.toString, "some", "test.txt")
      to.toFile.isFile shouldBe false
    }

    "check file existence" in {
      val client = new WsTestClient(address)
      val path   = Paths.get(testContentRoot.toString, "nonexistent.txt")
      path.toFile.exists shouldBe false

      // check file exists
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/exists",
            "id": 27,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "nonexistent.txt" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 27,
            "result": {
              "exists": false
            }
          }
          """)
    }

  }

}
