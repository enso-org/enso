package org.enso.languageserver.websocket.json

import java.nio.file.{Files, Paths}
import io.circe.literal._
import org.enso.logger.ReportLogsOnFailure
import org.enso.testkit.FlakySpec

class ReceivesTreeUpdatesHandlerTest
    extends BaseServerTest
    with FlakySpec
    with ReportLogsOnFailure {

  override val isFileWatcherEnabled = true

  "ReceivesTreeUpdatesHandler" must {

    "acquire capability receivesTreeUpdates" in {
      val client = getInitialisedWsClient()
      client.send(jsonrpc.acquireReceivesTreeUpdates(1))
      client.expectJson(jsonrpc.ok(1))
    }

    "fail to acquire capability if directory doesn't exist" in {
      val client = getInitialisedWsClient()
      client.send(jsonrpc.acquireReceivesTreeUpdates(1, "nonexistent"))
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": {
              "code": 1003,
              "message": "File not found"
            }
          }
          """)
    }

    "reacquire capability receivesTreeUpdates" in {
      val client = getInitialisedWsClient()
      // acquire
      client.send(jsonrpc.acquireReceivesTreeUpdates(1))
      client.expectJson(jsonrpc.ok(1))

      // reacquire
      client.send(jsonrpc.acquireReceivesTreeUpdates(2))
      client.expectJson(jsonrpc.ok(2))
    }

    "fail to release capability it does not hold" in {
      val client = getInitialisedWsClient()
      client.send(jsonrpc.releaseReceivesTreeUpdates(1))
      client.expectJson(jsonrpc.ok(1))
      client.send(jsonrpc.releaseReceivesTreeUpdates(2))
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "error": {
              "code" : 5001,
              "message" : "Capability not acquired"
            }
          }
          """)
    }

    "release capability receivesTreeUpdates" in {
      val client = getInitialisedWsClient()
      // acquire capability
      client.send(jsonrpc.acquireReceivesTreeUpdates(1))
      client.expectJson(jsonrpc.ok(1))

      // release capability
      client.send(jsonrpc.releaseReceivesTreeUpdates(2))
      client.expectJson(jsonrpc.ok(2))
    }

    "receive file system updates" taggedAs Flaky in {
      val client1 = getInitialisedWsClient()
      val client2 = getInitialisedWsClient()
      // acquire capability
      client1.send(jsonrpc.acquireReceivesTreeUpdates(1))
      client1.expectJson(jsonrpc.ok(1))
      client2.send(jsonrpc.acquireReceivesTreeUpdates(1))
      client2.expectJson(jsonrpc.ok(1))

      // create file
      val path = Paths.get(testContentRoot.file.toString, "oneone.txt")
      Files.createFile(path)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "file/event",
            "params": {
               "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "oneone.txt" ]
               },
               "kind": "Added"
             }
          }
          """)
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "file/event",
            "params": {
               "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "oneone.txt" ]
               },
               "kind": "Added"
             }
          }
          """)

      // update file
      Files.write(path, "Hello".getBytes())
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "file/event",
            "params": {
               "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "oneone.txt" ]
               },
               "kind": "Modified"
             }
          }
          """)
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "file/event",
            "params": {
               "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "oneone.txt" ]
               },
               "kind": "Modified"
             }
          }
          """)

      // release capability
      client2.send(jsonrpc.releaseReceivesTreeUpdates(2))
      client2.expectJson(jsonrpc.ok(2))

      // remove file
      Files.delete(path)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "file/event",
            "params": {
               "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "oneone.txt" ]
               },
               "kind": "Removed"
             }
          }
          """)
      client2.expectNoMessage()
    }

  }

  object jsonrpc {

    def ok(reqId: Int) =
      json"""
          { "jsonrpc": "2.0",
            "id": $reqId,
            "result": null
          }
          """

    def acquireReceivesTreeUpdates(reqId: Int) =
      json"""
            { "jsonrpc": "2.0",
              "method": "capability/acquire",
              "id": $reqId,
              "params": {
                "method": "file/receivesTreeUpdates",
                "registerOptions": {
                  "path": {
                    "rootId": $testContentRootId,
                    "segments": [ ]
                  }
                }
              }
            }
            """

    def acquireReceivesTreeUpdates(reqId: Int, segment: String) =
      json"""
            { "jsonrpc": "2.0",
              "method": "capability/acquire",
              "id": $reqId,
              "params": {
                "method": "file/receivesTreeUpdates",
                "registerOptions": {
                  "path": {
                    "rootId": $testContentRootId,
                    "segments": [ $segment ]
                  }
                }
              }
            }
            """

    def releaseReceivesTreeUpdates(reqId: Int) =
      json"""
          { "jsonrpc": "2.0",
            "method": "capability/release",
            "id": $reqId,
            "params": {
              "method": "file/receivesTreeUpdates",
              "registerOptions": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ ]
                }
              }
            }
          }
          """

    def releaseReceivesTreeUpdates(reqId: Int, segment: String) =
      json"""
          { "jsonrpc": "2.0",
            "method": "capability/release",
            "id": $reqId,
            "params": {
              "method": "file/receivesTreeUpdates",
              "registerOptions": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ $segment ]
                }
              }
            }
          }
          """
  }

}
