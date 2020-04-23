package org.enso.languageserver.websocket.rpc

import akka.testkit.TestProbe
import io.circe.literal._
import org.enso.languageserver.event.BufferClosed
import org.enso.languageserver.filemanager.Path

class TextOperationsTest extends BaseServerTest {

  "text/openFile" must {
    "fail opening a file if it does not exist" in {
      // Interaction:
      // 1. Client tries to open a non-existent file.
      // 2. Client receives an error message.
      val client = getInitialisedWsClient()
      // 1
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

      // 2
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": { "code": 1003, "message": "File not found" }
          }
          """)
    }

    "allow opening files for editing" in {
      // Interaction:
      // 1. Client creates a file.
      // 2. Client receives confirmation.
      // 3. Client opens the created file.
      // 4. Client receives the file contents and a canEdit capability.
      val client = getInitialisedWsClient()
      // 1
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)

      // 2
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)

      // 3
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

      // 4
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "writeCapability": {
                "method": "canEdit",
                "registerOptions": { "path": {
                  "rootId": $testContentRootId,
                  "segments": ["foo.txt"]
                } }
              },
              "content": "123456789",
              "currentVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)
    }

    "allow opening files for reading for another client" in {
      // Interaction:
      // 1. Client 1 creates a file.
      // 2. Client 1 receives confirmation.
      // 3. Client 1 opens the created file.
      // 4. Client 1 receives the file contents and a canEdit capability.
      // 5. Client 2 opens the file.
      // 6. Client 2 receives the file contents without a canEdit capability.
      val client1 = getInitialisedWsClient()
      val client2 = getInitialisedWsClient()
      // 1
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)

      // 2
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)

      // 3
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

      // 4
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "writeCapability": {
                "method": "canEdit",
                "registerOptions": { "path": {
                  "rootId": $testContentRootId,
                  "segments": ["foo.txt"]
                } }
              },
              "content": "123456789",
              "currentVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      // 5
      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 2,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

      // 6
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": {
              "writeCapability": null,
              "content": "123456789",
              "currentVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)
    }
  }

  "grant the canEdit capability if no one else holds it" in {
    // Interaction:
    // 1. Client 1 creates a file.
    // 2. Client 1 receives confirmation.
    // 3. Client 1 opens the created file.
    // 4. Client 1 receives the file contents and a canEdit capability.
    // 5. Client 1 releases the canEdit capability.
    // 6. Client 1 receives a confirmation.
    // 7. Client 2 opens the file.
    // 8. Client 2 receives the file contents and a canEdit capability.
    val client1 = getInitialisedWsClient()
    val client2 = getInitialisedWsClient()
    // 1
    client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)

    // 2
    client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)

    // 3
    client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

    // 4
    client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "writeCapability": {
                "method": "canEdit",
                "registerOptions": { "path": {
                  "rootId": $testContentRootId,
                  "segments": ["foo.txt"]
                } }
              },
              "content": "123456789",
              "currentVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

    // 5
    client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/release",
            "id": 2,
            "params": {
              "method": "canEdit",
                "registerOptions": { "path": {
                  "rootId": $testContentRootId,
                  "segments": ["foo.txt"]
                } }
            }
          }
          """)

    // 6
    client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)

    // 7
    client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 3,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

    // 8
    client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "result": {
              "writeCapability": {
                "method": "canEdit",
                "registerOptions": { "path": {
                  "rootId": $testContentRootId,
                  "segments": ["foo.txt"]
                } }
              },
              "content": "123456789",
              "currentVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)
  }

  "take canEdit capability away from clients when another client registers for it" in {
    val client1 = getInitialisedWsClient()
    val client2 = getInitialisedWsClient()
    val client3 = getInitialisedWsClient()
    client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)

    client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)

    client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

    client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "writeCapability": {
                "method": "canEdit",
                "registerOptions": { "path": {
                  "rootId": $testContentRootId,
                  "segments": ["foo.txt"]
                } }
              },
              "content": "123456789",
              "currentVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)
    client2.expectNoMessage()
    client3.expectNoMessage()

    client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 2,
            "params": {
              "method": "canEdit",
              "registerOptions": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                }
              }
            }
          }
          """)

    client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "capability/forceReleased",
            "params": {
              "method": "canEdit",
              "registerOptions": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                }
              }
            }
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
              "method": "canEdit",
              "registerOptions": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                }
              }
            }
          }
          """)

    client1.expectNoMessage()
    client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "capability/forceReleased",
            "params": {
              "method": "canEdit",
              "registerOptions": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                }
              }
            }
          }
          """)
    client3.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "result": null
          }
          """)
  }

  "text/closeFile" must {

    "fail when a client didn't open it before" in {
      val client1 = getInitialisedWsClient()
      val client2 = getInitialisedWsClient()
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)

      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client1.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/closeFile",
            "id": 2,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "error": { "code": 3001, "message": "File not opened" }
          }
          """)
    }

    "fail when a file wasn't opened first" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
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
            "id": 0,
            "result": null
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/closeFile",
            "id": 1,
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
            "id": 1,
            "error": { "code": 3001, "message": "File not opened" }
          }
          """)
    }

    "close file if it was open before" in {
      val client1 = getInitialisedWsClient()
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)

      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client1.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/closeFile",
            "id": 2,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
    }

    "close buffer when all participants close it" in {
      val eventProbe = TestProbe()
      system.eventStream.subscribe(eventProbe.ref, classOf[BufferClosed])
      val client1 = getInitialisedWsClient()
      val client2 = getInitialisedWsClient()
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)

      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client1.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client2.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : null,
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/closeFile",
            "id": 2,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
      eventProbe.expectNoMessage()
      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/closeFile",
            "id": 2,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
      eventProbe.expectMsg(
        BufferClosed(Path(testContentRootId, Vector("foo.txt")))
      )
    }

  }

  "text/applyEdit" must {
    "fail when a file wasn't opened first" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
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
            "id": 0,
            "result": null
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/applyEdit",
            "id": 1,
            "params": {
              "edit": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                },
                "oldVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522",
                "newVersion": "7602967cab172183d1a67ea40cb8e92e23218764bc9934c3795fcea5",
                "edits": [
                  {
                    "range": {
                      "start": { "line": 0, "character": 0 },
                      "end": { "line": 0, "character": 0 }
                    },
                    "text": "bar"
                  }
                ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": { "code": 3001, "message": "File not opened" }
          }
          """)
    }

    "fail when old version is incorrect" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
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
            "id": 0,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/applyEdit",
            "id": 2,
            "params": {
              "edit": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                },
                "oldVersion": "wrong_version",
                "newVersion": "7602967cab172183d1a67ea40cb8e92e23218764bc9934c3795fcea5",
                "edits": [
                  {
                    "range": {
                      "start": { "line": 0, "character": 0 },
                      "end": { "line": 0, "character": 0 }
                    },
                    "text": "bar"
                  }
                ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "error": { 
              "code": 3003, 
              "message": "Invalid version [client version: wrong_version, server version: 5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522]" 
            }
          }
          """)
    }

    "fail when new version is incorrect" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
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
            "id": 0,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/applyEdit",
            "id": 2,
            "params": {
              "edit": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                },
                "oldVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522",
                "newVersion": "wrong_version",
                "edits": [
                  {
                    "range": {
                      "start": { "line": 0, "character": 0 },
                      "end": { "line": 0, "character": 0 }
                    },
                    "text": "bar"
                  }
                ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "error": { 
              "code": 3003, 
              "message": "Invalid version [client version: wrong_version, server version: 7602967cab172183d1a67ea40cb8e92e23218764bc9934c3795fcea5]" 
            }
          }
          """)
    }

    "fail when changes are incorrect" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
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
            "id": 0,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/applyEdit",
            "id": 2,
            "params": {
              "edit": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                },
                "oldVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522",
                "newVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522",
                "edits": [
                  {
                    "range": {
                      "start": { "line": -1, "character": 0 },
                      "end": { "line": 0, "character": -1 }
                    },
                    "text": "bar"
                  }
                ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "error": { 
              "code": 3002, 
              "message": "Negative coordinate in a position object" 
            }
          }
          """)
    }

    "fail when a client doesn't have write lock" in {
      val client1 = getInitialisedWsClient()
      val client2 = getInitialisedWsClient()
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client1.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)
      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client2.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : null,
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/applyEdit",
            "id": 2,
            "params": {
              "edit": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                },
                "oldVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522",
                "newVersion": "7602967cab172183d1a67ea40cb8e92e23218764bc9934c3795fcea5",
                "edits": [
                  {
                    "range": {
                      "start": { "line": 0, "character": 0 },
                      "end": { "line": 0, "character": 0 }
                    },
                    "text": "bar"
                  }
                ]
              }
            }
          }
          """)
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "error": { 
              "code": 3004, 
              "message": "Write denied" 
            }
          }
          """)
    }

    "apply multiple changes at a clip" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
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
            "id": 0,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/applyEdit",
            "id": 2,
            "params": {
              "edit": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                },
                "oldVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522",
                "newVersion": "ebe55342f9c8b86857402797dd723fb4a2174e0b56d6ace0a6929ec3",
                "edits": [
                  {
                    "range": {
                      "start": { "line": 0, "character": 0 },
                      "end": { "line": 0, "character": 0 }
                    },
                    "text": "bar"
                  },
                  {
                    "range": {
                      "start": { "line": 0, "character": 12 },
                      "end": { "line": 0, "character": 12 }
                    },
                    "text": "foo"
                  }
                ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
    }

    "should notify subscribers about a change" in {
      val client1 = getInitialisedWsClient()
      val client2 = getInitialisedWsClient()
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client1.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client2.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : null,
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/applyEdit",
            "id": 2,
            "params": {
              "edit": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                },
                "oldVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522",
                "newVersion": "ebe55342f9c8b86857402797dd723fb4a2174e0b56d6ace0a6929ec3",
                "edits": [
                  {
                    "range": {
                      "start": { "line": 0, "character": 0 },
                      "end": { "line": 0, "character": 0 }
                    },
                    "text": "bar"
                  },
                  {
                    "range": {
                      "start": { "line": 0, "character": 12 },
                      "end": { "line": 0, "character": 12 }
                    },
                    "text": "foo"
                  }
                ]
              }
            }
          }
          """)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
      client2.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "method" : "text/didChange",
            "params" : {
              "edits" : [
                {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  },
                  "edits" : [
                    {
                      "range" : {
                        "start" : { "line" : 0, "character" : 0 },
                        "end" : { "line" : 0, "character" : 0 }
                      },
                      "text" : "bar"
                    },
                    {
                      "range" : {
                        "start" : { "line" : 0, "character" : 12 },
                        "end" : { "line" : 0, "character" : 12 }
                      },
                      "text" : "foo"
                    }
                  ],
                  "oldVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522",
                  "newVersion" : "ebe55342f9c8b86857402797dd723fb4a2174e0b56d6ace0a6929ec3"
                }
              ]
            }
          }
          """)
    }

  }

  "text/save" must {

    "fail when a client didn't open it" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
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
            "id": 0,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/applyEdit",
            "id": 2,
            "params": {
              "edit": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                },
                "oldVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522",
                "newVersion": "ebe55342f9c8b86857402797dd723fb4a2174e0b56d6ace0a6929ec3",
                "edits": [
                  {
                    "range": {
                      "start": { "line": 0, "character": 0 },
                      "end": { "line": 0, "character": 0 }
                    },
                    "text": "bar"
                  },
                  {
                    "range": {
                      "start": { "line": 0, "character": 12 },
                      "end": { "line": 0, "character": 12 }
                    },
                    "text": "foo"
                  }
                ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "error": { "code": 3001, "message": "File not opened" }
          }
          """)
    }

    "fail when a client's version doesn't match a server version" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
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
            "id": 0,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/save",
            "id": 3,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "currentVersion": "ebe55342f9c8b86857402797dd723fb4a2174e0b56d6ace0a6929ec3"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "error": { 
              "code": 3003, 
              "message": "Invalid version [client version: ebe55342f9c8b86857402797dd723fb4a2174e0b56d6ace0a6929ec3, server version: 5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522]" 
            }
          }
          """)
    }

    "fail when a client doesn't hold a write lock" in {
      val client1 = getInitialisedWsClient()
      val client2 = getInitialisedWsClient()
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client1.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)
      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client2.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : null,
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)
      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/save",
            "id": 3,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "currentVersion": "ebe55342f9c8b86857402797dd723fb4a2174e0b56d6ace0a6929ec3"
            }
          }
          """)
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "error": { 
              "code": 3004, 
              "message": "Write denied" 
            }
          }
          """)
    }

    "persist changes from a buffer to durable storage" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
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
            "id": 0,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "writeCapability" : {
                "method" : "canEdit",
                "registerOptions" : {
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "foo.txt"
                    ]
                  }
                }
              },
              "content" : "123456789",
              "currentVersion" : "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522"
            }
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/applyEdit",
            "id": 2,
            "params": {
              "edit": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "foo.txt" ]
                },
                "oldVersion": "5795c3d628fd638c9835a4c79a55809f265068c88729a1a3fcdf8522",
                "newVersion": "ebe55342f9c8b86857402797dd723fb4a2174e0b56d6ace0a6929ec3",
                "edits": [
                  {
                    "range": {
                      "start": { "line": 0, "character": 0 },
                      "end": { "line": 0, "character": 0 }
                    },
                    "text": "bar"
                  },
                  {
                    "range": {
                      "start": { "line": 0, "character": 12 },
                      "end": { "line": 0, "character": 12 }
                    },
                    "text": "foo"
                  }
                ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/save",
            "id": 3,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "currentVersion": "ebe55342f9c8b86857402797dd723fb4a2174e0b56d6ace0a6929ec3"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/read",
            "id": 4,
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
            "id": 4,
            "result": { "contents": "bar123456789foo" }
          }
          """)
    }

  }

}
