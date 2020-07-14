package org.enso.languageserver.websocket.json

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Paths}
import java.util.UUID

import io.circe.literal._
import org.apache.commons.io.FileUtils
import org.enso.testkit.RetrySpec

class FileManagerTest extends BaseServerTest with RetrySpec {
  "File Server" must {

    "write textual content to a file" taggedAs Retry() in {
      val client = getInitialisedWsClient()
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

      val path = Paths.get(testContentRoot.toString, "foo", "bar", "baz.txt")
      Files.readAllLines(path).get(0) shouldBe "123456789"
    }

    "return failure when a content root cannot be found" in {
      val client = getInitialisedWsClient()
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
    }

    "read a file content" in {
      val client = getInitialisedWsClient()
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
      val client = getInitialisedWsClient()
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
      val client = getInitialisedWsClient()
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
      val client = getInitialisedWsClient()
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
      val client = getInitialisedWsClient()
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
      val client = getInitialisedWsClient()
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
      val client = getInitialisedWsClient()
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
      val client = getInitialisedWsClient()
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
      val client = getInitialisedWsClient()
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
      val client = getInitialisedWsClient()
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
      val client = getInitialisedWsClient()
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

    "move a file" in {
      val client = getInitialisedWsClient()
      // create a file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 19,
            "params": {
              "object": {
                "type": "File",
                "name": "test.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "move" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 19,
            "result": null
          }
          """)
      val from = Paths.get(testContentRoot.toString, "move", "test.txt")
      from.toFile.isFile shouldBe true

      // move a file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/move",
            "id": 20,
            "params": {
              "from": {
                "rootId": $testContentRootId,
                "segments": [ "move", "test.txt" ]
              },
              "to": {
                "rootId": $testContentRootId,
                "segments": [ "move", "test1.txt" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 20,
            "result": null
          }
          """)

      val to = Paths.get(testContentRoot.toString, "move", "test1.txt")
      to.toFile.isFile shouldBe true
      from.toFile.exists shouldBe false
    }

    "move a directory" in {
      val client = getInitialisedWsClient()
      // create a file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 21,
            "params": {
              "object": {
                "type": "File",
                "name": "test.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "move" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 21,
            "result": null
          }
          """)

      val path = Paths.get(testContentRoot.toString, "move", "test.txt")
      path.toFile.isFile shouldBe true
      val from = path.getParent()
      from.toFile.isDirectory shouldBe true

      // move a directory
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/move",
            "id": 22,
            "params": {
              "from": {
                "rootId": $testContentRootId,
                "segments": [ "move" ]
              },
              "to": {
                "rootId": $testContentRootId,
                "segments": [ "move_to" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 22,
            "result": null
          }
          """)

      val to = Paths.get(testContentRoot.toString, "move_to", "test1.txt")
      to.toFile.isFile shouldBe true
      from.toFile.exists shouldBe false
    }

    "return failure when moving nonexistent file" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/move",
            "id": 23,
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
            "id": 23,
            "error": {
              "code": 1003,
              "message": "File not found"
            }
          }
          """)

      val to = Paths.get(testContentRoot.toString, "some", "test.txt")
      to.toFile.exists shouldBe false
    }

    "return failure when target file exists" in {
      val client = getInitialisedWsClient()
      // create a source file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 24,
            "params": {
              "object": {
                "type": "File",
                "name": "test.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "move" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 24,
            "result": null
          }
          """)
      val from = Paths.get(testContentRoot.toString, "move", "test.txt")
      from.toFile.isFile shouldBe true

      // create a destination file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 25,
            "params": {
              "object": {
                "type": "File",
                "name": "test1.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "move" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 25,
            "result": null
          }
          """)
      val to = Paths.get(testContentRoot.toString, "move", "test1.txt")
      to.toFile.isFile shouldBe true

      // move to existing file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/move",
            "id": 26,
            "params": {
              "from": {
                "rootId": $testContentRootId,
                "segments": [ "move", "test.txt" ]
              },
              "to": {
                "rootId": $testContentRootId,
                "segments": [ "move", "test1.txt" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 26,
            "error": {
              "code": 1004,
              "message": "File already exists"
            }
          }
          """)

      from.toFile.isFile shouldBe true
      to.toFile.isFile shouldBe true
    }

    "check file existence" in {
      val client = getInitialisedWsClient()
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

    "get a root tree" in withCleanRoot {
      val client = getInitialisedWsClient()
      // create:
      //
      // base
      // ├── a.txt
      // └── subdir
      //     └── b.txt

      // create base/a.txt
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 28,
            "params": {
              "object": {
                "type": "File",
                "name": "a.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "base" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 28,
            "result": null
          }
          """)

      // create base/subdir/b.txt
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 29,
            "params": {
              "object": {
                "type": "File",
                "name": "b.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "base", "subdir" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 29,
            "result": null
          }
          """)

      // get a tree of a root
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/tree",
            "id": 30,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ ]
              }
            }
          }
      """)
      // expect:
      //
      // $testContentRoot
      // └── base
      //     ├── a.txt
      //     └── subdir
      //         └── b.txt
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 30,
            "result": {
              "tree": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [
                    ".."
                  ]
                },
                "name": ${testContentRoot.getFileName.toString},
                "files": [
                ],
                "directories": [
                  {
                    "path": {
                      "rootId": $testContentRootId,
                      "segments": [
                      ]
                    },
                    "name": "base",
                    "files": [
                      {
                        "type": "File",
                        "name": "a.txt",
                        "path": {
                          "rootId": $testContentRootId,
                          "segments": [
                            "base"
                          ]
                        }
                      }
                    ],
                    "directories": [
                      {
                        "path" : {
                          "rootId" : $testContentRootId,
                          "segments": [
                            "base"
                          ]
                        },
                        "name": "subdir",
                        "files": [
                          {
                            "type": "File",
                            "name": "b.txt",
                            "path": {
                              "rootId": $testContentRootId,
                              "segments": [
                                "base",
                                "subdir"
                              ]
                            }
                          }
                        ],
                        "directories": [
                        ]
                      }
                    ]
                  }
                ]
              }
            }
          }
          """)
    }

    "get a directory tree" in {
      val client = getInitialisedWsClient()
      // create:
      //
      // base
      // ├── a.txt
      // └── subdir
      //     └── b.txt

      // create base/a.txt
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 31,
            "params": {
              "object": {
                "type": "File",
                "name": "a.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "base" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 31,
            "result": null
          }
          """)

      // create base/subdir/b.txt
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 32,
            "params": {
              "object": {
                "type": "File",
                "name": "b.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "base", "subdir" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 32,
            "result": null
          }
          """)

      // get a tree of 'base'
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/tree",
            "id": 33,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "base" ]
              }
            }
          }
      """)
      // expect:
      //
      // base
      // ├── a.txt
      // └── subdir
      //     └── b.txt
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 33,
            "result": {
              "tree": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [
                  ]
                },
                "name": "base",
                "files": [
                  {
                    "type": "File",
                    "name": "a.txt",
                    "path": {
                      "rootId": $testContentRootId,
                      "segments": [
                        "base"
                      ]
                    }
                  }
                ],
                "directories": [
                  {
                    "path": {
                      "rootId": $testContentRootId,
                      "segments": [
                        "base"
                      ]
                    },
                    "name": "subdir",
                    "files": [
                      {
                        "type": "File",
                        "name": "b.txt",
                        "path": {
                          "rootId": $testContentRootId,
                          "segments": [
                            "base",
                            "subdir"
                          ]
                        }
                      }
                    ],
                    "directories": [
                    ]
                  }
                ]
              }
            }
          }
          """)
    }

    "get a truncated directory tree" in {
      val client = getInitialisedWsClient()
      // create:
      //
      // base
      // ├── a.txt
      // └── subdir
      //     └── b.txt

      // create base/a.txt
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 34,
            "params": {
              "object": {
                "type": "File",
                "name": "a.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "base" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 34,
            "result": null
          }
          """)

      // create base/subdir/b.txt
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 35,
            "params": {
              "object": {
                "type": "File",
                "name": "b.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "base", "subdir" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 35,
            "result": null
          }
          """)

      // get a tree of 'base'
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/tree",
            "id": 36,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "base" ]
              },
              "depth": 1
            }
          }
      """)
      // expect:
      //
      // base
      // ├── a.txt
      // └── subdir
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 36,
            "result": {
              "tree": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [
                  ]
                },
                "name": "base",
                "files": [
                  {
                    "type": "File",
                    "name": "a.txt",
                    "path": {
                      "rootId": $testContentRootId,
                      "segments": [
                        "base"
                      ]
                    }
                  },
                  {
                    "type": "Directory",
                    "name": "subdir",
                    "path": {
                      "rootId": $testContentRootId,
                      "segments": [
                        "base"
                      ]
                    }
                  }
                ],
                "directories": [
                ]
              }
            }
          }
          """)
    }

    "get a subdirectory tree" in {
      val client = getInitialisedWsClient()
      // create base/a.txt
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 37,
            "params": {
              "object": {
                "type": "File",
                "name": "a.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "base" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 37,
            "result": null
          }
          """)

      // create base/subdir/b.txt
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 38,
            "params": {
              "object": {
                "type": "File",
                "name": "b.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "base", "subdir" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 38,
            "result": null
          }
          """)

      // get a tree of 'base/subdir'
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/tree",
            "id": 39,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "base", "subdir" ]
              }
            }
          }
      """)
      // expect:
      //
      // subdir
      // └── b.txt
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 39,
            "result": {
              "tree": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [
                    "base"
                  ]
                },
                "name": "subdir",
                "files": [
                  {
                    "type": "File",
                    "name": "b.txt",
                    "path": {
                      "rootId": $testContentRootId,
                      "segments": [
                        "base",
                        "subdir"
                      ]
                    }
                  }
                ],
                "directories": [
                ]
              }
            }
          }
          """)
    }

    "get a directory tree with symlink" in {
      val client = getInitialisedWsClient()
      // create:
      //
      // base2
      // ├── link -> subdir
      // └── subdir
      //     └── b.txt

      // create base2/subdir/b.txt
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 40,
            "params": {
              "object": {
                "type": "File",
                "name": "b.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "base2", "subdir" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 40,
            "result": null
          }
          """)

      // create symlink base/link -> base/subdir
      val symlink = Paths.get(testContentRoot.toString, "base2", "link")
      val subdir  = Paths.get(testContentRoot.toString, "base2", "subdir")
      Files.createSymbolicLink(symlink, subdir)
      Files.isSymbolicLink(symlink) shouldBe true

      // get a tree of 'base'
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/tree",
            "id": 41,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "base2" ]
              }
            }
          }
      """)
      // expect:
      //
      // base2
      // └── link
      //     └── b.txt
      // └── subdir
      //     └── b.txt
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 41,
            "result": {
              "tree": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [
                  ]
                },
                "name": "base2",
                "files": [
                ],
                "directories": [
                  {
                    "path" : {
                      "rootId" : $testContentRootId,
                      "segments" : [
                        "base2"
                      ]
                    },
                    "name" : "link",
                    "files" : [
                      {
                        "type" : "File",
                        "name" : "b.txt",
                        "path" : {
                          "rootId" : $testContentRootId,
                          "segments" : [
                            "base2",
                            "link"
                          ]
                        }
                      }
                    ],
                    "directories" : [
                    ]
                  },
                  {
                    "path": {
                      "rootId": $testContentRootId,
                      "segments": [
                        "base2"
                      ]
                    },
                    "name": "subdir",
                    "files": [
                      {
                        "type": "File",
                        "name": "b.txt",
                        "path": {
                          "rootId": $testContentRootId,
                          "segments": [
                            "base2",
                            "subdir"
                          ]
                        }
                      }
                    ],
                    "directories": [
                    ]
                  }
                ]
              }
            }
          }
          """)
    }

    "get a directory tree with symlink outside of root" in {
      val client = getInitialisedWsClient()
      // create base3
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 42,
            "params": {
              "object": {
                "type": "Directory",
                "name": "base3",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 42,
            "result": null
          }
          """)

      // create symlink base3/link -> $testOtherRoot
      val testOtherRoot = Files.createTempDirectory(null)
      val symlink       = Paths.get(testContentRoot.toString, "base3", "link")
      Files.createSymbolicLink(symlink, testOtherRoot)
      Files.isSymbolicLink(symlink) shouldBe true

      // get a tree of 'base3'
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/tree",
            "id": 43,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "base3" ]
              }
            }
          }
      """)
      // expect:
      //
      // base3
      // └── link
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 43,
            "result": {
              "tree": {
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [
                  ]
                },
                "name": "base3",
                "files": [
                ],
                "directories": [
                  {
                    "path" : {
                      "rootId" : $testContentRootId,
                      "segments" : [
                        "base3"
                      ]
                    },
                    "name" : "link",
                    "files" : [
                    ],
                    "directories" : [
                    ]
                  }
                ]
              }
            }
          }
          """)
    }

    "list a subdirectory" in {
      val client = getInitialisedWsClient()
      // create:
      //
      //  subdir
      //  └── b.txt

      // create subdir/b.txt
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 44,
            "params": {
              "object": {
                "type": "File",
                "name": "b.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "subdir" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 44,
            "result": null
          }
          """)

      // get a tree of subdir
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/list",
            "id": 45,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "subdir" ]
              }
            }
          }
      """)
      // expect: b.txt
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 45,
            "result" : {
              "paths" : [
                {
                  "type" : "File",
                  "name" : "b.txt",
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "subdir"
                    ]
                  }
                }
              ]
            }
          }
          """)
    }

    "get file info" in {
      val client = getInitialisedWsClient()
      // create a file
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/create",
            "id": 46,
            "params": {
              "object": {
                "type": "File",
                "name": "test.txt",
                "path": {
                  "rootId": $testContentRootId,
                  "segments": [ "info" ]
                }
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 46,
            "result": null
          }
          """)
      val path = Paths.get(testContentRoot.toString, "info", "test.txt")
      path.toFile.isFile shouldBe true
      val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])

      // get file info
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/info",
            "id": 47,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "info", "test.txt" ]
              }
            }
          }
      """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 47,
            "result" : {
              "attributes" : {
                "creationTime" : ${attrs.creationTime.toString()},
                "lastAccessTime" : ${attrs.lastAccessTime.toString()},
                "lastModifiedTime" : ${attrs.lastModifiedTime.toString()},
                "kind" : {
                  "type" : "File",
                  "name" : "test.txt",
                  "path" : {
                    "rootId" : $testContentRootId,
                    "segments" : [
                      "info"
                    ]
                  }
                },
                "byteSize" : 0
              }
            }
          }
          """)
    }

    "return FileNotFound when getting info of nonexistent file" in {
      val client = getInitialisedWsClient()
      val file   = Paths.get(testContentRoot.toString, "nonexistent.txt").toFile
      file.exists shouldBe false
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/info",
            "id": 48,
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
            "id": 48,
            "error": {
              "code": 1003,
              "message": "File not found"
            }
          }
          """)
    }

  }

  def withCleanRoot[T](test: => T): T = {
    FileUtils.cleanDirectory(testContentRoot.toFile)
    test
  }

}
