package org.enso.languageserver.websocket.json

import io.circe.literal._

class LibrariesTest extends BaseServerTest {
  "LocalLibraryManager" should {
    "create a library project and include it on the list of local projects" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "library/listLocal",
            "id": 0
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": {
              "localLibraries": []
            }
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "library/create",
            "id": 1,
            "params": {
              "namespace": "User",
              "name": "MyLocalLib",
              "authors": [],
              "maintainers": [],
              "license": ""
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "library/listLocal",
            "id": 2
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": {
              "localLibraries": [
                {
                  "namespace": "User",
                  "name": "MyLocalLib",
                  "version": {
                    "type": "LocalLibraryVersion"
                  }
                }
              ]
            }
          }
          """)
    }
  }

  "mocked library/preinstall" should {
    "send progress notifications" in {}
  }

  "editions/listAvailable" should {
    "list editions on the search path" in {}
  }

  "editions/listDefinedLibraries" should {
    "include Standard.Base in the list" in {}
  }

  "editions/resolve" should {
    "resolve the engine version associated with an edition" in {}
  }

  "ProjectSettingsManager" should {
    "get default settings" in {}

    "allow to set local libraries preference" in {}

    "allow to override parent edition" in {}

    "fail if the provided parent edition is not resolvable" in {}
  }
}
