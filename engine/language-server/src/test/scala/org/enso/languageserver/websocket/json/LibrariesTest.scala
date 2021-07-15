package org.enso.languageserver.websocket.json

import io.circe.Json
import io.circe.literal._
import org.enso.languageserver.libraries.LibraryEntry
import org.enso.languageserver.libraries.LibraryEntry.PublishedLibraryVersion

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
    "send progress notifications" ignore {}
  }

  "editions/listAvailable" should {
    "list editions on the search path" in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "editions/listAvailable",
            "id": 0,
            "params": {
              "update": false
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": {
              "editionNames": [
                ${buildinfo.Info.currentEdition}
              ]
            }
          }
          """)
    }

    "update the list of editions if requested" ignore {}

    // TODO [RW] do we need to support that?
    "work if the update field is missing" ignore {}
  }

  "editions/listDefinedLibraries" should {
    "include Standard.Base in the list" in {
      def containsBase(response: Json): Unit = {
        val result = response.asObject.value("result").value
        val libs   = result.asObject.value("availableLibraries").value
        val parsed = libs.asArray.value.map(_.as[LibraryEntry])
        val bases = parsed.collect {
          case Right(
                LibraryEntry("Standard", "Base", PublishedLibraryVersion(_, _))
              ) =>
            ()
        }
        bases should have size 1
      }

      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "editions/listDefinedLibraries",
            "id": 0,
            "params": {
              "edition": {
                "type": "CurrentProjectEdition"
              }
            }
          }
          """)
      containsBase(client.expectSomeJson())

      val currentEditionName = buildinfo.Info.currentEdition
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "editions/listDefinedLibraries",
            "id": 0,
            "params": {
              "edition": {
                "type": "NamedEdition",
                "editionName": $currentEditionName
              }
            }
          }
          """)
      containsBase(client.expectSomeJson())
    }
  }

  "editions/resolve" should {
    "resolve the engine version associated with an edition" in {
      val currentVersion = buildinfo.Info.ensoVersion

      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "editions/resolve",
            "id": 0,
            "params": {
              "edition": {
                "type": "CurrentProjectEdition"
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": {
              "engineVersion": $currentVersion
            }
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "editions/resolve",
            "id": 0,
            "params": {
              "edition": {
                "type": "NamedEdition",
                "editionName": ${buildinfo.Info.currentEdition}
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": {
              "engineVersion": $currentVersion
            }
          }
          """)
    }
  }

  "ProjectSettingsManager" should {
    "get default settings" ignore {}

    "allow to set local libraries preference" ignore {}

    "allow to override parent edition" ignore {}

    "fail if the provided parent edition is not resolvable" ignore {}
  }
}
