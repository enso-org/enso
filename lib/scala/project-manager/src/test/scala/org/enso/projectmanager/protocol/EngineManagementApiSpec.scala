package org.enso.projectmanager.protocol

import io.circe.literal._
import org.enso.projectmanager.{BaseServerSpec, ProjectManagementOps}
import org.enso.testkit.FlakySpec

class EngineManagementApiSpec
    extends BaseServerSpec
    with FlakySpec
    with ProjectManagementOps {

  "engine/*" must {
    "report no installed engines by default" in {
      implicit val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "engine/list-installed",
              "id": 0
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "result" : {
              "versions" : []
            }
          }
          """)
    }

    "report available engines" in {
      implicit val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "engine/list-available",
              "id": 0
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "result" : {
              "versions" : [
                {"version": "0.999.0-broken", "markedAsBroken": true},
                {"version": "0.0.1", "markedAsBroken": false},
                {"version": "0.0.1-pre", "markedAsBroken": false},
                {"version": "0.0.0", "markedAsBroken": false}
              ]
            }
          }
          """)
    }

    "install and uninstall the engine and reflect changes in list" in {
      implicit val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "engine/install",
              "id": 0,
              "params": {
                "version": "0.0.0"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "result": null
          }
          """)

      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "engine/install",
              "id": 1,
              "params": {
                "version": "0.0.1-pre"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":1,
            "result": null
          }
          """)

      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "engine/list-installed",
              "id": 2
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":2,
            "result" : {
              "versions" : [
                {"version": "0.0.1-pre", "markedAsBroken": false},
                {"version": "0.0.0", "markedAsBroken": false}
              ]
            }
          }
          """)

      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "engine/uninstall",
              "id": 3,
              "params": {
                "version": "0.0.1-pre"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":3,
            "result": null
          }
          """)

      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "engine/list-installed",
              "id": 4
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":4,
            "result" : {
              "versions" : [
                {"version": "0.0.0", "markedAsBroken": false}
              ]
            }
          }
          """)
    }

    "only install broken releases if specifically asked to" in {
      implicit val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "engine/install",
              "id": 0,
              "params": {
                "version": "0.999.0-broken"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "error": { "code": 4021, "message": "Installation has been cancelled by the user because the requested engine release is marked as broken." }
          }
          """)

      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "engine/install",
              "id": 1,
              "params": {
                "version": "0.999.0-broken",
                "forceInstallBroken": true
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":1,
            "result": null
          }
          """)
    }
  }
}
