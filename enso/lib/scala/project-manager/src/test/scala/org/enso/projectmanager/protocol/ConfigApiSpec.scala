package org.enso.projectmanager.protocol

import io.circe.literal._
import org.enso.projectmanager.{BaseServerSpec, ProjectManagementOps}
import org.enso.testkit.FlakySpec

class ConfigApiSpec
    extends BaseServerSpec
    with FlakySpec
    with ProjectManagementOps {

  "global-config/get" must {
    "return none for missing keys" in {
      implicit val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "global-config/get",
              "id": 0,
              "params": {
                "key": "nonexistent"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "result" : {
              "value": null
            }
          }
          """)
    }
  }

  "global-config/set" must {
    "add and update key value" in {
      implicit val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "global-config/set",
              "id": 0,
              "params": {
                "key": "key1",
                "value": "value1"
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
              "method": "global-config/get",
              "id": 1,
              "params": {
                "key": "key1"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":1,
            "result": {
              "value": "value1"
            }
          }
          """)

      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "global-config/set",
              "id": 2,
              "params": {
                "key": "key1",
                "value": "value2"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":2,
            "result": null
          }
          """)

      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "global-config/get",
              "id": 3,
              "params": {
                "key": "key1"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":3,
            "result" : {
              "value": "value2"
            }
          }
          """)
    }
  }

  "global-config/delete" must {
    "do not fail if the key is missing" in {
      implicit val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "global-config/delete",
              "id": 0,
              "params": {
                "key": "nonexistent"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "result" : null
          }
          """)
    }

    "remove added key" in {
      implicit val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "global-config/set",
              "id": 0,
              "params": {
                "key": "key1",
                "value": "value1"
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
              "method": "global-config/delete",
              "id": 1,
              "params": {
                "key": "key1"
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
              "method": "global-config/get",
              "id": 2,
              "params": {
                "key": "key1"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":2,
            "result": {
              "value": null
            }
          }
          """)
    }
  }

}
