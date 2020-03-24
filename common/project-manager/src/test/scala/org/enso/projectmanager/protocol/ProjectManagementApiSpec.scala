package org.enso.projectmanager.protocol

import java.io.File
import java.nio.file.Paths
import java.util.UUID

import io.circe.literal._

class ProjectManagementApiSpec extends BaseServerSpec {

  "project/create" must {

    "check if project name is not empty" in {
      val client = new WsTestClient(address)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "project/create",
            "id": 1,
            "params": {
              "name": ""
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": { "code": 4001, "message": "Cannot create project with empty name" }
          }
          """)
    }

    "validate project name" in {
      val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/create",
              "id": 1,
              "params": {
                "name": "luna-test-project4/#$$%^@!"
              }
            }
          """)
      client.expectJson(json"""
          {"jsonrpc":"2.0",
          "id":1,
          "error":{
            "code":4001,
            "message":"Project name contains forbidden characters: %,!,@,#,$$,^,/"
            }
          }
          """)
    }

    "fail when the project with the same name exists" in {
      val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/create",
              "id": 1,
              "params": {
                "name": "foo"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "projectId" : $TestUUID
            }
          }
          """)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/create",
              "id": 2,
              "params": {
                "name": "foo"
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":2,
            "error":{
              "code":4003,
              "message":"Project with the provided name exists"
            }
          }
          """)
    }

    "create project structure" in {
      val projectName = "luna_test-project1"
      val projectDir  = new File(userProjectDir, projectName)
      val packageFile = new File(projectDir, "package.yaml")
      val mainEnso    = Paths.get(projectDir.toString, "src", "Main.enso").toFile

      val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/create",
              "id": 1,
              "params": {
                "name": $projectName
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 1,
            "result" : {
              "projectId" : $TestUUID
            }
          }
          """)
      packageFile shouldBe Symbol("file")
      mainEnso shouldBe Symbol("file")
    }
  }

  "project/delete" must {

    "fail when project doesn't exist" in {
      val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/delete",
              "id": 1,
              "params": {
                "projectId": ${UUID.randomUUID()}
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":1,
            "error":{
              "code":4004,
              "message":"Project with the provided id does not exist"
            }
          }
          """)

    }

    "remove project structure" in {
      val projectName = "to-remove"
      val projectDir  = new File(userProjectDir, projectName)

      val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/create",
              "id": 0,
              "params": {
                "name": $projectName
              }
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc" : "2.0",
            "id" : 0,
            "result" : {
              "projectId" : $TestUUID
            }
          }
          """)
      projectDir shouldBe 'directory
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/delete",
              "id": 1,
              "params": {
                "projectId": $TestUUID
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

      projectDir.exists() shouldBe false
    }

  }

}
