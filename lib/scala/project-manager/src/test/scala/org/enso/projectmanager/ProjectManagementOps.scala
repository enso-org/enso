package org.enso.projectmanager

import java.util.UUID

import io.circe.literal._
import io.circe.parser.parse
import org.enso.projectmanager.data.Socket

import scala.concurrent.duration._

trait ProjectManagementOps { this: BaseServerSpec =>

  def createProject(name: String)(implicit client: WsTestClient): UUID = {
    client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/create",
              "id": 0,
              "params": {
                "name": $name
              }
            }
          """)
    val projectId = getGeneratedUUID
    client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "result": {
              "projectId": $projectId
            }
          }
          """)
    projectId
  }

  def openProject(
    projectId: UUID
  )(implicit client: WsTestClient): Socket = {
    client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/open",
              "id": 0,
              "params": {
                "projectId": $projectId
              }
            }
          """)
    val Right(openReply) = parse(client.expectMessage(10.seconds))
    val socketField = openReply.hcursor
      .downField("result")
      .downField("languageServerJsonAddress")
    val Right(host) = socketField.downField("host").as[String]
    val Right(port) = socketField.downField("port").as[Int]
    Socket(host, port)
  }

  def closeProject(
    projectId: UUID
  )(implicit client: WsTestClient): Unit = {
    client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/close",
              "id": 0,
              "params": {
                "projectId": $projectId
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
  }

  def deleteProject(
    projectId: UUID
  )(implicit client: WsTestClient): Unit = {
    client.send(json"""
            { "jsonrpc": "2.0",
              "method": "project/delete",
              "id": 0,
              "params": {
                "projectId": $projectId
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
  }

}
