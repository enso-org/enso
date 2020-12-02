package org.enso.projectmanager

import java.util.UUID

import akka.testkit.TestDuration
import io.circe.Json
import io.circe.syntax._
import io.circe.literal._
import org.enso.pkg.SemVerJson._
import io.circe.parser.parse
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.data.{MissingComponentAction, Socket}

import scala.concurrent.duration._

trait ProjectManagementOps { this: BaseServerSpec =>

  def createProject(
    name: String,
    version: Option[SemVer]                                = None,
    missingComponentAction: Option[MissingComponentAction] = None
  )(implicit client: WsTestClient): UUID = {
    val fields = Seq("name" -> name.asJson) ++
      version.map(v => "version" -> v.asJson).toSeq ++
      missingComponentAction
        .map(a => "missingComponentAction" -> a.asJson)
        .toSeq
    val params  = Json.obj(fields: _*)
    val request = json"""
            { "jsonrpc": "2.0",
              "method": "project/create",
              "id": 0,
              "params": $params
            }
          """
    client.send(request)
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
    val Right(openReply) = parse(client.expectMessage(10.seconds.dilated))
    val socket = for {
      result <- openReply.hcursor.downExpectedField("result")
      addr   <- result.downExpectedField("languageServerJsonAddress")
      host   <- addr.downField("host").as[String]
      port   <- addr.downField("port").as[Int]
    } yield Socket(host, port)

    socket.fold(fail(s"Failed to decode json: $openReply", _), identity)
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
    client.expectJson(
      json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "result": null
          }
          """,
      10.seconds.dilated
    )
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
