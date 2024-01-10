package org.enso.projectmanager.infrastructure.languageserver

import akka.testkit.TestDuration
import io.circe.literal._
import nl.gn0s1s.bump.SemVer
import org.enso.logger.ReportLogsOnFailure
import org.enso.projectmanager.test.Net._
import org.enso.projectmanager.{BaseServerSpec, ProjectManagementOps}
import org.enso.runtimeversionmanager.test.OverrideTestVersionSuite

import scala.concurrent.Await
import scala.concurrent.duration._

class LanguageServerGatewaySpec
    extends BaseServerSpec
    with OverrideTestVersionSuite
    with ProjectManagementOps
    with ReportLogsOnFailure {

  override val testVersion: SemVer = SemVer(0, 0, 1)

  override val engineToInstall = Some(SemVer(0, 0, 1))

  "A language server service" must {

    "kill all running language servers" ignore {
      implicit val client = new WsTestClient(address)
      val fooId           = createProject("foo")
      val barId           = createProject("bar")
      val bazId           = createProject("baz")
      val fooSocket       = openProject(fooId)
      val barSocket       = openProject(barId)
      val bazSocket       = openProject(bazId)
      tryConnect(fooSocket).isRight shouldBe true
      tryConnect(barSocket).isRight shouldBe true
      tryConnect(bazSocket).isRight shouldBe true
      //when
      val future = exec.exec(languageServerGateway.killAllServers())
      Await.result(future, 30.seconds.dilated)
      //then
      tryConnect(fooSocket).isLeft shouldBe true
      tryConnect(barSocket).isLeft shouldBe true
      tryConnect(bazSocket).isLeft shouldBe true
      //teardown
      deleteProject(fooId)
      deleteProject(barId)
      deleteProject(bazId)
    }

    "report language server status" in {
      implicit val client = new WsTestClient(address)
      val fooId           = createProject("foo")
      //val fooSocket       =
      openProject(fooId)
      client.send(s"""
          { "jsonrpc": "2.0",
            "method": "project/status",
            "id": 1,
            "params": {
              "projectId": "$fooId"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "status" : {
                 "open" : true,
                 "shuttingDown" : false
              }
            }
          }
          """)
      closeProject(fooId)
      deleteProject(fooId)
    }

  }

}
