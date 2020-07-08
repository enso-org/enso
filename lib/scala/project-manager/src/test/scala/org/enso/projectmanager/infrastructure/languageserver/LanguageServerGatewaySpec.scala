package org.enso.projectmanager.infrastructure.languageserver

import org.enso.jsonrpc.test.FlakySpec
import org.enso.projectmanager.test.Net._
import org.enso.projectmanager.{BaseServerSpec, ProjectManagementOps}

import scala.concurrent.Await
import scala.concurrent.duration._

class LanguageServerGatewaySpec
    extends BaseServerSpec
    with FlakySpec
    with ProjectManagementOps {

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
      Await.result(future, 20.seconds)
      //then
      tryConnect(fooSocket).isLeft shouldBe true
      tryConnect(barSocket).isLeft shouldBe true
      tryConnect(bazSocket).isLeft shouldBe true
      //teardown
      deleteProject(fooId)
      deleteProject(barId)
      deleteProject(bazId)
    }

  }

}
