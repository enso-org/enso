package org.enso.projectmanager.infrastructure.languageserver

import akka.testkit.TestDuration
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.test.Net._
import org.enso.projectmanager.{BaseServerSpec, ProjectManagementOps}
import org.enso.testkit.{FlakySpec, RetrySpec}

import scala.concurrent.Await
import scala.concurrent.duration._

class LanguageServerGatewaySpec
    extends BaseServerSpec
    with FlakySpec
    with ProjectManagementOps
    with RetrySpec {

  override val engineToInstall = Some(SemVer(0, 0, 1))

  "A language server service" must {

    "kill all running language servers" taggedAs Retry ignore {
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

  }

}
