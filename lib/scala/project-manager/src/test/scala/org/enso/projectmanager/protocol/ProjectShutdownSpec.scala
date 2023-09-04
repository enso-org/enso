package org.enso.projectmanager.protocol

import akka.actor.ActorRef
import nl.gn0s1s.bump.SemVer
import org.enso.jsonrpc.ClientControllerFactory
import org.enso.projectmanager.boot.configuration.TimeoutConfig
import org.enso.projectmanager.event.ClientEvent.ClientDisconnected
import zio.{ZAny, ZIO}

import java.util.UUID
import org.enso.projectmanager.{BaseServerSpec, ProjectManagementOps}
import org.enso.testkit.FlakySpec

import scala.concurrent.duration._

class ProjectShutdownSpec
    extends BaseServerSpec
    with FlakySpec
    with ProjectManagementOps {

  override def beforeEach(): Unit = {
    super.beforeEach()
    gen.reset()
  }

  override val engineToInstall = Some(SemVer(0, 0, 1))

  override val deleteProjectsRootAfterEachTest = false

  var delayedShutdown = 3.seconds

  var clientUUID: UUID = null

  override def clientControllerFactory: ClientControllerFactory = {
    new ManagerClientControllerFactory[ZIO[ZAny, +*, +*]](
      system                          = system,
      projectService                  = projectService,
      globalConfigService             = globalConfigService,
      runtimeVersionManagementService = runtimeVersionManagementService,
      loggingServiceDescriptor        = loggingService,
      timeoutConfig                   = timeoutConfig
    ) {
      override def createClientController(clientId: UUID): ActorRef = {
        clientUUID = clientId
        super.createClientController(clientId)
      }
    }
  }

  override lazy val timeoutConfig: TimeoutConfig = {
    config.timeout.copy(delayedShutdownTimeout = delayedShutdown)
  }

  "ensure language server does not shutdown immediately after last client disconnects" in {
    val client1   = new WsTestClient(address)
    val projectId = createProject("Foo")(client1)
    val socket1   = openProject(projectId)(client1)
    system.eventStream.publish(
      ClientDisconnected(clientUUID)
    )
    val client2 = new WsTestClient(address)
    val socket2 = openProject(projectId)(client2)
    socket2 shouldBe socket1

    closeProject(projectId)(client2)
    deleteProject(projectId)(client2)
  }

  "ensure language server does eventually shutdown after last client disconnects" in {
    val client1   = new WsTestClient(address)
    val projectId = createProject("Foo")(client1)
    val socket1   = openProject(projectId)(client1)
    system.eventStream.publish(
      ClientDisconnected(clientUUID)
    )
    Thread.sleep(
      (timeoutConfig.delayedShutdownTimeout + timeoutConfig.shutdownTimeout + 1.second).toMillis
    )
    val client2 = new WsTestClient(address)
    val socket2 = openProject(projectId)(client2)
    socket2 shouldNot be(socket1)

    closeProject(projectId)(client2)
    deleteProject(projectId)(client2)
  }

}
