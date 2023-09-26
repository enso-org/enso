package org.enso.projectmanager.protocol

import akka.actor.ActorRef
import io.circe.literal._
import org.enso.jsonrpc.ClientControllerFactory
import org.enso.projectmanager.boot.configuration.TimeoutConfig
import org.enso.projectmanager.event.ClientEvent.ClientDisconnected
import zio.{ZAny, ZIO}

import java.util.UUID
import org.enso.projectmanager.{BaseServerSpec, ProjectManagementOps}
import org.enso.runtimeversionmanager.CurrentVersion
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

  override val engineToInstall = Some(CurrentVersion.version)

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

  "ensure language server shuts down immediately when requesting to close the project" in {
    val client1   = new WsTestClient(address)
    val projectId = createProject("Foo")(client1)
    openProject(projectId)(client1)
    closeProject(projectId)(client1)
    deleteProject(projectId)(client1)
  }

  "ensure language server does not shutdown immediately after last client disconnects" in {
    val client1   = new WsTestClient(address)
    val projectId = createProject("Foo")(client1)
    val socket1   = openProject(projectId)(client1)
    system.eventStream.publish(
      ClientDisconnected(clientUUID, socket1.port)
    )
    client1.send(s"""
        { "jsonrpc": "2.0",
          "method": "project/status",
          "id": 1,
          "params": {
            "projectId": "$projectId"
          }
        }
        """)
    client1.expectJson(json"""
        { "jsonrpc": "2.0",
          "id": 1,
          "result": {
            "status" : {
               "open" : true,
               "shuttingDown" : true
            }
          }
        }
        """)
    val client2 = new WsTestClient(address)
    val socket2 = openProject(projectId)(client2)
    socket2 shouldBe socket1

    client2.send(s"""
        { "jsonrpc": "2.0",
          "method": "project/status",
          "id": 1,
          "params": {
            "projectId": "$projectId"
          }
        }
        """)
    client2.expectJson(json"""
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

    closeProject(projectId)(client2)
    deleteProject(projectId)(client2)
  }

  "ensure language server does eventually shutdown after last client disconnects" in {
    val client    = new WsTestClient(address)
    val projectId = createProject("Foo")(client)
    val socket1   = openProject(projectId)(client)
    system.eventStream.publish(
      ClientDisconnected(clientUUID, socket1.port)
    )
    client.send(s"""
        { "jsonrpc": "2.0",
          "method": "project/status",
          "id": 1,
          "params": {
            "projectId": "$projectId"
          }
        }
        """)
    client.expectJson(json"""
        { "jsonrpc": "2.0",
          "id": 1,
          "result": {
            "status" : {
               "open" : true,
               "shuttingDown" : true
            }
          }
        }
        """)
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
