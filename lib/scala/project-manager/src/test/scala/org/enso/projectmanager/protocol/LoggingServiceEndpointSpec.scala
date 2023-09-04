package org.enso.projectmanager.protocol

import io.circe.literal.JsonStringContext
import org.enso.projectmanager.BaseServerSpec
import org.enso.testkit.FlakySpec

import scala.concurrent.Future
import java.net.URI

class LoggingServiceEndpointSpec extends BaseServerSpec with FlakySpec {

  class TestException extends RuntimeException {
    override def toString: String = "test-exception"
  }

  "logging-service/get-endpoint" should {
    "fail if endpoint is not setup" in {
      implicit val client = new WsTestClient(address)
      client.send(json"""
            { "jsonrpc": "2.0",
              "method": "logging-service/get-endpoint",
              "id": 0
            }
          """)
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "error": { "code": 4013, "message": "Logging service is not available." }
          }
          """)
    }

    "return the endpoint if it has been set-up" in {
      implicit val client = new WsTestClient(address)
      loggingService.withOverriddenEndpoint(
        Future.successful(Some(URI.create("ws://test-uri/")))
      ) {
        client.send(json"""
            { "jsonrpc": "2.0",
              "method": "logging-service/get-endpoint",
              "id": 0
            }
          """)
        client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "result" : {
              "uri": "ws://test-uri/"
            }
          }
          """)
      }
    }

    "report logging service setup failures" in {
      implicit val client = new WsTestClient(address)
      loggingService.withOverriddenEndpoint(Future.failed(new TestException)) {
        client.send(json"""
            { "jsonrpc": "2.0",
              "method": "logging-service/get-endpoint",
              "id": 0
            }
          """)
        client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":0,
            "error": { "code": 4013, "message": "Logging service failed to set up: test-exception" }
          }
          """)
      }
    }
  }
}
