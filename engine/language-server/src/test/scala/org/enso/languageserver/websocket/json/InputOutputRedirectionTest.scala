package org.enso.languageserver.websocket.json
import io.circe.literal._
import org.enso.logger.ReportLogsOnFailure
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.testkit.RetrySpec

class InputOutputRedirectionTest
    extends BaseServerTest
    with RetrySpec
    with ReportLogsOnFailure {

  "Standard output redirection controller" must {

    "send append notifications only when stdOut is redirected" in {
      val client = getInitialisedWsClient()
      client.send(json"""
            {
              "jsonrpc": "2.0",
              "method": "io/redirectStandardOutput",
              "id": 1,
              "params": null
            }
          """)
      client.expectJson(json"""
             {"jsonrpc":"2.0","id":1,"result":null}
          """)
      stdOut.write("test1".getBytes)
      client.expectJson(json"""
             {
               "jsonrpc":"2.0",
               "method":"io/standardOutputAppended",
               "params":{"output":"test1"}
             }
          """)
      stdOut.write("test2".getBytes)
      client.expectJson(json"""
               {"jsonrpc":"2.0",
               "method":"io/standardOutputAppended",
               "params":{"output":"test2"}}
          """)
      client.send(json"""
            {
              "jsonrpc": "2.0",
              "method": "io/suppressStandardOutput",
              "id": 2,
              "params": null
            }
          """)
      client.expectJson(json"""
             {"jsonrpc":"2.0","id":2,"result":null}
          """)
      stdOut.write("test3".getBytes)
      client.expectNoMessage()
    }

  }

  "Standard error redirection controller" must {

    "send append notifications only when stdErr is redirected" in {
      val client = getInitialisedWsClient()
      client.send(json"""
            {
              "jsonrpc": "2.0",
              "method": "io/redirectStandardError",
              "id": 1,
              "params": null
            }
          """)
      client.expectJson(json"""
             {"jsonrpc":"2.0","id":1,"result":null}
          """)
      stdErr.write("test1".getBytes)
      client.expectJson(json"""
             {
               "jsonrpc":"2.0",
               "method":"io/standardErrorAppended",
               "params":{"output":"test1"}
             }
          """)
      stdErr.write("test2".getBytes)
      client.expectJson(json"""
               {"jsonrpc":"2.0",
               "method":"io/standardErrorAppended",
               "params":{"output":"test2"}}
          """)
      client.send(json"""
            {
              "jsonrpc": "2.0",
              "method": "io/suppressStandardError",
              "id": 2,
              "params": null
            }
          """)
      client.expectJson(json"""
             {"jsonrpc":"2.0","id":2,"result":null}
          """)
      stdErr.write("test3".getBytes)
      client.expectNoMessage()
    }

  }

  "Standard input controller" must {

    "notify context owners when read is blocked" taggedAs Retry in {
      val client = getInitialisedWsClient()
      client.send(ExecutionContextJsonMessages.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(
        ExecutionContextJsonMessages
          .executionContextCreateResponse(1, contextId)
      )
      val buffer = new Array[Byte](3)
      new Thread(() => stdIn.read(buffer)).start()
      client.expectJson(json"""
           {
             "jsonrpc":"2.0",
             "method":"io/waitingForStandardInput",
             "params":null
           }
          """)
      client.send(json"""
            {
              "jsonrpc": "2.0",
              "method": "io/feedStandardInput",
              "id": 2,
              "params": {
                "input": "abc",
                "isLineTerminated": false
              }
            }
          """)
      client.expectJson(json"""
             {"jsonrpc":"2.0","id":2,"result":null}
          """)
      Thread.sleep(1000)
      buffer.toList shouldBe List(97.byteValue, 98.byteValue, 99.byteValue)
    }

  }

}
