package org.enso.languageserver.websocket.json
import io.circe.literal._
import org.enso.languageserver.websocket.json.{SearchJsonMessages => json}
import org.enso.logging.service.logback.test.provider.ReportLogsOnFailure
import org.enso.testkit.{FlakySpec, RetrySpec}

class SuggestionsHandlerTest
    extends BaseServerTest
    with FlakySpec
    with RetrySpec
    with ReportLogsOnFailure {

  "SuggestionsHandler" must {

    "get initial suggestions database version" taggedAs Retry in {
      val client = getInitialisedWsClient()

      client.send(json.getSuggestionsDatabaseVersion(0))
      client.expectJson(json"""
          { "jsonrpc" : "2.0",
            "id" : 0,
            "result" : {
              "currentVersion" : 0
            }
          }
      """)
    }

    "get initial suggestions database" taggedAs Flaky in {
      val client = getInitialisedWsClient()

      client.send(json.getSuggestionsDatabase(0))
      client.expectJson(json"""
          { "jsonrpc" : "2.0",
            "id" : 0,
            "result" : {
              "entries" : [
              ],
              "currentVersion" : 0
            }
          }
      """)
    }

  }

}
