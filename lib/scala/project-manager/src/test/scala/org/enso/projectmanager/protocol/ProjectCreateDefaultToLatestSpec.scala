package org.enso.projectmanager.protocol

import io.circe.literal.JsonStringContext
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.BaseServerSpec
import org.enso.runtimeversionmanager.test.OverrideTestVersionSuite

class ProjectCreateDefaultToLatestSpec
    extends BaseServerSpec
    with OverrideTestVersionSuite {

  override val testVersion: SemVer = SemVer(0, 1, 0)

  "project/create" should {

    "default to latest available engine version if none are installed" in {
      implicit val client = new WsTestClient(address)
      client.send(json"""
        { "jsonrpc": "2.0",
          "method": "project/create",
          "id": 1,
          "params": {
            "name": "Testproj",
            "missingComponentAction": "Install"
          }
        }
        """)

      /** The error here is expected (only the latest version will give this
        * error), we just wanted to check the logic for selecting the latest
        * version, not installing.
        */
      val message =
        "Project manager 9999.0.0 is required to install the requested " +
        "engine. Please upgrade."
      client.expectJson(json"""
          {
            "jsonrpc":"2.0",
            "id":1,
            "error": {
              "code": 4022,
              "message": $message,
              "payload" : {
                "minimumRequiredVersion" : "9999.0.0"
              }
            }
          }
          """)
    }
  }
}
