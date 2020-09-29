package org.enso.loggingservice.internal.service

import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClientServerServiceSpec
    extends AnyWordSpec
    with Matchers
    with OptionValues {
  "Client and Server" should {
    "communicate" in {}
  }

  "Server" should {
    "also gather local messages" in {
      LocalServiceSpec.testServiceMessageGathering {
        (logLevel, queue, printers) =>
          Server.setup("localhost", 0, queue, printers, logLevel)
      }
    }
  }
}
