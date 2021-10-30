package org.enso.loggingservice.internal.service

class LocalServiceSpec extends ServiceTest {
  "Local service" should {
    "gather messages" in {
      testServiceMessageGathering { (logLevel, queue, printers) =>
        Local.setup(logLevel, queue, printers)
      }
    }
  }
}
