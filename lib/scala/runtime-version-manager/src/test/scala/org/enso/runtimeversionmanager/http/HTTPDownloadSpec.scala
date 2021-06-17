package org.enso.runtimeversionmanager.http

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HTTPDownloadSpec extends AnyWordSpec with Matchers {
  "HTTPDownload" should {
    "accept gzipped responses and decode them correctly" in {
      // TODO [RW] write the test using the simple-library-server
      // generate a 2kb yaml file
      // run the server
      // download the file
    }
  }
}
