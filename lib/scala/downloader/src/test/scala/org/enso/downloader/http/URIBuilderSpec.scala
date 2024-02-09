package org.enso.downloader.http

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class URIBuilderSpec extends AnyWordSpec with Matchers {
  "URIBuilder" should {
    "add path segments" in {
      val bldr = URIBuilder.fromUri("http://google.com")
      val uri  = bldr.addPathSegment("foo").addPathSegment("bar").build()
      uri.toString mustEqual "http://google.com/foo/bar"
    }
  }

}
