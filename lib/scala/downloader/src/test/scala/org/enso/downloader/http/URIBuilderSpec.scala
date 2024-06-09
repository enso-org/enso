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

    "add queries" in {
      val bldr = URIBuilder.fromUri("http://google.com")
      val uri  = bldr.addQuery("foo", "bar").addQuery("baz", "qux").build()
      uri.toString mustEqual "http://google.com?foo=bar&baz=qux"
    }

    "Handle non-standard symbols in queries" in {
      val bldr = URIBuilder.fromUri("http://google.com")
      val uri  = bldr.addQuery("foo", "bar baz").addQuery("baz", "qux").build()
      uri.toString mustEqual "http://google.com?foo=bar+baz&baz=qux"
    }
  }

}
