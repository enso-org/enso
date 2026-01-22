package org.enso.downloader.http

import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets

case class HTTPURIBuilder(uri: URI) extends URIBuilder {

  /** Resolve a segment over the path in the URI.
    *
    * For example adding `bar` to `http://example.com/foo` will result in
    * `http://example.com/foo/bar`.
    */
  def addPathSegment(segment: String): HTTPURIBuilder = {
    val pathItems = uri.getRawPath.split("/")
    val newPath   = (pathItems :+ segment).mkString("/")
    copy(uri.resolve(newPath))
  }

  def addQuery(key: String, value: String): HTTPURIBuilder = {
    val scheme       = uri.getScheme
    val authority    = uri.getAuthority
    val path         = uri.getPath
    val query        = if (uri.getQuery == null) "" else uri.getQuery + "&"
    val fragment     = uri.getFragment
    val encodedKey   = URLEncoder.encode(key, StandardCharsets.UTF_8)
    val encodedValue = URLEncoder.encode(value, StandardCharsets.UTF_8)
    val newQuery     = query + encodedKey + "=" + encodedValue
    val newUri       = new URI(scheme, authority, path, newQuery, fragment)
    copy(newUri)
  }

  def build(): URI = uri
}
