package org.enso.downloader.http

import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets

/** A simple immutable builder for URIs based on URLs.
  *
  * It contains very limited functionality that is needed by the APIs used in
  * the launcher. It can be easily extended if necessary.
  */
case class URIBuilder private (uri: URI) {

  /** Resolve a segment over the path in the URI.
    *
    * For example adding `bar` to `http://example.com/foo` will result in
    * `http://example.com/foo/bar`.
    */
  def addPathSegment(segment: String): URIBuilder = {
    val pathItems = uri.getRawPath.split("/")
    val newPath   = (pathItems :+ segment).mkString("/")
    copy(uri.resolve(newPath))
  }

  /** Add a query parameter to the URI.
    *
    * The query is appended at the end.
    */
  def addQuery(key: String, value: String): URIBuilder = {
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

  /** Build the URI represented by this builder. */
  def build(): URI = uri
}

object URIBuilder {

  /** Create a builder basing from a hostname.
    *
    * A builder created by `fromHost("example.com")` represents
    * `https://example.com/`.
    */
  def fromHost(host: String): URIBuilder = {
    val scheme           = "https"
    val path: String     = null
    val fragment: String = null
    new URIBuilder(new URI(scheme, host, path, fragment))
  }

  /** Creates a builder from an arbitrary [[URI]] instance. */
  def fromUri(uri: URI): URIBuilder = {
    new URIBuilder(uri)
  }

  /** Creates a builder from an arbitrary URI represented as string.
    */
  def fromUri(uri: String): URIBuilder = {
    new URIBuilder(new URI(uri))
  }

  /** A simple DSL for the URIBuilder. */
  implicit class URIBuilderSyntax(builder: URIBuilder) {

    /** Extends the URI with an additional path segment. */
    def /(part: String): URIBuilder =
      builder.addPathSegment(part)

    /** Adds a query to the URI. */
    def ?(query: (String, String)): URIBuilder =
      builder.addQuery(query._1, query._2)
  }
}
