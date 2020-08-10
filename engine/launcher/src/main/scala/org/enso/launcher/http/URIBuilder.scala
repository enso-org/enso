package org.enso.launcher.http

import java.net.URI

import org.apache.http.client.utils.{URIBuilder => ApacheURIBuilder}

/**
  * A simple immutable builder for URIs based on URLs.
  *
  * It contains very limited functionality that is needed by the APIs used in
  * the launcher. It can be easily extended if necessary.
  *
  * As all APIs we use support HTTPS, it does not allow to create a non-HTTPS
  * URL.
  */
case class URIBuilder private (
  host: String,
  segments: Vector[String],
  queries: Vector[(String, String)]
) {

  /**
    * Resolve a segment over the path in the URI.
    *
    * For example adding `bar` to `http://example.com/foo` will result in
    * `http://example.com/foo/bar`.
    */
  def addPathSegment(segment: String): URIBuilder =
    copy(segments = segments.appended(segment))

  /**
    * Add a query parameter to the URI.
    *
    * The query is appended at the end.
    */
  def addQuery(key: String, value: String): URIBuilder =
    copy(queries = queries.appended((key, value)))

  /**
    * Build the URI represented by this builder.
    */
  def build(): URI = {
    val base = (new ApacheURIBuilder)
      .setScheme("https")
      .setHost(host)
      .setPathSegments(segments: _*)
    val withQueries = queries.foldLeft(base)((builder, query) =>
      builder.addParameter(query._1, query._2)
    )
    withQueries.build()
  }
}

object URIBuilder {

  /**
    * Create a builder basing from a hostname.
    *
    * A builder created by `fromHost("example.com")` represents
    * `https://example.com/`.
    */
  def fromHost(host: String): URIBuilder =
    new URIBuilder(
      host     = host,
      segments = Vector.empty,
      queries  = Vector.empty
    )

  /**
    * A simple DSL for the URIBuilder.
    */
  implicit class URIBuilderSyntax(builder: URIBuilder) {
    def /(part: String): URIBuilder =
      builder.addPathSegment(part)

    def ?(query: (String, String)): URIBuilder =
      builder.addQuery(query._1, query._2)
  }
}
