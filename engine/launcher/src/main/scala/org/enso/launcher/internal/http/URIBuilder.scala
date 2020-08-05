package org.enso.launcher.internal.http

import java.net.URI

import org.apache.http.client.utils.{URIBuilder => ApacheURIBuilder}

case class URIBuilder private (
  host: String,
  segments: Vector[String],
  queries: Vector[(String, String)]
) {
  def addPathSegment(segment: String): URIBuilder =
    copy(segments = segments.appended(segment))

  def addQuery(key: String, value: String): URIBuilder =
    copy(queries = queries.appended((key, value)))

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
  def fromHost(host: String): URIBuilder =
    new URIBuilder(
      host     = host,
      segments = Vector.empty,
      queries  = Vector.empty
    )

  implicit class URLBuilderSyntax(builder: URIBuilder) {
    def /(part: String): URIBuilder =
      builder.addPathSegment(part)

    def ?(query: (String, String)): URIBuilder =
      builder.addQuery(query._1, query._2)
  }
}
