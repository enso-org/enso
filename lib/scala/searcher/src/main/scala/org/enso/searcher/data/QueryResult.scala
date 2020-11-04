package org.enso.searcher.data

case class QueryResult[A](ids: Seq[Long], value: A)
