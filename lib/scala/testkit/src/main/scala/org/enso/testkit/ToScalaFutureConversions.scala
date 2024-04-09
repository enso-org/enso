package org.enso.testkit

import org.scalatest.TestSuite

import java.util.concurrent.CompletableFuture

import scala.concurrent.Future
import scala.jdk.FutureConverters._

trait ToScalaFutureConversions extends TestSuite {

  /** Convert Java future to Scala. */
  implicit final def toScalaFuture[A](f: CompletableFuture[A]): Future[A] =
    f.asScala
}
