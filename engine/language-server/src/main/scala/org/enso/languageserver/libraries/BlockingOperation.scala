package org.enso.languageserver.libraries

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object BlockingOperation {
  def run[A](operation: => A): Future[A] =
    Future(operation)(blockingExecutionContext)

  private lazy val blockingExecutionContext =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
}
