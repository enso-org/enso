package org.enso.languageserver.util

import scala.collection.BuildFrom
import scala.concurrent.{ExecutionContext, Future}

object CollectionConversions {
  def liftSeqOfFutures[A, CC[X] <: IterableOnce[X], To](
    elements: CC[Future[A]]
  )(implicit
    bf: BuildFrom[CC[Future[A]], A, To],
    ec: ExecutionContext
  ): Future[To] = {
    Future.sequence[A, CC, To](elements)
  }

  def liftOptionOfFuture[T](
    element: Option[Future[T]]
  )(implicit ec: ExecutionContext): Future[Option[T]] = {
    element match {
      case Some(f) => f.map(Some(_))
      case None    => Future.successful(None)
    }
  }
}
