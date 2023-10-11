package org.enso.lockmanager.client

import org.enso.distribution.locking.{Lock, LockManager, LockType}
import org.enso.polyglot.runtime.Runtime
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

/** Implements the [[LockManager]] interface by using a
  * [[RuntimeServerConnectionEndpoint]] and delegating the locking requests to a
  * lock manager service.
  */
class ConnectedLockManager extends LockManager {
  private var endpoint: Option[RuntimeServerConnectionEndpoint] = None

  /** Establishes the connection with the endpoint.
    *
    * The lock manager is not usable before this function is called.
    */
  def connect(endpoint: RuntimeServerConnectionEndpoint): Unit = {
    this.endpoint = Some(endpoint)
  }

  private def getEndpoint: RuntimeServerConnectionEndpoint =
    endpoint.getOrElse {
      throw new IllegalStateException(
        "LockManager is used before the Language Server connection has " +
        "been established."
      )
    }

  private def isExclusive(lockType: LockType): Boolean = lockType match {
    case LockType.Exclusive => true
    case LockType.Shared    => false
  }

  /** @inheritdoc */
  override def acquireLock(resourceName: String, lockType: LockType): Lock = {
    val response = sendRequestAndWaitForResponse(
      Api.AcquireLockRequest(
        resourceName,
        isExclusive(lockType),
        returnImmediately = false
      )
    )
    response match {
      case Success(Api.LockAcquired(lockId)) =>
        WrappedConnectedLock(lockId)
      case Success(Api.LockAcquireFailed(errorMessage)) =>
        throw LockOperationFailed(errorMessage)
      case Success(unexpected) =>
        throw LockOperationFailed(s"Unexpected response [$unexpected].")
      case Failure(exception) =>
        throw LockOperationFailed(exception)
    }
  }

  /** @inheritdoc */
  override def tryAcquireLock(
    resourceName: String,
    lockType: LockType
  ): Option[Lock] = {
    val response = sendRequestAndWaitForResponse(
      Api.AcquireLockRequest(
        resourceName,
        isExclusive(lockType),
        returnImmediately = true
      )
    )
    response match {
      case Success(Api.LockAcquired(lockId)) =>
        Some(WrappedConnectedLock(lockId))
      case Success(Api.CannotAcquireImmediately()) =>
        None
      case Success(Api.LockAcquireFailed(errorMessage)) =>
        throw LockOperationFailed(errorMessage)
      case Success(unexpected) =>
        throw LockOperationFailed(s"Unexpected response [$unexpected].")
      case Failure(exception) =>
        throw LockOperationFailed(exception)
    }
  }

  private def sendRequestAndWaitForResponse(
    request: Runtime.ApiRequest
  ): Try[Runtime.ApiResponse] = {
    val future = getEndpoint.sendRequest(request)
    Try(Await.result(future, Duration(60, TimeUnit.SECONDS)))
  }

  private case class WrappedConnectedLock(lockId: UUID) extends Lock {
    override def release(): Unit = sendRequestAndWaitForResponse(
      Runtime.Api.ReleaseLockRequest(lockId)
    ) match {
      case Success(Api.LockReleased()) =>
      case Success(Api.LockReleaseFailed(errorMessage)) =>
        throw LockOperationFailed(errorMessage)
      case Success(unexpected) =>
        throw LockOperationFailed(s"Unexpected response [$unexpected].")
      case Failure(exception) =>
        throw LockOperationFailed(exception)

    }
  }

  /** Indicates that the lock operation has failed due to some internal errors.
    */
  class LockOperationFailed(message: String, exception: Option[Throwable])
      extends RuntimeException(message, exception.getOrElse(null))
  object LockOperationFailed {
    def apply(exception: Throwable): LockOperationFailed =
      new LockOperationFailed(
        s"Unexpected internal error while waiting on a lock request: ${exception.getMessage}",
        Some(exception)
      )
    def apply(message: String): LockOperationFailed =
      new LockOperationFailed(message, None)
  }
}
