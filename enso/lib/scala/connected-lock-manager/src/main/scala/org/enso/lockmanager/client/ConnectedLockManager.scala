package org.enso.lockmanager.client

import org.enso.distribution.locking.{Lock, LockManager, LockType}
import org.enso.polyglot.runtime.Runtime
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.Duration

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
      case Api.LockAcquired(lockId) =>
        WrappedConnectedLock(lockId)
      case Api.LockAcquireFailed(errorMessage) =>
        throw new LockOperationFailed(errorMessage)
      case unexpected =>
        throw new LockOperationFailed(s"Unexpected response [$unexpected].")
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
      case Api.LockAcquired(lockId) =>
        Some(WrappedConnectedLock(lockId))
      case Api.CannotAcquireImmediately() =>
        None
      case Api.LockAcquireFailed(errorMessage) =>
        throw new LockOperationFailed(errorMessage)
      case unexpected =>
        throw new LockOperationFailed(s"Unexpected response [$unexpected].")
    }
  }

  private def sendRequestAndWaitForResponse(
    request: Runtime.ApiRequest
  ): Runtime.ApiResponse = {
    val future = getEndpoint.sendRequest(request)
    Await.result(future, Duration.Inf)
  }

  private case class WrappedConnectedLock(lockId: UUID) extends Lock {
    override def release(): Unit = sendRequestAndWaitForResponse(
      Runtime.Api.ReleaseLockRequest(lockId)
    ) match {
      case Api.LockReleased() =>
      case Api.LockReleaseFailed(errorMessage) =>
        throw new LockOperationFailed(errorMessage)
      case unexpected =>
        throw new LockOperationFailed(s"Unexpected response [$unexpected].")
    }
  }

  /** Indicates that the lock operation has failed due to some internal errors.
    */
  class LockOperationFailed(message: String) extends RuntimeException(message)
}
