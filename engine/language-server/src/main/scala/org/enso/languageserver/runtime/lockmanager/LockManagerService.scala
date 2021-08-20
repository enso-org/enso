package org.enso.languageserver.runtime.lockmanager

import akka.actor.{Actor, Props}
import org.enso.distribution.locking.{Lock, LockType, ThreadSafeLockManager}
import org.enso.polyglot.runtime.Runtime
import org.enso.polyglot.runtime.Runtime.Api.{
  AcquireLockRequest,
  ReleaseLockRequest
}

import java.util.UUID
import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class LockManagerService(underlyingLockManager: ThreadSafeLockManager)
    extends Actor {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[Runtime.Api.Request])
  }

  /** This needs to be a mutable state, because a lock may be added
    * asynchronously from a Future waiting on a blocking lock.
    */
  private val locks: collection.concurrent.Map[UUID, Lock] =
    collection.concurrent.TrieMap.empty

  @tailrec
  private def addLock(lock: Lock): UUID = {
    val id         = UUID.randomUUID()
    val wasDefined = locks.putIfAbsent(id, lock).isDefined
    if (wasDefined) addLock(lock)
    else id
  }

  // TODO [RW] how to create a good EC for IO ops?
  implicit private val ec: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  override def receive: Receive = {
    case Runtime.Api.Request(
          requestId,
          AcquireLockRequest(resourceName, exclusive, returnImmediately)
        ) =>
      val lockType = if (exclusive) LockType.Exclusive else LockType.Shared
      if (returnImmediately) {
        val response = acquireLockImmediately(resourceName, lockType)
        sender() ! Runtime.Api.Response(requestId, response)
      } else {
        val replyTo = sender()
        acquireLockBlocking(resourceName, lockType).onComplete {
          case Success(lockId) =>
            replyTo ! Runtime.Api.Response(
              requestId,
              Runtime.Api.LockAcquired(lockId)
            )
          case Failure(exception) =>
            replyTo ! Runtime.Api.Response(
              requestId,
              Runtime.Api.LockAcquireFailed(
                s"${exception.toString}: ${exception.getMessage}"
              )
            )
        }
      }

    case Runtime.Api.Request(requestId, ReleaseLockRequest(lockId)) =>
      val response = releaseLock(lockId)
      sender() ! Runtime.Api.Response(requestId, response)

    // ignore other requests
    case Runtime.Api.Request(_, _) =>
  }

  private def releaseLock(
    lockId: UUID
  ): Runtime.ApiResponse = locks.remove(lockId) match {
    case Some(lock) =>
      try {
        lock.release()
        Runtime.Api.LockReleased()
      } catch {
        case NonFatal(error) =>
          Runtime.Api.LockReleaseFailed(
            s"${error.toString}: ${error.getMessage}"
          )
      }
    case None =>
      Runtime.Api.LockReleaseFailed(s"Lock with id [$lockId] is not known.")
  }

  private def acquireLockImmediately(
    resourceName: String,
    lockType: LockType
  ): Runtime.ApiResponse = try {
    underlyingLockManager.tryAcquireLock(resourceName, lockType) match {
      case Some(lock) =>
        val lockId = addLock(lock)
        Runtime.Api.LockAcquired(lockId)
      case None =>
        Runtime.Api.CannotAcquireImmediately()
    }
  } catch {
    case NonFatal(exception) =>
      Runtime.Api.LockAcquireFailed(
        s"${exception.toString}: ${exception.getMessage}"
      )
  }

  private def acquireLockBlocking(
    resourceName: String,
    lockType: LockType
  ): Future[UUID] =
    Future {
      val lock = underlyingLockManager.acquireLock(resourceName, lockType)
      addLock(lock)
    }
}

object LockManagerService {
  def props(underlyingLockManager: ThreadSafeLockManager): Props = Props(
    new LockManagerService(underlyingLockManager)
  )
}
