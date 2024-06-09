package org.enso.lockmanager.server

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.pipe
import org.enso.distribution.locking.{Lock, LockType, ThreadSafeLockManager}
import org.enso.lockmanager.server.LockManagerService.InternalLockAcquired
import org.enso.polyglot.runtime.Runtime
import org.enso.polyglot.runtime.Runtime.Api.{
  AcquireLockRequest,
  ReleaseLockRequest
}

import java.util.UUID
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** A service that wraps an underlying [[ThreadSafeLockManager]] instance and
  * exposes its interface to any ConnectedLockManager instances which can
  * connect to this service using the runtime connection.
  *
  * @param underlyingLockManager the lock manager to wrap
  */
class LockManagerService(underlyingLockManager: ThreadSafeLockManager)
    extends Actor {

  /** Internal state of the actor, keeping currently held locks identified by
    * unique random identifiers.
    */
  case class State(locks: Map[UUID, Lock]) {
    def addLock(lock: Lock): (State, UUID) = {
      val id = UUID.randomUUID()
      if (locks.contains(id)) addLock(lock)
      else (copy(locks = locks.updated(id, lock)), id)
    }

    def removeLock(id: UUID): Option[(State, Lock)] = {
      val newState = copy(locks = locks.removed(id))
      locks.get(id).map((newState, _))
    }
  }

  implicit private val ec: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  override def receive: Receive = mainStage(State(Map.empty))

  private def mainStage(state: State): Receive = {
    case Runtime.Api.Request(
          requestId,
          AcquireLockRequest(resourceName, exclusive, returnImmediately)
        ) =>
      val lockType = if (exclusive) LockType.Exclusive else LockType.Shared
      if (returnImmediately) {
        val response = acquireLockImmediately(state, resourceName, lockType)
        sender() ! Runtime.Api.Response(requestId, response)
      } else {
        val replyTo = sender()
        Future(underlyingLockManager.acquireLock(resourceName, lockType))
          .map(Right(_))
          .recover(Left(_))
          .map(InternalLockAcquired(requestId, replyTo, _)) pipeTo self
      }

    case InternalLockAcquired(requestId, replyTo, result) =>
      result match {
        case Left(exception) =>
          replyTo ! Runtime.Api.Response(
            requestId,
            Runtime.Api.LockAcquireFailed(
              s"${exception.toString}: ${exception.getMessage}"
            )
          )
        case Right(lock) =>
          val (newState, lockId) = state.addLock(lock)
          context.become(mainStage(newState))
          replyTo ! Runtime.Api.Response(
            requestId,
            Runtime.Api.LockAcquired(lockId)
          )
      }

    case Runtime.Api.Request(requestId, ReleaseLockRequest(lockId)) =>
      val response = releaseLock(state, lockId)
      sender() ! Runtime.Api.Response(requestId, response)
  }

  private def releaseLock(
    state: State,
    lockId: UUID
  ): Runtime.ApiResponse = state.removeLock(lockId) match {
    case Some((newState, lock)) =>
      try {
        lock.release()
        context.become(mainStage(newState))
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
    state: State,
    resourceName: String,
    lockType: LockType
  ): Runtime.ApiResponse = try {
    underlyingLockManager.tryAcquireLock(resourceName, lockType) match {
      case Some(lock) =>
        val (newState, lockId) = state.addLock(lock)
        context.become(mainStage(newState))
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
}

object LockManagerService {

  /** Creates a configuration object to create [[LockManagerService]].
    *
    * @param underlyingLockManager the lock manager to wrap
    */
  def props(underlyingLockManager: ThreadSafeLockManager): Props = Props(
    new LockManagerService(underlyingLockManager)
  )

  /** Indicates the types of requests from the runtime that this service
    * handles.
    */
  def handledRequestTypes: Seq[Class[_ <: Runtime.ApiRequest]] = Seq(
    classOf[Runtime.Api.AcquireLockRequest],
    classOf[Runtime.Api.ReleaseLockRequest]
  )

  /** An internal message used to pass the result of a Future waiting for a lock
    * to be acquired back to the actor to update the state and send the
    * response.
    */
  case class InternalLockAcquired(
    requestId: Option[Runtime.Api.RequestId],
    replyTo: ActorRef,
    result: Either[Throwable, Lock]
  )
}
