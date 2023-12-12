package org.enso.interpreter.test.instrument

import org.apache.commons.io.FileUtils
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot.{LanguageInfo, PolyglotContext}
import org.enso.polyglot.runtime.Runtime.Api
import org.graalvm.polyglot.Context

import java.io.File
import java.nio.file.{Files, Path}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration._

abstract class InstrumentTestContext(packageName: String) {
  protected val messageQueue: LinkedBlockingQueue[Api.Response] =
    new LinkedBlockingQueue()

  protected val tmpDir: Path = Files.createTempDirectory("enso-test-packages")

  private val lockManager = new ThreadSafeFileLockManager(
    tmpDir.resolve("locks")
  )

  val pkg: Package[File] =
    PackageManager.Default.create(tmpDir.toFile, packageName, "Enso_Test")

  protected val context: Context

  protected var executionContext: PolyglotContext = null

  def init(): Unit = {
    assert(context != null)
    executionContext = new PolyglotContext(context)
    context.initialize(LanguageInfo.ID)
  }

  protected val runtimeServerEmulator: RuntimeServerEmulator =
    new RuntimeServerEmulator(messageQueue, lockManager)

  def receiveNone: Option[Api.Response] = {
    Option(messageQueue.poll())
  }

  def receive: Option[Api.Response] = {
    Option(messageQueue.poll(10, TimeUnit.SECONDS))
  }

  def receiveWithTimeout(timeoutSeconds: Long): Option[Api.Response] = {
    Option(messageQueue.poll(timeoutSeconds, TimeUnit.SECONDS))
  }

  def receiveN(n: Int, timeoutSeconds: Long = 10): List[Api.Response] = {
    Iterator
      .continually(receiveWithTimeout(timeoutSeconds))
      .take(n)
      .flatten
      .toList
  }

  def receiveNIgnoreExpressionUpdates(
    n: Int,
    timeoutSeconds: Long = 60
  ): List[Api.Response] = {
    receiveNWithFilter(
      n,
      {
        case Some(Api.Response(None, Api.ExpressionUpdates(_, _))) => false
        case _                                                     => true
      },
      timeoutSeconds
    )
  }

  def receiveNIgnorePendingExpressionUpdates(
    n: Int,
    timeoutSeconds: Long                  = 60,
    updatesOnlyFor: Set[Api.ExpressionId] = Set()
  ): List[Api.Response] = {
    receiveNWithFilter(
      n,
      {
        case Some(Api.Response(None, Api.ExpressionUpdates(_, updates))) =>
          updates.find { u =>
            u.payload match {
              case _: Api.ExpressionUpdate.Payload.Pending => false
              case _ =>
                updatesOnlyFor.isEmpty || updatesOnlyFor.contains(
                  u.expressionId
                )
            }
          }.isDefined
        case _ => true
      },
      timeoutSeconds
    )
  }

  def receiveNIgnoreStdLib(
    n: Int,
    timeoutSeconds: Long = 60
  ): List[Api.Response] = {
    receiveNWithFilter(n, _ => true, timeoutSeconds)
  }

  private def receiveNWithFilter(
    n: Int,
    f: (Any => Boolean),
    timeoutSeconds: Long
  ): List[Api.Response] = {
    var count: Int                     = n
    var lastSeen: Option[Api.Response] = None
    Iterator
      .continually(receiveWithTimeout(timeoutSeconds))
      .filter(f)
      .takeWhile {
        case Some(Api.Response(None, Api.LibraryLoaded(_, _, _, _))) =>
          count > 0
        case msg @ Some(_) =>
          count -= 1
          lastSeen = msg
          count > 0
        case None =>
          false
      }
      .flatten
      .filter(excludeLibraryLoadingPayload)
      .toList ++ lastSeen
  }

  private def excludeLibraryLoadingPayload(response: Api.Response): Boolean =
    response match {
      case Api.Response(None, Api.LibraryLoaded(_, _, _, _)) =>
        false
      case _ =>
        true
    }

  def close(): Unit = {
    if (context != null) {
      context.close()
    }
    Await.ready(runtimeServerEmulator.terminate(), 5.seconds)
    lockManager.reset()
    FileUtils.deleteQuietly(tmpDir.toFile)
    messageQueue.clear()
  }

}

object InstrumentTestContext {
  val DISABLE_IR_CACHE =
    Option(System.getenv("ENSO_TEST_DISABLE_IR_CACHE")).getOrElse("true")
}
