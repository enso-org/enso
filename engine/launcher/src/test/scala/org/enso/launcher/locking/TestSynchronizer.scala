package org.enso.launcher.locking

import java.util.concurrent.{Semaphore, TimeUnit}

import org.scalatest.exceptions.TestFailedException
import scala.jdk.CollectionConverters._

/**
  * A helper class that can be used to synchronize actions between multiple
  * threads to create interleaving scenarios useful in testing concurrent
  * interaction.
  *
  * To avoid stalling the tests, all blocking actions can time out after a
  * specified time.
  */
class TestSynchronizer {

  /**
    * If enabled, prints additional debug messages describing when events are
    * signalled or waited for.
    *
    * Can be enabled to aid with debugging, but should be disabled by default.
    */
  val enableDebugOutput: Boolean = false

  /**
    * Executes the `action` in a separate thread with the given `name`
    */
  def startThread(name: String)(action: => Unit): Unit = {
    val thread = new Thread(() => action, name)
    threads ::= thread
    thread.setUncaughtExceptionHandler(reportException)
    thread.start()
  }

  /**
    * Waits for a signal indicating that an event with the specified name has
    * happened.
    *
    * Will return immediately if the event has happened prior to calling that
    * function.
    *
    * Each `waitFor` call corresponds to a single [[signal]] call, so waiting
    * twice on the same event name will need two signals to wake up.
    */
  def waitFor(event: String): Unit = {
    if (enableDebugOutput) {
      System.err.println(s"$threadName waiting for $event.")
      System.err.flush()
    }
    val acquired =
      getSemaphore(event).tryAcquire(timeOutSeconds, TimeUnit.SECONDS)
    if (!acquired) {
      throw new RuntimeException(s"$threadName waitFor($event) has timed out.")
    }
    if (enableDebugOutput) {
      System.err.println(s"$threadName has been woken up by $event.")
      System.err.flush()
    }
  }

  /**
    * Signals that an event has happened.
    *
    * Will wake up the thread waiting for it. Only one waiting thread is woken
    * up, so if multiple threads wait for the same event, it must be signalled
    * multiple times. It is advised to not make multiple threads wait on a
    * single event.
    */
  def signal(event: String): Unit = {
    if (enableDebugOutput) {
      System.err.println(s"$threadName signalling $event.")
      System.err.flush()
    }
    getSemaphore(event).release()
  }

  /**
    * Timeout used by [[waitFor]].
    */
  val timeOutSeconds: Long = 20

  /**
    * Reports that the event has happened now.
    *
    * Can be used to test for order of events. Independent of [[waitFor]] and
    * [[signal]].
    */
  def report(event: String): Unit = {
    if (enableDebugOutput) {
      System.err.println(s"$threadName reporting $event.")
      System.err.flush()
    }
    reports.add(event)
  }

  /**
    * Returns names of events reported using [[report]] in the order that they
    * were reported.
    *
    * Should only be called *after* [[join]].
    */
  def summarizeReports(): Seq[String] = reports.asScala.toSeq

  private def reportException(thread: Thread, throwable: Throwable): Unit = {
    throwable match {
      case _: InterruptedException =>
        System.err.println(s"${thread.getName} was interrupted: $throwable")
      case _ =>
        System.err.println(s"${thread.getName} got an exception: $throwable")
    }
    throwable.printStackTrace()
    hadException = true
  }

  /**
    * Waits for all threads started with [[startThread]] to finish execution.
    *
    * Reports any exceptions thrown inside of the threads or if any of the
    * threads has timed out.
    *
    * @param joinTimeOutSeconds timeout used when waiting for each thread
    */
  def join(joinTimeOutSeconds: Long = timeOutSeconds): Unit = {
    var hadTimeout = false
    try {
      for (thread <- threads.reverse) {
        thread.join(joinTimeOutSeconds * 1000)
        if (thread.isAlive) {
          System.err.println(s"Thread ${thread.getName} timed out.")
          thread.interrupt()
          hadTimeout = true
        }
      }
    } catch {
      case interrupt: InterruptedException =>
        for (thread <- threads) {
          try thread.interrupt()
          catch { case _: Throwable => }
        }
        throw interrupt
    }

    if (hadException) {
      throw new TestFailedException(
        "One of the threads caught an exception.",
        1
      )
    }

    if (hadTimeout) {
      throw new TestFailedException("One of the threads has timed out.", 1)
    }
  }

  private def threadName: String = Thread.currentThread.getName

  private val reports = new java.util.concurrent.LinkedTransferQueue[String]

  private var threads: List[Thread]           = Nil
  @volatile private var hadException: Boolean = false

  private val semaphores = collection.concurrent.TrieMap[String, Semaphore]()
  private def getSemaphore(event: String): Semaphore =
    semaphores.getOrElseUpdate(event, new Semaphore(0, true))
}
