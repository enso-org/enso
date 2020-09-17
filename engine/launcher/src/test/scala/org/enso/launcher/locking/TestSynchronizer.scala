package org.enso.launcher.locking

import java.util.concurrent.{Semaphore, TimeUnit}

import org.scalatest.exceptions.TestFailedException

class TestSynchronizer {

  /**
    * If enabled, prints additional debug messages describing when events are
    * signalled or waited for.
    *
    * Can be enabled to aid with debugging, but should be disabled by default.
    */
  val enableDebugOutput: Boolean = false

  def startThread(name: String)(action: => Unit): Unit =
    this.synchronized {
      val thread = new Thread(() => action, name)
      threads ::= thread
      thread.setUncaughtExceptionHandler(reportException)
      thread.start()
    }

  def waitFor(event: String): Unit = {
    if (enableDebugOutput) {
      System.err.println(s"Waiting for $event.")
      System.err.flush()
    }
    val acquired =
      getSemaphore(event).tryAcquire(timeOutSeconds, TimeUnit.SECONDS)
    if (!acquired) {
      throw new RuntimeException(s"waitFor($event) has timed out.")
    }
  }

  def signal(event: String): Unit = {
    if (enableDebugOutput) {
      System.err.println(s"Signalizing $event.")
      System.err.flush()
    }
    getSemaphore(event).release()
  }

  val timeOutSeconds: Long = 10

  def report(event: String): Unit =
    this.synchronized {
      if (enableDebugOutput) {
        System.err.println(s"Reporting $event.")
        System.err.flush()
      }
      reports ::= event
    }

  def summarizeReports(): Seq[String] = this.synchronized { reports.reverse }

  private def reportException(thread: Thread, throwable: Throwable): Unit = {
    System.err.println(s"${thread.getName} got an exception: $throwable")
    throwable.printStackTrace()
    hadException = true
  }

  def join(): Unit =
    this.synchronized {
      var hadTimeout = false
      for (thread <- threads.reverse) {
        thread.join(timeOutSeconds * 1000)
        if (thread.isAlive) {
          System.err.println(s"Thread ${thread.getName} timed out.")
          thread.interrupt()
          hadTimeout = true
        }
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

  private var reports: List[String]           = Nil
  private var threads: List[Thread]           = Nil
  @volatile private var hadException: Boolean = false

  private val semaphores = collection.concurrent.TrieMap[String, Semaphore]()
  private def getSemaphore(event: String): Semaphore =
    semaphores.getOrElseUpdate(event, new Semaphore(0, true))
}
