package org.enso.launcher.locking

import java.util.concurrent.{Semaphore, TimeUnit}

class TestSynchronizer {
  val enableDebugOutput: Boolean = true

  def waitFor(event: String): Unit =
    getSemaphore(event).tryAcquire(timeOutSeconds, TimeUnit.SECONDS)

  def signal(event: String): Unit = {
    if (enableDebugOutput) {
      System.err.println(s"Signalizing $event.")
      System.err.flush()
    }
    getSemaphore(event).release()
  }

  val timeOutSeconds: Long = 10

  def report(id: Int): Unit =
    this.synchronized {
      if (enableDebugOutput) {
        System.err.println(s"Reporting $id.")
        System.err.flush()
      }
      reports ::= id
    }

  def summarizeReports(): Seq[Int] = this.synchronized { reports.reverse }

  private var reports: List[Int] = Nil

  private val semaphores = collection.concurrent.TrieMap[String, Semaphore]()
  private def getSemaphore(event: String): Semaphore =
    semaphores.getOrElseUpdate(event, new Semaphore(0, true))
}
