package org.enso.runtimeversionmanager.test

import java.nio.channels.FileChannel
import java.nio.file.{Path, StandardOpenOption}

import scala.io.StdIn

object Main {

  /** Entry-point for the helper application used by
    * `runtime-version-manager-test` to test inter-process locking.
    *
    * It expects exactly one argument - path to the file that should be locked.
    * It acquires a lock for this file and reports that the lock has been
    * acquired to stderr. Afterwards, it waits for a new line in stdin and once
    * it appears, it releases the lock and exits.
    */
  def main(args: Array[String]): Unit = {
    args match {
      case Array(pathString) =>
        try {
          val lockfilePath = Path.of(pathString)
          val channel = FileChannel.open(
            lockfilePath,
            StandardOpenOption.CREATE,
            StandardOpenOption.READ,
            StandardOpenOption.WRITE
          )
          val shared = true
          val lock   = channel.lock(0L, Long.MaxValue, shared)
          System.err.println("Lock acquired")
          StdIn.readLine()
          lock.release()
          System.exit(0)
        } catch {
          case throwable: Throwable =>
            throwable.printStackTrace()
            System.exit(1)
        }
      case _ =>
        System.err.println("Invalid argument count")
        System.exit(1)
    }
  }
}
