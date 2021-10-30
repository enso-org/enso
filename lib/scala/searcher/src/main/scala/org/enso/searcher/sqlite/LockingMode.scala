package org.enso.searcher.sqlite

/** A mode used to handle file locking in SQLite */
case class LockingMode private (name: String)

object LockingMode {

  /** Default mode that uses POSIX advisory locks.
    */
  val UnixPosix = LockingMode("unix")

  /** It obtains and holds an exclusive lock on database files,
    * preventing other processes from accessing the database.
    * It uses the `flock` system call.
    */
  val UnixFlock = LockingMode("unix-excl")

  /** It uses dot-file locking rather than POSIX advisory locks. */
  val UnixDotFile = LockingMode("unix-dotfile")

  /** All file locking operations are no-ops. */
  val UnixNone = LockingMode("unix-none")

}
