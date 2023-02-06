package org.enso.languageserver.vcsmanager

import java.io.File
import org.eclipse.jgit.util.FS

/** Custom implementation of org.eclipse.jgit.util.FS which workarounds an issue on MacOS
  * when `bash --logic -c "which git"` returns more than a single line output.
  * One cannot delegate to proxy's `discoverGitExe` because of package's mismatch so we return null.
  * For our purposes this is acceptable because discovering git exec is only used for inferring
  * git config.
  *
  * @param proxy platform-dependent FS implementation
  */
class CustomFS(proxy: FS) extends FS {
  override def newInstance(): FS = proxy.newInstance()

  override def supportsExecute(): Boolean = proxy.supportsExecute()

  override def isCaseSensitive: Boolean = proxy.isCaseSensitive

  override def canExecute(f: File): Boolean = proxy.canExecute(f)

  override def setExecute(f: File, canExec: Boolean): Boolean =
    proxy.setExecute(f, canExec)

  override def retryFailedLockFileCommit(): Boolean =
    proxy.retryFailedLockFileCommit()

  override def discoverGitExe(): File = null

  override def runInShell(cmd: String, args: Array[String]): ProcessBuilder =
    proxy.runInShell(cmd, args)
}
