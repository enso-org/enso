import sbt.*

import java.io.File

object IRCaches {

  /** As of 2025-09-12, on latest develop (https://github.com/enso-org/enso/commit/b1f5f661b9ad45604b6e419d79b8bcb2d2cd59e6),
    * the total cache size is 114.91 MB.
    */
  val EXPECTED_MAX_SIZE_MB = 116

  /** Ensures that IR caches of all standard libraries
    * are within the size limit.
    * @param stdLibRoot Root dir for std libs inside `built-distribution`
    */
  def checkCacheSizes(
    stdLibRoot: File,
    ensoVersion: String,
    log: Logger
  ): Unit = {
    var totalBytes: Double = 0
    for (libName <- stdLibRoot.listFiles()) {
      val libDir    = libName / ensoVersion
      val cacheSize = getCacheSizeForLib(libDir, log)
      totalBytes += cacheSize
    }
    val totalMBs = totalBytes / (1024 * 1024)
    if (totalMBs > EXPECTED_MAX_SIZE_MB) {
      throw new IllegalStateException(
        f"IR cache size $totalMBs%.2f MB exceeds the expected maximum of $EXPECTED_MAX_SIZE_MB MB"
      )
    }
    log.info(
      f"Libs cache size check successful: $totalMBs%.2f/$EXPECTED_MAX_SIZE_MB MB"
    )
  }

  /** Lib cache size in bytes.
    * @param libDir Root lib dir.
    * @return
    */
  private def getCacheSizeForLib(
    libDir: File,
    log: Logger
  ): Double = {
    object FileOnlyFilter extends sbt.io.FileFilter {
      def accept(arg: File): Boolean = arg.isFile
    }
    val cacheDir   = libDir / ".enso"
    val glob       = cacheDir.globRecursive(FileOnlyFilter)
    val cacheFiles = glob.get()
    if (cacheFiles.isEmpty) {
      throw new IllegalStateException(
        s"No IR cache files found in $libDir." +
        "Ensure buildEngineDistribution was run prior to this check."
      )
    }
    val cacheSize: Long = cacheFiles.map(_.length()).sum
    log.debug(s"IR cache size for ${libDir.getAbsolutePath}: $cacheSize B")
    cacheSize
  }
}
