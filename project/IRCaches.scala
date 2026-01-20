import sbt.*

import java.io.File

object IRCaches {

  /** As of 2025-11-04, on latest develop (https://github.com/enso-org/enso/actions/runs/19065973719/job/54456606143?pr=14223#step:10:3289),
    * the total cache size is 90.49 MB.
    */
  val EXPECTED_MAX_SIZE_MB = 105

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
      val errMsg =
        f"""
           |Actual IR cache size exceed the expected maximum: ($totalMBs%.2f / $EXPECTED_MAX_SIZE_MB) MB.
           |It is computed as a sum of `.enso` dirs in each standard library.
           |If this is expected, update the `EXPECTED_MAX_SIZE_MB` constant in
           |`project/IRCaches.scala`.
           |""".stripMargin
      log.error(errMsg)
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
