package org.enso.pkg.archive

import org.apache.commons.compress.archivers.tar.{
  TarArchiveEntry,
  TarArchiveInputStream
}
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.ByteArrayInputStream
import java.nio.file.Path

class EnsoProjectArchiveSpec extends AnyWordSpec with Matchers {

  "EnsoProjectArchive" should {

    "build test project" in {
      val pkgPath =
        Path.of(
          getClass.getClassLoader
            .getResource("Enso_Project_Archive_Test")
            .getPath
        )
      println(pkgPath)

      val archive = EnsoProjectArchive.build(pkgPath)
      val entries = readTarGz(archive.outputStream().toByteArray)

      entries.map(_.getName) should contain theSameElementsAs Seq(
        "src/",
        "src/Main.enso",
        "package.yaml",
        "data/",
        "data/sample.txt"
      )
    }
  }

  private def readTarGz(bytes: Array[Byte]): Seq[TarArchiveEntry] = {
    val in    = new ByteArrayInputStream(bytes)
    val gzIn  = new GzipCompressorInputStream(in)
    val tarIn = new TarArchiveInputStream(gzIn)
    try {
      Iterator
        .continually(tarIn.getNextTarEntry)
        .takeWhile(_ ne null)
        .toSeq
    } finally {
      tarIn.close()
      gzIn.close()
    }
  }
}
