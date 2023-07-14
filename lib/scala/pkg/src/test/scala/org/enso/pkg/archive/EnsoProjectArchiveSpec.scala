package org.enso.pkg.archive

import org.apache.commons.compress.archivers.tar.{
  TarArchiveEntry,
  TarArchiveInputStream
}
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.{ByteArrayInputStream, File}

class EnsoProjectArchiveSpec extends AnyWordSpec with Matchers {

  "EnsoProjectArchive" should {

    "build test project" in {
      val pkgFile =
        new File(
          getClass.getClassLoader
            .getResource("Enso_Project_Archive_Test")
            .getPath
        )

      val archive = EnsoProjectArchive.build(pkgFile.toPath)
      val entries = readTarGz(archive.bytes())

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
