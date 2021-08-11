package org.enso.build.editionuploader

import org.enso.build.AWS
import org.enso.editions.EditionName
import org.enso.yaml.YamlHelper
import org.enso.editions.repository.Manifest

import java.io.PrintWriter
import java.nio.file.Path
import scala.util.Using
import scala.util.control.NonFatal

object EditionUploader {
  def main(args: Array[String]): Unit = try {
    val edition = EditionName(buildinfo.Info.currentEdition)
    updateEditionsRepository(edition)
  } catch {
    case NonFatal(error) =>
      println(s"Failed: $error")
      error.printStackTrace()
      sys.exit(1)
  }

  lazy val editionsBucket: String = {
    val repositoryVariable = "GITHUB_REPOSITORY"
    val fullRepoName = sys.env.getOrElse(
      repositoryVariable,
      throw new IllegalStateException(
        s"The environment variable $repositoryVariable was not set."
      )
    )
    val justRepoName = fullRepoName.split('/') match {
      case Array(_, name) => name
      case other =>
        throw new IllegalStateException(
          s"[${other.mkString("Array(", ", ", ")")}] is not a valid value " +
          s"for $repositoryVariable."
        )
    }

    s"s3://editions.release.enso.org/$justRepoName/"
  }

  val NIGHTLIES_TO_KEEP = "NIGHTLIES_TO_KEEP"

  def updateEditionsRepository(editionName: EditionName): Unit = {
    val fileName    = editionName.toFileName
    val source      = Path.of("distribution/editions").resolve(fileName)
    val destination = editionsBucket + fileName
    println(s"Uploading $destination")
    AWS.transfer(source, destination)
    println(s"Uploaded $fileName to the bucket.")

    updateRepositoryManifest(editionName)
    println(s"The manifest file has been updated.")
  }

  def updateRepositoryManifest(newEditionName: EditionName): Unit = {
    val manifestPath = Path.of("target").resolve("tmp-editions-manifest.yaml")
    val manifestUrl  = editionsBucket + Manifest.filename
    AWS.transfer(manifestUrl, manifestPath)

    val manifest = YamlHelper.load[Manifest](manifestPath).get

    def uploadUpdatedManifest(newManifest: Manifest): Unit = {
      Using(new PrintWriter(manifestPath.toFile)) { writer =>
        val content = YamlHelper.toYaml(newManifest)
        writer.write(content)
      }.get

      AWS.transfer(manifestPath, manifestUrl)
    }

    val withNewEdition =
      manifest.copy(editions = manifest.editions ++ Seq(newEditionName))

    sys.env.get(NIGHTLIES_TO_KEEP).map(_.toInt) match {
      case Some(limit) =>
        println(s"Will keep only $limit last nightly builds.")

        val (nightly, regular) =
          withNewEdition.editions.distinct.partition(
            _.name.contains("SNAPSHOT")
          )
        val taken   = nightly.takeRight(limit)
        val removed = nightly.dropRight(limit)

        val updatedManifest = Manifest(regular ++ taken)
        uploadUpdatedManifest(updatedManifest)

        for (edition <- removed) {
          AWS.delete(editionsBucket + edition.toFileName)
          println(s"Removed old nightly edition $edition.")
        }

      case None =>
        uploadUpdatedManifest(withNewEdition)
    }
  }
}
