package org.enso.build.editionuploader

import org.enso.build.AWS
import org.enso.editions.EditionName
import org.enso.yaml.YamlHelper
import org.enso.editions.repository.Manifest

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.util.Using
import scala.util.control.NonFatal

/** A helper program used by our CI to upload the built edition file to S3. */
object EditionUploader {

  /** The entry point.
    *
    * It takes no arguments.
    *
    * The name of the edition to use for the upload is taken from
    * [[buildinfo.Info.currentEdition]].
    *
    * The location for upload is taken from the repository name defined in
    * [[GITHUB_REPOSITORY]] environment variable as used in the GitHub Actions
    * runners.
    *
    * If the environment variable [[NIGHTLIES_TO_KEEP]]
    */
  def main(args: Array[String]): Unit = try {
    val edition = EditionName(buildinfo.Info.currentEdition)
    updateEditionsRepository(edition)
  } catch {
    case NonFatal(error) =>
      println(s"Failed: $error")
      error.printStackTrace()
      sys.exit(1)
  }

  private val GITHUB_REPOSITORY = "GITHUB_REPOSITORY"
  private val NIGHTLIES_TO_KEEP = "NIGHTLIES_TO_KEEP"
  private val nightlyPrefix     = "nightly"

  /** Name of the bucket to upload to.
    *
    * It is based on the name of the repository in which the workflow is being
    * run.
    *
    * This is only used so that any tests that happen in the enso-staging
    * repository do not affect the production repository.
    */
  lazy val editionsBucket: String = {
    val fullRepoName = sys.env.getOrElse(
      GITHUB_REPOSITORY,
      throw new IllegalStateException(
        s"The environment variable $GITHUB_REPOSITORY was not set."
      )
    )
    val justRepoName = fullRepoName.split('/') match {
      case Array(_, name) => name
      case other =>
        throw new IllegalStateException(
          s"[${other.mkString("Array(", ", ", ")")}] is not a valid value " +
          s"for $GITHUB_REPOSITORY."
        )
    }

    s"s3://editions.release.enso.org/$justRepoName/"
  }

  /** Uploads the new edition to the repository and updates the manifest.
    *
    * Also if the [[NIGHTLIES_TO_KEEP]] environment variable is set, it will
    * remove any older nightly editions from that repository.
    *
    * @param newEditionName name of the edition to upload
    */
  def updateEditionsRepository(newEditionName: EditionName): Unit = {
    val fileName = newEditionName.toFileName
    val source   = Path.of("distribution/editions").resolve(fileName)
    if (!Files.exists(source)) {
      throw new IllegalStateException(
        s"The edition to upload [$source] does not exist."
      )
    }
    val destination = editionsBucket + fileName
    println(s"Uploading $destination")
    AWS.transfer(source, destination)
    println(s"Uploaded $fileName to the bucket.")

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
            _.name.startsWith(nightlyPrefix)
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
