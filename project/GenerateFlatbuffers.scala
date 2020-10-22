import java.io.IOException

import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

object GenerateFlatbuffers {

  val flatcVersion     = settingKey[String]("flatc version used in the project")
  private val flatcCmd = "flatc"

  lazy val task = Def.task {
    val log = state.value.log
    verifyFlatcVersion(flatcVersion.value) match {
      case Left(explanation) =>
        log.error(explanation)
        throw new RuntimeException("flatc version check failed.")
      case Right(_) =>
    }

    val root = baseDirectory.value
    val schemas =
      (file(s"$root/src/main/schema") ** "*.fbs").get

    val generatedSourcesStore =
      streams.value.cacheStoreFactory.make("flatc_generated_sources")
    val schemaSourcesStore =
      streams.value.cacheStoreFactory.make("flatc_schemas")
    val out              = (sourceManaged in Compile).value
    val generatedSources = gatherGeneratedSources(schemas, out, log)

    Tracked.diffOutputs(generatedSourcesStore, FileInfo.exists)(
      generatedSources
    ) { generatedDiff: ChangeReport[File] =>
      generatedDiff.removed foreach { removedFile => removedFile.delete() }
    }

    Tracked.diffInputs(schemaSourcesStore, FileInfo.lastModified)(
      schemas.toSet
    ) { schemasDiff: ChangeReport[File] =>
      val allGeneratedSourcesExist = generatedSources.forall(_.exists())
      if (schemasDiff.modified.nonEmpty || !allGeneratedSourcesExist) {
        schemas foreach { schema =>
          val cmdGenerate =
            s"$flatcCmd --java -o ${out.getAbsolutePath} $schema"
          cmdGenerate.!! // Note [flatc Error Reporting]
        }

        val projectName = name.value
        log.info(
          s"Flatbuffers code generation generated ${generatedSources.size} " +
          s"files in project $projectName."
        )
      }
    }

    generatedSources.toSeq
  }

  /* Note [flatc Error Reporting]
   * As flatc reports errors to stdout and not stderr, if it fails the output is
   * captured but not accessible (because !! only returns the output if the
   * command succeeded).
   *
   * To avoid complex process handling logic, upon detecting an error,
   * flatc is re-run giving it a chance to print the errors.
   *
   * The errors are printed when gathering source files, so when flatc is re-run
   * afterwards for actual code generation, no new errors should show up, so in
   * this case the error reporting logic is skipped.
   * In case an error did show up, an exception will be thrown (but this could
   * only happen if the file has been edited during compilation).
   */

  /** Runs flatc to ensure that it is properly installed and checks if the
    * reported version is consistent with expectations.
    *
    * @param expectedVersion flatc version that is expected to be installed,
    *                        should be based on project settings
    * @return either an error message explaining what is wrong with the flatc
    *         version or Unit meaning it is correct
    */
  def verifyFlatcVersion(expectedVersion: String): Either[String, Unit] = {
    val cmd = f"$flatcCmd --version"
    try {
      val versionStr         = cmd.!!.trim
      val expectedVersionStr = s"flatc version $expectedVersion"
      if (expectedVersionStr != versionStr) {
        Left(
          s"flatc version mismatch. $expectedVersionStr is expected, " +
          s"but it seems $versionStr is installed"
        )
      } else Right(())
    } catch {
      case _ @(_: RuntimeException | _: IOException) =>
        Left(
          "flatc version check failed. Make sure flatc is in your PATH"
        )
    }
  }

  /** Parses the Make rules returned by flatc to get a list of affected files.
    *
    * The flatc compiler can print Make rules for files it generates
    * (using the -M option).
    * We use this feature to easily get a list of files affected by each schema
    * definition.
    *
    * The printed rules have the following format:
    * ```
    * file1.java \
    * file2.java \
    *   ...
    * fileN.java: \
    * fileA.fbs \
    * fileB.fbs \
    *   ...
    * ```
    * Each flatc run lists all affected files, for files used in multiple
    * schemas that means they may appear more than once. Because of that we make
    * sure to remove any potential duplicates before returning the list of
    * generated sources.
    *
    * @param makeRules a string representing the rules returned by flatc
    * @return either a parse error message or a sequence of filenames that are
    *         mentioned in the provided rules
    */
  private def parseMakeRules(
    makeRules: String
  ): Either[String, Seq[String]] = {
    val cleaned = makeRules.replaceAllLiterally("\\", "")
    cleaned.split(": ") match {
      case Array(javaPart, _) =>
        val filenames = javaPart.split('\n').map(_.trim).filter(_.length > 0)
        Right(filenames)
      case _ =>
        val errorMessage =
          "Unexpected format of Make rules returned by flatc:\n" + makeRules
        Left(errorMessage)
    }
  }

  /** Runs flatc to discover what sources will be generated.
    * flatc will fail if there are errors in the files, as this is the first
    * time it is run on the files, the errors are captured and reported
    * (see Note [flatc Error Reporting]).
    *
    * @param schemas a sequence of files containing schemas for flatc
    * @param out the directory where the generated sources should be placed
    * @return set of files that will be generated by running flatc on provided
    *         schemas
    */
  private def gatherGeneratedSources(
    schemas: Seq[File],
    out: File,
    log: ManagedLogger
  ): Set[File] = {
    val affectedSources =
      schemas.flatMap { schema =>
        val cmdMakeRules =
          s"$flatcCmd -M --java -o ${out.getAbsolutePath} ${schema.getAbsolutePath}"
        val makeRules =
          try {
            cmdMakeRules.!!
          } catch {
            case ex: RuntimeException =>
              val exitCode = cmdMakeRules.! // Note [flatc Error Reporting]
              log.error(
                s"flatc on ${schema.getAbsolutePath} failed with exit code $exitCode"
              )
              throw ex
          }

        parseMakeRules(makeRules) match {
          case Left(errorMessage) =>
            val exceptionMessage =
              s"Cannot parse flatc Make rules, flatc command: $cmdMakeRules"
            log.error(exceptionMessage)
            log.error(errorMessage)
            throw new RuntimeException(exceptionMessage)
          case Right(affectedSources) => affectedSources
        }
      }
    affectedSources.toSet.map(file)
  }
}
