package org.enso.pkg

import java.io.File

import scala.jdk.CollectionConverters._
import org.enso.filesystem.FileSystem

import scala.io.Source
import scala.util.Try

object CouldNotCreateDirectory extends Exception

/**
  * Represents a source file with known qualified name.
  *
  * @param qualifiedName the qualified name of this file
  * @param file the location of this file
  */
case class SourceFile[F](qualifiedName: QualifiedName, file: F)

/**
  * Represents an Enso package stored on the hard drive.
  *
  * @param root the root directory of this package
  * @param config the metadata contained in the package configuration
  *  @param fileSystem the file system access module
  */
case class Package[F](
  root: F,
  config: Config,
  implicit val fileSystem: FileSystem[F]
) {
  import FileSystem.Syntax

  val sourceDir   = root.getChild(Package.sourceDirName)
  val configFile  = root.getChild(Package.configFileName)
  val thumbFile   = root.getChild(Package.thumbFileName)
  val polyglotDir = root.getChild(Package.polyglotExtensionsDirName)

  /**
    * Stores the package metadata on the hard drive. If the package does not exist,
    * creates the required directory structure.
    */
  def save(): Unit = {
    if (!root.exists) createDirectories()
    if (!sourceDir.exists) createSourceDir()
    saveConfig()
  }

  /**
    * Creates the package directory structure.
    */
  def createDirectories(): Unit = {
    val created = Try(root.createDirectories()).map(_ => true).getOrElse(false)
    if (!created) throw CouldNotCreateDirectory
    createSourceDir()
  }

  /**
    * Changes the package name.
    *
    * @param newName the new package name
    * @return The package object with changed name. The old package is not valid anymore.
    */
  def rename(newName: String): Package[F] = {
    val newPkg = copy(config = config.copy(name = newName))
    newPkg.save()
    newPkg
  }

  /**
    * Creates the sources directory and populates it with a dummy Main file.
    */
  def createSourceDir(): Unit = {
    Try(sourceDir.createDirectories()).getOrElse(throw CouldNotCreateDirectory)
    val mainCodeSrc = Source.fromResource(Package.mainFileName)
    val writer      = sourceDir.getChild(Package.mainFileName).newBufferedWriter
    writer.write(mainCodeSrc.mkString)
    writer.close()
    mainCodeSrc.close()
  }

  /**
    * Saves the config metadata into the package configuration file.
    */
  def saveConfig(): Unit = {
    val writer = configFile.newBufferedWriter
    Try(writer.write(config.toYaml))
    writer.close()
  }

  /**
    * Gets the location of the package's Main file.
    *
    * @return the location of the Main file.
    */
  def mainFile: F = {
    sourceDir.getChild(Package.mainFileName)
  }

  /**
    * Checks if the package has a thumbnail file.
    *
    * @return `true` if the thumbnail file exists, `false` otherwise.
    */
  def hasThumb: Boolean = thumbFile.exists

  /**
    * Returns the name of this package.
    * @return the name of this package.
    */
  def name: String = config.name

  /**
    * Parses a file path into a qualified module name belonging to this
    * package.
    *
    * @param file the source file path to translate into a qualified name.
    * @return a qualified name of the input source path.
    */
  def moduleNameForFile(file: F): QualifiedName = {
    val segments                 = sourceDir.relativize(file).getSegments.asScala.toList
    val dirSegments              = segments.take(segments.length - 1)
    val fileNameWithoutExtension = file.getName.takeWhile(_ != '.')
    QualifiedName(name :: dirSegments, fileNameWithoutExtension)
  }

  /**
    * Lists the source files in this package.
    *
    * @return the list of all source files in this package, together with their qualified names.
    */
  def listSources: List[SourceFile[F]] = {
    val sources = sourceDir.walk
      .filter(_.isRegularFile)
      .iterator
      .asScala
      .toList
    sources.map { path => SourceFile(moduleNameForFile(path), path) }
  }

  /**
    * Lists contents of the polyglot extensions directory for a given language.
    *
    * @param languageName the language to list extenstions for
    * @return a list of files and directories contained in the relevant
    *         polyglot extensions directory.
    */
  def listPolyglotExtensions(languageName: String): List[F] = {
    val dir = polyglotDir.getChild(languageName)
    if (!dir.isDirectory) return List()
    dir.list
      .iterator()
      .asScala
      .toList
  }
}

/**
  * A class responsible for creating and parsing package structures.
  * @param fileSystem the file system to use.
  * @tparam F the type of paths used by `fileSystem`.
  */
class PackageManager[F](implicit val fileSystem: FileSystem[F]) {
  import FileSystem.Syntax

  /**
    * Creates a new Package in a given location and with config file.
    *
    * @param root the root location of the package.
    * @param config the config for the new package.
    * @return a package object representing the newly created package.
    */
  def create(
    root: F,
    config: Config
  ): Package[F] = {
    val pkg = Package(root, config, fileSystem)
    pkg.save()
    pkg
  }

  /**
    * Creates a new Package in a given location and with given name. Leaves all the other config fields blank.
    *
    * @param root  the root location of the package.
    * @param name the name for the new package.
    * @param version version of the newly-created package.
    * @return a package object representing the newly created package.
    */
  def create(
    root: F,
    name: String,
    version: String = "0.0.1"
  ): Package[F] = {
    val config = Config(
      name         = normalizeName(name),
      version      = version,
      license      = "",
      author       = List(),
      maintainer   = List(),
      dependencies = List()
    )
    create(root, config)
  }

  /**
    * Tries to parse package structure from a given root location.
    *
    * @param root the root location to get package info from.
    * @return `Some(pkg)` if the location represents a package, `None` otherwise.
    */
  def fromDirectory(root: F): Option[Package[F]] = {
    if (!root.exists) return None
    val configFile = root.getChild(Package.configFileName)
    val reader     = Try(configFile.newBufferedReader)
    val resultStr = reader
      .flatMap(rd => Try(rd.lines().iterator().asScala.mkString("\n")))
      .toOption
    val result = resultStr.flatMap(Config.fromYaml)
    reader.map(_.close())
    result.map(Package(root, _, fileSystem))
  }

  /**
    * Tries to parse package structure from a given root location or creates a new package if it fails.
    *
    * @param root the package root location.
    * @return the package object for the package found or created in the root location.
    */
  def getOrCreate(root: F): Package[F] = {
    val existing = fromDirectory(root)
    existing.getOrElse(create(root, generateName(root)))
  }

  /**
    * Transforms the given string into a valid package name (i.e. a CamelCased identifier).
    *
    * @param name the original name.
    * @return the transformed name conforming to the specification.
    */
  def normalizeName(name: String): String = {
    val startingWithLetter =
      if (name.length == 0 || !name(0).isLetter) "Project" ++ name else name
    val startingWithUppercase = startingWithLetter.capitalize
    val onlyAlphanumeric      = startingWithUppercase.filter(_.isLetterOrDigit)
    onlyAlphanumeric
  }

  /**
    * Generates a name for the package, by normalizing the last segment of its root path.
    *
    * @param file the root location of the package.
    * @return the generated package name.
    */
  def generateName(file: F): String = {
    val dirname = file.getName
    normalizeName(dirname)
  }
}

object PackageManager {
  val Default = new PackageManager[File]()(FileSystem.Default)
}

/**
  * A companion object for static methods on the [[Package]] class.
  */
object Package {
  val fileExtension             = "enso"
  val configFileName            = "package.yaml"
  val sourceDirName             = "src"
  val polyglotExtensionsDirName = "polyglot"
  val mainFileName              = "Main.enso"
  val thumbFileName             = "thumb.png"
}
