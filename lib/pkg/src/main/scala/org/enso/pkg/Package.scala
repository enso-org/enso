package org.enso.pkg

import java.io.File
import java.io.PrintWriter
import java.nio.file.Files

import scala.jdk.CollectionConverters._
import org.apache.commons.io.FileUtils
import org.enso.pkg.Package.qualifiedNameSeparator

import scala.io.Source
import scala.util.Try

object CouldNotCreateDirectory extends Exception

/**
  * Represents a source file with known qualified name.
  *
  * @param qualifiedName the qualified name of this file
  * @param file the location of this file
  */
case class SourceFile(qualifiedName: QualifiedName, file: File)

/**
  * Represents an Enso package stored on the hard drive.
  *
  * @param root the root directory of this package
  * @param config the metadata contained in the package configuration
  */
case class Package(root: File, config: Config) {

  val sourceDir   = new File(root, Package.sourceDirName)
  val configFile  = new File(root, Package.configFileName)
  val thumbFile   = new File(root, Package.thumbFileName)
  val polyglotDir = new File(root, Package.polyglotExtensionsDirName)

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
    val created = Try(root.mkdirs).getOrElse(false)
    if (!created) throw CouldNotCreateDirectory
    createSourceDir()
  }

  /**
    * Changes the package name.
    *
    * @param newName the new package name
    * @return The package object with changed name. The old package is not valid anymore.
    */
  def rename(newName: String): Package = {
    val newPkg = copy(config = config.copy(name = newName))
    newPkg.save()
    newPkg
  }

  /**
    * Deletes the package from hard drive.
    */
  def remove(): Unit = {
    FileUtils.deleteDirectory(root)
  }

  /**
    * Moves the package to a new location.
    *
    * @param newRoot the new root location
    * @return The package object with new location. The old package is not valid anymore.
    */
  def move(newRoot: File): Package = {
    val newPkg = copyPackage(newRoot)
    remove()
    newPkg
  }

  /**
    * Creates a copy of this package.
    *
    * @param newRoot the root location of the copied package.
    * @return the package object for the copied package.
    */
  def copyPackage(newRoot: File): Package = {
    FileUtils.copyDirectory(root, newRoot)
    copy(root = newRoot)
  }

  /**
    * Creates the sources directory and populates it with a dummy Main file.
    */
  def createSourceDir(): Unit = {
    if (!Try(sourceDir.mkdir).getOrElse(false)) throw CouldNotCreateDirectory
    val mainCodeSrc = Source.fromResource(Package.mainFileName)
    val writer      = new PrintWriter(new File(sourceDir, Package.mainFileName))
    writer.write(mainCodeSrc.mkString)
    writer.close()
    mainCodeSrc.close()
  }

  /**
    * Saves the config metadata into the package configuration file.
    */
  def saveConfig(): Unit = {
    val writer = new PrintWriter(configFile)
    Try(writer.write(config.toYaml))
    writer.close()
  }

  /**
    * Gets the location of the package's Main file.
    *
    * @return the location of the Main file.
    */
  def mainFile: File = {
    new File(sourceDir, Package.mainFileName)
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
  def moduleNameForFile(file: File): QualifiedName = {
    val path        = file.toPath
    val sourcesPath = sourceDir.toPath
    val segments    = sourcesPath.relativize(path).iterator().asScala.toList
    val dirSegments = segments.take(segments.length - 1).map(_.toString)
    val fileNameWithoutExtension =
      path.getFileName.toString.takeWhile(_ != '.')
    QualifiedName(name :: dirSegments, fileNameWithoutExtension)
  }

  /**
    * Lists the source files in this package.
    *
    * @return the list of all source files in this package, together with their qualified names.
    */
  def listSources: List[SourceFile] = {
    val sourcesPath = sourceDir.toPath
    val sources = Files
      .walk(sourcesPath)
      .filter(Files.isRegularFile(_))
      .iterator
      .asScala
      .toList
    sources.map { path =>
      SourceFile(moduleNameForFile(path.toFile), path.toFile)
    }
  }

  /**
    * Lists contents of the polyglot extensions directory for a given language.
    *
    * @param languageName the language to list extenstions for
    * @return a list of files and directories contained in the relevant
    *         polyglot extensions directory.
    */
  def listPolyglotExtensions(languageName: String): List[File] = {
    val dir = new File(polyglotDir, languageName)
    if (!dir.isDirectory) return List()
    Files
      .list(dir.toPath)
      .map(_.toFile)
      .iterator()
      .asScala
      .toList
  }
}

/**
  * Represents a qualified name of a source module.
  *
  * @param path the names of the package and directories the module is
  *             contained in
  * @param module the name of the module
  */
case class QualifiedName(path: List[String], module: String) {
  override def toString: String =
    (path :+ module).mkString(qualifiedNameSeparator)

  /**
    * Get the parent of this qualified name.
    *
    * @return the parent of this qualified name.
    */
  def getParent: Option[QualifiedName] =
    path.lastOption.map(QualifiedName(path.init, _))

  /**
    * Create a child qualified name taking this name as a parent.
    *
    * @param name the name of a child node.
    * @return a new qualified name based on this name.
    */
  def createChild(name: String): QualifiedName =
    QualifiedName(path :+ module, name)
}

object QualifiedName {

  /**
    * Parses a dot-separated string representation of a qualified name into
    * a [[QualifiedName]] object.
    *
    * @param qualName the string representation of a qualified name.
    * @return the corresponding [[QualifiedName]] object.
    */
  def fromString(qualName: String): Option[QualifiedName] = {
    val segments = qualName.split(Package.qualifiedNameSeparatorRegex).toList
    if (segments.nonEmpty) {
      Some(QualifiedName(segments.dropRight(1), segments.last))
    } else {
      None
    }
  }

  def simpleName(modName: String): QualifiedName =
    QualifiedName(List(), modName)
}

/**
  * A companion object for static methods on the [[Package]] class.
  */
object Package {
  val fileExtension               = "enso"
  val configFileName              = "package.yaml"
  val sourceDirName               = "src"
  val polyglotExtensionsDirName   = "polyglot"
  val mainFileName                = "Main.enso"
  val thumbFileName               = "thumb.png"
  val qualifiedNameSeparator      = "."
  val qualifiedNameSeparatorRegex = "\\."

  /**
    * Creates a new Package in a given location and with config file.
    *
    * @param root the root location of the package.
    * @param config the config for the new package.
    * @return a package object representing the newly created package.
    */
  def create(root: File, config: Config): Package = {
    val pkg = Package(root, config)
    pkg.save()
    pkg
  }

  /**
    * Creates a new Package in a given location and with given name. Leaves all the other config fields blank.
    *
    * @param root  the root location of the package.
    * @param name the name for the new package.
    * @return a package object representing the newly created package.
    */
  def create(root: File, name: String): Package = {
    val config = Config(
      author     = "",
      maintainer = "",
      name       = normalizeName(name),
      version    = "",
      license    = ""
    )
    create(root, config)
  }

  /**
    * Tries to parse package structure from a given root location.
    *
    * @param root the root location to get package info from.
    * @return `Some(pkg)` if the location represents a package, `None` otherwise.
    */
  def fromDirectory(root: File): Option[Package] = {
    if (!root.exists()) return None
    val configFile = new File(root, configFileName)
    val source     = Try(Source.fromFile(configFile))
    val result     = source.map(_.mkString).toOption.flatMap(Config.fromYaml)
    source.foreach(_.close())
    result.map(Package(root, _))
  }

  /**
    * Tries to parse package structure from a given root location or creates a new package if it fails.
    *
    * @param root the package root location.
    * @return the package object for the package found or created in the root location.
    */
  def getOrCreate(root: File): Package = {
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
  def generateName(file: File): String = {
    val dirname = file.getName
    normalizeName(dirname)
  }
}
