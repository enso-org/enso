package org.enso.pkg

import org.enso.editions.{Editions, LibraryName}
import org.enso.filesystem.FileSystem
import org.enso.pkg.validation.NameValidation

import java.io.{File, InputStream, OutputStream}
import java.net.URI
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal
import scala.util.{Failure, Try, Using}

case class CouldNotCreateDirectory(cause: Throwable) extends RuntimeException {
  override def getMessage: String =
    s"Could not create directory: ${cause.getMessage}. Perhaps there is a permission issue."
  override def toString: String = getMessage
}

/** Represents a source file with known qualified name.
  *
  * @param qualifiedName the qualified name of this file
  * @param file the location of this file
  */
case class SourceFile[F](qualifiedName: QualifiedName, file: F)

/** Represents an Enso package stored on the hard drive.
  *
  * @param root the root directory of this package
  * @param initialConfig the metadata contained in the package configuration
  * @param fileSystem the file system access module
  */
class Package[F](
  val root: F,
  initialConfig: Config,
  implicit val fileSystem: FileSystem[F]
) {
  import FileSystem.Syntax

  val sourceDir: F         = root.getChild(Package.sourceDirName)
  val configFile: F        = root.getChild(Package.configFileName)
  val thumbFile: F         = root.getChild(Package.thumbFileName)
  val polyglotDir: F       = root.getChild(Package.polyglotExtensionsDirName)
  val internalDirectory: F = root.getChild(Package.internalDirName)
  val irCacheDirectory: F = internalDirectory
    .getChild(Package.cacheDirName)
    .getChild(Package.irCacheDirName)
  val bindingsCacheDirectory: F = internalDirectory
    .getChild(Package.cacheDirName)
    .getChild(Package.bindingsCacheDirName)
  val suggestionsCacheDirectory: F = internalDirectory
    .getChild(Package.cacheDirName)
    .getChild(Package.suggestionsCacheDirName)

  private[this] var config: Config = initialConfig
  def getConfig(): Config          = config

  /** Reloads the config from file system */
  def reloadConfig(): Try[Config] = {
    val configFile = root.getChild(Package.configFileName)
    val newConfig  = Using(configFile.newBufferedReader)(Config.fromYaml).flatten
    newConfig.foreach(config = _)
    newConfig
  }

  /** Sets the package name.
    *
    * @param newName the new package name
    * @return a package with the updated name
    */
  def setPackageName(newName: String): Package[F] = {
    new Package(root, config.copy(normalizedName = Some(newName)), fileSystem)
  }

  /** Stores the package metadata on the hard drive. If the package does not exist,
    * creates the required directory structure.
    */
  def save(): Unit = {
    try {
      if (!root.exists) root.createDirectories()
      if (!sourceDir.exists) sourceDir.createDirectories()
    } catch {
      case NonFatal(e) => throw CouldNotCreateDirectory(e)
    }

    saveConfig()
  }

  /** Gets the cache root location within this package for a given Enso version.
    *
    * This will create the location if it does not exist.
    *
    * @param ensoVersion the enso version to get the cache root for
    * @return the cache root location
    */
  def getIrCacheRootForPackage(ensoVersion: String): F = {
    irCacheDirectory.getChild(ensoVersion)
  }

  /** Gets the bindings cache root location within this package for a given Enso
    * version.
    *
    * This will create the location if it does not exist.
    *
    * @param ensoVersion the enso version to get the cache root for
    * @return the cache root location
    */
  def getBindingsCacheRootForPackage(ensoVersion: String): F = {
    bindingsCacheDirectory.getChild(ensoVersion)
  }

  /** Gets the suggestions cache root location within this package for a given
    * Enso version.
    *
    * This will create the location if it does not exist.
    *
    * @param ensoVersion the enso version to get the cache root for
    * @return the cache root location
    */
  def getSuggestionsCacheRootForPackage(ensoVersion: String): F = {
    suggestionsCacheDirectory.getChild(ensoVersion)
  }

  /** Changes the package name.
    *
    * @param newName the new package name
    * @return The package object with changed name. The old package is not
    *         valid anymore.
    */
  def rename(newName: String): Package[F] = updateConfig { config =>
    config.copy(
      name = newName,
      normalizedName =
        config.normalizedName.map(_ => NameValidation.normalizeName(newName))
    )
  }

  /** Updates the package config.
    *
    * The changes are automatically saved to the filesystem.
    *
    * @param update the function that modifies the config
    * @return The package object with changed config. The old package is not
    *         valid anymore.
    */
  def updateConfig(update: Config => Config): Package[F] = {
    val newPkg = new Package(root, update(config), fileSystem)
    newPkg.saveConfig()
    newPkg
  }

  /** Saves the config metadata into the package configuration file.
    */
  private def saveConfig(): Unit =
    Using(configFile.newBufferedWriter) { writer =>
      writer.write(config.toYaml)
    }

  /** Gets the location of the package's Main file.
    *
    * @return the location of the Main file.
    */
  def mainFile: F = {
    sourceDir.getChild(Package.mainFileName)
  }

  /** Checks if the package has a thumbnail file.
    *
    * @return `true` if the thumbnail file exists, `false` otherwise.
    */
  def hasThumb: Boolean = thumbFile.exists

  /** Returns the name of this package.
    * @return the name of this package.
    */
  def name: String = config.name

  /** Returns the normalized name of this package.
    * @return the normalized name of this package.
    */
  def normalizedName: String =
    config.normalizedName.getOrElse(NameValidation.normalizeName(name))

  def namespace: String = config.namespace

  /** A [[LibraryName]] associated with the package. */
  def libraryName: LibraryName = LibraryName(config.namespace, normalizedName)

  /** Parses a file path into a qualified module name belonging to this
    * package.
    *
    * @param file the source file path to translate into a qualified name.
    * @return a qualified name of the input source path.
    */
  def moduleNameForFile(file: F): QualifiedName = {
    val segments                 = sourceDir.relativize(file).getSegments.asScala.toList
    val dirSegments              = segments.take(segments.length - 1)
    val fileNameWithoutExtension = file.getName.takeWhile(_ != '.')
    QualifiedName(
      namespace :: normalizedName :: dirSegments,
      fileNameWithoutExtension
    )
  }

  /** Lists the source files in this package.
    *
    * @return the list of all source files in this package, together with their qualified names.
    */
  def listSources(): List[SourceFile[F]] = {
    listSourcesJava().asScala.toList
  }

  /** Lists the source files in this package.
    *
    * @return the list of all source files in this package, together with their qualified names.
    */
  def listSourcesJava(): java.util.List[SourceFile[F]] = {
    sourceDir.walk
      .filter(f => f.isRegularFile && f.getName.endsWith(".enso"))
      .map(path => SourceFile(moduleNameForFile(path), path))
      .collect(java.util.stream.Collectors.toList[SourceFile[F]])
  }

  /** Lists contents of the polyglot extensions directory for a given language.
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

  override def toString: String = {
    s"Package[$name]"
  }
}

/** A class responsible for creating and parsing package structures.
  * @param fileSystem the file system to use.
  * @tparam F the type of paths used by `fileSystem`.
  */
class PackageManager[F](implicit val fileSystem: FileSystem[F]) {
  import FileSystem.Syntax

  /** Creates a new Package in a given location and with config file.
    *
    * @param root the root location of the package.
    * @param config the config for the new package.
    * @return a package object representing the newly created package.
    */
  def create(
    root: F,
    config: Config,
    template: Template
  ): Package[F] = {
    val pkg = new Package(root, config, fileSystem)
    pkg.save()
    copyResources(pkg, template)
    pkg
  }

  /** Creates a new Package in a given location and with given name. Leaves all the other config fields blank.
    *
    * @param root the root location of the package
    * @param name the name of the new package
    * @param namespace the package namespace
    * @param normalizedName normalized name of the new package
    * @param version version of the newly-created package
    * @param template the template for the new package
    * @param edition the edition to use for the project; if not specified, it
    *                will not specify any, meaning that the current default one
    *                will be used
    * @return a package object representing the newly created package.
    */
  def create(
    root: F,
    name: String,
    namespace: String                        = "local",
    normalizedName: Option[String]           = None,
    version: String                          = "0.0.1",
    template: Template                       = Template.Default,
    edition: Option[Editions.RawEdition]     = None,
    authors: List[Contact]                   = List(),
    maintainers: List[Contact]               = List(),
    license: String                          = "",
    componentGroups: Option[ComponentGroups] = None
  ): Package[F] = {
    val config = Config(
      name                 = name,
      normalizedName       = normalizedName,
      namespace            = namespace,
      version              = version,
      license              = license,
      authors              = authors,
      edition              = edition,
      preferLocalLibraries = true,
      maintainers          = maintainers,
      componentGroups      = componentGroups
    )
    create(root, config, template)
  }

  /** Tries to parse package structure from a given root location.
    *
    * @param root the root location to get package info from.
    * @return `Some(pkg)` if the location represents a package, `None`
    *        otherwise.
    */
  def fromDirectory(root: F): Option[Package[F]] =
    loadPackage(root).toOption

  /** Loads the package structure at the given root location and reports any
    * errors.
    *
    * @param root the root location to get package info from.
    * @return `Success(pkg)` if the location represents a valid package, and
    *        `Failure` otherwise. If the package file or its parent directory do
    *        not exist, a `PackageNotFound` exception is returned. Otherwise,
    *        the exception that made it not possible to load the package is
    *        returned.
    */
  def loadPackage(root: F): Try[Package[F]] = {
    val result =
      if (!root.exists) Failure(PackageManager.PackageNotFound("root"))
      else {
        val configFile = root.getChild(Package.configFileName)
        if (configFile.exists) {
          for {
            result <- Using(configFile.newBufferedReader)(
              Config.fromYaml
            ).flatten
          } yield new Package(root, result, fileSystem)
        } else Failure(PackageManager.PackageNotFound("config"))
      }
    result.recoverWith {
      case packageLoadingException: PackageManager.PackageLoadingException =>
        Failure(packageLoadingException)
      case otherError =>
        Failure(
          PackageManager.PackageLoadingFailure(
            s"Cannot load the package: $otherError",
            otherError
          )
        )
    }
  }

  /** Tries to parse package structure from a given root location or creates a new package if it fails.
    *
    * @param root the package root location.
    * @return the package object for the package found or created in the root location.
    */
  def getOrCreate(root: F): Package[F] = {
    val existing = fromDirectory(root)
    existing.getOrElse(create(root, generateName(root)))
  }

  /** Generates a name for the package, by normalizing the last segment of its root path.
    *
    * @param file the root location of the package.
    * @return the generated package name.
    */
  def generateName(file: F): String = {
    val dirname = file.getName
    NameValidation.normalizeName(dirname)
  }

  /** Copy the template resources to the package.
    *
    * @param template the template to copy the resources from
    * @param pkg the package to copy the resources to
    */
  private def copyResources(pkg: Package[F], template: Template): Unit =
    template match {
      case Template.Default =>
        copyTemplateFiles(pkg, "default", List(Package.mainFileName), List())

      case Template.ColoradoCovid =>
        val srcFiles = List(
          Package.mainFileName,
          "eaep.png",
          "map.png",
          "table1.png",
          "table2.png"
        )
        val dataFiles = List(
          "CDPHE_COVID19_County_Status_Metrics.csv",
          "ColoradoGeoData.db"
        )
        copyTemplateFiles(pkg, "colorado_covid", srcFiles, dataFiles)

      case Template.Kmeans =>
        copyTemplateFiles(pkg, "kmeans", List(Package.mainFileName), List())

      case Template.NasdaqReturns =>
        copyTemplateFiles(
          pkg,
          "nasdaqreturns",
          List(Package.mainFileName),
          List()
        )

      case Template.Orders =>
        val srcFiles = List(
          Package.mainFileName,
          "eaep.png",
          "excel1.png",
          "excel2.png",
          "excel3.png",
          "eyeball_viz.png"
        )
        val dataFiles = List(
          "store_data.xlsx"
        )
        copyTemplateFiles(pkg, "orders", srcFiles, dataFiles)

      case Template.Restaurants =>
        val srcFiles = List(
          Package.mainFileName,
          "eaep.png",
          "map1.png",
          "map2.png",
          "table1.png"
        )
        val dataFiles = List(
          "la_districts.csv",
          "mapcolors.json",
          "restaurants.csv"
        )
        copyTemplateFiles(pkg, "restaurants", srcFiles, dataFiles)

      case Template.Stargazers =>
        copyTemplateFiles(pkg, "stargazers", List(Package.mainFileName), List())

      case Template.MonthlySales =>
        val srcFiles = List(
          Package.mainFileName,
          "eaep.png",
          "excel1.png"
        )
        val dataFiles = List(
          "Sales_Sample_Data.xlsx"
        )
        copyTemplateFiles(pkg, "monthly_sales", srcFiles, dataFiles)

      case Template.BankHolidayRain =>
        val srcFiles = List(
          Package.mainFileName,
          "eaep.png",
          "bankholiday.png"
        )
        copyTemplateFiles(pkg, "bank_holiday_rain", srcFiles, List())

      case Template.GettingStartedReading =>
        val srcFiles = List(
          Package.mainFileName,
          "eags.png",
          "loadfile.gif",
          "sheets.gif",
          "showdata.gif",
          "simpleexpression.gif",
          "table_solution.png",
          "table_viz.png"
        )
        copyTemplateFiles(pkg, "getting_started_reading", srcFiles, List())

      case Template.GettingStartedAggregating =>
        val srcFiles = List(
          Package.mainFileName,
          "answer_table.png",
          "eags.png",
          "set.gif",
          "table1.png"
        )
        val dataFiles = List(
          "sample_bank_data.xlsx"
        )
        copyTemplateFiles(
          pkg,
          "getting_started_aggregating",
          srcFiles,
          dataFiles
        )

      case Template.GettingStartedCleansing =>
        val srcFiles = List(
          Package.mainFileName,
          "eags.png"
        )
        val dataFiles = List(
          "crm_data.csv"
        )
        copyTemplateFiles(pkg, "getting_started_cleansing", srcFiles, dataFiles)

      case Template.GettingStartedSelecting =>
        val srcFiles = List(
          Package.mainFileName,
          "eags.png",
          "table1.png",
          "table2.png"
        )
        val dataFiles = List(
          "crm_data.csv",
          "Customer_Data.xlsx"
        )
        copyTemplateFiles(pkg, "getting_started_selecting", srcFiles, dataFiles)
    }

  private def copyTemplateFiles(
    pkg: Package[F],
    project_name: String,
    srcFiles: List[String],
    dataFiles: List[String]
  ): Unit = {
    srcFiles.foreach { file =>
      val srcPath = new URI(s"/$project_name/src/$file")
      copyResource(
        srcPath,
        pkg.sourceDir.getChild(file)
      )
    }

    if (dataFiles.nonEmpty) {
      pkg.root.getChild("data").createDirectories()
      dataFiles.foreach { file =>
        val dataPath = new URI(s"/$project_name/data/$file")
        copyResource(
          dataPath,
          pkg.root.getChild("data").getChild(file)
        )
      }
    }
  }

  /** Copy the resource to provided resource.
    *
    * @param from the source
    * @param to the destination
    */
  private def copyResource(from: URI, to: F): Unit = {
    val fromStream = getClass.getResourceAsStream(from.toString)
    val toStream   = to.newOutputStream
    if (fromStream != null && toStream != null) {
      try PackageManager.copyStream(fromStream, toStream)
      finally {
        fromStream.close()
        toStream.close()
      }
    }
  }
}

object PackageManager {
  val Default = new PackageManager[File]()(FileSystem.Default)

  /** A general exception indicating that a package cannot be loaded. */
  class PackageLoadingException(message: String, cause: Throwable)
      extends RuntimeException(message, cause) {

    /** @inheritdoc */
    override def toString: String = message
  }

  /** The error indicating that the requested package does not exist. */
  case class PackageNotFound(kind: String)
      extends PackageLoadingException(
        s"The $kind package file does not exist.",
        null
      )

  /** The error indicating that the package exists, but cannot be loaded. */
  case class PackageLoadingFailure(message: String, cause: Throwable)
      extends PackageLoadingException(message, cause)

  /** The error indicating that the project name is invalid. */
  case class InvalidNameException(message: String)
      extends RuntimeException(message)

  private def copyStream(in: InputStream, out: OutputStream): Unit = {
    val buffer = Array.ofDim[Byte](4096)
    var length = in.read(buffer)
    while (length != -1) {
      out.write(buffer, 0, length)
      length = in.read(buffer)
    }
  }

}

/** A companion object for static methods on the [[Package]] class.
  */
object Package {
  val fileExtension             = "enso"
  val configFileName            = "package.yaml"
  val sourceDirName             = "src"
  val polyglotExtensionsDirName = "polyglot"
  val internalDirName           = ".enso"
  val mainFileName              = "Main.enso"
  val thumbFileName             = "thumb.png"
  val cacheDirName              = "cache"
  val irCacheDirName            = "ir"
  val bindingsCacheDirName      = "bindings"
  val suggestionsCacheDirName   = "suggestions"
}
