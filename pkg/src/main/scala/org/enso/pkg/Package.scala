package org.enso.pkg

import java.io.File
import java.io.PrintWriter

import org.apache.commons.io.FileUtils

import scala.io.Source
import scala.util.Try

object CouldNotCreateDirectory extends Exception

case class Package(root: File, config: Config) {

  val sourceDir = new File(root, Package.sourceDirName)
  val configFile = new File(root, Package.configFileName)
  val thumbFile = new File(root, Package.thumbFileName)

  def save(): Unit = {
    if (!root.exists) createDirectories()
    if (!sourceDir.exists) createSourceDir()
    saveConfig()
  }

  def createDirectories() {
    val created = Try(root.mkdirs).getOrElse(false)
    if (!created) throw CouldNotCreateDirectory
    createSourceDir()
  }

  def rename(newName: String): Package = {
    val newPkg = copy(config = config.copy(name = newName))
    newPkg.save()
    newPkg
  }

  def remove(): Unit = {
    FileUtils.deleteDirectory(root)
  }

  def move(newRoot: File): Package = {
    val newPkg = copyPackage(newRoot)
    remove()
    newPkg
  }

  def copyPackage(newRoot: File): Package = {
    FileUtils.copyDirectory(root, newRoot)
    copy(root = newRoot)
  }

  def createSourceDir(): Unit = {
    if (!Try(sourceDir.mkdir).getOrElse(false)) throw CouldNotCreateDirectory
    val lunaCodeSrc = Source.fromResource(Package.mainFileName)
    val writer = new PrintWriter(new File(sourceDir, Package.mainFileName))
    writer.write(lunaCodeSrc.mkString)
    writer.close()
    lunaCodeSrc.close()
  }

  def saveConfig(): Unit = {
    val writer = new PrintWriter(configFile)
    Try(writer.write(config.toYaml))
    writer.close()
  }

  def hasThumb: Boolean = thumbFile.exists
  def name: String = config.name
}

object Package {
  val configFileName = "package.yaml"
  val sourceDirName = "src"
  val mainFileName = "Main.luna"
  val thumbFileName = "thumb.png"

  def create(root: File, config: Config): Package = {
    val pkg = Package(root, config)
    pkg.save()
    pkg
  }

  def create(root: File, name: String): Package = {
    val config = Config(
      author = "",
      maintainer = "",
      name = name,
      version = "",
      license = ""
    )
    create(root, config)
  }

  def fromDirectory(root: File): Option[Package] = {
    if (!root.exists()) return None
    val configFile = new File(root, configFileName)
    val source = Try(Source.fromFile(configFile))
    val result = source.map(_.mkString).toOption.flatMap(Config.fromYaml)
    source.foreach(_.close())
    result.map(Package(root, _))
  }

  def getOrCreate(root: File): Package = {
    val existing = fromDirectory(root)
    existing.getOrElse(create(root, generateName(root)))
  }

  def generateName(file: File): String = {
    val dirname = file.getName
    val startingWithLetter =
      if (!dirname(0).isLetter) "Project" ++ dirname else dirname
    val startingWithUppercase = startingWithLetter.capitalize
    val onlyAlphanumeric = startingWithUppercase.filter(_.isLetterOrDigit)
    onlyAlphanumeric
  }
}
