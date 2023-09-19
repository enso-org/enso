package org.enso.interpreter.runtime

import org.enso.compiler.PackageRepository
import org.enso.compiler.ImportExportCache
import org.enso.compiler.SerializationManager
import com.oracle.truffle.api.TruffleFile
import com.typesafe.scalalogging.Logger
import org.apache.commons.lang3.StringUtils
import org.enso.editions.LibraryVersion
import org.enso.interpreter.runtime.util.TruffleFileSystem
import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.librarymanager.resolved.LibraryRoot
import org.enso.librarymanager.ResolvingLibraryProvider
import org.enso.logger.masking.MaskedPath
import org.enso.pkg.{
  Component,
  ComponentGroup,
  ExtendedComponentGroup,
  PackageManager,
  QualifiedName,
  SourceFile
}
import org.enso.text.buffer.Rope
import org.enso.polyglot.CompilationStage

import java.nio.file.Path
import scala.collection.immutable.ListSet
import scala.jdk.CollectionConverters.{IterableHasAsJava, SeqHasAsJava}
import scala.util.{Failure, Try, Using}
import org.enso.distribution.locking.ResourceManager
import org.enso.distribution.{DistributionManager, LanguageHome}
import org.enso.editions.updater.EditionManager
import org.enso.editions.{DefaultEdition, Editions, LibraryName}
import org.enso.interpreter.instrument.NotificationHandler
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.librarymanager.DefaultLibraryProvider
import org.enso.pkg.{ComponentGroups, Package}

/** The default [[PackageRepository]] implementation.
  *
  * @param libraryProvider     the [[ResolvingLibraryProvider]] which resolves
  *                            which library version should be imported and
  *                            locates them (or downloads if they are missing)
  * @param context             the language context
  * @param builtins            the builtins module
  * @param notificationHandler a notification handler
  */
private class DefaultPackageRepository(
  libraryProvider: ResolvingLibraryProvider,
  context: EnsoContext,
  builtins: Builtins,
  notificationHandler: NotificationHandler
) extends PackageRepository {

  private val logger = Logger[DefaultPackageRepository]

  implicit private val fs: TruffleFileSystem               = new TruffleFileSystem
  private val packageManager                               = new PackageManager[TruffleFile]
  private var projectPackage: Option[Package[TruffleFile]] = None

  /** The mapping containing all loaded packages.
    *
    * It should be modified only from within synchronized sections, but it may
    * be always read. Thus elements should be added to this mapping only after
    * all library loading bookkeeping has been finished - so that if other,
    * unsynchronized threads read this map, every element it contains is
    * already fully processed.
    */
  private val loadedPackages
    : collection.mutable.Map[LibraryName, Option[Package[TruffleFile]]] = {
    val builtinsName = LibraryName(Builtins.NAMESPACE, Builtins.PACKAGE_NAME)
    collection.mutable.LinkedHashMap(builtinsName -> None)
  }

  /** The mapping containing loaded modules.
    *
    * We use [[String]] as the key as we often index into this map based on
    * qualified names that come from interop (via
    * [[org.enso.interpreter.runtime.scope.TopLevelScope]]). These arrive as
    * Strings, and constantly converting them into [[QualifiedName]]s would
    * add more overhead than is probably necessary.
    */
  private val loadedModules: collection.concurrent.Map[String, Module] =
    collection.concurrent.TrieMap(Builtins.MODULE_NAME -> builtins.getModule)

  /** The mapping containing loaded component groups.
    *
    * It should be modified and read only from within synchronized sections.
    * The component mapping is added to the collection after ensuring that the
    * corresponding library was loaded.
    */
  private val loadedComponents
    : collection.mutable.Map[LibraryName, ComponentGroups] = {
    val builtinsName = LibraryName(Builtins.NAMESPACE, Builtins.PACKAGE_NAME)
    collection.mutable.LinkedHashMap(builtinsName -> ComponentGroups.empty)
  }

  /** The mapping between the library and its cached bindings, if already laoded. */
  private val loadedLibraryBindings: collection.mutable.Map[
    LibraryName,
    ImportExportCache.CachedBindings
  ] =
    collection.mutable.LinkedHashMap()

  private def getComponentModules: ListSet[Module] = {
    val modules = for {
      componentGroups <- loadedComponents.values
      newComponents      = componentGroups.newGroups.flatMap(_.exports)
      extendedComponents = componentGroups.extendedGroups.flatMap(_.exports)
      component <- newComponents ++ extendedComponents
      module    <- findComponentModule(component)
    } yield module
    modules.to(ListSet)
  }

  private def findComponentModule(component: Component): Option[Module] = {
    def mkModuleName(path: Array[String]): String =
      path.mkString(LibraryName.separator.toString)
    @scala.annotation.tailrec
    def go(path: Array[String]): Option[Module] =
      if (path.isEmpty) None
      else {
        loadedModules.get(mkModuleName(path)) match {
          case Some(module) => Some(module)
          case None         => go(path.init)
        }
      }

    go(component.name.split(LibraryName.separator))
  }

  /** @inheritdoc */
  override def getModuleMap: PackageRepository.ModuleMap = loadedModules

  /** @inheritdoc */
  override def freezeModuleMap: PackageRepository.FrozenModuleMap =
    loadedModules.toMap

  /** @inheritdoc */
  override def getComponents: PackageRepository.ComponentsMap =
    this.synchronized {
      loadedComponents.toMap
    }

  /** @inheritdoc */
  override def getPendingModules: ListSet[Module] =
    this.synchronized {
      for {
        module <- getComponentModules
        isCompiled =
          module.getCompilationStage.isAtLeast(
            CompilationStage.AFTER_CODEGEN
          )
        if !isCompiled
      } yield module
    }

  /** @inheritdoc */
  override def registerMainProjectPackage(
    libraryName: LibraryName,
    pkg: Package[TruffleFile]
  ): Unit = {
    projectPackage = Some(pkg)
    registerPackageInternal(
      libraryName    = libraryName,
      pkg            = pkg,
      libraryVersion = LibraryVersion.Local,
      isLibrary      = false
    )
  }

  /** @inheritdoc */
  override def getMainProjectPackage: Option[Package[TruffleFile]] = {
    projectPackage
  }

  private def registerPackageInternal(
    libraryName: LibraryName,
    libraryVersion: LibraryVersion,
    pkg: Package[TruffleFile],
    isLibrary: Boolean
  ): Unit = {
    val extensions = pkg.listPolyglotExtensions("java")
    extensions.foreach(context.addToClassPath)

    val (regularModules, syntheticModulesMetadata) = pkg
      .listSources()
      .map(srcFile =>
        (
          new Module(srcFile.qualifiedName, pkg, srcFile.file),
          inferSyntheticModules(srcFile)
        )
      )
      .unzip

    regularModules.foreach(registerModule)

    syntheticModulesMetadata.flatten
      .groupMap(_._1)(v => (v._2, v._3))
      .foreach { case (qName, modulesWithSources) =>
        val source = modulesWithSources
          .map(_._2)
          .foldLeft("")(_ ++ "\n" ++ _)
        registerSyntheticModule(
          Module.synthetic(
            qName,
            pkg,
            Rope(source)
          ),
          modulesWithSources.map(_._1)
        )
      }

    if (isLibrary) {
      val root = Path.of(pkg.root.toString)
      notificationHandler.addedLibrary(libraryName, libraryVersion, root)
    }

    loadedPackages.put(libraryName, Some(pkg))
  }

  /** For any given source file, infer data necessary to generate synthetic modules as well as their contents.
    * E.g., for A/B/C.enso it infers modules
    * - A.B that exports A.B.C
    * - A that exports A.B
    *
    * @param srcFile Enso source file to consider
    * @return a list of triples representing the name of submodule along the path, what submodule it exports and its contents
    */
  private def inferSyntheticModules(
    srcFile: SourceFile[TruffleFile]
  ): List[(QualifiedName, QualifiedName, String)] = {
    def listAllIntermediateModules(
      namespace: String,
      name: String,
      elements: List[String],
      exportItem: String
    ): List[(QualifiedName, QualifiedName, String)] = {
      elements match {
        case Nil =>
          Nil
        case lastModuleName :: parts =>
          val pathElems = elements.reverse
          val modName =
            s"${namespace}.$name.${pathElems.mkString(".")}.$exportItem"
          val modSource =
            s"""|import $modName
                |export $modName
                |""".stripMargin
          val syntheticModuleInfo = (
            QualifiedName(namespace :: name :: parts.reverse, lastModuleName),
            QualifiedName(namespace :: name :: pathElems, exportItem),
            modSource
          )
          syntheticModuleInfo :: listAllIntermediateModules(
            namespace,
            name,
            parts,
            lastModuleName
          )
      }
    }
    srcFile.qualifiedName.path match {
      case namespace :: name :: rest =>
        listAllIntermediateModules(
          namespace,
          name,
          rest.reverse,
          srcFile.qualifiedName.item
        )
      case _ =>
        Nil
    }
  }

  /** This package modifies the [[loadedPackages]], so it should be only
    * called from within synchronized sections.
    */
  private def loadPackage(
    libraryName: LibraryName,
    libraryVersion: LibraryVersion,
    root: LibraryRoot
  ): Either[PackageRepository.Error, Package[TruffleFile]] = Try {
    logger.debug(
      s"Loading library $libraryName from " +
      s"[${MaskedPath(root.location).applyMasking()}]."
    )
    val rootFile = context.getEnvironment.getInternalTruffleFile(
      root.location.toAbsolutePath.normalize.toString
    )
    val pkg = packageManager.loadPackage(rootFile).get
    registerPackageInternal(
      libraryName    = libraryName,
      libraryVersion = libraryVersion,
      pkg            = pkg,
      isLibrary      = true
    )
    pkg
  }.toEither.left.map { error =>
    PackageRepository.Error.PackageLoadingError(error.getMessage)
  }

  /** @inheritdoc */
  override def initialize(): Either[PackageRepository.Error, Unit] =
    this.synchronized {
      val unprocessedPackages =
        loadedPackages.keySet
          .diff(loadedComponents.keySet)
          .flatMap(loadedPackages(_))
      unprocessedPackages.foldLeft[Either[PackageRepository.Error, Unit]](
        Right(())
      ) { (accumulator, pkg) =>
        for {
          _ <- accumulator
          _ <- resolveComponentGroups(pkg)
        } yield ()
      }
    }

  private def resolveComponentGroups(
    pkg: Package[TruffleFile]
  ): Either[PackageRepository.Error, Unit] =
    if (loadedComponents.contains(pkg.libraryName)) Right(())
    else {
      pkg.getConfig().componentGroups match {
        case Left(err) =>
          Left(PackageRepository.Error.PackageLoadingError(err.getMessage()))
        case Right(componentGroups) =>
          logger.debug(
            s"Resolving component groups of package [${pkg.normalizedName}]."
          )

          registerComponentGroups(pkg.libraryName, componentGroups.newGroups)
          componentGroups.extendedGroups
            .foldLeft[Either[PackageRepository.Error, Unit]](Right(())) {
              (accumulator, componentGroup) =>
                for {
                  _ <- accumulator
                  extendedLibraryName = componentGroup.group.libraryName
                  _ <- ensurePackageIsLoaded(extendedLibraryName)
                  pkgOpt = loadedPackages(extendedLibraryName)
                  _ <- pkgOpt.fold[Either[PackageRepository.Error, Unit]](
                    Right(())
                  )(
                    resolveComponentGroups
                  )
                  _ = registerExtendedComponentGroup(
                    pkg.libraryName,
                    componentGroup
                  )
                } yield ()
            }
      }
    }

  /** Register the list of component groups defined by a library.
    *
    * @param library the library name
    * @param newGroups the list of component groups that the library defines
    */
  private def registerComponentGroups(
    library: LibraryName,
    newGroups: List[ComponentGroup]
  ): Unit =
    loadedComponents.updateWith(library) {
      case Some(groups) =>
        Some(groups.copy(newGroups = groups.newGroups ::: newGroups))
      case None =>
        Some(ComponentGroups(newGroups, List()))
    }

  /** Register a component group extended by a library.
    *
    * @param library the library name
    * @param group the extended component group
    */
  private def registerExtendedComponentGroup(
    library: LibraryName,
    group: ExtendedComponentGroup
  ): Unit =
    loadedComponents.updateWith(library) {
      case Some(groups) =>
        Some(groups.copy(extendedGroups = groups.extendedGroups :+ group))
      case None =>
        Some(ComponentGroups(List(), List(group)))
    }

  /** @inheritdoc */
  override def ensurePackageIsLoaded(
    libraryName: LibraryName
  ): Either[PackageRepository.Error, Unit] =
    if (loadedPackages.contains(libraryName)) Right(())
    else {
      logger.trace(s"Resolving library $libraryName.")
      val resolvedLibrary =
        libraryProvider
          .findLibrary(libraryName)
          .left
          .map {
            case ResolvingLibraryProvider.Error.NotResolved(details) =>
              PackageRepository.Error.PackageCouldNotBeResolved(details)
            case ResolvingLibraryProvider.Error.DownloadFailed(_, reason) =>
              PackageRepository.Error.PackageDownloadFailed(reason)
            case ResolvingLibraryProvider.Error.RequestedLocalLibraryDoesNotExist =>
              PackageRepository.Error.PackageLoadingError(
                "The local library has not been found on the local " +
                "libraries search paths."
              )
          }
      resolvedLibrary match {
        case Left(error) =>
          logger.warn(s"Resolution failed with [$error].", error)
        case Right(resolved) =>
          logger.info(
            s"Found library ${resolved.name} @ ${resolved.version} " +
            s"at [${MaskedPath(resolved.root.location).applyMasking()}]."
          )
      }

      this.synchronized {
        // We check again inside of the monitor, in case that some other
        // thread has just added this library.
        if (loadedPackages.contains(libraryName)) Right(())
        else
          resolvedLibrary
            .flatMap { library =>
              loadPackage(library.name, library.version, library.root)
            }
            .flatMap(resolveComponentGroups)
      }
    }

  /** @inheritdoc */
  def isPackageLoaded(libraryName: LibraryName): Boolean = {
    loadedPackages.keySet.contains(libraryName)
  }

  /** @inheritdoc */
  override def getLoadedModules: Seq[Module] =
    loadedModules.values.toSeq

  /** @inheritdoc */
  override def getLoadedPackages: Seq[Package[TruffleFile]] =
    loadedPackages.values.toSeq.flatten

  override def getLoadedPackagesJava: java.lang.Iterable[Package[TruffleFile]] =
    loadedPackages.flatMap(_._2).asJava

  /** @inheritdoc */
  override def getLoadedModule(qualifiedName: String): Option[Module] =
    loadedModules.get(qualifiedName)

  /** @inheritdoc */
  override def registerModuleCreatedInRuntime(module: Module): Unit =
    registerModule(module)

  private def registerModule(module: Module): Unit = {
    loadedModules.put(module.getName.toString, module)
  }

  override def registerSyntheticPackage(
    namespace: String,
    name: String
  ): Unit =
    loadedPackages.put(LibraryName(namespace, name), None)

  /** Registering synthetic module, unlike the non-compiler generated one, is conditional
    * in a sense that if a module already exists with a given name we only update its
    * list of synthetic modules that it should export.
    * If no module exists under the given name, we register the synthetic one.
    *
    * @param syntheticModule a synthetic module to register
    * @param refs list of names of modules that should be exported by the module under the given name
    */
  private def registerSyntheticModule(
    syntheticModule: Module,
    refs: List[QualifiedName]
  ): Unit = {
    assert(syntheticModule.isSynthetic)
    if (!loadedModules.contains(syntheticModule.getName.toString)) {
      loadedModules.put(syntheticModule.getName.toString, syntheticModule)
    } else {
      val loaded = loadedModules(syntheticModule.getName.toString)
      assert(!loaded.isSynthetic)
      loaded.setDirectModulesRefs(refs.asJava)
    }
  }

  override def deregisterModule(qualifiedName: String): Unit =
    loadedModules.remove(qualifiedName)

  /** @inheritdoc */
  override def renameProject(
    namespace: String,
    oldName: String,
    newName: String
  ): Unit = this.synchronized {
    renamePackages(namespace, oldName, newName)
    renameModules(namespace, oldName, newName)
  }

  private def renamePackages(
    namespace: String,
    oldName: String,
    newName: String
  ): Unit = {
    val toChange = loadedPackages.toSeq.filter { case (name, _) =>
      name.namespace == namespace && name.name == oldName
    }

    for ((key, _) <- toChange) {
      loadedPackages.remove(key)
    }

    for ((key, pkgOption) <- toChange) {
      val newPkg = pkgOption.map(_.setPackageName(newName))
      val newKey = key.copy(name = newName)
      loadedPackages.put(newKey, newPkg)
    }
  }

  private def renameModules(
    namespace: String,
    oldName: String,
    newName: String
  ): Unit = {
    val separator: String = QualifiedName.separator
    val keys = loadedModules.keySet.filter(name =>
      name.startsWith(namespace + separator + oldName + separator)
    )

    for {
      key    <- keys
      module <- loadedModules.remove(key)
    } {
      module.renameProject(newName)
      loadedModules.put(module.getName.toString, module)
    }
  }

  override def isNamespaceRegistered(namespace: String): Boolean =
    loadedPackages.keySet.exists(_.namespace == namespace)

  override def getPackageForLibrary(
    libraryName: LibraryName
  ): Option[Package[TruffleFile]] =
    loadedPackages.get(libraryName).flatten

  override def getModulesForLibrary(libraryName: LibraryName): List[Module] =
    getPackageForLibrary(libraryName)
      .map(pkg => loadedModules.values.filter(_.getPackage == pkg).toList)
      .getOrElse(Nil)

  override def getLibraryBindings(
    libraryName: LibraryName,
    serializationManager: SerializationManager
  ): Option[ImportExportCache.CachedBindings] = {
    ensurePackageIsLoaded(libraryName).toOption.flatMap { _ =>
      if (!loadedLibraryBindings.contains(libraryName)) {
        loadedPackages.get(libraryName).flatten.foreach(loadDependencies(_))
        serializationManager
          .deserializeLibraryBindings(libraryName)
          .foreach(cache => loadedLibraryBindings.addOne((libraryName, cache)))
      }
      loadedLibraryBindings.get(libraryName)
    }
  }

  private def loadDependencies(pkg: Package[TruffleFile]): Unit = {
    val manifestFile = fs.getChild(pkg.root, LibraryManifest.filename)
    readManifest(manifestFile)
      .flatMap(LibraryManifest.fromYaml(_))
      .foreach(
        _.dependencies.foreach(ensurePackageIsLoaded)
      )
  }

  private def readManifest(file: TruffleFile): Try[String] = {
    if (file.exists())
      Using(file.newBufferedReader) { reader =>
        StringUtils.join(reader.lines().iterator(), "\n")
      }
    else Failure(PackageManager.PackageNotFound())
  }
}

private object DefaultPackageRepository {

  /** Creates a [[PackageRepository]] for the run.
    *
    * It tries to load and resolve the edition used in the project (or the
    * default edition), so that any libraries to be loaded can be resolved using
    * that edition.
    *
    * Edition and library search paths are based on the distribution and
    * language home (if it is provided).
    *
    * @param projectPackage      the package of the current project (if ran inside of a project)
    * @param languageHome        the language home (if set)
    * @param distributionManager the distribution manager
    * @param resourceManager     the resource manager instance
    * @param context             the context reference, needed to add polyglot libraries to
    *                            the classpath
    * @param builtins            the builtins that are always preloaded
    * @param notificationHandler a handler for library addition and progress
    *                            notifications
    * @return an initialized [[PackageRepository]]
    */
  def initializeRepository(
    projectPackage: Option[Package[TruffleFile]],
    languageHome: Option[String],
    editionOverride: Option[String],
    distributionManager: DistributionManager,
    resourceManager: ResourceManager,
    context: EnsoContext,
    builtins: Builtins,
    notificationHandler: NotificationHandler
  ): PackageRepository = {
    val rawEdition = editionOverride
      .map(v => Editions.Raw.Edition(parent = Some(v)))
      .orElse(
        projectPackage
          .flatMap(_.getConfig().edition)
      )
      .getOrElse(DefaultEdition.getDefaultEdition)

    val homeManager    = languageHome.map { home => LanguageHome(Path.of(home)) }
    val editionManager = EditionManager(distributionManager, homeManager)
    val edition        = editionManager.resolveEdition(rawEdition).get

    val projectRoot = projectPackage.map { pkg =>
      val root = pkg.root
      Path.of(root.getAbsoluteFile.toUri)
    }

    val resolvingLibraryProvider =
      DefaultLibraryProvider.make(
        distributionManager = distributionManager,
        resourceManager     = resourceManager,
        lockUserInterface   = notificationHandler,
        progressReporter    = notificationHandler,
        languageHome        = homeManager,
        edition             = edition,
        preferLocalLibraries =
          projectPackage.exists(_.getConfig().preferLocalLibraries),
        projectRoot = projectRoot
      )
    new DefaultPackageRepository(
      resolvingLibraryProvider,
      context,
      builtins,
      notificationHandler
    )
  }
}
