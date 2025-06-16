package org.enso.compiler.test.mock;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import org.enso.common.CompilationStage;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.context.CompilerContext.Module;
import org.enso.editions.LibraryName;
import org.enso.pkg.ComponentGroups;
import org.enso.pkg.Package;
import org.enso.pkg.PackageManager;
import org.enso.pkg.QualifiedName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.collection.immutable.List;
import scala.collection.immutable.ListSet;
import scala.collection.immutable.ListSet$;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;
import scala.runtime.BoxedUnit;
import scala.util.Either;
import scala.util.Left;
import scala.util.Right;

/**
 * Implementation of {@link PackageRepository} with {@link Path} as its type member ({@code
 * PackageRepository#TFile}).
 *
 * <p>Currently, it is located in test sources, but ultimately, we would like to use it as an
 * alternative to {@code DefaultPackageRepository} which depends on Truffle file system.
 *
 * <p>All the {@link Path} objects passed to or returned from this class are assumed to be created
 * by the {@link VirtualFileSystem}.
 *
 * @see #createModule(QualifiedName, String)
 */
final class MockPackageRepository implements PackageRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(MockPackageRepository.class);
  private final VirtualFileSystem vfs;
  private final Path vfsRoot;
  private final PackageManager<Path> pkgManager;
  private final Map<LibraryName, Package<Path>> loadedPackages = new HashMap<>();
  private final Map<String, CompilerContext.Module> loadedModules = new HashMap<>();
  private Package<Path> mainProjectPkg;

  private MockPackageRepository() {
    this.vfs = VirtualFileSystem.create();
    this.vfsRoot = vfs.getRoot();
    this.pkgManager = new PackageManager<>(vfs);
  }

  static MockPackageRepository create() {
    return new MockPackageRepository();
  }

  String listAllFilesInVfs() {
    try {
      return vfs.listAllFiles();
    } catch (IOException e) {
      LOGGER.error("Failed to list files in VFS", e);
      return "Failed to list files in VFS";
    }
  }

  VirtualFileSystem getVfs() {
    return vfs;
  }

  private Package<Path> createEmptyPackage(LibraryName pkgName, Path pkgRoot) throws IOException {
    var pkg = pkgManager.getOrCreate(pkgRoot);
    // Delete all the automatically created sources, and replace them with
    // our custom sources
    for (var src : pkg.listSourcesJava()) {
      Files.delete(src.file());
    }
    loadedPackages.put(pkgName, pkg);
    return pkg;
  }

  /**
   * Creates a module. If the package of the module does not exist, it is created. Note that the
   * package name is derived from the first two items of the qualified name of the module.
   *
   * <p>If the module with the given name already exists, an {@link IllegalArgumentException} is
   * thrown.
   *
   * @param modName Qualified name of the module. If the name is longer than 3, for example {@code
   *     local.Proj.A.B.C}, all intermediate synthetic parent modules will be created. For example
   *     for {@code local.Proj.A.B.C}, synthetic parent modules for {@code local.Proj.A} and {@code
   *     local.Proj.A.B} will be created.
   * @param content Content for the module. Can be empty, but not null.
   * @return Created module. Not null
   */
  Module createModule(QualifiedName modName, String content) {
    assert !modName.isSimple();
    var modPath = modName.pathAsJava();
    LibraryName pkgName = LibraryName.apply(modPath.get(0), modPath.get(1));
    try {
      var pkgDir = vfsRoot.resolve(modPath.get(0)).resolve(modPath.get(1));
      Package<Path> pkg;
      if (!Files.exists(pkgDir)) {
        pkg = createEmptyPackage(pkgName, pkgDir);
      } else {
        pkg = pkgManager.getOrCreate(pkgDir);
      }
      var srcDir = pkgDir.resolve("src");
      if (!Files.exists(srcDir)) {
        Files.createDirectories(srcDir);
      }
      var modPathInPkg = modPath.stream().skip(2).toList();
      MockModule parentSyntheticModule = null;
      if (!modPathInPkg.isEmpty()) {
        parentSyntheticModule = createSyntheticModules(modPathInPkg, pkg);
      }
      var srcPath = modPath.stream().skip(2).collect(Collectors.joining("/"));
      var subSrcDir = srcDir.resolve(srcPath);
      assert Files.exists(subSrcDir)
          : String.format(
              "Subdirectory %s does not exist in package %s. It should have been "
                  + "created by createSyntheticModules()",
              subSrcDir, pkg.libraryName());
      var srcFile = subSrcDir.resolve(modName.item() + ".enso");
      if (Files.exists(srcFile)) {
        throw new IllegalArgumentException("Module '" + modName + "' already exists");
      }
      VirtualFileSystem.write(srcFile, content);
      var modAbsPath = vfs.getAbsolutePath(srcFile);
      boolean isSynthetic = false;
      var module = new MockModule(pkg, modName, modAbsPath, content, this, isSynthetic);
      if (parentSyntheticModule != null) {
        parentSyntheticModule.addSubmodule(modName);
      }
      loadedModules.put(modName.toString(), module);
      return module;
    } catch (IOException e) {
      LOGGER.error("Failed to create module " + modName, e);
      throw new IllegalStateException(e);
    }
  }

  /**
   * @param modPathInPkg Module path inside package, e.g., for module name {@code local.Proj.A.B.C},
   *     it would be {@code [A, B, C]}.
   * @param pkg Package for which synthetic modules are created.
   * @return Returns the last synthetic module. For example, when creating synthetic modules for
   *     {@code local.Proj.A.B.C}, it will return synthetic module for {@code local.Proj.A.B}.
   */
  private MockModule createSyntheticModules(java.util.List<String> modPathInPkg, Package<Path> pkg)
      throws IOException {
    assert !modPathInPkg.isEmpty();
    var curDir = pkg.sourceDir();
    var curName = QualifiedName.fromString(pkg.libraryName().toString());
    MockModule parentModule = null;
    for (var pathItem : modPathInPkg) {
      curDir = curDir.resolve(pathItem);
      curName = curName.createChild(pathItem);
      if (!Files.exists(curDir)) {
        Files.createDirectory(curDir);
        var absPath = vfs.getAbsolutePath(curDir);
        var emptySyntheticModule = createEmptySyntheticModule(curName, pkg, absPath);
        if (parentModule != null) {
          parentModule.addSubmodule(curName);
        }
        parentModule = emptySyntheticModule;
        loadedModules.put(curName.toString(), emptySyntheticModule);
      }
    }
    assert parentModule != null;
    return parentModule;
  }

  private MockModule createEmptySyntheticModule(
      QualifiedName modName, Package<Path> pkg, String absolutePath) {
    return new MockModule(pkg, modName, absolutePath, "", this, true);
  }

  @Override
  public Either<Error, BoxedUnit> initialize() {
    return new Right<>(null);
  }

  @Override
  public Either<Error, BoxedUnit> ensurePackageIsLoaded(LibraryName libraryName) {
    if (loadedPackages.containsKey(libraryName)) {
      return Right.apply(BoxedUnit.UNIT);
    } else {
      return Left.apply(
          PackageRepository.packageLoadingError("Library " + libraryName + " was not loaded"));
    }
  }

  @Override
  public boolean isPackageLoaded(LibraryName libraryName) {
    var ret = loadedPackages.containsKey(libraryName);
    LOGGER.trace("isPackageLoaded({}) = {}", libraryName, ret);
    return ret;
  }

  @Override
  public Seq<org.enso.pkg.Package<Object>> getLoadedPackages() {
    var pkgs = loadedPackages.values().stream().map(MockPackageRepository::castVirtualPkg).toList();
    return CollectionConverters.asScala(pkgs).toSeq();
  }

  @Override
  public Seq<CompilerContext.Module> getLoadedModules() {
    var modules = loadedModules.values().stream().toList();
    return CollectionConverters.asScala(modules).toSeq();
  }

  @Override
  public scala.collection.concurrent.Map<String, Module> getModuleMap() {
    var map = new scala.collection.concurrent.TrieMap<String, Module>();
    for (var entry : loadedModules.entrySet()) {
      map.put(entry.getKey(), entry.getValue());
    }
    return map;
  }

  @Override
  public scala.collection.immutable.Map<String, Module> freezeModuleMap() {
    var map = new scala.collection.immutable.HashMap<String, Module>();
    for (var entry : loadedModules.entrySet()) {
      map = map.updated(entry.getKey(), entry.getValue());
    }
    return map;
  }

  @Override
  public scala.collection.immutable.Map<LibraryName, ComponentGroups> getComponents() {
    throw new UnsupportedOperationException();
  }

  @SuppressWarnings("unchecked")
  @Override
  public ListSet<Module> getPendingModules() {
    var notCompiledModules =
        loadedModules.values().stream()
            .filter(mod -> !mod.getCompilationStage().isAtLeast(CompilationStage.AFTER_CODEGEN))
            .toList();
    return (ListSet)
        ListSet$.MODULE$.apply(CollectionConverters.asScala(notCompiledModules).toSeq());
  }

  @Override
  public Option<Module> getLoadedModule(String qualifiedName) {
    return Option.apply(loadedModules.get(qualifiedName));
  }

  @Override
  public void registerMainProjectPackage(
      LibraryName libraryName, org.enso.pkg.Package<Object> pkg) {
    var virtualPkg = castObjectPkg(pkg);
    loadedPackages.put(libraryName, virtualPkg);
    for (var src : virtualPkg.listSourcesJava()) {
      var modName = src.qualifiedName();
      var srcPath = vfs.getAbsolutePath(src.file());
      var srcContent = readFile(src.file());
      var isSynthetic = false;
      var mod = new MockModule(virtualPkg, modName, srcPath, srcContent, this, isSynthetic);
      loadedModules.put(modName.toString(), mod);
    }
    mainProjectPkg = castObjectPkg(pkg);
  }

  @Override
  public Option<org.enso.pkg.Package<Object>> getMainProjectPackage() {
    return Option.apply(castVirtualPkg(mainProjectPkg));
  }

  @Override
  public void registerModuleCreatedInRuntime(Module module) {
    loadedModules.put(module.getName().toString(), module);
  }

  @Override
  public void registerSyntheticPackage(String namespace, String name) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void deregisterModule(String qualifiedName) {
    loadedModules.remove(qualifiedName);
  }

  @Override
  public void renameProject(String namespace, String oldName, String newName) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean isNamespaceRegistered(String namespace) {
    var libWithNamespace =
        loadedPackages.keySet().stream()
            .filter(libName -> libName.namespace().equals(namespace))
            .findFirst();
    return libWithNamespace.isPresent();
  }

  @Override
  public Option<Package<Object>> getPackageForLibrary(LibraryName lib) {
    throw new UnsupportedOperationException();
  }

  @Override
  public List<Module> getModulesForLibrary(LibraryName libraryName) {
    throw new UnsupportedOperationException();
  }

  @Override
  public Option<org.enso.compiler.core.ir.Module> getLibraryBindings(
      LibraryName libraryName, QualifiedName moduleName, CompilerContext context) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void shutdown() {
    try {
      vfs.deleteAll();
    } catch (IOException e) {
      LOGGER.error("Failed to clear VFS", e);
    }
    loadedModules.clear();
    loadedPackages.clear();
    mainProjectPkg = null;
  }

  @SuppressWarnings("unchecked")
  static Package<Object> castVirtualPkg(Package<Path> pkg) {
    return (Package) pkg;
  }

  @SuppressWarnings("unchecked")
  static Package<Path> castObjectPkg(Package<Object> pkg) {
    return (Package) pkg;
  }

  private String readFile(Path file) {
    var lines = new ArrayList<String>();
    try (var reader = vfs.newBufferedReader(file)) {
      var line = reader.readLine();
      while (line != null) {
        lines.add(line);
        line = reader.readLine();
      }
    } catch (IOException e) {
      LOGGER.error("Failed to read file " + vfs.getAbsolutePath(file), e);
      throw new IllegalStateException(e);
    }
    return lines.stream().collect(Collectors.joining("\n"));
  }
}
