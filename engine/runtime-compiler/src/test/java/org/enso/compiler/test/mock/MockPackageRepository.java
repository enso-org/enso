package org.enso.compiler.test.mock;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
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
import scala.util.Right;

/**
 * {@link PackageRepository} implementation with {@link org.apache.commons.vfs2.FileObject} as its
 * type member.
 */
final class MockPackageRepository implements PackageRepository {
  private static final Logger LOGGER = LoggerFactory.getLogger(MockPackageRepository.class);
  private final VirtualFileSystem vfs;
  private final FileObject vfsRoot;
  private final PackageManager<FileObject> pkgManager;
  private final Map<LibraryName, Package<FileObject>> loadedPackages = new HashMap<>();
  private final Map<String, CompilerContext.Module> loadedModules = new HashMap<>();
  private Package<FileObject> mainProjectPkg;

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
      return null;
    }
  }

  VirtualFileSystem getVfs() {
    return vfs;
  }

  Package<FileObject> createPackage(LibraryName pkgName, Set<SourceModule> modules) {
    Package<FileObject> pkg = null;
    try {
      var pkgRoot = vfsRoot.resolveFile(pkgName.namespace()).resolveFile(pkgName.name());
      pkg = pkgManager.getOrCreate(pkgRoot);
      // Delete all the automatically created sources, and replace them with
      // our custom sources
      for (var src : pkg.listSourcesJava()) {
        src.file().delete();
      }
      var srcDir = pkg.sourceDir();
      for (var module : modules) {
        var srcPath = module.name().pathAsJava();
        var srcName = module.name().item() + ".enso";
        var subSrcDir = srcDir.resolveFile(String.join("/", srcPath));
        subSrcDir.createFolder();
        var srcFile = subSrcDir.resolveFile(srcName);
        srcFile.createFile();
        try (var os = new OutputStreamWriter(srcFile.getContent().getOutputStream())) {
          os.write(module.content());
        } catch (IOException e) {
          LOGGER.error("Failed to write to file " + srcFile.getName().getFriendlyURI(), e);
          throw new IllegalStateException(e);
        }
        if (!readFile(srcFile).equals(module.content())) {
          var contentRead = readFile(srcFile);
          var expectedContent = module.content();
          LOGGER.error(
              "Writing content to file {} failed. Read content: '{}'. Expected content: '{}'",
              srcFile.getName().getPath(),
              contentRead,
              expectedContent);
          throw new AssertionError("Read content mismatch in " + srcFile.getName().getPath());
        }
      }
    } catch (FileSystemException e) {
      LOGGER.error("Failed to create package " + pkgName, e);
    }
    return pkg;
  }

  /** Same as {@link #createPackage(LibraryName, Set)}, but with just a single source module */
  Package<FileObject> createPackage(LibraryName pkgName, SourceModule module) {
    return createPackage(pkgName, Set.of(module));
  }

  @Override
  public Either<Error, BoxedUnit> initialize() {
    return new Right<>(null);
  }

  @Override
  public Either<Error, BoxedUnit> ensurePackageIsLoaded(LibraryName libraryName) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean isPackageLoaded(LibraryName libraryName) {
    return loadedPackages.containsKey(libraryName);
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
      var mod = new MockModule(virtualPkg, modName, srcPath, srcContent);
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
    throw new UnsupportedOperationException();
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
  static Package<Object> castVirtualPkg(Package<FileObject> pkg) {
    return (Package) pkg;
  }

  @SuppressWarnings("unchecked")
  static Package<FileObject> castObjectPkg(Package<Object> pkg) {
    return (Package) pkg;
  }

  private String readFile(FileObject file) {
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
    return lines.stream().collect(Collectors.joining("\n")) + "\n";
  }
}
