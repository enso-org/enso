package org.enso.compiler.test.mock;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.context.CompilerContext.Module;
import org.enso.editions.LibraryName;
import org.enso.filesystem.FileSystem;
import org.enso.pkg.ComponentGroups;
import org.enso.pkg.Package;
import org.enso.pkg.PackageManager;
import org.enso.pkg.QualifiedName;
import scala.Option;
import scala.collection.concurrent.Map;
import scala.collection.immutable.List;
import scala.collection.immutable.ListSet;
import scala.collection.immutable.Seq;
import scala.runtime.BoxedUnit;
import scala.util.Either;
import scala.util.Right;

public final class MockPackageRepository implements PackageRepository {

  public MockPackageRepository() {}

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
    throw new UnsupportedOperationException();
  }

  @Override
  public Seq<org.enso.pkg.Package<Object>> getLoadedPackages() {
    throw new UnsupportedOperationException();
  }

  @Override
  public Seq<CompilerContext.Module> getLoadedModules() {
    throw new UnsupportedOperationException();
  }

  @Override
  public Map<String, Module> getModuleMap() {
    throw new UnsupportedOperationException();
  }

  @Override
  public scala.collection.immutable.Map<String, Module> freezeModuleMap() {
    throw new UnsupportedOperationException();
  }

  @Override
  public scala.collection.immutable.Map<LibraryName, ComponentGroups> getComponents() {
    throw new UnsupportedOperationException();
  }

  @Override
  public ListSet<Module> getPendingModules() {
    throw new UnsupportedOperationException();
  }

  @Override
  public Option<Module> getLoadedModule(String qualifiedName) {
    return switch (qualifiedName) {
      case "Standard.Base.Any" -> Option.apply(null);
      default -> throw new UnsupportedOperationException("no module: " + qualifiedName);
    };
  }

  @Override
  public void registerMainProjectPackage(
      LibraryName libraryName, org.enso.pkg.Package<Object> pkg) {
    throw new UnsupportedOperationException();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Option<org.enso.pkg.Package<Object>> getMainProjectPackage() {
    try {
      var pm = new PackageManager<File>(FileSystem.Default$.MODULE$);
      var tmp = Files.createTempDirectory("mockdir");
      var dir = pm.getOrCreate(tmp.toFile());
      return Option.apply((org.enso.pkg.Package) dir);
    } catch (IOException ex) {
      throw new IllegalStateException(ex);
    }
  }

  @Override
  public void registerModuleCreatedInRuntime(Module module) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void registerSyntheticPackage(String namespace, String name) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void deregisterModule(String qualifiedName) {
    throw new UnsupportedOperationException();
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
    throw new UnsupportedOperationException();
  }
}
