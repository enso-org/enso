package org.enso.compiler;

import com.oracle.truffle.api.TruffleFile;
import java.util.Optional;
import java.util.stream.StreamSupport;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;

public final class PackageRepositoryUtils {
  private PackageRepositoryUtils() {}

  /**
   * Fetches the module name associated with a given file, using the environment packages
   * information.
   *
   * @param packageRepository repository to work on
   * @param file the path to decode.
   * @return a qualified name of the module corresponding to the file, if exists.
   */
  public static Optional<QualifiedName> getModuleNameForFile(
      PackageRepository packageRepository, TruffleFile file) {
    return scala.jdk.javaapi.CollectionConverters.asJava(packageRepository.getLoadedPackages())
        .stream()
        .filter(pkg -> file.startsWith(pkg.sourceDir()))
        .map(pkg -> pkg.moduleNameForFile(file))
        .findFirst();
  }

  /**
   * Finds the package the provided module belongs to.
   *
   * @param packageRepository repository to work on
   * @param file the module to find the package of
   * @return {@code module}'s package, if exists
   */
  public static Optional<Package<TruffleFile>> getPackageOf(
      PackageRepository packageRepository, TruffleFile file) {
    if (file == null) {
      return Optional.empty();
    }
    return StreamSupport.stream(packageRepository.getLoadedPackagesJava().spliterator(), true)
        .filter(pkg -> file.getAbsoluteFile().startsWith(pkg.root().getAbsoluteFile()))
        .findFirst();
  }
}
