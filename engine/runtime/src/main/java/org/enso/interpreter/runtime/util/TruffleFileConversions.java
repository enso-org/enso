package org.enso.interpreter.runtime.util;

import com.oracle.truffle.api.TruffleFile;
import java.io.File;
import org.enso.filesystem.FileSystem;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.pkg.Package;

/**
 * A collection of utilities for converting between various structures containing {@link TruffleFile} to
 * their {@link File} counterparts.
 *
 * This is needed when we pass data from the {@code runtime} project into {@code runtime-compiler},
 * as {@code runtime-compiler} does not depend on truffle API.
 */
public class TruffleFileConversions {
  private TruffleFileConversions() {}

  public static Package<File> convertTruffleFilePackage(Package<TruffleFile> trufflePkg) {
    if (trufflePkg != null) {
      Package<File> filePkg = new Package<>(
          convertTruffleFile(trufflePkg.root()),
          trufflePkg.getConfig(),
          defaultFileSystem()
      );
      return filePkg;
    } else {
      return null;
    }
  }

  public static Package<TruffleFile> convertFilePackage(EnsoContext ctx, Package<File> filePkg) {
    if (filePkg != null) {
      Package<TruffleFile> trufflePkg = new Package<>(
          ctx.getTruffleFile(filePkg.root()),
          filePkg.getConfig(),
          TruffleFileSystem.instance()
      );
      return trufflePkg;
    } else {
      return null;
    }
  }

  public static File convertTruffleFile(TruffleFile truffleFile) {
    if (truffleFile != null) {
      var path = truffleFile.getAbsoluteFile().getPath();
      return new File(path);
    } else {
      return null;
    }
  }

  public static TruffleFile convertFile(EnsoContext ctx, File file) {
    return ctx.getTruffleFile(file);
  }

  private static FileSystem<File> defaultFileSystem() {
    return FileSystem.getDefault();
  }
}
