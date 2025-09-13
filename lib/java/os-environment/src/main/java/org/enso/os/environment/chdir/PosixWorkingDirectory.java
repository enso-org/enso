package org.enso.os.environment.chdir;

import java.io.File;
import java.util.List;
import org.enso.common.Platform;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.constant.CConstant;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@CContext(PosixWorkingDirectory.Directives.class)
final class PosixWorkingDirectory extends WorkingDirectory {
  static final PosixWorkingDirectory INSTANCE = new PosixWorkingDirectory();
  private static final Logger LOGGER = LoggerFactory.getLogger(PosixWorkingDirectory.class);

  private PosixWorkingDirectory() {}

  @Override
  public boolean changeWorkingDir(String path) {
    try (var cPath = CTypeConversion.toCString(path)) {
      int res = chdir(cPath.get());
      if (res != 0) {
        LOGGER.error("chdir({}) syscall returned {}", path, res);
        return false;
      }
      return true;
    } catch (Throwable e) {
      if (!ImageInfo.inImageRuntimeCode()) {
        LOGGER.warn("Changing working directory is not supported in non-AOT mode", e);
      } else {
        LOGGER.error("Cannot change working directory to " + path + " on Unix", e);
      }
      return false;
    }
  }

  @Override
  public String currentWorkingDir() {
    String cwd;
    try {
      cwd = invokeCwd();
    } catch (Throwable t) {
      LOGGER.error("Cannot invoke `getcwd` on Unix", t);
      return System.getProperty("user.dir");
    }
    return cwd;
  }

  @Override
  public boolean exists(String dir, String file) {
    String full;
    if (dir.endsWith(File.separator)) {
      full = dir + file;
    } else {
      full = dir + File.separator + file;
    }
    try (var cPath = CTypeConversion.toCString(full)) {
      var res = access(cPath.get(), R_OK());
      return res == 0;
    }
  }

  @CConstant
  static native int R_OK();

  @CFunction
  static native int access(CCharPointer path, int mode);

  @CFunction
  static native int chdir(CCharPointer path);

  @CFunction
  static native CCharPointer getcwd(CCharPointer buf, int size);

  private String invokeCwd() {
    byte[] buf = new byte[4096];
    String path;
    try (var ptrHolder = CTypeConversion.toCBytes(buf)) {
      var ptr = ptrHolder.get();
      var retPtr = getcwd(ptr, 4096);
      if (retPtr.isNull()) {
        LOGGER.error("getcwd() syscall returned null");
      }
      if (!retPtr.equal(ptr)) {
        LOGGER.error("getcwd() syscall returned different pointer");
      }
      path = new String(buf);
    }
    return path.trim();
  }

  static final class Directives implements CContext.Directives {

    @Override
    public boolean isInConfiguration() {
      return switch (Platform.getOperatingSystem()) {
        case LINUX, MACOS -> true;
        case WINDOWS -> false;
      };
    }

    @Override
    public List<String> getHeaderFiles() {
      return List.of("<unistd.h>");
    }

    @Override
    public List<String> getLibraries() {
      return List.of("c");
    }
  }
}
