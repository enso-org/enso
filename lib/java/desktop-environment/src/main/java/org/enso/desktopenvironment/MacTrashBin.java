package org.enso.desktopenvironment;

import java.nio.file.Path;
import java.util.List;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.Pointer;
import org.graalvm.word.WordFactory;
import org.slf4j.LoggerFactory;

@CContext(MacTrashBin.CoreServices.class)
final class MacTrashBin implements TrashBin {

  @CFunction
  static native int FSPathMakeRefWithOptions(
      CCharPointer path, int flags, Pointer buffer, Pointer any);

  @CFunction
  static native int FSMoveObjectToTrashSync(Pointer source, Pointer target, int flags);

  @Override
  public boolean isSupported() {
    return true;
  }

  @Override
  public boolean moveToTrash(Path path) {
    if (Platform.getOperatingSystem().isMacOs()) {
      try {
        return moveToTrashImpl(path);
      } catch (NullPointerException | LinkageError err) {
        if (!Boolean.getBoolean("com.oracle.graalvm.isaot")) {
          var logger = LoggerFactory.getLogger(MacTrashBin.class);
          logger.warn("Moving to MacOS's Trash Bin is not supported in non-AOT mode.");
          return false;
        } else {
          throw err;
        }
      }
    } else {
      return false;
    }
  }

  private boolean moveToTrashImpl(Path path) {
    Pointer source = UnmanagedMemory.malloc(80);
    Pointer target = UnmanagedMemory.malloc(80);

    var kFSPathMakeRefDoNotFollowLeafSymlink = 0x01;
    var kFSFileOperationDefaultOptions = 0x00;
    try (var cPath = CTypeConversion.toCString(path.toString())) {
      var r1 =
          FSPathMakeRefWithOptions(
              cPath.get(), kFSPathMakeRefDoNotFollowLeafSymlink, source, WordFactory.nullPointer());
      var r2 = FSMoveObjectToTrashSync(source, target, kFSFileOperationDefaultOptions);
      return r1 == 0 && r2 == 0;
    } catch (Throwable error) {
      return false;
    } finally {
      UnmanagedMemory.free(source);
      UnmanagedMemory.free(target);
    }
  }

  public static final class CoreServices implements CContext.Directives {
    @Override
    public boolean isInConfiguration() {
      return Platform.getOperatingSystem().isMacOs();
    }

    @Override
    public List<String> getHeaderFiles() {
      return List.of("<CoreServices/CoreServices.h>");
    }

    @Override
    public List<String> getLibraries() {
      return List.of("-framework CoreServices");
    }
  }
}
