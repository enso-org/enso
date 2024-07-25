package org.enso.desktopenvironment;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.Pointer;
import org.graalvm.word.WordFactory;

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
    if (Platform.getOperatingSystem().isMacOs()) return moveToTrashImpl(path);
    else return false;
  }

  private boolean moveToTrashImpl(Path path) {
    Pointer source = UnmanagedMemory.malloc(80);
    Pointer target = UnmanagedMemory.malloc(80);

    var kFSPathMakeRefDoNotFollowLeafSymlink = 0x01;
    var kFSFileOperationDefaultOptions = 0x00;
    CTypeConversion.CCharPointerHolder cPath;
    try {
      cPath = CTypeConversion.toCString(path.toString());
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
      return Arrays.asList("<CoreServices/CoreServices.h>");
    }

    @Override
    public List<String> getLibraries() {
      return Arrays.asList("-framework CoreServices");
    }
  }
}
