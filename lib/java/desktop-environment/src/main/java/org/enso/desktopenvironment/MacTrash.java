package org.enso.desktopenvironment;

import java.nio.file.Path;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.function.CLibrary;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.Pointer;
import org.graalvm.word.WordFactory;

@CLibrary("-framework CoreServices")
final class MacTrash implements Trash {

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
    Pointer ref = WordFactory.nullPointer();
    Pointer source = UnmanagedMemory.malloc(80);
    Pointer target = UnmanagedMemory.malloc(80);

    var kFSPathMakeRefDoNotFollowLeafSymlink = 0x01;
    var kFSFileOperationDefaultOptions = 0x00;
    CTypeConversion.CCharPointerHolder cPath;
    try {
      cPath = CTypeConversion.toCString(path.toString());
      var r1 =
          FSPathMakeRefWithOptions(cPath.get(), kFSPathMakeRefDoNotFollowLeafSymlink, source, ref);
      var r2 = FSMoveObjectToTrashSync(source, target, kFSFileOperationDefaultOptions);
      return r1 == 0 && r2 == 0;
    } catch (Throwable error) {
      return false;
    }
  }
}
