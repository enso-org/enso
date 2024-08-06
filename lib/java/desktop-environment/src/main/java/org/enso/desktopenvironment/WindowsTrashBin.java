package org.enso.desktopenvironment;

import java.nio.file.Path;
import java.util.List;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.constant.CConstant;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.struct.CField;
import org.graalvm.nativeimage.c.struct.CStruct;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.PointerBase;
import org.slf4j.LoggerFactory;

@CContext(WindowsTrashBin.ShellApi.class)
final class WindowsTrashBin implements TrashBin {
  @CConstant
  public static native int FO_DELETE();

  @CConstant
  public static native int FOF_ALLOWUNDO();

  @CConstant
  public static native int FOF_NO_UI();

  @Override
  public boolean isSupported() {
    return true;
  }

  @Override
  public boolean moveToTrash(Path path) {
    if (Platform.getOperatingSystem().isWindows()) {
      try {
        return moveToTrashImpl(path);
      } catch (NullPointerException | LinkageError err) {
        if (!Boolean.getBoolean("com.oracle.graalvm.isaot")) {
          var logger = LoggerFactory.getLogger(MacTrashBin.class);
          logger.warn("Moving to Windows' Trash Bin is not supported in non-AOT mode.");
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
    try (var cPath = CTypeConversion.toCString(path.toString() + "\0")) {
      var fileop = StackValue.get(SHFileOperation.class);
      fileop.wFunc(FO_DELETE());
      fileop.pFrom(cPath.get());
      fileop.fFlags((short) (FOF_ALLOWUNDO() | FOF_NO_UI()));
      var res = SHFileOperationA(fileop);
      return res == 0;
    } catch (Throwable error) {
      return false;
    }
  }

  @CStruct(value = "SHFILEOPSTRUCTA")
  static interface SHFileOperation extends PointerBase {
    @CField
    void pFrom(CCharPointer from);

    @CField
    void wFunc(int op);

    @CField
    void fFlags(short flags);
  }

  @CFunction
  static native int SHFileOperationA(SHFileOperation fileop);

  public static final class ShellApi implements CContext.Directives {

    @Override
    public boolean isInConfiguration() {
      return Platform.getOperatingSystem().isWindows();
    }

    @Override
    public List<String> getHeaderFiles() {
      return List.of("<windows.h>");
    }

    @Override
    public List<String> getLibraries() {
      return List.of("shell32");
    }
  }
}
