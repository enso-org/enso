package org.enso.desktopenvironment;

import java.nio.file.Path;
import java.util.Arrays;
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

@CContext(WindowsTrash.ShellApi.class)
final class WindowsTrash implements Trash {
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
    if (Platform.getOperatingSystem().isWindows()) return moveToTrashImpl(path);
    else return false;
  }

  private boolean moveToTrashImpl(Path path) {
    CTypeConversion.CCharPointerHolder cPath;
    try {
      cPath = CTypeConversion.toCString(path.toString() + "\0");
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
      return Arrays.asList("<windows.h>");
    }

    @Override
    public List<String> getLibraries() {
      return Arrays.asList("shell32");
    }
  }
}
