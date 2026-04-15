package org.enso.os.environment;

import java.util.List;
import org.enso.common.Platform;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.struct.CPointerTo;
import org.graalvm.nativeimage.c.type.CIntPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.PointerBase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@CContext(WindowsArguments.Directives.class)
public class WindowsArguments {
  private static final Logger LOGGER = LoggerFactory.getLogger(WindowsArguments.class);

  private static final int WCHAR_SIZE = 2;

  private WindowsArguments() {}

  public static String[] readCommandLineArgs() {
    var cmd = GetCommandLineW();

    CIntPointer numOfArgs = StackValue.get(Long.BYTES);
    var strs = CommandLineToArgvW(cmd, numOfArgs);

    var numArgs = numOfArgs.read();

    var results = new String[numArgs];
    for (var i = 0; i < results.length; i++) {
      var arg = strs.read(i);
      results[i] = toJavaString(arg);
      LOGGER.debug("Read command line argument {}: {}", i, results[i]);
    }

    LocalFree(strs);
    return results;
  }

  private static String toJavaString(WCharPointer arg) {
    return CTypeConversion.asByteBuffer(arg, wcslen(arg) * WCHAR_SIZE)
        .order(java.nio.ByteOrder.LITTLE_ENDIAN)
        .asCharBuffer()
        .toString();
  }

  @CPointerTo(nameOfCType = "wchar_t")
  public interface WCharPointer extends PointerBase {}

  @CPointerTo(WCharPointer.class)
  public interface WCharPointerPointer extends PointerBase {
    WCharPointer read(int index);
  }

  @CFunction
  private static native WCharPointer GetCommandLineW();

  @CFunction
  private static native WCharPointerPointer CommandLineToArgvW(
      WCharPointer cmdLine, CIntPointer numArgsOut);

  @CFunction
  private static native int wcslen(WCharPointer str);

  @CFunction
  private static native void LocalFree(PointerBase p);

  static final class Directives implements CContext.Directives {
    @Override
    public boolean isInConfiguration() {
      return Platform.getOperatingSystem().isWindows();
    }

    @Override
    public List<String> getHeaderFiles() {
      return List.of("<windows.h>", "<wchar.h>");
    }

    @Override
    public List<String> getLibraries() {
      return List.of("Kernel32", "Shell32");
    }
  }
}
