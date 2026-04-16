package org.enso.os.environment;

import java.util.List;
import org.enso.common.Platform;
import org.graalvm.nativeimage.ImageInfo;
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
final class WindowsArguments implements Arguments {
  static final WindowsArguments INSTANCE = new WindowsArguments();

  private WindowsArguments() {}

  @Override
  public String[] alterArgs(String[] originalArgs) {
    if (!ImageInfo.inImageRuntimeCode()) {
      return originalArgs;
    }

    return readCommandLineArgs();
  }

  private static final Logger LOGGER = LoggerFactory.getLogger(WindowsArguments.class);

  private static final int WCHAR_SIZE = 2;

  private static String[] readCommandLineArgs() {
    var cmd = GetCommandLineW();

    CIntPointer numOfArgs = StackValue.get(Long.BYTES);

    WCharPointerPointer args = CommandLineToArgvW(cmd, numOfArgs);
    try {
      var numArgs = numOfArgs.read();

      var results = new String[numArgs - 1];
      for (var i = 0; i < results.length; i++) {
        var arg = args.read(i + 1);
        results[i] = toJavaString(arg);
        LOGGER.trace("Read command line argument {}: {}", i, results[i]);
      }

      return results;
    } finally {
      LocalFree(args);
    }
  }

  private static String toJavaString(WCharPointer arg) {
    return CTypeConversion.asByteBuffer(arg, wcslen(arg) * WCHAR_SIZE)
        .order(java.nio.ByteOrder.LITTLE_ENDIAN)
        .asCharBuffer()
        .toString();
  }

  @CPointerTo(nameOfCType = "wchar_t")
  private interface WCharPointer extends PointerBase {}

  @CPointerTo(WCharPointer.class)
  private interface WCharPointerPointer extends PointerBase {
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
