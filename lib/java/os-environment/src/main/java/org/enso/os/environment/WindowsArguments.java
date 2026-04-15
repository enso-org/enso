package org.enso.os.environment;

import java.nio.charset.StandardCharsets;
import java.util.List;
import org.enso.common.Platform;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.PointerBase;

@CContext(WindowsArguments.Directives.class)
public class WindowsArguments {
  private static final int WCHAR_SIZE = 2;

  private WindowsArguments() {}

  public static String readCommandLineArgs() {
    var buffer = GetCommandLineW();
    var len = wcslen(buffer);
    var byteBuffer = CTypeConversion.asByteBuffer(buffer, len * WCHAR_SIZE);
    return StandardCharsets.UTF_16LE.decode(byteBuffer).toString();
  }

  @CFunction
  static native PointerBase GetCommandLineW();

  @CFunction
  static native int wcslen(PointerBase str);

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
      return List.of("Kernel32");
    }
  }
}
