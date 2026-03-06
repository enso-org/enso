package org.enso.jvm.channel;

import static org.graalvm.nativeimage.c.function.CFunction.Transition.NO_TRANSITION;

import java.io.File;
import java.util.List;
import org.enso.jvm.channel.JNIBoot.JNICreateJavaVMPointer;
import org.graalvm.nativeimage.Platform;
import org.graalvm.nativeimage.Platforms;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.PointerBase;

@CContext(WindowsJVM.Direct.class)
final class WindowsJVM {
  static JNICreateJavaVMPointer loadImpl(String dllPath) {
    try (var libPath = CTypeConversion.toCString(dllPath);
        var createJvm = CTypeConversion.toCString("JNI_CreateJavaVM")) {
      var dll = LoadLibraryA(libPath.get());
      assert dll.isNonNull();
      return GetProcAddress(dll, createJvm.get());
    }
  }

  static File findDynamicLibrary(File javaHome) {
    var dll = new File(new File(new File(javaHome, "bin"), "server"), "jvm.dll");
    if (!dll.exists()) {
      throw new AssertionError("Cannot find " + dll);
    }
    return dll;
  }

  /** Loads the specified module into the address space of the calling process. */
  @CFunction(transition = NO_TRANSITION)
  static native HMODULE LoadLibraryA(CCharPointer lpLibFileName);

  @CFunction(transition = NO_TRANSITION)
  static native <T extends PointerBase> T GetProcAddress(HMODULE hModule, CCharPointer lpProcName);

  /** Windows Module Handle type */
  interface HMODULE extends PointerBase {}

  @Platforms(Platform.WINDOWS.class)
  static final class Direct implements CContext.Directives {
    @Override
    public final boolean isInConfiguration() {
      return Platform.includedIn(Platform.WINDOWS.class);
    }

    @Override
    public final List<String> getHeaderFiles() {
      return List.of("<windows.h>");
    }
  }
}
