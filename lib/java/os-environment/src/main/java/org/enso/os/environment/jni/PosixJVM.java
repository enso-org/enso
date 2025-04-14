package org.enso.os.environment.jni;

import java.io.File;
import java.util.List;
import org.graalvm.nativeimage.Platform;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.constant.CConstant;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.PointerBase;

@CContext(PosixJVM.Direct.class)
final class PosixJVM {
  static JNIBoot.JNICreateJavaVMPointer createImpl(String javaHome) {
    var libJvmPath = findDynamicLibrary(javaHome).getPath();
    try (var libPath = CTypeConversion.toCString(libJvmPath);
        var createJvm = CTypeConversion.toCString("JNI_CreateJavaVM")) {
      var jvmSo = dlopen(libPath.get(), RTLD_NOW());
      assert jvmSo.isNonNull()
          : "Cannot load dynamic library "
              + libJvmPath
              + " error: "
              + CTypeConversion.toJavaString(dlerror());
      return dlsym(jvmSo, createJvm.get());
    }
  }

  private static File findDynamicLibrary(String javaHome) {
    var libName = "libjvm.so";
    if (System.getProperty("os.name").contains("Mac")) {
      libName = "libjvm.dylib";
    }
    return new File(new File(new File(new File(javaHome), "lib"), "server"), libName);
  }

  @CConstant
  static native int RTLD_NOW();

  @CFunction
  static native PointerBase dlopen(CCharPointer file, int mode);

  @CFunction(transition = CFunction.Transition.NO_TRANSITION)
  static native <T extends PointerBase> T dlsym(PointerBase handle, CCharPointer name);

  @CFunction
  static native CCharPointer dlerror();

  static final class Direct implements CContext.Directives {

    @Override
    public boolean isInConfiguration() {
      return Platform.includedIn(Platform.LINUX.class)
          || Platform.includedIn(Platform.DARWIN.class);
    }

    @Override
    public List<String> getHeaderFiles() {
      return List.of("<dlfcn.h>");
    }
  }
}
