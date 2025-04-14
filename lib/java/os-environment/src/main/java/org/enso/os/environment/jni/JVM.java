package org.enso.os.environment.jni;

import org.enso.common.Platform;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.struct.SizeOf;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.WordFactory;

/** Represents a JVM inside of current process. */
public final class JVM {
  private final JNIBoot.JNICreateJavaVMPointer createJvmFn;
  private final String[] options;
  private JNI.JNIEnv env = WordFactory.nullPointer();

  JVM(JNIBoot.JNICreateJavaVMPointer factory, String[] options) {
    this.createJvmFn = factory;
    this.options = options;
  }

  /**
   * Create new JVM. Use {@link #env()} to obtain reference to JNI interface and make calls into the
   * JVM.
   *
   * @param javaHome path where the JDK is installed
   * @return new instance of the JVM
   */
  public static JVM create(String javaHome, String... options) {
    var createJvmFn =
        switch (Platform.getOperatingSystem()) {
          case WINDOWS -> WindowsJVM.createImpl(javaHome);
          case LINUX, MACOS -> PosixJVM.createImpl(javaHome);
        };
    return new JVM(createJvmFn, options);
  }

  /**
   * Initialize or just obtain environment associated with this JVM.
   *
   * @return JNI environment to make calls into the JVM
   */
  public final JNI.JNIEnv env() {
    if (env.isNull()) {
      env = initializeEnv();
    }
    return env;
  }

  private synchronized JNI.JNIEnv initializeEnv() {
    var jvmArgs = StackValue.get(JNIBoot.Args.class);
    jvmArgs.nOptions(options.length);
    var sizeOfOption = SizeOf.get(JNIBoot.Option.class);
    JNIBoot.Option jvmOpts = UnmanagedMemory.calloc(options.length * sizeOfOption);
    var holder = new CTypeConversion.CCharPointerHolder[options.length];
    for (var i = 0; i < options.length; i++) {
      holder[i] = CTypeConversion.toCString(options[i]);
      var nth = jvmOpts.addressOf(i);
      nth.setOptionString(holder[i].get());
      nth.setExtraInfo(WordFactory.nullPointer());
    }
    jvmArgs.options(jvmOpts);
    jvmArgs.version(JNI.JNI_VERSION_10());
    jvmArgs.ignoreUnrecognized(false);

    var jvmPtr = StackValue.get(JNI.JavaVMPointer.class);
    var envPtr = StackValue.get(JNI.JNIEnvPointer.class);

    int res = createJvmFn.call(jvmPtr, envPtr, jvmArgs);
    assert res == 0 : "Error creating JVM: " + res;

    for (var i = 0; i < options.length; i++) {
      holder[i].close();
    }
    UnmanagedMemory.free(jvmOpts);

    return envPtr.readJNIEnv();
  }
}
