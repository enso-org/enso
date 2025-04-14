package org.enso.os.environment.jni;

import org.enso.common.Platform;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.word.WordFactory;

public final class JVM {
  private final JNIBoot.JNICreateJavaVMPointer createJvmFn;
  private JNI.JNIEnv env = WordFactory.nullPointer();

  JVM(JNIBoot.JNICreateJavaVMPointer factory) {
    this.createJvmFn = factory;
  }

  /**
   * Create new JVM. Use {@link #env()} to obtain reference to JNI interface and make calls into the
   * JVM.
   *
   * @param javaHome path where the JDK is installed
   * @return new instance of the JVM
   */
  public static JVM create(String javaHome) {
    return switch (Platform.getOperatingSystem()) {
      case WINDOWS -> WindowsJVM.createImpl(javaHome);
      case LINUX, MACOS -> PosixJVM.createImpl(javaHome);
    };
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
    jvmArgs.nOptions(0);
    var options = StackValue.get(1, JNIBoot.Option.class);
    jvmArgs.options(options);
    jvmArgs.version(JNI.JNI_VERSION_10());
    jvmArgs.nOptions(0);
    jvmArgs.ignoreUnrecognized(false);

    var jvmPtr = StackValue.get(JNI.JavaVMPointer.class);
    var envPtr = StackValue.get(JNI.JNIEnvPointer.class);

    int res = createJvmFn.call(jvmPtr, envPtr, jvmArgs);
    assert res == 0;
    return envPtr.readJNIEnv();
  }
}
