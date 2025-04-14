package org.enso.os.environment.jni;

import org.enso.common.Platform;

public final class JVM {
  private final JNI.JNIEnv env;

  JVM(JNI.JNIEnv env) {
    this.env = env;
  }

  public static JVM create(String javaHome) {
    return switch (Platform.getOperatingSystem()) {
      case WINDOWS -> WindowsJVM.createImpl(javaHome);
      case LINUX, MACOS -> PosixJVM.createImpl(javaHome);
    };
  }

  public JNI.JNIEnv env() {
    return env;
  }
}
