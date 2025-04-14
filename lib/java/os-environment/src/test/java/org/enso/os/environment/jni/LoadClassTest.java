package org.enso.os.environment.jni;

import static org.junit.Assert.assertTrue;

import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.junit.Test;

public class LoadClassTest {
  private static final String PATH = System.getProperty("java.home");

  @Test
  public void loadJavaLangShortClass() {
    var env = JVM.create(PATH).env();
    assertTrue("JNI created", env.isNonNull());

    var findClassFn = env.getFunctions().getFindClass();

    try (var nameHolder = CTypeConversion.toCString("java/lang/Short")) {
      var clazz = findClassFn.call(env, nameHolder.get());

      assertTrue("Short class is loaded", clazz.isNonNull());
    }
  }
}
