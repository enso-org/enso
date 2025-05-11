package org.enso.os.environment.jni;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import org.enso.os.environment.jni.JNI.JValue;
import org.graalvm.nativeimage.CurrentIsolate;
import org.graalvm.nativeimage.IsolateThread;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.c.function.CEntryPoint;
import org.graalvm.nativeimage.c.function.CEntryPointLiteral;
import org.graalvm.nativeimage.c.function.CFunctionPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.junit.Test;

public class LoadClassTest {
  private static final String PATH = System.getProperty("java.home");
  // set from TestCollectorFeature
  public static String MODULE_PATH;

  private static JVM impl;

  private static JVM jvm() {
    if (impl == null) {
      assert MODULE_PATH != null : "MODULE_PATH field must be set!";
      var path = new File(PATH);
      assert path.isDirectory() : "Java home exists: " + path;
      impl =
          JVM.create(
              path,
              "--module-path=" + MODULE_PATH,
              "--enable-native-access=org.enso.os.environment",
              "-Djdk.module.main=org.enso.os.environment",
              "-Dsay=Ahoj");
    }
    return impl;
  }

  private static JNI.JNIEnv env() {
    return jvm().env();
  }

  @Test
  public void invokeParseShortMethod() {
    var env = env();
    assertTrue("JNI created", env.isNonNull());

    var findClassFn = env.getFunctions().getFindClass();
    var getStaticMethodIDFn = env.getFunctions().getGetStaticMethodID();
    var newStringFn = env.getFunctions().getNewStringUTF();
    var callStaticMethodFn = env.getFunctions().getCallStaticIntMethodA();

    try (var shortName = CTypeConversion.toCString("java/lang/Short");
        var valueOfName = CTypeConversion.toCString("parseShort");
        var valueOfSig = CTypeConversion.toCString("(Ljava/lang/String;)S");
        var toParse = CTypeConversion.toCString("345"); ) {
      var Short = findClassFn.call(env, shortName.get());

      assertTrue("Short class is loaded", Short.isNonNull());

      var valueOf = getStaticMethodIDFn.call(env, Short, valueOfName.get(), valueOfSig.get());
      assertTrue("valueOf method found", valueOf.isNonNull());

      var args = StackValue.get(JNI.JValue.class);
      var str = newStringFn.call(env, toParse.get());
      args.setJObject(str);
      var res = callStaticMethodFn.call(env, Short, valueOf, args);
      assertEquals(345, res);
    }
  }

  @Test
  public void setSystemProperty() {
    var env = env();
    assertTrue("JNI created", env.isNonNull());

    var findClassFn = env.getFunctions().getFindClass();
    var getStaticMethodIDFn = env.getFunctions().getGetStaticMethodID();
    var newStringFn = env.getFunctions().getNewStringUTF();
    var strLengthFn = env.getFunctions().getGetStringUTFLength();
    var strCharsFn = env.getFunctions().getGetStringUTFChars();
    var strReleaseFn = env.getFunctions().getReleaseStringUTFChars();
    var callStaticMethodFn = env.getFunctions().getCallStaticObjectMethodA();

    try (var systemName = CTypeConversion.toCString("java/lang/System");
        var getPropertyName = CTypeConversion.toCString("getProperty");
        var getPropertySig = CTypeConversion.toCString("(Ljava/lang/String;)Ljava/lang/String;");
        var propName = CTypeConversion.toCString("say"); ) {
      var System = findClassFn.call(env, systemName.get());

      assertTrue("System class is loaded", System.isNonNull());

      var valueOf =
          getStaticMethodIDFn.call(env, System, getPropertyName.get(), getPropertySig.get());
      assertTrue("getProperty method found", valueOf.isNonNull());

      var args = StackValue.get(JNI.JValue.class);
      var str = newStringFn.call(env, propName.get());
      args.setJObject(str);
      var res = (JNI.JString) callStaticMethodFn.call(env, System, valueOf, args);
      assertTrue("There should be a property 'say' defined", res.isNonNull());
      var len = strLengthFn.call(env, res);
      assertEquals("'Ahoj' has four letters", 4, len);
      var valueFalse = StackValue.get(JValue.class);
      valueFalse.setBoolean(false);
      var chars = strCharsFn.call(env, res, valueFalse);
      assertEquals("Ahoj", CTypeConversion.toJavaString(chars));
      strReleaseFn.call(env, res, chars);
    }
  }

  @Test
  public void executeMainClass() throws Exception {
    var callbackFn = CALLBACK_FN.getFunctionPointer().rawValue();
    var jvmIsolate = CurrentIsolate.getCurrentThread().rawValue();
    var gen = new Random();
    for (var i = 0; i < 5; i++) {
      var n = gen.nextLong(1, 15);
      jvm()
          .executeMain(
              "org/enso/os/environment/jni/TestMain", "" + jvmIsolate, "" + callbackFn, "" + n);
      long content = COLLECTED_RESULTS.get(n);
      assertEquals(
          "Factorial of " + n + " is the same", TestMain.factorial(n).longValue(), content);
    }
  }

  private static final Map<Long, Long> COLLECTED_RESULTS = new HashMap<>();

  @CEntryPoint
  private static void acceptResultFromHotSpotJvm(IsolateThread threadId, long key, long value) {
    COLLECTED_RESULTS.put(key, value);
  }

  private static final CEntryPointLiteral<CFunctionPointer> CALLBACK_FN =
      CEntryPointLiteral.create(
          LoadClassTest.class,
          "acceptResultFromHotSpotJvm",
          IsolateThread.class,
          long.class,
          long.class);
}
