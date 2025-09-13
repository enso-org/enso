package org.enso.os.environment;

import java.io.File;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.graalvm.nativeimage.hosted.Feature;
import org.graalvm.nativeimage.hosted.RuntimeReflection;

public final class TestCollectorFeature implements Feature {
  @Override
  public void beforeAnalysis(BeforeAnalysisAccess access) {
    recordModulePath(access);

    for (var testClass : ListOfTests.TEST_CLASSES) {
      var testClazz = access.findClassByName(testClass);
      if (testClazz == null) {
        throw new IllegalStateException("Test class " + testClass + " not found");
      }
      RuntimeReflection.register(testClazz);
      RuntimeReflection.register(testClazz.getConstructors());
      RuntimeReflection.register(testClazz.getMethods());
      RuntimeReflection.register(testClazz.getFields());
      RuntimeReflection.registerAllConstructors(testClazz);
      RuntimeReflection.registerAllFields(testClazz);
      RuntimeReflection.registerAllMethods(testClazz);
    }
    System.err.println("Registered test classes for reflection: " + ListOfTests.TEST_CLASSES);

    var jvmPeerClass = access.findClassByName("org.enso.os.environment.jni.JVMPeer");
    RuntimeReflection.register(jvmPeerClass);
    RuntimeReflection.register(jvmPeerClass.getConstructors());
  }

  private static void recordModulePath(BeforeAnalysisAccess access) {
    var bootClassName = "org.enso.os.environment.jni.LoadClassTest";
    var bootClass = access.findClassByName(bootClassName);
    try {
      var f = bootClass.getField("MODULE_PATH");
      var fromMp = access.getApplicationModulePath().stream().map((t) -> t.toFile().getPath());
      var fromCp =
          access.getApplicationClassPath().stream()
              .map((t) -> t.toFile().getPath())
              .filter(
                  p -> {
                    return !p.contains("frgaal")
                        && !p.contains("junit-interface")
                        && !p.contains("hamcrest")
                        && !p.contains("test-interface");
                  });
      var allPath = Stream.concat(fromMp, fromCp).collect(Collectors.joining(File.pathSeparator));
      f.set(null, allPath);
    } catch (ReflectiveOperationException ex) {
      throw new IllegalStateException(ex);
    }
  }
}
