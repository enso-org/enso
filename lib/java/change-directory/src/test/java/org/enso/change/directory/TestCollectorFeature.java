package org.enso.change.directory;

import java.util.List;
import org.graalvm.nativeimage.hosted.Feature;
import org.graalvm.nativeimage.hosted.RuntimeReflection;

public final class TestCollectorFeature implements Feature {
  private static final List<String> TEST_CLASSES =
      List.of("org.enso.change.directory.TestChangeDirectory");

  @Override
  public void beforeAnalysis(BeforeAnalysisAccess access) {
    for (var testClass : TEST_CLASSES) {
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
    System.err.println("Registered test classes for reflection: " + TEST_CLASSES);
  }
}
