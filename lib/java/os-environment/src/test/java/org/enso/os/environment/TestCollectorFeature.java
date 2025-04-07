package org.enso.os.environment;

import org.graalvm.nativeimage.hosted.Feature;
import org.graalvm.nativeimage.hosted.RuntimeReflection;

public final class TestCollectorFeature implements Feature {
  @Override
  public void beforeAnalysis(BeforeAnalysisAccess access) {
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
  }
}
