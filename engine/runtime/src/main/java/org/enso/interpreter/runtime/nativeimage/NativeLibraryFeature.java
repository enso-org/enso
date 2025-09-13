package org.enso.interpreter.runtime.nativeimage;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Field;
import org.graalvm.nativeimage.ImageSingletons;
import org.graalvm.nativeimage.hosted.Feature;

/** This feature sets the static fields of {@link NativeLibrarySearchPath} during NI build time. */
final class NativeLibraryFeature implements Feature {
  private static final String NATIVE_LIBS_CLASS_NAME = "com.oracle.svm.core.jdk.NativeLibraries";
  private static final String NATIVE_LIBS_SUPPORT_CLASS_NAME =
      "com.oracle.svm.core.jdk.NativeLibrarySupport";
  private static final String LIB_LOADER_CLASS_NAME =
      "org.enso.interpreter.runtime.nativeimage.NativeLibrarySearchPath";
  private static final String USR_PATH_GETTER_FIELD_NAME = "usrPathsGetter";
  private static final String USR_PATH_SETTER_FIELD_NAME = "usrPathsSetter";
  private static final String NATIVE_LIBS_INSTANCE_FIELD_NAME = "nativeLibsInstance";
  private static final String USR_PATHS_FIELD_NAME = "usrPaths";

  @Override
  public void beforeAnalysis(BeforeAnalysisAccess access) {
    var nativeLibsClass = access.findClassByName(NATIVE_LIBS_CLASS_NAME);
    var nativeLibSupportClass = access.findClassByName(NATIVE_LIBS_SUPPORT_CLASS_NAME);
    assert nativeLibSupportClass != null;
    var lookup = MethodHandles.lookup();
    MethodHandle getterMethodHandle;
    MethodHandle setterMethodHandle;
    try {
      var privateLookup = MethodHandles.privateLookupIn(nativeLibsClass, lookup);
      getterMethodHandle =
          privateLookup.findGetter(nativeLibSupportClass, USR_PATHS_FIELD_NAME, String[].class);
      setterMethodHandle =
          privateLookup.findSetter(nativeLibSupportClass, USR_PATHS_FIELD_NAME, String[].class);
    } catch (IllegalAccessException | NoSuchFieldException e) {
      throw new AssertionError(e);
    }
    var libLoaderClass = access.findClassByName(LIB_LOADER_CLASS_NAME);
    Field usrPathsGetterField;
    Field usrPathsSetterField;
    Field nativeLibsInstanceField;
    try {
      usrPathsGetterField = libLoaderClass.getDeclaredField(USR_PATH_GETTER_FIELD_NAME);
      usrPathsSetterField = libLoaderClass.getDeclaredField(USR_PATH_SETTER_FIELD_NAME);
      nativeLibsInstanceField = libLoaderClass.getDeclaredField(NATIVE_LIBS_INSTANCE_FIELD_NAME);
    } catch (NoSuchFieldException e) {
      throw new AssertionError(e);
    }
    var singleton = ImageSingletons.lookup(nativeLibSupportClass);
    access.registerFieldValueTransformer(nativeLibsInstanceField, (receiver, origVal) -> singleton);
    access.registerFieldValueTransformer(
        usrPathsGetterField, (receiver, origVal) -> getterMethodHandle);
    access.registerFieldValueTransformer(
        usrPathsSetterField, (receiver, origVal) -> setterMethodHandle);
  }
}
