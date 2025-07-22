package org.enso.database.nativeimage;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import org.graalvm.nativeimage.hosted.Feature;
import org.graalvm.nativeimage.hosted.RuntimeClassInitialization;
import org.graalvm.nativeimage.hosted.RuntimeJNIAccess;
import org.sqlite.BusyHandler;
import org.sqlite.Collation;
import org.sqlite.Function;
import org.sqlite.ProgressHandler;
import org.sqlite.SQLiteJDBCLoader;
import org.sqlite.core.DB;
import org.sqlite.core.NativeDB;
import org.sqlite.jdbc3.JDBC3DatabaseMetaData;
import org.sqlite.util.LibraryLoaderUtil;
import org.sqlite.util.OSInfo;
import org.sqlite.util.ProcessRunner;

/**
 * Replacement of {@code org.sqlite.nativeimage.SqliteJdbcFeature}. Is almost a direct copy of that
 * feature, but does not include native libraries inside resources.
 */
public class SqliteJdbcPatchedFeature implements Feature {

  @Override
  public void beforeAnalysis(BeforeAnalysisAccess access) {
    RuntimeClassInitialization.initializeAtBuildTime(SQLiteJDBCLoader.VersionHolder.class);
    RuntimeClassInitialization.initializeAtBuildTime(JDBC3DatabaseMetaData.class);
    RuntimeClassInitialization.initializeAtBuildTime(OSInfo.class);
    RuntimeClassInitialization.initializeAtBuildTime(ProcessRunner.class);
    RuntimeClassInitialization.initializeAtBuildTime(LibraryLoaderUtil.class);
    access.registerReachabilityHandler(
        this::nativeDbReachable, this.method(SQLiteJDBCLoader.class, "initialize"));
  }

  private void nativeDbReachable(Feature.DuringAnalysisAccess a) {
    registerJNICalls();
  }

  private void registerJNICalls() {
    RuntimeJNIAccess.register(NativeDB.class);
    RuntimeJNIAccess.register(
        this.fields(
            NativeDB.class,
            "pointer",
            "busyHandler",
            "commitListener",
            "updateListener",
            "progressHandler"));
    RuntimeJNIAccess.register(
        this.method(DB.class, "onUpdate", Integer.TYPE, String.class, String.class, Long.TYPE));
    RuntimeJNIAccess.register(this.method(DB.class, "onCommit", Boolean.TYPE));
    RuntimeJNIAccess.register(this.method(NativeDB.class, "stringToUtf8ByteArray", String.class));
    RuntimeJNIAccess.register(this.method(DB.class, "throwex"));
    RuntimeJNIAccess.register(this.method(DB.class, "throwex", Integer.TYPE));
    RuntimeJNIAccess.register(this.method(NativeDB.class, "throwex", String.class));
    RuntimeJNIAccess.register(Function.class);
    RuntimeJNIAccess.register(this.fields(Function.class, "context", "value", "args"));
    RuntimeJNIAccess.register(this.method(Function.class, "xFunc"));
    RuntimeJNIAccess.register(Collation.class);
    RuntimeJNIAccess.register(this.method(Collation.class, "xCompare", String.class, String.class));
    RuntimeJNIAccess.register(Function.Aggregate.class);
    RuntimeJNIAccess.register(this.method(Function.Aggregate.class, "xStep"));
    RuntimeJNIAccess.register(this.method(Function.Aggregate.class, "xFinal"));
    RuntimeJNIAccess.register(this.method(Function.Aggregate.class, "clone"));
    RuntimeJNIAccess.register(Function.Window.class);
    RuntimeJNIAccess.register(this.method(Function.Window.class, "xInverse"));
    RuntimeJNIAccess.register(this.method(Function.Window.class, "xValue"));
    RuntimeJNIAccess.register(DB.ProgressObserver.class);
    RuntimeJNIAccess.register(
        this.method(DB.ProgressObserver.class, "progress", Integer.TYPE, Integer.TYPE));
    RuntimeJNIAccess.register(ProgressHandler.class);
    RuntimeJNIAccess.register(this.method(ProgressHandler.class, "progress"));
    RuntimeJNIAccess.register(BusyHandler.class);
    RuntimeJNIAccess.register(this.method(BusyHandler.class, "callback", Integer.TYPE));
    RuntimeJNIAccess.register(Throwable.class);
    RuntimeJNIAccess.register(this.method(Throwable.class, "toString"));
    RuntimeJNIAccess.register(boolean[].class);
  }

  private Method method(Class<?> clazz, String methodName, Class<?>... args) {
    try {
      return clazz.getDeclaredMethod(methodName, args);
    } catch (NoSuchMethodException e) {
      throw new AssertionError(e);
    }
  }

  private Field[] fields(Class<?> clazz, String... fieldNames) {
    try {
      Field[] fields = new Field[fieldNames.length];

      for (int i = 0; i < fieldNames.length; ++i) {
        fields[i] = clazz.getDeclaredField(fieldNames[i]);
      }

      return fields;
    } catch (NoSuchFieldException e) {
      throw new AssertionError(e);
    }
  }
}
