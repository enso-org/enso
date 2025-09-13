package org.enso.jvm.channel;

import java.io.File;
import java.util.List;
import org.graalvm.nativeimage.c.CContext;

final class JNIDirectives implements CContext.Directives {

  @Override
  public List<String> getLibraryPaths() {
    var javaHome = new File(System.getProperty("java.home"));
    var binServer = new File(new File(javaHome, "bin"), "server");
    var libServer = new File(new File(javaHome, "lib"), "server");
    if (binServer.isDirectory()) {
      return List.of(binServer.getPath());
    } else {
      return List.of(libServer.getPath());
    }
  }

  @Override
  public List<String> getOptions() {
    var javaHome = new File(System.getProperty("java.home"));
    var include = new File(javaHome, "include");
    assert include.isDirectory();
    var jni = new File(include, "jni.h");
    assert jni.canRead();
    for (var subDir : include.listFiles()) {
      var md = new File(subDir, "jni_md.h");
      if (md.canRead()) {
        var includes = List.of("-I", jni.getParent(), "-I", md.getParent());
        return includes;
      }
    }
    throw new AssertionError("Cannot find libs in " + javaHome);
  }

  @Override
  public List<String> getHeaderFiles() {
    return List.of("<jni.h>", "<jni_md.h>");
  }
}
