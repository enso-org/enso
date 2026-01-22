package org.enso.ydoc.server.registration;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import org.enso.jvm.interop.api.OtherJvmClassLoader;
import org.enso.runner.common.WrongOption;
import org.enso.runner.common.YdocServerApi;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.polyglot.proxy.ProxyArray;

public final class YdocServerImpl extends YdocServerApi {
  public YdocServerImpl() {}

  @Override
  protected AutoCloseable runYdocServer(String hostname, int port)
      throws WrongOption, IOException, URISyntaxException {
    // the following shall invoke:
    //   return launch(hostname, port);
    // but in the other JVM
    var isAot = ImageInfo.inImageRuntimeCode();
    var loader = OtherJvmClassLoader.create("org.enso.ydoc.server", null, null, isAot, null);
    if (isAot) {
      // in AOT mode the org.enso.ydoc.server is the main module loaded
      // to the JVM's boot layer - e.g. its classes are available
    } else {
      // in "single JVM mock mode" we have to make sure JAR is added to
      // the classloader - right now by calling addPath
      var myJar =
          new File(
              OtherJvmClassLoader.class
                  .getProtectionDomain()
                  .getCodeSource()
                  .getLocation()
                  .toURI());
      var ydocServerJar = new File(myJar.getParentFile(), "ydoc-server.jar");
      assert ydocServerJar.exists() : "Found " + ydocServerJar;
      loader.addPath(ydocServerJar);
    }
    var fqn = "org.enso.ydoc.server.Main";
    var impl = loader.loadClass(fqn);
    assert impl != null;
    var arr = ProxyArray.fromArray(hostname, "" + port);
    impl.invokeMember("main", arr);
    return loader;
  }
}
