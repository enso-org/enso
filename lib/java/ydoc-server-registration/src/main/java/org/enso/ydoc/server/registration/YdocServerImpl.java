package org.enso.ydoc.server.registration;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import org.enso.jvm.interop.api.OtherJvmClassLoader;
import org.enso.ydoc.api.YdocServerApi;
import org.enso.ydoc.api.YjsChannel;
import org.graalvm.nativeimage.ImageInfo;

public final class YdocServerImpl extends YdocServerApi {
  public YdocServerImpl() {}

  @Override
  protected AutoCloseable runYdocServer(
      String hostname,
      int port,
      YjsChannel.Server jsonChannelCallbacks,
      YjsChannel.Server binaryChannelCallbacks)
      throws IOException, URISyntaxException {
    // the following shall invoke:
    //   return launch(hostname, port);
    // but in the other JVM
    var isAot = ImageInfo.inImageRuntimeCode();
    var loader = OtherJvmClassLoader.create("org.enso.ydoc.server", null, null, isAot, null, null);
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
    impl.invokeMember("launch", hostname, port + "", jsonChannelCallbacks, binaryChannelCallbacks);
    return loader;
  }
}
