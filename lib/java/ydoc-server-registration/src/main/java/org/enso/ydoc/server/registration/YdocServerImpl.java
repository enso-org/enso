package org.enso.ydoc.server.registration;

import java.io.IOException;
import java.net.URISyntaxException;
import org.enso.jvm.interop.api.OtherJvmClassLoader;
import org.enso.runner.common.WrongOption;
import org.enso.runner.common.YdocServerApi;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.polyglot.Context;
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
    var ctx = Context.create("hosted");
    var loader = OtherJvmClassLoader.create("org.enso.ydoc.server", null, isAot, null);
    var loadValue = ctx.asValue(loader);
    var fqn = "org.enso.ydoc.server.DualMain";
    var impl = loadValue.getMember(fqn);
    assert impl != null;
    Object arr = ProxyArray.fromArray(hostname, "" + port);
    impl.invokeMember("main", arr);
    return () -> {
      loadValue.invokeMember("close");
      ctx.close();
    };
  }
}
