package org.enso.ydoc.server;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.concurrent.ExecutionException;
import org.enso.jvm.interop.api.OtherJvmClassLoader;
import org.enso.runner.common.WrongOption;
import org.enso.runner.common.YdocServerApi;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.proxy.ProxyArray;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@org.openide.util.lookup.ServiceProvider(service = YdocServerApi.class)
public final class YdocServerImpl extends YdocServerApi {
  private static final Logger log = LoggerFactory.getLogger(YdocServerImpl.class);

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
    var fqn = YdocServerImpl.class.getName();
    var impl = loadValue.getMember(fqn);
    assert impl != null;
    Object arr = ProxyArray.fromArray(hostname, "" + port);
    impl.invokeMember("main", arr);
    return () -> {
      loadValue.invokeMember("close");
      ctx.close();
    };
  }

  public static void main(String[] args) throws WrongOption, IOException {
    if (args.length != 2) {
      throw new IOException("Usage: java org.enso.ydoc.server.YdocServerImpl hostname port");
    }
    var hostname = args[0];
    var port = Integer.parseInt(args[1]);
    launch(hostname, port);
  }

  private static AutoCloseable launch(String hostname, int port) throws WrongOption, IOException {
    try {
      var then = System.currentTimeMillis();
      var ydoc = Ydoc.builder().hostname(hostname).port(port).build();
      ydoc.start();
      var now = System.currentTimeMillis();
      log.warn("Ydoc server at {}:{} started in {} ms", hostname, port, now - then);
      System.err.printf("Ydoc server at %s:%d started in %d ms\n", hostname, port, now - then);
      return ydoc;
    } catch (ExecutionException | InterruptedException ex) {
      throw new IOException(ex);
    }
  }
}
