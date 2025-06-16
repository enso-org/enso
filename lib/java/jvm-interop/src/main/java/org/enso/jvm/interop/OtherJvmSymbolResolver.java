package org.enso.jvm.interop;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import org.enso.common.HostEnsoUtils;
import org.enso.common.PolyglotSymbolResolver;
import org.enso.jvm.channel.Channel;
import org.enso.jvm.channel.JVM;

/** Resolves symbols via interop messages to the "other" HotSpot JVM. */
@org.openide.util.lookup.ServiceProvider(service = PolyglotSymbolResolver.class)
public final class OtherJvmSymbolResolver extends PolyglotSymbolResolver {
  private Channel<OtherJvmPool> channel;

  private Channel<OtherJvmPool> getChannel() throws URISyntaxException, IOException {
    if (channel == null) {
      channel = initializeChannel();
    }
    return channel;
  }

  @Override
  protected Object handleLoadClass(String name) throws ClassNotFoundException {
    try {
      var ch = getChannel();
      var result = ch.execute(OtherJvmResult.class, new OtherJvmMessage.LoadClass(name));
      return result.value();
    } catch (IOException | URISyntaxException ex) {
      throw new ClassNotFoundException(name, ex);
    }
  }

  @Override
  protected void handleAddToClassPath(URL url) {
    try {
      getChannel().execute(Void.class, new OtherJvmMessage.AddToClassPath(url.toString()));
    } catch (URISyntaxException | IOException ex) {
      ex.printStackTrace();
    }
  }

  private Channel<OtherJvmPool> initializeChannel() throws IOException, URISyntaxException {
    var jvm =
        HostEnsoUtils.isAot()
            ? // normally we run in AOT mode
            initializeJvm() // then create HotSpot JVM
            : // but for debugging purposes we can also
            null; // emulate the connection in a single JVM
    return Channel.create(jvm, OtherJvmPool.class);
  }

  private JVM initializeJvm() throws IOException, URISyntaxException {
    var home = System.getProperty("java.home");
    if (home == null) {
      throw new IOException("No java.home specified");
    }
    var javaHome = new File(home);
    if (!javaHome.exists()) {
      throw new IOException("JVM doesn't exists: " + javaHome);
    }
    var loc = getClass().getProtectionDomain().getCodeSource().getLocation();
    var component = new File(loc.toURI().resolve("..")).getAbsoluteFile();
    if (!component.getName().equals("component")) {
      component = new File(component, "component");
    }
    var commandAndArgs = new ArrayList<String>();
    var assertsOn = false;
    assert assertsOn = true;
    if (assertsOn) {
      commandAndArgs.add("-ea");
    }
    commandAndArgs.add("--sun-misc-unsafe-memory-access=allow");
    commandAndArgs.add("-Dpolyglot.engine.WarnInterpreterOnly=false");
    commandAndArgs.add("-Dtruffle.UseFallbackRuntime=true");
    commandAndArgs.add("--enable-native-access=org.graalvm.truffle");
    commandAndArgs.add("--enable-native-access=org.enso.jvm.channel");
    commandAndArgs.add("--add-opens=java.base/java.nio=ALL-UNNAMED");
    if (!component.isDirectory()) {
      throw new IOException("Cannot find " + component + " directory");
    }
    commandAndArgs.add("--module-path=" + component.getPath());
    commandAndArgs.add("-Djdk.module.main=org.enso.jvm.interop");
    return JVM.create(javaHome, commandAndArgs.toArray(new String[0]));
  }
}
