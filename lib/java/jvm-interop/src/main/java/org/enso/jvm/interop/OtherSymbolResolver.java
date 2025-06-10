package org.enso.jvm.interop;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Properties;
import org.enso.common.HostEnsoUtils;
import org.enso.common.PolyglotSymbolResolver;
import org.enso.jvm.channel.Channel;
import org.enso.jvm.channel.JVM;

/** Resolves symbols via interop messages to the "other" HotSpot JVM. */
@org.openide.util.lookup.ServiceProvider(service = PolyglotSymbolResolver.class)
public final class OtherSymbolResolver extends PolyglotSymbolResolver {
  private Channel jvm;

  @Override
  protected Object handleLoadClass(String name) throws ClassNotFoundException {
    if (!HostEnsoUtils.isAot()) {
      throw new ClassNotFoundException("Only works in AOT mode!");
    }
    if (jvm == null) {
      try {
        jvm = initializeChannel();
      } catch (IOException | URISyntaxException ex) {
        throw new ClassNotFoundException("Cannot initialize JVM", ex);
      }
    }
    var result = jvm.execute(OtherResult.class, new OtherMessage.LoadClass(name));
    return result.value();
  }

  private Channel initializeChannel() throws IOException, URISyntaxException {
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
    Properties props = null;
    if (props != null) {
      for (var e : props.entrySet()) {
        commandAndArgs.add("-D" + e.getKey() + "=" + e.getValue());
      }
    }
    commandAndArgs.add("--sun-misc-unsafe-memory-access=allow");
    commandAndArgs.add("--enable-native-access=org.graalvm.truffle");
    commandAndArgs.add("--add-opens=java.base/java.nio=ALL-UNNAMED");
    if (!component.isDirectory()) {
      throw new IOException("Cannot find " + component + " directory");
    }
    commandAndArgs.add("--module-path=" + component.getPath());
    commandAndArgs.add("-Djdk.module.main=org.enso.jvm.interop");
    var jvm = JVM.create(javaHome, commandAndArgs.toArray(new String[0]));
    return Channel.create(jvm, Persistables.class);
  }
}
