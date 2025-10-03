package org.enso.jvm.interop.api;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import org.enso.jvm.channel.Channel;
import org.enso.jvm.channel.JVM;
import org.enso.jvm.interop.impl.OtherJvmMessage;
import org.enso.jvm.interop.impl.OtherJvmPool;
import org.enso.jvm.interop.impl.OtherJvmResult;

/**
 * Class responsible for loading Java classes from <em>other JVM</em> connected via a {@link
 * Channel}.
 */
@ExportLibrary(InteropLibrary.class)
public final class OtherJvmClassLoader implements TruffleObject {
  private final Channel<OtherJvmPool> channel;

  private OtherJvmClassLoader(Channel<OtherJvmPool> ch) {
    this.channel = ch;
  }

  /**
   * Creates instance of the class loader.
   *
   * @param language the language to associate objects loaded by this loader with
   * @param otherJvm normally we run in AOT mode but for debugging purposes we can also emulate the
   *     connection in a single JVM
   * @param ctx own context to execute code in
   * @return new instance of the class loader
   * @throws IOException
   * @throws URISyntaxException
   */
  public static OtherJvmClassLoader create(
      Class<? extends TruffleLanguage> language, boolean otherJvm, TruffleContext ctx)
      throws IOException, URISyntaxException {
    var jvm = otherJvm ? initializeJvm() : null;
    var ch = Channel.create(jvm, OtherJvmPool.class);
    var pool = ch.getConfig();
    pool.onEnterLeave(language, ctx::enter, ctx::leave);
    return new OtherJvmClassLoader(ch);
  }

  @ExportMessage
  final boolean hasMembers() {
    return true;
  }

  @ExportMessage
  boolean isMemberReadable(String member) {
    return true;
  }

  @ExportMessage
  boolean isMemberInvocable(String member) {
    return "addPath".equals(member);
  }

  @ExportMessage
  final Object getMembers(boolean includeInternal) {
    return this;
  }

  @ExportMessage
  final TruffleObject readMember(String name) throws UnknownIdentifierException {
    try {
      return loadClass(name);
    } catch (ClassNotFoundException ex) {
      throw UnknownIdentifierException.create(name, ex);
    }
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final TruffleObject invokeMember(String name, Object[] args)
      throws UnknownIdentifierException, UnsupportedMessageException, UnsupportedTypeException {
    switch (name) {
      case "addPath" -> {
        var path = InteropLibrary.getUncached().asString(args[0]);
        channel.execute(Void.class, new OtherJvmMessage.AddToClassPath(path));
      }
      case "findLibraries" -> {
        if (args[0] instanceof TruffleObject obj) {
          channel.execute(Void.class, new OtherJvmMessage.FindLibraries(obj));
        } else {
          throw UnsupportedTypeException.create(args);
        }
      }
      default -> throw UnknownIdentifierException.create(name);
    }
    return this;
  }

  @CompilerDirectives.TruffleBoundary
  private final TruffleObject loadClass(String name) throws ClassNotFoundException {
    var result = channel.execute(OtherJvmResult.class, new OtherJvmMessage.LoadClass(name));
    return result.value(null);
  }

  private static JVM initializeJvm() throws IOException, URISyntaxException {
    var home = System.getProperty("java.home");
    if (home == null) {
      throw new IOException("No java.home specified");
    }
    var javaHome = new File(home);
    if (!javaHome.exists()) {
      throw new IOException("JVM doesn't exists: " + javaHome);
    }
    var loc = OtherJvmClassLoader.class.getProtectionDomain().getCodeSource().getLocation();
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
