package org.enso.jvm.interop.api;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Function;
import org.enso.jvm.channel.Channel;
import org.enso.jvm.channel.JVM;
import org.enso.jvm.interop.impl.OtherJvmMessage;
import org.enso.jvm.interop.impl.OtherJvmPool;
import org.enso.jvm.interop.impl.OtherJvmResult;
import org.enso.jvm.interop.impl.OtherJvmUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Value;

/**
 * Responsible for loading Java classes from <em>other JVM</em> connected via a {@link Channel}.
 * Provides basic methods for direct configuration, but also exposes its functionality with {@link
 * TruffleObject} messages.
 */
@ExportLibrary(InteropLibrary.class)
public final class OtherJvmClassLoader implements TruffleObject, AutoCloseable {
  private final Channel<OtherJvmPool> channel;
  private Context ctx;

  private OtherJvmClassLoader(Channel<OtherJvmPool> ch) {
    this.channel = ch;
  }

  /**
   * Creates instance of the class loader.
   *
   * @param jvm the "other" JVM to load classes from (can be {@code null} to mock the system inside
   *     of the existing JVM)
   * @return new instance of the class loader from the provided JVM
   */
  public static OtherJvmClassLoader create(JVM jvm) {
    return createImpl(jvm, null, null, null);
  }

  /**
   * Creates instance of the class loader.
   *
   * @param mainModule name of the main module to initialize
   * @param language the language to associate objects loaded by this loader with
   * @param polyglotBindings function to find polyglot context of a language
   * @param otherJvm normally we run in AOT mode but for debugging purposes we can also emulate the
   *     connection in a single JVM - pass in value of TruffleOptions.AOT or equivalent
   * @param ctx own context to execute code in
   * @param onlyWithModules only enable given module names (can be {@code null})
   * @return new instance of the class loader
   * @throws IOException
   * @throws URISyntaxException
   */
  public static OtherJvmClassLoader create(
      String mainModule,
      Class<? extends TruffleLanguage> language,
      Function<String, Object> polyglotBindings,
      boolean otherJvm,
      TruffleContext ctx,
      Set<String> onlyWithModules)
      throws IOException, URISyntaxException {
    var jvm = otherJvm ? initializeJvm(mainModule, onlyWithModules) : null;
    return createImpl(jvm, polyglotBindings, ctx, language);
  }

  /**
   * Adds provided directory to the classpath.
   *
   * @param dir directory to add to classpath
   */
  public final void addPath(File dir) {
    addPath(dir.getAbsolutePath());
  }

  /**
   * Loads a class as a value.
   *
   * @param fqn fully qualified name of class to load
   * @return
   */
  public final Value loadClass(String fqn) {
    try {
      var rawClass = loadRawClass(fqn);
      if (ctx == null) {
        ctx =
            Context.newBuilder("hosted")
                .allowHostAccess(HostAccess.ALL)
                .allowExperimentalOptions(true)
                .build();
      }
      return ctx.asValue(rawClass);
    } catch (ClassNotFoundException ex) {
      throw new IllegalArgumentException(ex);
    }
  }

  /** Closes the loader. Closes associated channel and/or context. */
  @Override
  public final void close() {
    try {
      try {
        channel.getConfig().close(channel);
      } finally {
        if (ctx != null) {
          ctx.close();
        }
      }
    } catch (AbstractTruffleException ex) {
      throw ex;
    } catch (Exception ex) {
      throw new org.enso.jvm.interop.impl.OtherJvmException(ex);
    }
  }

  private static OtherJvmClassLoader createImpl(
      JVM jvm,
      Function<String, Object> polyglotBindings,
      TruffleContext ctx,
      Class<? extends TruffleLanguage> language) {
    var ch = Channel.create(jvm, OtherJvmPool.class);
    var pool = ch.getConfig();
    Function<Node, Object> enter = ctx != null ? ctx::enter : null;
    BiConsumer<Node, Object> leave = ctx != null ? ctx::leave : null;
    pool.onEnterLeave(language, polyglotBindings, enter, leave);
    return new OtherJvmClassLoader(ch);
  }

  private void addPath(String path) {
    channel.execute(Void.class, new OtherJvmMessage.AddToClassPath(path));
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
    return "addPath".equals(member) || "findLibraries".equals(member) || "close".equals(member);
  }

  @ExportMessage
  final Object getMembers(boolean includeInternal) {
    return this;
  }

  @ExportMessage
  final TruffleObject readMember(String name) throws UnknownIdentifierException {
    try {
      return loadRawClass(name);
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
        addPath(path);
      }
      case "findLibraries" -> {
        if (args[0] instanceof TruffleObject obj) {
          channel.execute(Void.class, new OtherJvmMessage.FindLibraries(obj));
        } else {
          throw UnsupportedTypeException.create(args);
        }
      }
      case "close" -> {
        close();
      }
      default -> throw UnknownIdentifierException.create(name);
    }
    return this;
  }

  @CompilerDirectives.TruffleBoundary
  private final TruffleObject loadRawClass(String name) throws ClassNotFoundException {
    var result = channel.execute(OtherJvmResult.class, new OtherJvmMessage.LoadClass(name));
    return result.value(null);
  }

  private static JVM initializeJvm(String mainModule, Set<String> moduleNamesOrNull)
      throws IOException, URISyntaxException {
    var libFile = findDynamicLibrary(mainModule);
    if (libFile.exists()) {
      return JVM.create(libFile);
    } else {
      return initializeHotSpotJVM(mainModule, moduleNamesOrNull);
    }
  }

  private static JVM initializeHotSpotJVM(String mainModule, Set<String> moduleNamesOrNull)
      throws IOException, URISyntaxException {
    var home = System.getProperty("java.home");
    if (home == null) {
      throw new IOException("No java.home specified");
    }
    var javaHome = new File(home);
    var args = OtherJvmUtils.findJvmArgs(javaHome, mainModule, moduleNamesOrNull);
    return JVM.create(javaHome, args);
  }

  private static File findDynamicLibrary(String name) {
    var dir = OtherJvmUtils.findModules();
    var ext =
        switch (org.enso.common.Platform.getOperatingSystem()) {
          case LINUX -> ".so";
          case MACOS -> ".dylib";
          case WINDOWS -> ".dll";
        };
    var file = new File(dir, name + ext);
    return file;
  }
}
