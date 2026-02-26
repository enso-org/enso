package org.enso.interpreter.runtime.system;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.io.TruffleProcessBuilder;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import org.apache.commons.lang3.SystemUtils;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomNewInstanceNode;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapToVectorNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCoerceToArrayNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;

public class System {

  private static final Text LINUX = Text.create("linux");
  private static final Text MACOS = Text.create("macos");
  private static final Text WINDOWS = Text.create("windows");
  private static final Text UNKNOWN = Text.create("unknown");

  @Builtin.Method(description = "Get the type of operating system.", autoRegister = false)
  @CompilerDirectives.TruffleBoundary
  public static Text os() {
    if (SystemUtils.IS_OS_LINUX) return LINUX;
    if (SystemUtils.IS_OS_MAC_OSX) return MACOS;
    if (SystemUtils.IS_OS_WINDOWS) return WINDOWS;
    return UNKNOWN;
  }

  @Builtin.Method(description = "Check if the operating system is UNIX.", autoRegister = false)
  @CompilerDirectives.TruffleBoundary
  public static boolean is_unix() {
    return SystemUtils.IS_OS_UNIX;
  }

  @Builtin.Method(description = "Gets the nanosecond resolution system time.", autoRegister = false)
  @CompilerDirectives.TruffleBoundary
  public static long nanoTime() {
    return java.lang.System.nanoTime();
  }

  @Builtin.Specialize
  @Builtin.Method(
      description = "Exits the process, returning the provided code.",
      autoRegister = false)
  @CompilerDirectives.TruffleBoundary
  public static void exit(long code, @Cached ExpectStringNode expectStringNode) {
    // expectStringNode is an artificial Node just to provide a location for
    // the exception
    throw new ExitException((int) code, expectStringNode);
  }

  @Builtin.Specialize
  @Builtin.Method(
      description = "Create a system process, returning the exit code.",
      autoRegister = false)
  @Builtin.WrapException(from = IOException.class)
  @Builtin.WrapException(from = InterruptedException.class)
  @CompilerDirectives.TruffleBoundary
  @ExplodeLoop
  public static Atom create_process(
      EnsoContext ctx,
      Object command,
      Object arguments,
      Object input,
      boolean redirectIn,
      boolean redirectOut,
      boolean redirectErr,
      Object cwdOrNothing,
      Object envOrNothing,
      @Cached ArrayLikeCoerceToArrayNode coerce,
      @Cached ExpectStringNode expectStringNode)
      throws IOException, InterruptedException {
    var arrArguments = coerce.execute(arguments);
    var cmd = new String[arrArguments.length + 1];
    cmd[0] = expectStringNode.execute(command);
    for (int i = 1; i <= arrArguments.length; i++) {
      cmd[i] = expectStringNode.execute(arrArguments[i - 1]);
    }
    TruffleProcessBuilder pb = ctx.newProcessBuilder(cmd);
    if (ctx.getNothing() != cwdOrNothing) {
      var path = expectStringNode.execute(cwdOrNothing);
      var tPath = ctx.getPublicTruffleFile(path);
      pb.directory(tPath);
    }

    if (envOrNothing instanceof EnsoHashMap env) {
      var vectorOfPairs = HashMapToVectorNode.getUncached().execute(env);
      var len = ArrayLikeLengthNode.getUncached().executeLength(vectorOfPairs);
      for (var i = 0L; i < len; i++) {
        try {
          var pair = ArrayLikeAtNode.getUncached().executeAt(vectorOfPairs, i);
          var key = ArrayLikeAtNode.getUncached().executeAt(pair, 0);
          var value = ArrayLikeAtNode.getUncached().executeAt(pair, 1);

          var strKey = expectStringNode.execute(key);
          var strValue = expectStringNode.execute(value);
          pb.environment(strKey, strValue);
        } catch (InvalidArrayIndexException ex) {
          throw ctx.raiseAssertionPanic(expectStringNode, null, ex);
        }
      }
    }

    var p = pb.start();
    var in = new ByteArrayInputStream(expectStringNode.execute(input).getBytes());
    var out = new ByteArrayOutputStream();
    var err = new ByteArrayOutputStream();

    boolean startedWritingtoOut = false;
    try (OutputStream processIn = p.getOutputStream()) {
      InputStream stdin;
      if (redirectIn) {
        stdin = ctx.getIn();
      } else {
        stdin = in;
      }
      int nread;
      startedWritingtoOut = true;
      byte[] buf = new byte[8096];
      while (stdin.available() > 0 && (nread = stdin.read(buf)) != -1) {
        processIn.write(buf, 0, nread);
      }
    } catch (IOException e) {
      // Getting the output stream of a finished process results in an IOException.
      // We can ignore it at this point.
      // Unless this exception is from writing to buffer/reading from stdin.
      if (startedWritingtoOut) throw e;
    }

    // First read from stdout and stderr from the subprocess to prevent a deadlock.
    // In other words, call `p.waitFor()` after reading from the streams.
    // For more info, see https://stackoverflow.com/a/882795/4816269
    try (InputStream processOut = p.getInputStream()) {
      OutputStream stdout;
      if (redirectOut) {
        stdout = ctx.getOut();
      } else {
        stdout = out;
      }
      int nread;
      byte[] buf = new byte[8096];
      while ((nread = processOut.read(buf)) != -1) {
        stdout.write(buf, 0, nread);
      }
    }

    try (InputStream processErr = p.getErrorStream()) {
      OutputStream stderr;
      if (redirectErr) {
        stderr = ctx.getErr();
      } else {
        stderr = err;
      }
      int nread;
      byte[] buf = new byte[8096];
      while ((nread = processErr.read(buf)) != -1) {
        stderr.write(buf, 0, nread);
      }
    }

    p.waitFor();
    var exitCode = p.exitValue();
    var returnOut = Text.create(out.toString());
    var returnErr = Text.create(err.toString());

    var system = ctx.getTopScope().getModule("Standard.Base.System").get().getScope();
    var type = system.getType("System_Process_Result", true);
    var cons = type.getSingleConstructor();
    var result =
        AtomNewInstanceNode.getUncached().newInstance(cons, exitCode, returnOut, returnErr);
    return result;
  }
}
