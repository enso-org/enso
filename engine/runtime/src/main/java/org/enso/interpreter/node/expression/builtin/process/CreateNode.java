package org.enso.interpreter.node.expression.builtin.process;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.PanicException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;

@BuiltinMethod(
    type = "Process",
    name = "create",
    description = "Create a system process, returning the exit code.")
public abstract class CreateNode extends Node {

  static CreateNode build() {
    return CreateNodeGen.create();
  }

  abstract Object execute(
      Object _this, Vector commands, boolean redirectIn, boolean redirectOut, boolean redirectErr);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  long doCreate(
      Object _this,
      Vector commands,
      boolean redirectIn,
      boolean redirectOut,
      boolean redirectErr,
      @CachedContext(Language.class) Context ctx) {
    String[] command =
        Arrays.copyOf(commands.getItems(), (int) commands.getArraySize(), String[].class);
    ProcessBuilder pb = new ProcessBuilder(command);
    try {
      Process p = pb.start();

      if (redirectIn) {
        try (OutputStream processin = p.getOutputStream()) {
          InputStream stdin = ctx.getIn();
          int nread;
          byte[] buf = new byte[8096];
          while (stdin.available() > 0 && (nread = stdin.read(buf)) != -1) {
            processin.write(buf, 0, nread);
          }
        }
      }

      p.waitFor();

      if (redirectOut) {
        try (InputStream processout = p.getInputStream()) {
          int nread;
          byte[] buf = new byte[8096];
          while ((nread = processout.read(buf)) != -1) {
            ctx.getOut().write(buf, 0, nread);
          }
        }
      }

      if (redirectErr) {
        try (InputStream processerr = p.getErrorStream()) {
          int nread;
          byte[] buf = new byte[8096];
          while ((nread = processerr.read(buf)) != -1) {
            ctx.getErr().write(buf, 0, nread);
          }
        }
      }

      return p.exitValue();
    } catch (IOException | InterruptedException e) {
      throw new PanicException(e.getMessage(), this);
    }
  }
}
