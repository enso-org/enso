package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.CopyOption;
import java.nio.file.OpenOption;
import java.time.ZonedDateTime;
import java.time.ZoneOffset;

/**
 * A wrapper for {@link TruffleFile} objects exposed to the language. For methods documentation
 * please refer to {@link TruffleFile}.
 */
@ExportLibrary(MethodDispatchLibrary.class)
@Builtin(pkg = "io", name = "File", stdlibName = "Standard.Base.System.File.File")
public class EnsoFile implements TruffleObject {
  private final TruffleFile truffleFile;

  public EnsoFile(TruffleFile truffleFile) {
    this.truffleFile = truffleFile;
  }

  @Builtin.Method
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class, propagate = true)
  @Builtin.ReturningGuestObject
  public OutputStream outputStream(OpenOption[] opts) throws IOException {
    return this.truffleFile.newOutputStream(opts);
  }

  @Builtin.Method
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class, propagate = true)
  @Builtin.ReturningGuestObject
  public InputStream inputStream(OpenOption[] opts) throws IOException {
    return this.truffleFile.newInputStream(opts);
  }

  @Builtin.Method(name = "resolve")
  @Builtin.Specialize
  public EnsoFile resolve(String subPath) {
    return new EnsoFile(this.truffleFile.resolve(subPath));
  }

  @Builtin.Method(name = "resolve")
  @Builtin.Specialize
  public EnsoFile resolve(EnsoFile subPath) {
    return new EnsoFile(this.truffleFile.resolve(subPath.truffleFile.getPath()));
  }

  @Builtin.Method
  public boolean exists() {
    return truffleFile.exists();
  }

  @Builtin.Method(name = "creation_time_builtin")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class, propagate = true)
  @Builtin.ReturningGuestObject
  public ZonedDateTime getCreationTime() throws IOException {
    return ZonedDateTime.ofInstant(truffleFile.getCreationTime().toInstant(), ZoneOffset.UTC);
  }

  @Builtin.Method(name = "last_modified_time_builtin")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class, propagate = true)
  @Builtin.ReturningGuestObject
  public ZonedDateTime getLastModifiedTime() throws IOException {
    return ZonedDateTime.ofInstant(truffleFile.getLastModifiedTime().toInstant(), ZoneOffset.UTC);
  }

  @Builtin.Method(name = "parent")
  public EnsoFile getParent() {
    return new EnsoFile(this.truffleFile.getParent());
  }

  @Builtin.Method(name = "absolute")
  public EnsoFile getAbsoluteFile() {
    return new EnsoFile(this.truffleFile.getAbsoluteFile());
  }

  @Builtin.Method(name = "path")
  public String getPath() {
    return this.truffleFile.getPath();
  }

  @Builtin.Method
  public boolean isAbsolute() {
    return this.truffleFile.isAbsolute();
  }

  @Builtin.Method
  public boolean isDirectory() {
    return this.truffleFile.isDirectory();
  }

  @Builtin.Method(name = "create_directory")
  public void createDirectories() {
    try {
      this.truffleFile.createDirectories();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  @Builtin.Method(name = "list_immediate_children_array")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class, propagate = true)
  public EnsoFile[] list() throws IOException {
    return this.truffleFile.list().stream().map(EnsoFile::new).toArray(EnsoFile[]::new);
  }

  @Builtin.Method
  public EnsoFile relativize(EnsoFile other) {
    return new EnsoFile(this.truffleFile.relativize(other.truffleFile));
  }

  @Builtin.Method
  public boolean isRegularFile() {
    return this.truffleFile.isRegularFile();
  }

  @Builtin.Method(name = "name")
  public String getName() {
    return this.truffleFile.getName();
  }

  @Builtin.Method(name = "==")
  public boolean isEqual(EnsoFile that) {
    // It seems that fsContext is not equal in files coming from stacktraces.
    // Once a solution to that is found replace it with a simple
    // return this.truffleFile.equals(that.truffleFile);
    return this.getPath().equals(that.getPath());
  }

  @Builtin.Method
  public EnsoFile normalize() {
    return new EnsoFile(truffleFile.normalize());
  }

  @Builtin.Method(name = "delete_builtin")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class, propagate = true)
  public void delete() throws IOException {
    truffleFile.delete();
  }

  @Builtin.Method(name = "copy_builtin", description = "Copy this file to a target destination")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class, propagate = true)
  public void copy(EnsoFile target, CopyOption[] options) throws IOException {
    truffleFile.copy(target.truffleFile, options);
  }

  @Builtin.Method(name = "move_builtin", description = "Move this file to a target destination")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class, propagate = true)
  public void move(EnsoFile target, CopyOption[] options) throws IOException {
    truffleFile.move(target.truffleFile, options);
  }

  @Builtin.Method
  public boolean startsWith(EnsoFile parent) {
    return truffleFile.startsWith(parent.truffleFile);
  }

  @Builtin.Method(
      name = "get_file",
      description =
          "Takes the text representation of a path and returns a TruffleFile corresponding to it.")
  @Builtin.Specialize
  public static EnsoFile fromString(Context context, String path) {
    TruffleFile file = context.getEnvironment().getPublicTruffleFile(path);
    return new EnsoFile(file);
  }

  @Builtin.Method(
      name = "get_cwd",
      description = "A file corresponding to the current working directory.")
  @Builtin.Specialize
  public static EnsoFile currentDirectory(Context context) {
    TruffleFile file = context.getEnvironment().getCurrentWorkingDirectory();
    return new EnsoFile(file);
  }

  @Builtin.Method(name = "home", description = "Gets the user's system-defined home directory.")
  @Builtin.Specialize
  public static EnsoFile userHome(Context context) {
    return fromString(context, System.getProperty("user.home"));
  }

  @Override
  public String toString() {
    return "(File " + truffleFile.getPath() + ")";
  }

  @ExportMessage
  boolean hasFunctionalDispatch() {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {

    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(UnresolvedSymbol symbol) {
      Context context = getContext();
      return symbol.resolveFor(context.getBuiltins().file(), context.getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        EnsoFile self,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("doResolve(cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(EnsoFile self, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }
}
