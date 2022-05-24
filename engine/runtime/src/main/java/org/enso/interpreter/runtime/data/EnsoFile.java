package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.OpenOption;

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

  public <T> T withTruffleFile(FunctionThrowingIOException<TruffleFile, T> fun) throws IOException {
    return fun.apply(truffleFile);
  }

  public TruffleFile resolveUnderlying(String subPath) {
    return this.truffleFile.resolve(subPath);
  }

  public TruffleFile resolveUnderlying(EnsoFile subPath) {
    return this.truffleFile.resolve(subPath.truffleFile.getPath());
  }

  @Builtin.Method
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class, propagate = true)
  @Builtin.ReturningGuestObject
  public OutputStream outputStream(OpenOption[] opts) throws IOException {
    return this.withTruffleFile(f -> f.newOutputStream(opts));
  }

  @Builtin.Method
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class, propagate = true)
  @Builtin.ReturningGuestObject
  public InputStream inputStream(OpenOption[] opts) throws IOException {
    return this.withTruffleFile(f -> f.newInputStream(opts));
  }

  // TODO
  // @Builtin.Method
  abstract class Resolve extends Node {
    public EnsoFile resolve(EnsoFile _this, String subPath) {
      return new EnsoFile(_this.resolveUnderlying(subPath));
    }

    public EnsoFile resolve(EnsoFile _this, EnsoFile subPath) {
      return new EnsoFile(_this.resolveUnderlying(subPath.truffleFile.getPath()));
    }
  }

  @Builtin.Method
  public boolean exists() {
    return truffleFile.exists();
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

  @Builtin.Method(name = "list_immediate_children")
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

  @Builtin.Method
  public boolean isEqual(EnsoFile that) {
    return this.truffleFile.equals(that.truffleFile);
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

  @Builtin.Method
  public boolean startsWith(EnsoFile parent) {
    return truffleFile.startsWith(parent.truffleFile);
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
        EnsoFile _this,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("doResolve(cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(EnsoFile _this, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }
}
