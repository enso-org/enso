package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.CopyOption;
import java.nio.file.FileSystemException;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.NotDirectoryException;
import java.nio.file.OpenOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;
import java.util.function.IntFunction;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/**
 * A wrapper for {@link TruffleFile} objects exposed to the language. For methods documentation
 * please refer to {@link TruffleFile}.
 */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "io", name = "File", stdlibName = "Standard.Base.System.File.File")
public final class EnsoFile implements EnsoObject {
  private final TruffleFile truffleFile;

  public EnsoFile(TruffleFile truffleFile) {
    if (truffleFile == null) {
      throw CompilerDirectives.shouldNotReachHere();
    }
    this.truffleFile = truffleFile;
  }

  @Builtin.Method(name = "output_stream_builtin")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.ReturningGuestObject
  @Builtin.Specialize
  @CompilerDirectives.TruffleBoundary
  public OutputStream outputStream(Object opts, EnsoContext ctx) throws IOException {
    OpenOption[] openOptions =
        convertInteropArray(opts, InteropLibrary.getUncached(), ctx, OpenOption[]::new);
    return this.truffleFile.newOutputStream(openOptions);
  }

  @Builtin.Method(name = "input_stream_builtin")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.Specialize
  @Builtin.ReturningGuestObject
  @CompilerDirectives.TruffleBoundary
  public InputStream inputStream(Object opts, EnsoContext ctx) throws IOException {
    OpenOption[] openOptions =
        convertInteropArray(opts, InteropLibrary.getUncached(), ctx, OpenOption[]::new);
    return this.truffleFile.newInputStream(openOptions);
  }

  @SuppressWarnings("unchecked")
  private static <T> T[] convertInteropArray(
      Object arr, InteropLibrary interop, EnsoContext ctx, IntFunction<T[]> hostArrayCtor) {
    if (!interop.hasArrayElements(arr)) {
      var vecType = ctx.getBuiltins().vector().getType();
      var typeError = ctx.getBuiltins().error().makeTypeError(vecType, arr, "opts");
      throw new PanicException(typeError, interop);
    }
    T[] hostArr;
    try {
      int size = Math.toIntExact(interop.getArraySize(arr));
      hostArr = hostArrayCtor.apply(size);
      for (int i = 0; i < size; i++) {
        Object elem = interop.readArrayElement(arr, i);
        if (!ctx.isJavaPolyglotObject(elem)) {
          var err =
              ctx.getBuiltins()
                  .error()
                  .makeUnsupportedArgumentsError(
                      new Object[] {arr},
                      "Arguments to opts should be host objects from java.io package");
          throw new PanicException(err, interop);
        }
        hostArr[i] = (T) ctx.asJavaPolyglotObject(elem);
      }
    } catch (ClassCastException | UnsupportedMessageException | InvalidArrayIndexException e) {
      throw EnsoContext.get(interop).raiseAssertionPanic(interop, null, e);
    }
    return hostArr;
  }

  @Builtin.Method(name = "read_last_bytes_builtin")
  @Builtin.WrapException(from = IOException.class)
  @CompilerDirectives.TruffleBoundary
  public EnsoObject readLastBytes(long n) throws IOException {
    try (SeekableByteChannel channel =
        this.truffleFile.newByteChannel(Set.of(StandardOpenOption.READ))) {
      int bytesToRead = Math.toIntExact(Math.min(channel.size(), n));
      channel.position(channel.size() - bytesToRead);
      ByteBuffer buffer = ByteBuffer.allocate(bytesToRead);
      while (buffer.hasRemaining()) {
        channel.read(buffer);
      }

      buffer.flip();
      return ArrayLikeHelpers.wrapBuffer(buffer);
    }
  }

  @Builtin.Method(name = "resolve")
  @Builtin.Specialize
  public EnsoFile resolve(String subPath) {
    return new EnsoFile(this.truffleFile.resolve(subPath));
  }

  @Builtin.Method
  public boolean exists() {
    return truffleFile.exists();
  }

  @Builtin.Method(name = "creation_time_builtin")
  @Builtin.WrapException(from = IOException.class)
  @CompilerDirectives.TruffleBoundary
  public EnsoDateTime getCreationTime() throws IOException {
    return new EnsoDateTime(
        ZonedDateTime.ofInstant(truffleFile.getCreationTime().toInstant(), ZoneOffset.UTC));
  }

  @Builtin.Method(name = "last_modified_time_builtin")
  @Builtin.WrapException(from = IOException.class)
  @CompilerDirectives.TruffleBoundary
  public EnsoDateTime getLastModifiedTime() throws IOException {
    return new EnsoDateTime(
        ZonedDateTime.ofInstant(truffleFile.getLastModifiedTime().toInstant(), ZoneOffset.UTC));
  }

  @Builtin.Method(name = "posix_permissions_builtin")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.ReturningGuestObject
  @CompilerDirectives.TruffleBoundary
  public Set<PosixFilePermission> getPosixPermissions() throws IOException {
    return truffleFile.getPosixPermissions();
  }

  @Builtin.Method(name = "parent")
  @CompilerDirectives.TruffleBoundary
  public EnsoObject getParent() {
    // Normalization is needed to correctly handle paths containing `..` and `.`.
    var parentOrNull = this.normalize().truffleFile.getParent();

    // If the path has no parent because it is relative and there are no more segments, try again
    // after making it absolute:
    if (parentOrNull == null && !this.truffleFile.isAbsolute()) {
      parentOrNull = this.truffleFile.getAbsoluteFile().normalize().getParent();
    }

    if (parentOrNull != null) {
      return new EnsoFile(parentOrNull);
    } else {
      var ctx = EnsoContext.get(null);
      return ctx.getBuiltins().nothing();
    }
  }

  @Builtin.Method(name = "absolute")
  @CompilerDirectives.TruffleBoundary
  public EnsoFile getAbsoluteFile() {
    return new EnsoFile(this.truffleFile.getAbsoluteFile());
  }

  @Builtin.Method(name = "path")
  @CompilerDirectives.TruffleBoundary
  public Text getPath() {
    return Text.create(this.truffleFile.getPath());
  }

  @Builtin.Method
  @CompilerDirectives.TruffleBoundary
  public boolean isAbsolute() {
    return this.truffleFile.isAbsolute();
  }

  @Builtin.Method
  @CompilerDirectives.TruffleBoundary
  public boolean isDirectory() {
    return this.truffleFile.isDirectory();
  }

  @Builtin.Method(name = "create_directory_builtin")
  @Builtin.WrapException(from = IOException.class)
  @CompilerDirectives.TruffleBoundary
  public void createDirectories() throws IOException {
    try {
      this.truffleFile.createDirectories();
    } catch (NoSuchFileException e) {
      System.out.println(e.getReason());
      throw replaceCreateDirectoriesNoSuchFileException(e);
    } catch (FileSystemException e) {
      throw replaceCreateDirectoriesGenericException(e);
    }
  }

  /**
   * This method detects if a more correct exception can be thrown instead of unrelated {@link
   * NoSuchFileException}.
   *
   * <p>On Windows `createDirectories` wrongly throws a {@link NoSuchFileException} instead of
   * {@link NotDirectoryException}, if a file on the parents path is not a directory.
   */
  private static FileSystemException replaceCreateDirectoriesNoSuchFileException(
      NoSuchFileException noSuchFileException) {
    var path = noSuchFileException.getFile();
    if (path == null) {
      return noSuchFileException;
    }

    var parent = fromString(EnsoContext.get(null), path).truffleFile.getParent();
    // Unknown parent, so the heuristic cannot be applied - return the original.
    if (parent == null) {
      return noSuchFileException;
    }

    // On Windows, when creating a directory tree `foo/my-file.txt/a/b/c`, the operation fails with
    // `NoSuchFileException` with path `foo/my-file.txt/a`. So the heuristic checks the path's
    // parent `foo/my-file.txt` if it exists but is not a directory that means we encountered this
    // edge case and the exception should be replaced.
    if (parent.exists() && !parent.isDirectory()) {
      return new NotDirectoryException(parent.getPath());
    } else {
      return noSuchFileException;
    }
  }

  /**
   * This method detects if a more specific exception can be thrown instead of generic {@link
   * FileSystemException}.
   *
   * <p>Apparently, on Linux `createDirectories` throws a generic {@link FileSystemException}
   * instead of the more fitting {@link NotDirectoryException}.
   */
  private static FileSystemException replaceCreateDirectoriesGenericException(
      FileSystemException genericException) {
    if (genericException.getReason().equals("Not a directory")) {
      var path = genericException.getFile();
      if (path == null) {
        return genericException;
      }

      // On Linux, when creating a directory tree `foo/my-file.txt/a/b/c`, the operation fails with
      // `FileSystemException` with the full path (`foo/my-file.txt/a/b/c`). So we need to traverse
      // this path to find the actually problematic part.
      var file = fromString(EnsoContext.get(null), path).truffleFile;
      // We try to find the first file that exists on the path.
      while (file != null && !file.exists()) {
        file = file.getParent();
      }

      if (file != null && !file.isDirectory()) {
        return new NotDirectoryException(file.getPath());
      } else {
        return genericException;
      }
    } else {
      return genericException;
    }
  }

  @Builtin.Method(name = "list_immediate_children_array")
  @Builtin.WrapException(from = IOException.class)
  @CompilerDirectives.TruffleBoundary
  public EnsoObject list() throws IOException {
    return ArrayLikeHelpers.wrapEnsoObjects(
        this.truffleFile.list().stream().map(EnsoFile::new).toArray(EnsoFile[]::new));
  }

  @Builtin.Method
  @CompilerDirectives.TruffleBoundary
  public EnsoFile relativize(EnsoFile other) {
    return new EnsoFile(this.truffleFile.relativize(other.truffleFile));
  }

  @Builtin.Method
  @CompilerDirectives.TruffleBoundary
  public boolean isRegularFile() {
    return this.truffleFile.isRegularFile();
  }

  @Builtin.Method
  @CompilerDirectives.TruffleBoundary
  public boolean isWritable() {
    return this.truffleFile.isWritable();
  }

  @Builtin.Method(name = "name")
  @CompilerDirectives.TruffleBoundary
  public Text getName() {
    var name = this.normalize().truffleFile.getName();
    return Text.create(name == null ? "/" : name);
  }

  @Builtin.Method(name = "size_builtin")
  @Builtin.WrapException(from = IOException.class)
  @CompilerDirectives.TruffleBoundary
  public long getSize() throws IOException {
    if (this.truffleFile.isDirectory()) {
      throw new IOException("size can only be called on files.");
    }
    return this.truffleFile.size();
  }

  @TruffleBoundary
  @Override
  public boolean equals(Object obj) {
    if (obj instanceof EnsoFile otherFile) {
      if (truffleFile.getPath().equals(otherFile.truffleFile.getPath())) {
        return true;
      } else {
        return truffleFile
            .getAbsoluteFile()
            .normalize()
            .getPath()
            .equals(otherFile.truffleFile.getAbsoluteFile().normalize().getPath());
      }
    } else {
      return false;
    }
  }

  @Builtin.Method
  @CompilerDirectives.TruffleBoundary
  public EnsoFile normalize() {
    TruffleFile simplyNormalized = truffleFile.normalize();
    String name = simplyNormalized.getName();
    boolean needsAbsolute = name != null && (name.equals("..") || name.equals("."));
    if (needsAbsolute) {
      simplyNormalized = simplyNormalized.getAbsoluteFile().normalize();
    }
    return new EnsoFile(simplyNormalized);
  }

  @Builtin.Method(name = "delete_builtin")
  @Builtin.WrapException(from = IOException.class)
  @CompilerDirectives.TruffleBoundary
  public void delete(boolean recursive) throws IOException {
    if (recursive && truffleFile.isDirectory(LinkOption.NOFOLLOW_LINKS)) {
      deleteRecursively(truffleFile);
    } else {
      truffleFile.delete();
    }
  }

  private void deleteRecursively(TruffleFile file) throws IOException {
    if (file.isDirectory(LinkOption.NOFOLLOW_LINKS)) {
      for (TruffleFile child : file.list()) {
        deleteRecursively(child);
      }
    }
    file.delete();
  }

  @Builtin.Method(name = "copy_builtin", description = "Copy this file to a target destination")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.Specialize
  @CompilerDirectives.TruffleBoundary
  public void copy(EnsoFile target, Object options, EnsoContext ctx) throws IOException {
    CopyOption[] copyOptions =
        convertInteropArray(options, InteropLibrary.getUncached(), ctx, CopyOption[]::new);
    truffleFile.copy(target.truffleFile, copyOptions);
  }

  @Builtin.Method(name = "move_builtin", description = "Move this file to a target destination")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.Specialize
  @CompilerDirectives.TruffleBoundary
  public void move(EnsoFile target, Object options, EnsoContext ctx) throws IOException {
    CopyOption[] copyOptions =
        convertInteropArray(options, InteropLibrary.getUncached(), ctx, CopyOption[]::new);
    truffleFile.move(target.truffleFile, copyOptions);
  }

  @Builtin.Method
  @CompilerDirectives.TruffleBoundary
  public boolean startsWith(EnsoFile parent) {
    return truffleFile.startsWith(parent.truffleFile);
  }

  @Builtin.Method(
      name = "get_file",
      description =
          "Takes the text representation of a path and returns a TruffleFile corresponding to it.",
      autoRegister = false)
  @Builtin.Specialize
  @CompilerDirectives.TruffleBoundary
  public static EnsoFile fromString(EnsoContext context, String path) {
    TruffleFile file = context.getPublicTruffleFile(path);
    return new EnsoFile(file);
  }

  @Builtin.Method(
      name = "get_cwd",
      description = "A file corresponding to the current working directory.",
      autoRegister = false)
  @Builtin.Specialize
  @CompilerDirectives.TruffleBoundary
  public static EnsoFile currentDirectory(EnsoContext context) {
    TruffleFile file = context.getCurrentWorkingDirectory();
    return new EnsoFile(file);
  }

  @Builtin.Method(
      name = "home",
      description = "Gets the user's system-defined home directory.",
      autoRegister = false)
  @Builtin.Specialize
  @CompilerDirectives.TruffleBoundary
  public static EnsoFile userHome(EnsoContext context) {
    return fromString(context, System.getProperty("user.home"));
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return "(File " + truffleFile.getPath() + ")";
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().file();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@Bind("$node") Node node) {
    return EnsoContext.get(node).getBuiltins().file();
  }
}
