package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
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
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.NotDirectoryException;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.PosixFilePermissions;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.BuiltinObject;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.error.PanicException;

@ExportLibrary(InteropLibrary.class)
@Builtin(pkg = "io", name = "File", stdlibName = "Standard.Base.System.File.File")
public final class EnsoFile extends BuiltinObject {
  private final Path path;

  public EnsoFile(Path path) {
    if (path == null) {
      throw CompilerDirectives.shouldNotReachHere();
    }
    this.path = path;
  }

  @Override
  protected String builtinName() {
    return "File";
  }

  @Builtin.Method(name = "output_stream_builtin")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.Specialize
  @TruffleBoundary
  public static EnsoObject outputStream(
      EnsoFile file,
      Object opts,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode,
      EnsoContext ctx)
      throws IOException {
    var options = namesToValues(opts, lengthNode, atNode, ctx, StandardOpenOption::valueOf);
    var os = Files.newOutputStream(file.path, options.toArray(OpenOption[]::new));
    return new EnsoOutputStream(os);
  }

  @ExportLibrary(InteropLibrary.class)
  static final class EnsoOutputStream extends EnsoObject {
    private static final String[] MEMBERS = new String[] {"write", "flush", "close"};
    private final OutputStream os;

    EnsoOutputStream(OutputStream os) {
      this.os = os;
    }

    @ExportMessage
    boolean hasMembers() {
      return true;
    }

    @TruffleBoundary
    @ExportMessage
    boolean isMemberInvocable(String member) {
      return Arrays.asList(MEMBERS).contains(member);
    }

    @ExportMessage
    Object getMembers(boolean includeInternal) throws UnsupportedMessageException {
      return ArrayLikeHelpers.wrapStrings(MEMBERS);
    }

    @ExportMessage
    static Object invokeMember(
        EnsoOutputStream os,
        String name,
        Object[] args,
        @Cached ArrayLikeLengthNode lengthNode,
        @Cached ArrayLikeAtNode atNode,
        @CachedLibrary(limit = "3") InteropLibrary iop)
        throws ArityException, UnsupportedMessageException, UnknownIdentifierException {
      try {
        return switch (name) {
          case "write" -> {
            long from;
            long to;
            switch (args.length) {
              case 1 -> {
                from = 0;
                to = lengthNode.executeLength(args[0]);
              }
              case 3 -> {
                from = iop.asLong(args[1]);
                to = from + iop.asLong(args[2]);
              }
              default -> {
                throw ArityException.create(1, 3, args.length);
              }
            }
            var buf = new byte[8192];
            var at = 0;
            for (long i = from; i < to; i++) {
              var elem = atNode.executeAt(args[0], i);
              buf[at++] = iop.asByte(elem);
              if (at == buf.length) {
                os.write(buf, 0, buf.length);
                at = 0;
              }
            }
            if (at > 0) {
              os.write(buf, 0, at);
            }
            yield os;
          }
          case "flush" -> {
            os.flush();
            yield os;
          }
          case "close" -> {
            os.close();
            yield os;
          }
          default -> throw UnknownIdentifierException.create(name);
        };
      } catch (IOException ex) {
        throw raiseIOException(iop, ex);
      } catch (InvalidArrayIndexException ex) {
        var ctx = EnsoContext.get(iop);
        throw ctx.raiseAssertionPanic(iop, name, ex);
      }
    }

    @TruffleBoundary
    final void write(byte[] buf, int offset, int length) throws IOException {
      os.write(buf, offset, length);
    }

    @TruffleBoundary
    final void flush() throws IOException {
      os.flush();
    }

    @TruffleBoundary
    final void close() throws IOException {
      os.close();
    }

    @Override
    public String toString() {
      return "EnsoOutputStream";
    }

    @Override
    @ExportMessage
    public Object toDisplayString(boolean allowSideEffects) {
      return toString();
    }
  }

  @Builtin.Method(name = "input_stream_builtin")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.Specialize
  @TruffleBoundary
  public static EnsoObject inputStream(
      EnsoFile file,
      Object opts,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode,
      EnsoContext ctx)
      throws IOException {
    var options = namesToValues(opts, lengthNode, atNode, ctx, StandardOpenOption::valueOf);
    var is = Files.newInputStream(file.path, options.toArray(OpenOption[]::new));
    return new EnsoInputStream(is);
  }

  @ExportLibrary(InteropLibrary.class)
  static final class EnsoInputStream extends EnsoObject {
    private static final String[] MEMBERS =
        new String[] {
          "read", "readAllBytes", "readNBytes", "skipNBytes", "markSupported", "available", "close"
        };
    private final InputStream delegate;

    EnsoInputStream(InputStream is) {
      this.delegate = is;
    }

    @ExportMessage
    boolean hasMembers() {
      return true;
    }

    @TruffleBoundary
    @ExportMessage
    boolean isMemberInvocable(String member) {
      return Arrays.asList(MEMBERS).contains(member);
    }

    @ExportMessage
    Object getMembers(boolean includeInternal) throws UnsupportedMessageException {
      return ArrayLikeHelpers.wrapStrings(MEMBERS);
    }

    @ExportMessage
    @Override
    public Object toDisplayString(boolean allowSideEffects) {
      return "EnsoInputStream";
    }

    @TruffleBoundary(allowInlining = true)
    private int read() throws IOException {
      return delegate.read();
    }

    @TruffleBoundary(allowInlining = true)
    private byte[] readNBytes(int limit) throws IOException {
      return delegate.readNBytes(limit);
    }

    @TruffleBoundary(allowInlining = true)
    private ByteBuffer readNByteBuffer(int limit) throws IOException {
      return ByteBuffer.wrap(delegate.readNBytes(limit));
    }

    @TruffleBoundary(allowInlining = true)
    private ByteBuffer readAllBytes() throws IOException {
      return ByteBuffer.wrap(delegate.readAllBytes());
    }

    @TruffleBoundary(allowInlining = true)
    private void skipNBytes(long n) throws IOException {
      delegate.skipNBytes(n);
    }

    @TruffleBoundary(allowInlining = true)
    private boolean markSupported() throws IOException {
      return delegate.markSupported();
    }

    @TruffleBoundary(allowInlining = true)
    private void mark(int readlimit) throws IOException {
      delegate.mark(readlimit);
    }

    @TruffleBoundary(allowInlining = true)
    private void reset() throws IOException {
      delegate.reset();
    }

    @TruffleBoundary(allowInlining = true)
    private int available() throws IOException {
      return delegate.available();
    }

    @TruffleBoundary(allowInlining = true)
    private void close() throws IOException {
      delegate.close();
    }

    @ExportMessage
    static Object invokeMember(
        EnsoInputStream is,
        String name,
        Object[] args,
        @CachedLibrary(limit = "3") InteropLibrary iop)
        throws UnknownIdentifierException,
            UnsupportedMessageException,
            ArityException,
            UnsupportedTypeException {
      try {
        return switch (name) {
          case "read" -> {
            if (args.length == 0) {
              yield is.read();
            }
            long from;
            long to;
            switch (args.length) {
              case 1 -> {
                from = 0;
                to = iop.getArraySize(args[0]);
              }
              case 3 -> {
                from = iop.asLong(args[1]);
                to = from + iop.asLong(args[2]);
              }
              default -> throw ArityException.create(0, 3, args.length);
            }
            for (var i = from; i < to; ) {
              var size = (int) Math.min(to - i, 8192);
              var arr = is.readNBytes(size);
              if (arr.length == 0) {
                var count = i - from;
                yield count > 0 ? count : -1;
              }
              for (var j = 0; j < arr.length; j++) {
                iop.writeArrayElement(args[0], i++, arr[j]);
              }
            }
            yield to - from;
          }
          case "readAllBytes" -> {
            if (args.length != 0) {
              throw ArityException.create(0, 0, args.length);
            }
            var buf = is.readAllBytes();
            yield ArrayLikeHelpers.wrapBuffer(buf);
          }
          case "readNBytes" -> {
            if (args.length != 1) {
              throw ArityException.create(1, 1, args.length);
            }
            var len = iop.asInt(args[0]);
            var buf = is.readNByteBuffer(len);
            yield ArrayLikeHelpers.wrapBuffer(buf);
          }
          case "skipNBytes" -> {
            if (args.length != 1) {
              throw ArityException.create(1, 1, args.length);
            }
            var len = iop.asLong(args[0]);
            is.skipNBytes(len);
            yield is;
          }
          case "markSupported" -> {
            if (args.length != 0) {
              throw ArityException.create(0, 0, args.length);
            }
            yield is.markSupported();
          }
          case "mark" -> {
            if (args.length != 1) {
              throw ArityException.create(1, 1, args.length);
            }
            var readlimit = iop.asInt(args[0]);
            is.mark(readlimit);
            yield is;
          }
          case "reset" -> {
            if (args.length != 0) {
              throw ArityException.create(0, 0, args.length);
            }
            is.reset();
            yield is;
          }
          case "available" -> {
            if (args.length != 0) {
              throw ArityException.create(0, 0, args.length);
            }
            yield is.available();
          }
          case "close" -> {
            if (args.length != 0) {
              throw ArityException.create(0, 0, args.length);
            }
            is.close();
            yield is;
          }
          default -> throw UnknownIdentifierException.create(name);
        };
      } catch (IOException ex) {
        throw raiseIOException(iop, ex);
      } catch (InvalidArrayIndexException ex) {
        var ctx = EnsoContext.get(iop);
        throw ctx.raiseAssertionPanic(iop, name, ex);
      }
    }

    @Override
    public String toString() {
      return "EnsoInputStream";
    }
  }

  @SuppressWarnings("unchecked")
  @TruffleBoundary
  private static <T> List<T> namesToValues(
      Object arr,
      ArrayLikeLengthNode lengthNode,
      ArrayLikeAtNode atNode,
      EnsoContext ctx,
      Function<String, T> convertor) {
    var size = (int) lengthNode.executeLength(arr);
    List<T> hostArr = new ArrayList<>();
    try {
      for (var i = 0; i < size; i++) {
        var elem = atNode.executeAt(arr, i);
        if (elem instanceof Text name) {
          hostArr.add(convertor.apply(name.toString()));
        } else {
          var err =
              ctx.getBuiltins()
                  .error()
                  .makeTypeError(ctx.getBuiltins().text(), elem, "File_Access permissions");
          throw new PanicException(err, lengthNode);
        }
      }
    } catch (ClassCastException | InvalidArrayIndexException e) {
      throw EnsoContext.get(lengthNode).raiseAssertionPanic(lengthNode, null, e);
    }
    return hostArr;
  }

  @Builtin.Method(name = "read_last_bytes_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public static EnsoObject readLastBytes(EnsoFile file, long n) throws IOException {
    try (SeekableByteChannel channel =
        Files.newByteChannel(file.path, Set.of(StandardOpenOption.READ))) {
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

  @Builtin.Method(name = "resolve_builtin")
  @Builtin.WrapException(from = IllegalArgumentException.class)
  @Builtin.Specialize
  @TruffleBoundary
  public static EnsoFile resolve(EnsoFile file, Text part) {
    return new EnsoFile(file.path.resolve(part.toString()));
  }

  @Builtin.Method
  @TruffleBoundary
  public boolean exists() {
    return Files.exists(path.normalize());
  }

  @Builtin.Method(name = "creation_time_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public static EnsoDateTime getCreationTime(EnsoFile file) throws IOException {
    var attrs = Files.readAttributes(file.path, BasicFileAttributes.class);
    return new EnsoDateTime(
        ZonedDateTime.ofInstant(attrs.creationTime().toInstant(), ZoneOffset.UTC));
  }

  @Builtin.Method(name = "last_modified_time_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public static EnsoDateTime getLastModifiedTime(EnsoFile file) throws IOException {
    var mtime = Files.getLastModifiedTime(file.path);
    return new EnsoDateTime(ZonedDateTime.ofInstant(mtime.toInstant(), ZoneOffset.UTC));
  }

  @Builtin.Method(name = "posix_permissions_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public static Text getPosixPermissions(EnsoFile file) throws IOException {
    return Text.create(PosixFilePermissions.toString(Files.getPosixFilePermissions(file.path)));
  }

  @Builtin.Method(name = "parent")
  @TruffleBoundary
  public EnsoObject getParent() {
    // Normalization is needed to correctly handle paths containing `..` and `.`.
    var parentOrNull = this.normalize().path.getParent();

    // If the path has no parent because it is relative and there are no more segments, try again
    // after making it absolute:
    if (parentOrNull == null && !this.path.isAbsolute()) {
      parentOrNull = this.path.toAbsolutePath().normalize().getParent();
    }

    if (parentOrNull != null) {
      return new EnsoFile(parentOrNull);
    } else {
      var ctx = EnsoContext.get(null);
      return ctx.getBuiltins().nothing();
    }
  }

  @Builtin.Method(name = "absolute")
  @TruffleBoundary
  public EnsoFile getAbsoluteFile() {
    return new EnsoFile(this.path.toAbsolutePath());
  }

  @Builtin.Method(name = "path")
  @TruffleBoundary
  public Text getPath() {
    return Text.create(this.path.toString());
  }

  @Builtin.Method
  @TruffleBoundary
  public boolean isAbsolute() {
    return this.path.isAbsolute();
  }

  @Builtin.Method(name = "is_directory_builtin")
  @TruffleBoundary
  public static boolean isDirectory(EnsoFile file) {
    return Files.isDirectory(file.path);
  }

  @Builtin.Method(name = "create_directory_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public static void createDirectories(EnsoFile file) throws IOException {
    try {
      Files.createDirectories(file.path);
    } catch (NoSuchFileException e) {
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

    var parent =
        switch (fromString(path)) {
          case EnsoFile f -> f.path.getParent();
          case null -> null;
          default -> null;
        };
    // Unknown parent, so the heuristic cannot be applied - return the original.
    if (parent == null) {
      return noSuchFileException;
    }

    // On Windows, when creating a directory tree `foo/my-file.txt/a/b/c`, the operation fails with
    // `NoSuchFileException` with path `foo/my-file.txt/a`. So the heuristic checks the path's
    // parent `foo/my-file.txt` if it exists but is not a directory that means we encountered this
    // edge case and the exception should be replaced.
    if (Files.exists(parent) && !Files.isDirectory(parent)) {
      return new NotDirectoryException(parent.toString());
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
    if (genericException.getReason() != null
        && genericException.getReason().equals("Not a directory")) {
      var path = genericException.getFile();
      if (path == null) {
        return genericException;
      }

      // On Linux, when creating a directory tree `foo/my-file.txt/a/b/c`, the operation fails with
      // `FileSystemException` with the full path (`foo/my-file.txt/a/b/c`). So we need to traverse
      // this path to find the actually problematic part.
      var p = Path.of(path);

      // We try to find the first file that exists on the path.
      while (p != null && !Files.exists(p)) {
        p = p.getParent();
      }

      if (p != null && !Files.isDirectory(p)) {
        return new NotDirectoryException(p.toString());
      } else {
        return genericException;
      }
    } else {
      return genericException;
    }
  }

  @Builtin.Method(name = "list_immediate_children_array")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public EnsoObject list() throws IOException {
    return ArrayLikeHelpers.wrapEnsoObjects(
        Files.list(path).map(EnsoFile::new).toArray(EnsoFile[]::new));
  }

  @Builtin.Method
  @Builtin.WrapException(from = IllegalArgumentException.class)
  @TruffleBoundary
  public EnsoFile relativize(EnsoFile other) {
    return new EnsoFile(this.path.relativize(other.path));
  }

  @Builtin.Method(name = "is_regular_file_builtin")
  @TruffleBoundary
  public static boolean isRegularFile(EnsoFile file) {
    return Files.isRegularFile(file.path);
  }

  @Builtin.Method
  @TruffleBoundary
  public boolean isWritable() {
    return Files.isWritable(this.path);
  }

  @Builtin.Method(name = "name")
  @TruffleBoundary
  public Text getName() {
    var name = this.normalize().path.getFileName();
    return Text.create(name == null ? "/" : name.toString());
  }

  @Builtin.Method(name = "size_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public static long getSize(EnsoFile file) throws IOException {
    if (Files.isDirectory(file.path)) {
      throw new IOException("size can only be called on files.");
    }
    return Files.size(file.path);
  }

  @TruffleBoundary
  @Override
  public boolean equals(Object obj) {
    if (obj instanceof EnsoFile otherFile) {
      return path.equals(otherFile.path);
    } else {
      return false;
    }
  }

  @Builtin.Method
  @TruffleBoundary
  public EnsoFile normalize() {
    var simplyNormalized = path.normalize();
    var name = simplyNormalized.getFileName();
    boolean needsAbsolute =
        name != null
            && (name.toString().equals("..")
                || name.toString().equals(".")
                || name.toString().isEmpty());
    if (needsAbsolute) {
      simplyNormalized = simplyNormalized.toAbsolutePath().normalize();
    }
    return new EnsoFile(simplyNormalized);
  }

  @Builtin.Method(name = "delete_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public static void delete(EnsoFile file, boolean recursive) throws IOException {
    if (recursive && Files.isDirectory(file.path, LinkOption.NOFOLLOW_LINKS)) {
      deleteRecursively(file.path);
    } else {
      Files.delete(file.path);
    }
  }

  private static void deleteRecursively(Path file) throws IOException {
    if (Files.isDirectory(file, LinkOption.NOFOLLOW_LINKS)) {
      try (var entries = Files.newDirectoryStream(file)) {
        for (var entry : entries) {
          deleteRecursively(entry);
        }
      }
    }
    Files.delete(file);
  }

  @Builtin.Method(name = "copy_builtin", description = "Copy this file to a target destination")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.Specialize
  @TruffleBoundary
  public static void copy(
      EnsoFile source,
      EnsoFile target,
      Object options,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode,
      EnsoContext ctx)
      throws IOException {
    var copyOptions = namesToValues(options, lengthNode, atNode, ctx, StandardCopyOption::valueOf);
    Files.copy(
        source.path.normalize(), target.path.normalize(), copyOptions.toArray(CopyOption[]::new));
  }

  @Builtin.Method(name = "move_builtin", description = "Move this file to a target destination")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.Specialize
  @TruffleBoundary
  public static void move(
      EnsoFile source,
      EnsoFile target,
      Object options,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode,
      EnsoContext ctx)
      throws IOException {
    var copyOptions = namesToValues(options, lengthNode, atNode, ctx, StandardCopyOption::valueOf);
    Files.move(
        source.path.normalize(), target.path.normalize(), copyOptions.toArray(CopyOption[]::new));
  }

  @Builtin.Method
  @TruffleBoundary
  public boolean startsWith(EnsoFile parent) {
    return path.startsWith(parent.path);
  }

  @Builtin.Method(
      name = "get_file",
      description =
          "Takes the text representation of a path and returns a file corresponding to it.",
      autoRegister = false)
  @Builtin.Specialize
  @Builtin.WrapException(from = IllegalArgumentException.class)
  @Builtin.WrapException(from = UnsupportedOperationException.class)
  @TruffleBoundary
  @SuppressWarnings("generic-enso-builtin-type")
  public static Object fromString(String path) {
    return new EnsoFile(Path.of(path));
  }

  @Builtin.Method(
      name = "get_cwd",
      description = "A file corresponding to the current working directory.",
      autoRegister = false)
  @Builtin.Specialize
  @TruffleBoundary
  public static EnsoFile currentDirectory() {
    return new EnsoFile(Path.of("."));
  }

  @Builtin.Method(
      name = "home",
      description = "Gets the user's system-defined home directory.",
      autoRegister = false)
  @Builtin.Specialize
  @TruffleBoundary
  @SuppressWarnings("generic-enso-builtin-type")
  public static Object userHome() {
    return fromString(System.getProperty("user.home"));
  }

  @ExportMessage
  @TruffleBoundary
  @Override
  public String toDisplayString(boolean allowSideEffects) {
    return "(File " + path + ")";
  }

  @Override
  @TruffleBoundary
  public String toString() {
    return toDisplayString(false);
  }

  static RuntimeException raiseIOException(Node where, IOException ex) {
    var ctx = EnsoContext.get(where);
    var guestEx = ctx.asGuestValue(ex);
    throw new PanicException(guestEx, where);
  }
}
