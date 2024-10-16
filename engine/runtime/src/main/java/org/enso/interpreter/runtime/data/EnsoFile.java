package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Bind;
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
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.NotDirectoryException;
import java.nio.file.OpenOption;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
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
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.error.DataflowError;
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
  @Builtin.Specialize
  @TruffleBoundary
  public EnsoObject outputStream(
      Object opts,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode,
      EnsoContext ctx)
      throws IOException {
    var options = namesToValues(opts, lengthNode, atNode, ctx, StandardOpenOption::valueOf);
    var os = this.truffleFile.newOutputStream(options.toArray(OpenOption[]::new));
    return new EnsoOutputStream(os);
  }

  @ExportLibrary(InteropLibrary.class)
  static final class EnsoOutputStream implements EnsoObject {
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
  }

  @Builtin.Method(name = "input_stream_builtin")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.Specialize
  @TruffleBoundary
  public EnsoObject inputStream(
      Object opts,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode,
      EnsoContext ctx)
      throws IOException {
    var options = namesToValues(opts, lengthNode, atNode, ctx, StandardOpenOption::valueOf);
    var is = this.truffleFile.newInputStream(options.toArray(OpenOption[]::new));
    return new EnsoInputStream(is);
  }

  @ExportLibrary(InteropLibrary.class)
  static final class EnsoInputStream implements EnsoObject {
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
  @TruffleBoundary
  public EnsoDateTime getCreationTime() throws IOException {
    return new EnsoDateTime(
        ZonedDateTime.ofInstant(truffleFile.getCreationTime().toInstant(), ZoneOffset.UTC));
  }

  @Builtin.Method(name = "last_modified_time_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public EnsoDateTime getLastModifiedTime() throws IOException {
    return new EnsoDateTime(
        ZonedDateTime.ofInstant(truffleFile.getLastModifiedTime().toInstant(), ZoneOffset.UTC));
  }

  @Builtin.Method(name = "posix_permissions_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public Text getPosixPermissions() throws IOException {
    return Text.create(PosixFilePermissions.toString(truffleFile.getPosixPermissions()));
  }

  @Builtin.Method(name = "parent")
  @TruffleBoundary
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
  @TruffleBoundary
  public EnsoFile getAbsoluteFile() {
    return new EnsoFile(this.truffleFile.getAbsoluteFile());
  }

  @Builtin.Method(name = "path")
  @TruffleBoundary
  public Text getPath() {
    return Text.create(this.truffleFile.getPath());
  }

  @Builtin.Method
  @TruffleBoundary
  public boolean isAbsolute() {
    return this.truffleFile.isAbsolute();
  }

  @Builtin.Method
  @TruffleBoundary
  public boolean isDirectory() {
    return this.truffleFile.isDirectory();
  }

  @Builtin.Method(name = "create_directory_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
  public void createDirectories() throws IOException {
    try {
      this.truffleFile.createDirectories();
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
        switch (fromString(EnsoContext.get(null), path)) {
          case EnsoFile f -> f.truffleFile.getParent();
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
      var file =
          switch (fromString(EnsoContext.get(null), path)) {
            case EnsoFile f -> f.truffleFile;
            case null -> null;
            default -> null;
          };
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
  @TruffleBoundary
  public EnsoObject list() throws IOException {
    return ArrayLikeHelpers.wrapEnsoObjects(
        this.truffleFile.list().stream().map(EnsoFile::new).toArray(EnsoFile[]::new));
  }

  @Builtin.Method
  @Builtin.WrapException(from = IllegalArgumentException.class)
  @TruffleBoundary
  public EnsoFile relativize(EnsoFile other) {
    return new EnsoFile(this.truffleFile.relativize(other.truffleFile));
  }

  @Builtin.Method
  @TruffleBoundary
  public boolean isRegularFile() {
    return this.truffleFile.isRegularFile();
  }

  @Builtin.Method
  @TruffleBoundary
  public boolean isWritable() {
    return this.truffleFile.isWritable();
  }

  @Builtin.Method(name = "name")
  @TruffleBoundary
  public Text getName() {
    var name = this.normalize().truffleFile.getName();
    return Text.create(name == null ? "/" : name);
  }

  @Builtin.Method(name = "size_builtin")
  @Builtin.WrapException(from = IOException.class)
  @TruffleBoundary
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
      return truffleFile.getPath().equals(otherFile.truffleFile.getPath());
    } else {
      return false;
    }
  }

  @Builtin.Method
  @TruffleBoundary
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
  @TruffleBoundary
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
  @TruffleBoundary
  public void copy(
      EnsoFile target,
      Object options,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode,
      EnsoContext ctx)
      throws IOException {
    var copyOptions = namesToValues(options, lengthNode, atNode, ctx, StandardCopyOption::valueOf);
    truffleFile.copy(target.truffleFile, copyOptions.toArray(CopyOption[]::new));
  }

  @Builtin.Method(name = "move_builtin", description = "Move this file to a target destination")
  @Builtin.WrapException(from = IOException.class)
  @Builtin.Specialize
  @TruffleBoundary
  public void move(
      EnsoFile target,
      Object options,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode,
      EnsoContext ctx)
      throws IOException {
    var copyOptions = namesToValues(options, lengthNode, atNode, ctx, StandardCopyOption::valueOf);
    truffleFile.move(target.truffleFile, copyOptions.toArray(CopyOption[]::new));
  }

  @Builtin.Method
  @TruffleBoundary
  public boolean startsWith(EnsoFile parent) {
    return truffleFile.startsWith(parent.truffleFile);
  }

  @Builtin.Method(
      name = "get_file",
      description =
          "Takes the text representation of a path and returns a TruffleFile corresponding to it.",
      autoRegister = false)
  @Builtin.Specialize
  @TruffleBoundary
  public static EnsoObject fromString(EnsoContext context, String path)
      throws IllegalArgumentException {
    try {
      TruffleFile file = context.getPublicTruffleFile(path);
      return new EnsoFile(file);
    } catch (IllegalArgumentException | UnsupportedOperationException ex) {
      var err =
          context
              .getBuiltins()
              .error()
              .makeUnsupportedArgumentsError(new Object[] {Text.create(path)}, ex.getMessage());
      return DataflowError.withDefaultTrace(err, null);
    }
  }

  @Builtin.Method(
      name = "get_cwd",
      description = "A file corresponding to the current working directory.",
      autoRegister = false)
  @Builtin.Specialize
  @TruffleBoundary
  public static EnsoFile currentDirectory(EnsoContext context) {
    TruffleFile file = context.getCurrentWorkingDirectory();
    return new EnsoFile(file);
  }

  @Builtin.Method(
      name = "home",
      description = "Gets the user's system-defined home directory.",
      autoRegister = false)
  @Builtin.Specialize
  @TruffleBoundary
  public static EnsoObject userHome(EnsoContext context) {
    return fromString(context, System.getProperty("user.home"));
  }

  @Override
  @TruffleBoundary
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

  static RuntimeException raiseIOException(Node where, IOException ex) {
    var ctx = EnsoContext.get(where);
    var guestEx = ctx.asGuestValue(ex);
    throw new PanicException(guestEx, where);
  }
}
