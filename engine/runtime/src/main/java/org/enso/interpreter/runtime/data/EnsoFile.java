package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.CopyOption;
import java.nio.file.OpenOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;

/**
 * A wrapper for {@link TruffleFile} objects exposed to the language. For methods documentation
 * please refer to {@link TruffleFile}.
 */
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "io", name = "File", stdlibName = "Standard.Base.System.File.File")
public class EnsoFile implements TruffleObject {
  private final TruffleFile truffleFile;

  public EnsoFile(TruffleFile truffleFile) {
    this.truffleFile = truffleFile;
  }

  @Builtin.Method
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class)
  @Builtin.ReturningGuestObject
  public OutputStream outputStream(OpenOption[] opts) throws IOException {
    return this.truffleFile.newOutputStream(opts);
  }

  @Builtin.Method
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class)
  @Builtin.ReturningGuestObject
  public InputStream inputStream(OpenOption[] opts) throws IOException {
    return this.truffleFile.newInputStream(opts);
  }

  @Builtin.Method(name = "read_last_bytes_builtin")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class)
  public ArrayOverBuffer readLastBytes(long n) throws IOException {
    try (SeekableByteChannel channel =
        this.truffleFile.newByteChannel(Set.of(StandardOpenOption.READ))) {
      int bytesToRead = Math.toIntExact(Math.min(channel.size(), n));
      channel.position(channel.size() - bytesToRead);
      ByteBuffer buffer = ByteBuffer.allocate(bytesToRead);
      while (buffer.hasRemaining()) {
        channel.read(buffer);
      }

      buffer.flip();
      return ArrayOverBuffer.wrapBuffer(buffer);
    }
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
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class)
  @Builtin.ReturningGuestObject
  public EnsoDateTime getCreationTime() throws IOException {
    return new EnsoDateTime(
        ZonedDateTime.ofInstant(truffleFile.getCreationTime().toInstant(), ZoneOffset.UTC));
  }

  @Builtin.Method(name = "last_modified_time_builtin")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class)
  @Builtin.ReturningGuestObject
  public EnsoDateTime getLastModifiedTime() throws IOException {
    return new EnsoDateTime(
        ZonedDateTime.ofInstant(truffleFile.getLastModifiedTime().toInstant(), ZoneOffset.UTC));
  }

  @Builtin.Method(name = "posix_permissions_builtin")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class)
  @Builtin.ReturningGuestObject
  public Set<PosixFilePermission> getPosixPermissions() throws IOException {
    return truffleFile.getPosixPermissions();
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
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class)
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
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class)
  public void delete() throws IOException {
    truffleFile.delete();
  }

  @Builtin.Method(name = "copy_builtin", description = "Copy this file to a target destination")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class)
  public void copy(EnsoFile target, CopyOption[] options) throws IOException {
    truffleFile.copy(target.truffleFile, options);
  }

  @Builtin.Method(name = "move_builtin", description = "Move this file to a target destination")
  @Builtin.WrapException(from = IOException.class, to = PolyglotError.class)
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
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().file();
  }
}
