package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.TruffleFile;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.CopyOption;
import java.nio.file.OpenOption;

/**
 * A wrapper for {@link TruffleFile} objects exposed to the language. For methods documentation
 * please refer to {@link TruffleFile}.
 */
public class EnsoFile {
  private final TruffleFile truffleFile;

  public EnsoFile(TruffleFile truffleFile) {
    this.truffleFile = truffleFile;
  }

  public OutputStream newOutputStream(OpenOption[] opts) throws IOException {
    return truffleFile.newOutputStream(opts);
  }

  public InputStream newInputStream(OpenOption[] opts) throws IOException {
    return truffleFile.newInputStream(opts);
  }

  public EnsoFile resolve(String subPath) {
    return new EnsoFile(this.truffleFile.resolve(subPath));
  }

  public EnsoFile resolve(EnsoFile subPath) {
    return new EnsoFile(this.truffleFile.resolve(subPath.truffleFile.getPath()));
  }

  public boolean exists() {
    return truffleFile.exists();
  }

  public EnsoFile getParent() {
    return new EnsoFile(this.truffleFile.getParent());
  }

  public EnsoFile getAbsoluteFile() {
    return new EnsoFile(this.truffleFile.getAbsoluteFile());
  }

  public String getPath() {
    return this.truffleFile.getPath();
  }

  public boolean isAbsolute() {
    return this.truffleFile.isAbsolute();
  }

  public boolean isDirectory() {
    return this.truffleFile.isDirectory();
  }

  public void createDirectories() throws IOException {
    this.truffleFile.createDirectories();
  }

  public EnsoFile[] list() throws IOException {
    return this.truffleFile.list().stream().map(EnsoFile::new).toArray(EnsoFile[]::new);
  }

  public EnsoFile relativize(EnsoFile other) {
    return new EnsoFile(this.truffleFile.relativize(other.truffleFile));
  }

  public boolean isRegularFile() {
    return this.truffleFile.isRegularFile();
  }

  public String getName() {
    return this.truffleFile.getName();
  }

  public boolean isEqual(EnsoFile that) {
    return this.truffleFile.equals(that.truffleFile);
  }

  public EnsoFile normalize() {
    return new EnsoFile(truffleFile.normalize());
  }

  public void delete() throws IOException {
    truffleFile.delete();
  }

  public void move(EnsoFile target, CopyOption... options) throws IOException {
    truffleFile.move(target.truffleFile, options);
  }

  public boolean startsWith(EnsoFile parent) {
    return truffleFile.startsWith(parent.truffleFile);
  }

  @Override
  public String toString() {
    return "(File " + truffleFile.getPath() + ")";
  }
}
