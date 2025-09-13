package org.enso.compiler.test.mock;

import com.google.common.jimfs.Configuration;
import com.google.common.jimfs.Jimfs;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.Arrays;
import java.util.stream.Stream;
import org.enso.filesystem.FileSystem;

/**
 * {@link FileSystem} implementation that works on a generic {@link Path}. More specifically, it
 * respects different {@link java.nio.file.spi.FileSystemProvider file system providers}.
 */
final class VirtualFileSystem implements FileSystem<Path>, AutoCloseable {
  private static final String ROOT_FILE_NAME = "root";
  private final java.nio.file.FileSystem fileSystem;
  private final Path inMemoryRoot;

  private VirtualFileSystem(java.nio.file.FileSystem fileSystem, Path inMemoryRoot) {
    this.fileSystem = fileSystem;
    this.inMemoryRoot = inMemoryRoot;
  }

  static void write(Path file, String content) throws IOException {
    Files.writeString(file, content);
  }

  @Override
  public void close() throws Exception {
    fileSystem.close();
  }

  public void deleteAll() throws IOException {
    Files.walkFileTree(
        inMemoryRoot,
        new SimpleFileVisitor<>() {
          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
              throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
          }

          @Override
          public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
            var isRoot = Files.isSameFile(inMemoryRoot, dir);
            if (!isRoot) {
              Files.delete(dir);
            }
            return FileVisitResult.CONTINUE;
          }
        });
  }

  /**
   * Creates a VFS implementation that keeps all the created files in memory and does not access the
   * real file system.
   */
  public static VirtualFileSystem create() {
    var fs = Jimfs.newFileSystem(Configuration.unix());
    var inMemoryRoot = fs.getPath("/");
    return new VirtualFileSystem(fs, inMemoryRoot);
  }

  Path getRoot() {
    return inMemoryRoot;
  }

  @Override
  public Path getChild(Path parent, String childName) {
    return parent.resolve(childName);
  }

  @Override
  public Path getParent(Path path) {
    return path.getParent();
  }

  @Override
  public boolean exists(Path file) {
    return Files.exists(file);
  }

  @Override
  public void createDirectories(Path file) throws IOException {
    Files.createDirectories(file);
  }

  @Override
  public Path relativize(Path parent, Path child) {
    return parent.relativize(child);
  }

  @Override
  public Iterable<String> getSegments(Path file) {
    var absPath = file.toString();
    return Arrays.stream(absPath.split(fileSystem.getSeparator()))
        .dropWhile(String::isEmpty)
        .toList();
  }

  @Override
  public String getAbsolutePath(Path file) {
    return file.toAbsolutePath().toString();
  }

  @Override
  public String getName(Path file) {
    return file.getFileName().toString();
  }

  @Override
  public InputStream newInputStream(Path file) throws IOException {
    return Files.newInputStream(file);
  }

  @Override
  public OutputStream newOutputStream(Path file) throws IOException {
    return Files.newOutputStream(file);
  }

  @Override
  public BufferedWriter newBufferedWriter(Path file) throws IOException {
    return Files.newBufferedWriter(file);
  }

  @Override
  public BufferedReader newBufferedReader(Path file) throws IOException {
    return Files.newBufferedReader(file);
  }

  @Override
  public Stream<Path> list(Path file) throws IOException {
    return Files.list(file);
  }

  @Override
  public Stream<Path> walk(Path file) throws IOException {
    return Files.walk(file);
  }

  @Override
  public boolean isDirectory(Path file) {
    return Files.isDirectory(file);
  }

  @Override
  public boolean isRegularFile(Path file) {
    return Files.isRegularFile(file);
  }

  @Override
  public FileTime getCreationTime(Path file) throws IOException {
    var attrs = Files.readAttributes(file, BasicFileAttributes.class);
    return attrs.creationTime();
  }

  String listAllFiles() throws IOException {
    var sb = new StringBuilder();
    var printFileVisitor = new PrintFileVisitor(sb);
    Files.walkFileTree(inMemoryRoot, printFileVisitor);
    return sb.toString();
  }

  public URI getUri(String path) {
    var p = inMemoryRoot.resolve(path);
    return p.toUri();
  }

  private static final class PrintFileVisitor extends SimpleFileVisitor<Path> {
    private int depth;
    private StringBuilder sb;

    private PrintFileVisitor(StringBuilder sb) {
      this.sb = sb;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
      addEntry(fileName(file));
      return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
        throws IOException {
      addEntry(fileName(dir) + "/");
      depth++;
      return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
      depth--;
      return FileVisitResult.CONTINUE;
    }

    private void addEntry(String msg) {
      sb.append(System.lineSeparator()).append("  ".repeat(depth)).append(msg);
    }

    private static String fileName(Path path) {
      return path.getFileName() == null ? ROOT_FILE_NAME : path.getFileName().toString();
    }
  }
}
