package org.enso.compiler.test.mock;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.file.attribute.FileTime;
import java.util.Arrays;
import java.util.stream.Stream;
import org.apache.commons.vfs2.AllFileSelector;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileSystemManager;
import org.apache.commons.vfs2.VFS;
import org.enso.filesystem.FileSystem;

/**
 * {@link FileSystem} implementation for Apache Commons VFS2. So far, only RAM file system is
 * supported.
 */
final class VirtualFileSystem implements FileSystem<FileObject> {
  private final FileSystemManager fileSystemManager;
  private final FileObject ramRoot;

  private VirtualFileSystem(FileSystemManager fileSystemManager, FileObject ramRoot) {
    this.fileSystemManager = fileSystemManager;
    this.ramRoot = ramRoot;
  }

  public void deleteAll() throws IOException {
    for (var child : ramRoot.getChildren()) {
      child.deleteAll();
    }
  }

  public static VirtualFileSystem create() {
    FileSystemManager manager;
    FileObject ramRoot;
    try {
      manager = VFS.getManager();
      ramRoot = manager.resolveFile("ram:///");
    } catch (FileSystemException e) {
      throw new IllegalStateException("Cannot create Virtual file system", e);
    }
    return new VirtualFileSystem(manager, ramRoot);
  }

  /**
   * Creates the file in RAM file system.
   *
   * @param path Absolute or relative path. Does not have to start with "ram://"
   * @param content Content of the file.
   */
  public void createFile(String path, String content) throws IOException {
    var newFile = ramRoot.resolveFile(path);
    newFile.createFile();
    try (var writer =
        new PrintWriter(new OutputStreamWriter(newFile.getContent().getOutputStream()))) {
      writer.println(content);
    }
  }

  FileObject getRoot() {
    return ramRoot;
  }

  @Override
  public FileObject getChild(FileObject parent, String childName) {
    var newPath = parent.getName().getURI() + "/" + childName;
    try {
      return fileSystemManager.resolveFile(newPath);
    } catch (FileSystemException ex) {
      throw new AssertionError("path should be ok", ex);
    }
  }

  @Override
  public FileObject getParent(FileObject path) {
    try {
      return path.getParent();
    } catch (FileSystemException e) {
      var parentPath = path.getPath().getParent();
      try {
        fileSystemManager.resolveFile(parentPath.toUri());
      } catch (FileSystemException ex) {
        throw new IllegalStateException(ex);
      }
    }
    return null;
  }

  @Override
  public boolean exists(FileObject file) {
    try {
      return file.exists();
    } catch (FileSystemException e) {
      throw new IllegalStateException(e);
    }
  }

  @Override
  public void createDirectories(FileObject file) throws IOException {
    file.createFolder();
  }

  @Override
  public FileObject relativize(FileObject parent, FileObject child) {
    try {
      var relativeName = parent.getName().getRelativeName(child.getName());
      return ramRoot.resolveFile(relativeName);
    } catch (FileSystemException e) {
      throw new IllegalStateException(e);
    }
  }

  @Override
  public Iterable<String> getSegments(FileObject file) {
    return Arrays.stream(file.getName().getPath().split("/"))
        // Skip the first empty string
        .skip(1)
        .toList();
  }

  @Override
  public String getAbsolutePath(FileObject file) {
    return file.getName().getPath();
  }

  @Override
  public String getName(FileObject file) {
    return file.getName().getBaseName();
  }

  @Override
  public InputStream newInputStream(FileObject file) throws IOException {
    return file.getContent().getInputStream();
  }

  @Override
  public OutputStream newOutputStream(FileObject file) throws IOException {
    return file.getContent().getOutputStream();
  }

  @Override
  public BufferedWriter newBufferedWriter(FileObject file) throws IOException {
    var os = file.getContent().getOutputStream();
    return new BufferedWriter(new OutputStreamWriter(os));
  }

  @Override
  public BufferedReader newBufferedReader(FileObject file) throws IOException {
    if (!file.exists()) {
      throw new IOException("File does not exist: " + file.getName().getPath());
    }
    var is = file.getContent().getInputStream();
    return new BufferedReader(new java.io.InputStreamReader(is));
  }

  @Override
  public Stream<FileObject> list(FileObject file) throws IOException {
    return Arrays.stream(file.getChildren());
  }

  @Override
  public Stream<FileObject> walk(FileObject file) throws IOException {
    return Arrays.stream(file.findFiles(new AllFileSelector()));
  }

  @Override
  public boolean isDirectory(FileObject file) {
    try {
      return file.isFolder();
    } catch (FileSystemException e) {
      throw new IllegalStateException(e);
    }
  }

  @Override
  public boolean isRegularFile(FileObject file) {
    try {
      return file.isFile();
    } catch (FileSystemException e) {
      throw new IllegalStateException(e);
    }
  }

  @Override
  public FileTime getCreationTime(FileObject file) throws IOException {
    return FileTime.fromMillis(file.getContent().getLastModifiedTime());
  }

  String listAllFiles() throws IOException {
    var bldr = new StringBuilder();
    for (var child : ramRoot.getChildren()) {
      listFiles(child, 0, bldr);
    }
    return bldr.toString();
  }

  void listFiles(FileObject current, int depth, StringBuilder strBldr)
      throws FileSystemException {
    var fName = current.getName().getBaseName();
    if (current.isFile()) {
      addEntry(strBldr, depth, fName);
    } else {
      addEntry(strBldr, depth, fName + "/");
      for (var child : current.getChildren()) {
        listFiles(child, depth + 1, strBldr);
      }
    }
  }

  private static void addEntry(StringBuilder bldr, int depth, String msg) {
    bldr.append(System.lineSeparator()).append("  ".repeat(depth)).append(msg);
  }
}
