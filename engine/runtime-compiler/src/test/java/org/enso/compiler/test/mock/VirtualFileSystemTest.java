package org.enso.compiler.test.mock;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import org.apache.commons.vfs2.FileObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class VirtualFileSystemTest {
  private VirtualFileSystem vfs;

  @Before
  public void before() {
    vfs = VirtualFileSystem.create();
  }

  @After
  public void after() throws IOException {
    vfs.deleteAll();
  }

  @Test
  public void getSegments() throws IOException {
    var projDir = vfs.getRoot().resolveFile("Proj");
    assertThat(vfs.getSegments(projDir), contains("Proj"));
  }

  @Test
  public void createDirectories() throws IOException {
    var projDir = vfs.getRoot().resolveFile("Proj");
    vfs.createDirectories(projDir);
    assertThat(projDir.exists(), is(true));
    assertThat(projDir.isFolder(), is(true));

    var modDir = vfs.getRoot().resolveFile("Proj/src/Mod");
    vfs.createDirectories(modDir);
    assertThat(modDir.exists(), is(true));
    assertThat(modDir.isFolder(), is(true));
  }

  @Test
  public void getName() throws IOException {
    var projDir = vfs.getRoot().resolveFile("Proj");
    assertThat(vfs.getName(projDir), is("Proj"));
  }

  @Test
  public void canRelativize() throws IOException {
    var projDir = vfs.getRoot().resolveFile("Proj");
    var srcFile = projDir.resolveFile("src/Mod/My_Type.enso");
    var relative = vfs.relativize(projDir, srcFile);
    assertThat(relative, is(notNullValue()));
    assertThat(vfs.getSegments(relative), contains("src", "Mod", "My_Type.enso"));
  }

  @Test
  public void isRegularFile() throws IOException {
    var file = vfs.getRoot().resolveFile("tmp.txt");
    file.createFile();
    assertThat(vfs.isRegularFile(file), is(true));
    assertThat(vfs.isDirectory(file), is(false));
  }

  @Test
  public void isDirectory() throws IOException {
    var dir = vfs.getRoot().resolveFile("tmp");
    dir.createFolder();
    assertThat(vfs.isDirectory(dir), is(true));
    assertThat(vfs.isRegularFile(dir), is(false));
  }

  @Test
  public void getChild() throws IOException {
    var projDir = vfs.getRoot().resolveFile("Proj");
    projDir.createFolder();
    var file = projDir.resolveFile("package.yaml");
    file.createFile();
    var f = vfs.getChild(projDir, "package.yaml");
    assertThat(vfs.exists(f), is(true));
  }

  @Test
  public void getParentOfRoot() {
    var root = vfs.getRoot();
    var parent = vfs.getParent(root);
    assertThat(parent, is(nullValue()));
  }

  @Test
  public void getAbsolutePath() throws IOException {
    var file = vfs.getRoot().resolveFile("Proj/src/Main.enso");
    var absPath = vfs.getAbsolutePath(file);
    assertThat(absPath, is(notNullValue()));
    assertThat(absPath, is("/Proj/src/Main.enso"));
  }

  @Test
  public void writeFile() throws IOException {
    var file = vfs.getRoot().resolveFile("tmp.txt");
    file.createFile();
    try (var writer = vfs.newBufferedWriter(file)) {
      writer.write("Hello");
    }
    assertThat(file.getContent().getString(Charset.defaultCharset()), is("Hello"));
  }

  @Test
  public void readFile() throws IOException {
    var file = vfs.getRoot().resolveFile("tmp.txt");
    file.createFile();
    try (var os = new OutputStreamWriter(file.getContent().getOutputStream())) {
      os.write("Hello");
    }
    try (var reader = vfs.newBufferedReader(file)) {
      var line = reader.readLine();
      assertThat(line, is("Hello"));
    }
  }

  @Test
  public void writeToNonExistingFile() throws IOException {
    var file = vfs.getRoot().resolveFile("tmp.txt");
    try (var writer = vfs.newBufferedWriter(file)) {
      writer.write("Hello");
    }
    assertThat(file.exists(), is(true));
    assertThat(readLine(file), is("Hello"));
  }

  @Test
  public void readNonExistingFile_ShouldThrow() throws IOException {
    var file = vfs.getRoot().resolveFile("tmp.txt");
    try {
      vfs.newBufferedReader(file);
      fail("Should throw IOException");
    } catch (IOException e) {
      // nop
    }
  }

  private static String readLine(FileObject file) throws IOException {
    try (var reader =
        new BufferedReader(new InputStreamReader(file.getContent().getInputStream()))) {
      return reader.readLine();
    }
  }
}
