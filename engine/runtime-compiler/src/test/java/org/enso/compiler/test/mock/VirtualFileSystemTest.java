package org.enso.compiler.test.mock;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.nio.file.Files;
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
    var projDir = vfs.getRoot().resolve("Proj");
    assertThat(vfs.getSegments(projDir), contains("Proj"));
  }

  @Test
  public void createDirectories() throws IOException {
    var projDir = vfs.getRoot().resolve("Proj");
    vfs.createDirectories(projDir);
    assertThat(Files.exists(projDir), is(true));
    assertThat(Files.isDirectory(projDir), is(true));

    var modDir = vfs.getRoot().resolve("Proj/src/Mod");
    vfs.createDirectories(modDir);
    assertThat(Files.exists(modDir), is(true));
    assertThat(Files.isDirectory(modDir), is(true));
  }

  @Test
  public void getName() throws IOException {
    var projDir = vfs.getRoot().resolve("Proj");
    assertThat(vfs.getName(projDir), is("Proj"));
  }

  @Test
  public void canRelativize() throws IOException {
    var projDir = vfs.getRoot().resolve("Proj");
    var srcFile = projDir.resolve("src/Mod/My_Type.enso");
    var relative = vfs.relativize(projDir, srcFile);
    assertThat(relative, is(notNullValue()));
    assertThat(vfs.getSegments(relative), contains("src", "Mod", "My_Type.enso"));
  }

  @Test
  public void isRegularFile() throws IOException {
    var file = vfs.getRoot().resolve("tmp.txt");
    Files.createFile(file);
    assertThat(vfs.isRegularFile(file), is(true));
    assertThat(vfs.isDirectory(file), is(false));
  }

  @Test
  public void isDirectory() throws IOException {
    var dir = vfs.getRoot().resolve("tmp");
    Files.createDirectories(dir);
    assertThat(vfs.isDirectory(dir), is(true));
    assertThat(vfs.isRegularFile(dir), is(false));
  }

  @Test
  public void getChild() throws IOException {
    var projDir = vfs.getRoot().resolve("Proj");
    Files.createDirectories(projDir);
    var file = projDir.resolve("package.yaml");
    Files.createFile(file);
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
    var file = vfs.getRoot().resolve("Proj/src/Main.enso");
    var absPath = vfs.getAbsolutePath(file);
    assertThat(absPath, is(notNullValue()));
    assertThat(absPath, is("/Proj/src/Main.enso"));
  }

  @Test
  public void writeFile() throws IOException {
    var file = vfs.getRoot().resolve("tmp.txt");
    Files.createFile(file);
    try (var writer = vfs.newBufferedWriter(file)) {
      writer.write("Hello");
    }
    assertThat(Files.readString(file), is("Hello"));
  }

  @Test
  public void readFile() throws IOException {
    var file = vfs.getRoot().resolve("tmp.txt");
    Files.createFile(file);
    Files.writeString(file, "Hello");
    try (var reader = vfs.newBufferedReader(file)) {
      var line = reader.readLine();
      assertThat(line, is("Hello"));
    }
  }

  @Test
  public void writeToNonExistingFile() throws IOException {
    var file = vfs.getRoot().resolve("tmp.txt");
    try (var writer = vfs.newBufferedWriter(file)) {
      writer.write("Hello");
    }
    assertThat(Files.exists(file), is(true));
    assertThat(Files.readString(file), is("Hello"));
  }

  @Test
  public void readNonExistingFile_ShouldThrow() throws IOException {
    var file = vfs.getRoot().resolve("tmp.txt");
    try {
      vfs.newBufferedReader(file);
      fail("Should throw IOException");
    } catch (IOException e) {
      // nop
    }
  }
}
