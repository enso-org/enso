package org.enso.runner;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.graalvm.polyglot.SourceSection;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class UtilsTest {
  @Rule public TemporaryFolder folder = new TemporaryFolder();

  public UtilsTest() {}

  @Test
  public void printStackTrace1() throws Exception {
    var f = folder.newFile("p.enso");
    var code =
        """
    from Standard.Base import all

    a = b
    b = c
    c = Panic.throw "foo"

    main = a
    """;
    Files.writeString(f.toPath(), code);

    var lines = new ArrayList<String>();

    var stack =
        List.of(
            new MockFrame("enso", "Panic", "throw", "Internal", -1, -1, -1),
            new MockFrame("enso", "p", "c", "p.enso", 5, 5, 21),
            new MockFrame("enso", "m", "main", "p.enso", 20, 4, 8));

    Utils.printStackTrace(
        stack,
        false,
        "foo",
        new File("/"),
        lines::add,
        MockFrame::lang,
        MockFrame::method,
        MockFrame::section);

    assertEquals(
        """
      Execution finished with an error: foo
              at <enso> throw(Internal)
              at <enso> c(Internal)
              at <enso> main(Internal)""",
        lines.stream().collect(Collectors.joining("\n")));
  }

  @Test
  public void detectParentProject() throws Exception {
    var dir = folder.newFolder("dir", "prj", "src", "some", "file").getCanonicalFile();
    var prj = dir.getParentFile().getParentFile().getParentFile();
    assertEquals("prj", prj.getName());
    var yaml = new File(prj, "package.yaml");
    Files.writeString(yaml.toPath(), "enso pkg");
    var src = new File(dir, "Main.enso");
    Files.writeString(src.toPath(), "main = 42");

    var found = Utils.findFileAndProject(src.getPath(), null);
    assertNotNull("Project found", found);
    assertFalse("No project mode for a source file", found._1());
    assertEquals("Source detected", src, found._2());
    assertEquals("Project folder found", prj.getPath(), found._3());
  }

  @Test
  public void specifyProjectDir() throws Exception {
    var dir = folder.newFolder("dir", "prj", "src", "some", "file").getCanonicalFile();
    var prj = dir.getParentFile().getParentFile().getParentFile();
    assertEquals("prj", prj.getName());
    var yaml = new File(prj, "package.yaml");
    Files.writeString(yaml.toPath(), "enso pkg");
    var src = new File(dir, "Main.enso");
    Files.writeString(src.toPath(), "main = 42");

    var found = Utils.findFileAndProject(prj.getPath(), null);
    assertNotNull("Project found", found);
    assertTrue("prj directory means project mode", found._1());
    assertEquals("Source is the project", prj, found._2());
    assertEquals("Project folder found", prj.getPath(), found._3());
  }

  @Test
  public void specifyProjectAndFile() throws Exception {
    var dir = folder.newFolder("dir", "prj", "src", "some", "file").getCanonicalFile();
    var prj = dir.getParentFile().getParentFile().getParentFile();
    assertEquals("prj", prj.getName());
    var yaml = new File(prj, "package.yaml");
    Files.writeString(yaml.toPath(), "enso pkg");
    var src = folder.newFile("Standalone.enso").getCanonicalFile();
    Files.writeString(src.toPath(), "main = 42");

    var found = Utils.findFileAndProject(src.getPath(), prj.getPath());
    assertNotNull("Project found", found);
    assertFalse("source and project implies non-project mode", found._1());
    assertEquals("Source is kept", src, found._2());
    assertEquals("Project folder is kept", prj.getPath(), found._3());
  }

  @Test
  public void dontDetectParentProjectIfMissingPackageYaml() throws Exception {
    var dir = folder.newFolder("dir", "prj", "src", "some", "file").getCanonicalFile();
    var prj = dir.getParentFile().getParentFile().getParentFile();
    assertEquals("prj", prj.getName());
    var yamlInWrongDir = new File(dir.getParent(), "package.yaml");
    Files.writeString(yamlInWrongDir.toPath(), "enso pkg");
    var src = new File(dir, "Main.enso");
    Files.writeString(src.toPath(), "main = 42");

    var found = Utils.findFileAndProject(src.getPath(), null);
    assertNotNull("Project found", found);
    assertFalse("No project mode for a source file", found._1());
    assertEquals("Source detected", src, found._2());
    assertEquals("No project folder found", "", found._3());
  }

  @Test
  public void dontDetectParentProjectWithoutSrcDir() throws Exception {
    var dir = folder.newFolder("dir", "prj", "nosrc", "some", "file").getCanonicalFile();
    var prj = dir.getParentFile().getParentFile().getParentFile();
    assertEquals("prj", prj.getName());
    var yaml = new File(prj, "package.yaml");
    Files.writeString(yaml.toPath(), "enso pkg");
    var src = new File(dir, "Main.enso");
    Files.writeString(src.toPath(), "main = 42");

    var found = Utils.findFileAndProject(src.getPath(), null);
    assertNotNull("Project found", found);
    assertFalse("No project mode for a source file", found._1());
    assertEquals("Source detected", src, found._2());
    assertEquals("No project folder detected without src dir", "", found._3());
  }

  private static record MockFrame(
      String lang,
      String type,
      String method,
      String src,
      int line,
      int startColumn,
      int endColumn) {
    SourceSection section() {
      return null;
    }
  }
}
