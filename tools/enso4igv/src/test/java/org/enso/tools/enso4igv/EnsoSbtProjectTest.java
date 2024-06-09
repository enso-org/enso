package org.enso.tools.enso4igv;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.Sources;
import org.netbeans.junit.NbTestCase;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

public class EnsoSbtProjectTest extends NbTestCase {

  public EnsoSbtProjectTest(String name) {
    super(name);
  }

  @Override
  protected void setUp() throws Exception {
    clearWorkDir();
  }

  public void testLanguageServerProject() throws Exception {
    FileObject root = setLanguageServerProjectUp();

    var prj = ProjectManager.getDefault().findProject(root);
    assertNotNull("Project found", prj);
    assertEquals("Right type", EnsoSbtProject.class, prj.getClass());

    var s = prj.getLookup().lookup(Sources.class);
    assertNotNull("Sources found", s);

    var genericGroups = s.getSourceGroups(Sources.TYPE_GENERIC);
    assertEquals("One", 1, genericGroups.length);
    assertEquals("One at root", root, genericGroups[0].getRootFolder());

    var javaGroups = s.getSourceGroups("java");
    assertEquals("1 bench, 2 tests, 4 main: " + Arrays.toString(javaGroups), 7, javaGroups.length);

    var javaFile = root.getFileObject("src/main/java/MainJava.java");
    assertNotNull("Main java found", javaFile);
    var javaCp = ClassPath.getClassPath(javaFile, ClassPath.SOURCE);
    assertNotNull("java classpath found", javaCp);

    assertNotNull("Main java is on source path", javaCp.findResource("MainJava.java"));
    assertNotNull("Main scala is on source path", javaCp.findResource("MainScala.scala"));
    assertNull("Test scala is not on source path", javaCp.findResource("TestScala.scala"));

    var scalaTestFile = root.getFileObject("src/test/scala/TestScala.scala");
    assertNotNull("Test scala found", scalaTestFile);
    for (var g : javaGroups) {
      if (g.contains(scalaTestFile)) {
        assertEquals("test/scala", g.getName());
      }
    }
  }

  private static FileObject setLanguageServerProjectUp() throws IOException {
    var fs = FileUtil.createMemoryFileSystem();
    var root = fs.getRoot().createFolder("langsrv");
    var src = root.createFolder("src");
    var srcBench = src.createFolder("bench");
    var srcBenchScala = srcBench.createFolder("scala");
    var srcTest = src.createFolder("test");
    var srcTestScala = srcTest.createFolder("scala");
    var srcTestScalaFile = srcTestScala.createData("TestScala.scala");
    var srcTestResources = srcTest.createFolder("resources");
    var srcMain = src.createFolder("main");
    var srcMainJava = srcMain.createFolder("java");
    var srcMainJavaFile = srcMainJava.createData("MainJava.java");
    var srcMainResources = srcMain.createFolder("resources");
    var srcMainScala = srcMain.createFolder("scala");
    var srcMainScalaFile = srcMainScala.createData("MainScala.scala");
    var srcMainSchema = srcMain.createFolder("schema");
    var ensoSources = root.createData(".enso-sources");
    try (var os = root.createAndOpen(".enso-sources-classes")) {
      var txt = """
      java.home=/graalvm-ce-java17-22.3.1
      target=11
      output=./target/scala-2.13/classes
      input=./src/main/java
      generated=./target/scala-2.13/src_managed/main
      options.9=./target/scala-2.13/classes
      options.8=-classpath
      options.7=--enable-preview
      options.6=19
      options.5=-source
      options.4=-Xlint\\:unchecked
      options.3=-g
      options.2=-deprecation
      options.1=UTF-8
      options.0=-encoding
      """;
      os.write(txt.getBytes(StandardCharsets.UTF_8));
    }
    return root;
  }
}
