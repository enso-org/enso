package org.enso.tools.enso4igv;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.stream.Collectors;
import org.enso.tools.enso4igv.enso.EnsoYamlProject;
import org.junit.AssumptionViolatedException;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.Sources;
import org.netbeans.junit.NbTestCase;
import org.netbeans.spi.project.SubprojectProvider;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Utilities;

public class EnsoSbtProjectTest extends NbTestCase {

  public EnsoSbtProjectTest(String name) {
    super(name);
  }

  @Override
  protected void setUp() throws Exception {
    clearWorkDir();
  }

  public void testRuntimeParserProjectFound() throws Exception {
    var root = findRepoRoot();

    var rootFO = FileUtil.toFileObject(root);
    var prj = ProjectManager.getDefault().findProject(rootFO);

    var spp = prj.getLookup().lookup(SubprojectProvider.class);
    assertNotNull("subprojects are supported", spp);
    var projects = spp.getSubprojects();

    assertTrue("At least few projects found: " + projects, projects.size() >= 5);

    var set = projects.stream().map(p -> p.getProjectDirectory()).collect(Collectors.toSet());

    assertTrue("runtime-parser found: " + set, set.contains(rootFO.getFileObject("engine/runtime-parser")));
  }

  public void testAllProjectsWillBeFound() throws Exception {
    var root = findRepoRoot();
    var sbt = new File(root, "build.sbt");
    if (!assumeEssentialsAreCompiled(root)) {
      return;
    }

    assertTrue("build script found", sbt.exists());
    var text = Files.readAllLines(sbt.toPath());
    var aggregateAndRest = text.stream().dropWhile(l -> !l.contains(".aggregate(")).toList();
    var aggregate = aggregateAndRest.stream().skip(1).takeWhile(l -> !l.contains(")")).toList();

    assertSimilar(aggregate.size() + " aggregates are we searching for: " + aggregate, 96, aggregate.size(), 20);

    var inFiles = text.stream().filter(l -> l.contains(".in(file(") || l.contains("project in file(")).toList();
    assertSimilar("Same amount of in(file( as aggregates", aggregate.size(), inFiles.size(), 10);

    var rootFO = FileUtil.toFileObject(root);
    var prj = ProjectManager.getDefault().findProject(rootFO);

    var spp = prj.getLookup().lookup(SubprojectProvider.class);
    assertNotNull("subprojects are supported", spp);
    var allProjects = spp.getSubprojects();
    var jvmProjects = allProjects.stream().filter(p -> p.getLookup().lookup(EnsoYamlProject.class) == null).toList();
    assertSimilar("Found exactly the same amount of projects: " + jvmProjects, aggregate.size(), jvmProjects.size(), 15);
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
    assertEquals("One at root", root.getFileObject("src"), genericGroups[0].getRootFolder());

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

  private static void assertSimilar(String msg, int expected, int actual, int allowedDifference) {
    if (Math.abs(expected - actual) <= allowedDifference) {
      return;
    }
    assertEquals(msg, expected, actual);
  }

  private static File findRepoRoot() throws URISyntaxException, IllegalArgumentException {
    var root = Utilities.toFile(EnsoSbtProjectTest.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    for (;;) {
      assertNotNull("Root isn't a dir", root);
      var sbt = new File(root, "build.sbt");
      if (sbt.exists()) {
        break;
      }
      root = root.getParentFile();
    }
    return root;
  }

  private static boolean assumeEssentialsAreCompiled(File root) {
    var engineRuntimeCheck = new File(new File(new File(root, "engine"), "runtime"), ".enso-sources");
    if (!engineRuntimeCheck.exists()) {
      var msg = """
      This test requires `sbt compile` to be executed first.
      Such command generates `.enso-sources` meta data.
      Without them running "check all" test makes little sense.
      Not found: """ + engineRuntimeCheck;
      var ex = new AssumptionViolatedException(msg);
      ex.printStackTrace();
      return false;
    } else {
      return true;
    }
  }

}
