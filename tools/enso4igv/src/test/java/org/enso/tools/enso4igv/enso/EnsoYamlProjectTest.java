package org.enso.tools.enso4igv.enso;

import java.awt.datatransfer.DataFlavor;
import java.io.File;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import junit.framework.TestCase;
import static junit.framework.TestCase.assertNotNull;
import org.enso.tools.enso4igv.EnsoSbtProjectTest;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.junit.NbTestCase;
import org.netbeans.spi.project.ui.LogicalViewProvider;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.nodes.Node;
import org.openide.util.Utilities;

public class EnsoYamlProjectTest extends NbTestCase {
  private FileObject root;

  public EnsoYamlProjectTest(String name) {
    super(name);
  }

  @Override
  protected void setUp() throws Exception {
    clearWorkDir();
    root = FileUtil.toFileObject(getWorkDir()).createFolder(getName());
  }

  public void testRecognizePackageYaml() throws Exception {
    var yaml = FileUtil.createData(root, "prj/package.yaml");
    var main = FileUtil.createData(root, "prj/src/Main.enso");

    var rootFO = root.getFileObject("prj");
    var prj = ProjectManager.getDefault().findProject(rootFO);
    assertNotNull("Project found", prj);
    var lvp = prj.getLookup().lookup(LogicalViewProvider.class);

    var node = lvp.createLogicalView();

    assertEquals("prj", node.getName());
    var prjNodes = node.getChildren().getNodes(true);
    assertEquals("Two nodes", 2, prjNodes.length);
    assertEquals("package.yaml", prjNodes[1].getName());
    assertEquals("represents the package.yaml file", yaml, prjNodes[1].getLookup().lookup(FileObject.class));
    assertEquals("src", prjNodes[0].getName());
    var srcNodes = prjNodes[0].getChildren().getNodes(true);
    assertEquals("One source", 1, srcNodes.length);
    assertEquals("Main", srcNodes[0].getName());
    assertEquals("represents the Main.enso file", main, srcNodes[0].getLookup().lookup(FileObject.class));

    var mainNode = lvp.findPath(node, main);
    assertEquals("Finds Main.enso node", srcNodes[0], mainNode);

    var yamlNode = lvp.findPath(node, yaml);
    assertEquals("Finds package.yaml node", prjNodes[1], yamlNode);
  }

  public void testRecognizeStandardDistributionWith000dev() throws Exception {
    var repoRoot = FileUtil.toFileObject(findRepoRoot());
    var stdlib = repoRoot.getFileObject("distribution/lib/Standard");
    assertNotNull("distribution/lib/Standard found", stdlib);

    var stdlibCount = 0;
    for (var ch : stdlib.getChildren()) {
        var prj = ProjectManager.getDefault().findProject(ch);
        assertNotNull("Project for " + ch + " found", prj);
        var info = ProjectUtils.getInformation(prj);
        assertEquals(ch.getName(), info.getDisplayName());

        var yaml = FileUtil.createData(ch, "0.0.0-dev/package.yaml");
        assertNotNull("There is package Yaml in the project " + ch, yaml);
        var main = FileUtil.createData(ch, "0.0.0-dev/src/Main.enso");
        assertNotNull("There is Main.enso in the project " + ch, main);

        var noPrj = ProjectManager.getDefault().findProject(yaml.getParent());
        assertNull("No project for 0.0.0-dev directory: " + noPrj, noPrj);

        var lvp = prj.getLookup().lookup(LogicalViewProvider.class);

        var node = lvp.createLogicalView();

        assertEquals(ch.getName(), node.getName());
        var prjNodes = node.getChildren().getNodes(true);
        var prjNames = Stream.of(prjNodes).map(n -> n.getName()).toList();
        assertEquals("package.yaml", prjNames.get(prjNames.size() - 1));
        assertEquals("represents the package.yaml file", yaml, prjNodes[prjNodes.length - 1].getLookup().lookup(FileObject.class));
        assertEquals("First node represents sources", "src", prjNames.get(0));
        var srcNodes = prjNodes[0].getChildren().getNodes(true);
        var foundMain = Stream.of(srcNodes)
            .filter(n -> "Main".equals(n.getName()))
            .findAny();
        assertTrue("Found main among: " + Arrays.toString(srcNodes), foundMain.isPresent());
        assertEquals("Main", foundMain.get().getName());
        assertEquals("represents the Main.enso file", main, foundMain.get().getLookup().lookup(FileObject.class));

        stdlibCount++;
    }

    assertTrue("Found enough libs in " + stdlib + " was: " + stdlibCount, stdlibCount > 5);
  }


  public void testPolyglot() throws Exception {
    var yaml = FileUtil.createData(root, "poly/package.yaml");
    var main = FileUtil.createData(root, "poly/src/Main.enso");
    var jar = FileUtil.createData(root, "poly/polyglot/java/junit.jar");
    var origJunitUrl = TestCase.class.getProtectionDomain().getCodeSource().getLocation();
    try (java.io.OutputStream os = jar.getOutputStream()) {
      FileUtil.copy(origJunitUrl.openStream(), os);
    }
    var js = FileUtil.createData(root, "poly/polyglot/js/test.js");
    var python = FileUtil.createData(root, "poly/polyglot/python/run.py");
    var libSo = FileUtil.createData(root, "poly/polyglot/lib/dummy.so");
    var libDll = FileUtil.createData(root, "poly/polyglot/lib/windows/dummy.dll");

    var rootFO = root.getFileObject("poly");
    var poly = ProjectManager.getDefault().findProject(rootFO);
    assertNotNull("Project found", poly);
    var lvp = poly.getLookup().lookup(LogicalViewProvider.class);

    var node = lvp.createLogicalView();

    assertEquals("poly", node.getName());
    var prjNodes = node.getChildren().getNodes(true);
    assertEquals("Three nodes", 3, prjNodes.length);

    assertEquals("package.yaml", prjNodes[2].getName());
    assertEquals("represents the package.yaml file", yaml, prjNodes[2].getLookup().lookup(FileObject.class));

    assertEquals("src", prjNodes[0].getName());
    assertEquals("Enso Sources", prjNodes[0].getDisplayName());

    assertEquals("polyglot", prjNodes[1].getName());
    assertEquals("Polyglot Sources", prjNodes[1].getDisplayName());
    var polyNodes = prjNodes[1].getChildren().getNodes(true);
    assertEquals("Few nodes: " + Arrays.toString(polyNodes), 4, polyNodes.length);

    var polyTypeNames = Stream.of(polyNodes).map(n -> n.getName()).distinct().sorted().toList();
    assertEquals(polyTypeNames, List.of("java", "js", "lib", "python"));

    var javaNode = Stream.of(polyNodes).filter(n -> "java".equals(n.getName())).findAny().get();
    var javaLibs = javaNode.getChildren().getNodes(true);
    assertEquals("There is one library", 1, javaLibs.length);
    assertEquals("junit.jar", javaLibs[0].getName());

    var packages = javaLibs[0].getChildren().getNodes(true);
    var testCasePkg = Stream.of(packages).filter(n -> TestCase.class.getPackageName().equals(n.getName())).findAny();
    assertTrue("junit framework package is found among " + Arrays.toString(packages), testCasePkg.isPresent());
  }

  public void testData() throws Exception {
    var yaml = FileUtil.createData(root, "data/package.yaml");
    var main = FileUtil.createData(root, "data/src/Main.enso");
    var input = FileUtil.createData(root, "data/data/input.txt");

    var rootFO = root.getFileObject("data");
    var poly = ProjectManager.getDefault().findProject(rootFO);
    assertNotNull("Project found", poly);
    var lvp = poly.getLookup().lookup(LogicalViewProvider.class);

    var node = lvp.createLogicalView();

    assertEquals("data", node.getName());
    var prjNodes = node.getChildren().getNodes(true);
    assertEquals("Three nodes", 3, prjNodes.length);

    assertEquals("package.yaml", prjNodes[2].getName());
    assertEquals("represents the package.yaml file", yaml, prjNodes[2].getLookup().lookup(FileObject.class));

    assertEquals("src", prjNodes[0].getName());
    assertEquals("Enso Sources", prjNodes[0].getDisplayName());

    var dataNode = prjNodes[1];
    assertEquals("data", dataNode.getName());
    assertEquals("Data", dataNode.getDisplayName());
    var dataNodes = dataNode.getChildren().getNodes(true);
    assertEquals("One nodes: " + Arrays.toString(dataNodes), 1, dataNodes.length);
    assertEquals("input.txt", dataNodes[0].getName());

    assertEquals(input, dataNodes[0].getLookup().lookup(FileObject.class));

    var copy = dataNode.clipboardCopy();
    var text = copy.getTransferData(DataFlavor.stringFlavor);
    assertEquals("Meta.Enso_Project.enso_project.data", text);
  }

  public void testDocumentation() throws Exception {
    var yaml = FileUtil.createData(root, "document/package.yaml");
    var main = FileUtil.createData(root, "document/src/Main.enso");
    var md = FileUtil.createData(root, "document/docs/md/README.md");
    var api = FileUtil.createData(root, "document/docs/api/Main.md");

    var rootFO = root.getFileObject("document");
    var document = ProjectManager.getDefault().findProject(rootFO);
    assertNotNull("Project found", document);
    var lvp = document.getLookup().lookup(LogicalViewProvider.class);

    var node = lvp.createLogicalView();

    assertEquals("document", node.getName());
    var prjNodes = node.getChildren().getNodes(true);
    assertEquals("Three nodes: " + Arrays.toString(prjNodes), 3, prjNodes.length);

    assertEquals("package.yaml", prjNodes[2].getName());
    assertEquals("represents the package.yaml file", yaml, prjNodes[2].getLookup().lookup(FileObject.class));

    assertEquals("src", prjNodes[0].getName());
    assertEquals("Enso Sources", prjNodes[0].getDisplayName());

    assertEquals("docs", prjNodes[1].getName());
    assertEquals("Documentation", prjNodes[1].getDisplayName());
    var docsNodes = prjNodes[1].getChildren().getNodes(true);
    assertEquals("Few nodes: " + Arrays.toString(docsNodes), 2, docsNodes.length);

    var polyTypeNames = Stream.of(docsNodes).map(n -> n.getName()).distinct().sorted().toList();
    assertEquals(polyTypeNames, List.of("api", "md"));

    var javaNode = Stream.of(docsNodes).filter(n -> "api".equals(n.getName())).findAny().get();
    var javaLibs = javaNode.getChildren().getNodes(true);
    assertEquals("There is one library", 1, javaLibs.length);
    assertEquals("Main.md", javaLibs[0].getName());
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

}
