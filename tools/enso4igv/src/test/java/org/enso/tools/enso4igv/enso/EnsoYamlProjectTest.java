package org.enso.tools.enso4igv.enso;

import java.io.File;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.stream.Stream;
import static junit.framework.TestCase.assertNotNull;
import org.enso.tools.enso4igv.EnsoSbtProjectTest;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.junit.NbTestCase;
import org.netbeans.spi.project.ui.LogicalViewProvider;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
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

        var lvp = prj.getLookup().lookup(LogicalViewProvider.class);

        var node = lvp.createLogicalView();

        assertEquals(ch.getName(), node.getName());
        var prjNodes = node.getChildren().getNodes(true);
        assertEquals("Two nodes", 2, prjNodes.length);
        assertEquals("package.yaml", prjNodes[1].getName());
        assertEquals("represents the package.yaml file", yaml, prjNodes[1].getLookup().lookup(FileObject.class));
        assertEquals("src", prjNodes[0].getName());
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
