package org.enso.tools.enso4igv.enso;

import org.netbeans.api.project.ProjectManager;
import org.netbeans.junit.NbTestCase;
import org.netbeans.spi.project.ui.LogicalViewProvider;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

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

}
