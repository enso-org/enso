package org.enso.tools.enso4igv.enso;

import javax.swing.Action;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.spi.project.ProjectState;
import org.netbeans.spi.project.ui.LogicalViewProvider;
import org.netbeans.spi.project.ui.support.CommonProjectActions;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.nodes.AbstractNode;
import org.openide.nodes.Children;
import org.openide.nodes.Node;
import org.openide.util.Exceptions;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.lookup.Lookups;

@NbBundle.Messages({
    "LAB_EnsoSources=Enso Sources"
})
public final class EnsoYamlProject implements Project {

  private final FileObject prj;
  private final ProjectState ps;
  private final Lookup lkp;

  EnsoYamlProject(FileObject fo, ProjectState ps) {
    this.prj = fo;
    this.ps = ps;
    this.lkp = Lookups.fixed(
        this,
        new LogicalView()
    );
  }
  
  public static Project create(FileObject fo, ProjectState ps) {
      return new EnsoYamlProject(fo, ps);
  }

  @Override
  public FileObject getProjectDirectory() {
    return prj;
  }

  @Override
  public Lookup getLookup() {
    return lkp;
  }

  @Override
  public String toString() {
    return "EnsoYamlProject{prj=" + prj + "}";
  }

  private final class LogicalView implements LogicalViewProvider {

    LogicalView() {
    }

    @Override
    public Node createLogicalView() {
      return new LogicalNode(EnsoYamlProject.this);
    }

    @Override
    public Node findPath(Node node, Object o) {
      if (o instanceof String path) {
        return org.openide.nodes.NodeOp.findChild(node, path);
      } else {
        return null;
      }
    }
  }

  private static class ContainerNode extends AbstractNode {
    ContainerNode(Children ch, Lookup l) {
      super(ch, l);
      setIconBaseWithExtension("org/enso/tools/enso4igv/enso.svg");
    }
      
    @Override
    public String getHtmlDisplayName() {
      return null;
    }

    @Override
    public Action[] getActions(boolean context) {
      return CommonProjectActions.forType("ensoprj"); // NOI18N
    }
  }

  private static final class LogicalNode extends ContainerNode {
    private final EnsoYamlProject project;

    private LogicalNode(EnsoYamlProject p) {
      super(createChildren(p), Lookups.fixed(p));
      this.project = p;
      var nameDir = p.getProjectDirectory();
      if ("0.0.0-dev".equals(nameDir.getNameExt())) {
          nameDir = nameDir.getParent();
      }
      setName(nameDir.getNameExt());
    }
    
    
    private static Children createChildren(EnsoYamlProject p) {
        var ch = new Children.Array();
        try {
            var src = p.getProjectDirectory().getFileObject("src", false);
            var srcNode = DataObject.find(src).getNodeDelegate().cloneNode();
            srcNode.setDisplayName(Bundle.LAB_EnsoSources());
            ch.add(new Node[]{srcNode});
        } catch (DataObjectNotFoundException ex) {
            Exceptions.printStackTrace(ex);
        }
        try {
            var yaml = p.getProjectDirectory().getFileObject("package.yaml", false);
            var srcNode = DataObject.find(yaml).getNodeDelegate().cloneNode();
            ch.add(new Node[]{srcNode});
        } catch (DataObjectNotFoundException ex) {
            Exceptions.printStackTrace(ex);
        }
        return ch;
    }
  }
}
