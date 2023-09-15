package org.enso.tools.enso4igv;

import java.io.IOException;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import javax.swing.Action;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.spi.project.ProjectContainerProvider;
import org.netbeans.spi.project.ProjectState;
import org.netbeans.spi.project.SubprojectProvider;
import org.netbeans.spi.project.ui.LogicalViewProvider;
import org.netbeans.spi.project.ui.support.CommonProjectActions;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.nodes.AbstractNode;
import org.openide.nodes.ChildFactory;
import org.openide.nodes.Children;
import org.openide.nodes.Node;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;

final class EnsoRootProject implements Project {

  private final FileObject prj;
  private final ProjectState ps;
  private final Lookup lkp;

  EnsoRootProject(FileObject fo, ProjectState ps) {
    this.prj = fo;
    this.ps = ps;
    this.lkp = Lookups.fixed(
            this,
            new LogicalView(),
            new Subprojects()
    );
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
    return "EnsoRootProject{prj=" + prj + "}";
  }

  private final class LogicalView implements LogicalViewProvider {

    LogicalView() {
    }

    @Override
    public Node createLogicalView() {
      return new MainNode(EnsoRootProject.this);
    }

    @Override
    public Node findPath(Node node, Object o) {
      return org.openide.nodes.NodeOp.findChild(node, (String) o);
    }
  }

  private final class Subprojects implements ProjectContainerProvider, SubprojectProvider, Comparator<Project> {
    @Override
    public Set<? extends Project> getSubprojects() {
      var found = new TreeSet<Project>(this);
      searchForProjects(getProjectDirectory(), found, 3);
      System.err.println("subprojects: " + found);
      return found;
    }

    private static void searchForProjects(FileObject fo, Collection<Project> found, int depth) {
      if (fo.isFolder() && depth > 0) {
        if (EnsoProjectFactory.isProjectCheck(fo) == 1) {
          try {
            var p = ProjectManager.getDefault().findProject(fo);
            if (p != null) {
              found.add(p);
            }
          } catch (IllegalArgumentException | IOException ex) {
          }
        } else {
          for (var ch : fo.getChildren()) {
            searchForProjects(ch, found, depth - 1);
          }
        }
      }
    }

    @Override
    public void addChangeListener(ChangeListener cl) {
    }

    @Override
    public void removeChangeListener(ChangeListener cl) {
    }

    @Override
    public int compare(Project o1, Project o2) {
      var p1 = o1.getProjectDirectory().getPath();
      var p2 = o2.getProjectDirectory().getPath();
      return p1.compareTo(p2);
    }

    @Override
    public Result getContainedProjects() {
      var result = new Result(getSubprojects(), false);
      System.err.println("get contained fop: " + result.getProjects());
      return result;
    }
    
    private final class Factory extends ChildFactory<FileObject>  {
      @Override
      protected boolean createKeys(List<FileObject> list) {
        list.add(getProjectDirectory().getFileObject("README.md"));
        list.add(getProjectDirectory().getFileObject("build.sbt"));
        for (var p : getSubprojects()) {
          list.add(p.getProjectDirectory());
        }
        return true;
      }

      protected Node createNodeForKey(FileObject key) {
        try {
          try {
            var p = ProjectManager.getDefault().findProject(key);
            if (p != null && p.getLookup().lookup(LogicalViewProvider.class) instanceof LogicalViewProvider lvp) {
              return lvp.createLogicalView();
            }
          } catch (IOException | IllegalArgumentException ex) {
          }
          return DataObject.find(key).getNodeDelegate();
        } catch (DataObjectNotFoundException ex) {
          return null;
        }
      }
    }

  }

  private static final class MainNode extends AbstractNode {

    private final EnsoRootProject project;

    private MainNode(EnsoRootProject p) {
      super(Children.create(p.getLookup().lookup(Subprojects.class).new Factory(), true), Lookups.fixed(p));
      this.project = p;
      setDisplayName();
      setIconBaseWithExtension("org/enso/tools/enso4igv/enso.svg");
    }

    private void setDisplayName() {
      setDisplayName(ProjectUtils.getInformation(project).getDisplayName());
    }

    @Override
    public String getHtmlDisplayName() {
      return null;
    }

    @Override
    public Action[] getActions(boolean context) {
      return CommonProjectActions.forType("ensosbtprj"); // NOI18N
    }
  }
}
