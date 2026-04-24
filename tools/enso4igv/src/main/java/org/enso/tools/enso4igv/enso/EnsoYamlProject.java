package org.enso.tools.enso4igv.enso;

import java.awt.GraphicsEnvironment;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.beans.BeanInfo;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.Collections;
import java.util.Set;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.spi.java.project.support.ui.PackageView;
import org.netbeans.spi.project.ProjectContainerProvider;
import org.netbeans.spi.project.ProjectState;
import org.netbeans.spi.project.SubprojectProvider;
import org.netbeans.spi.project.ui.LogicalViewProvider;
import org.netbeans.spi.project.ui.support.CommonProjectActions;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.nodes.AbstractNode;
import org.openide.nodes.Children;
import org.openide.nodes.FilterNode;
import org.openide.nodes.Node;
import org.openide.nodes.NodeNotFoundException;
import org.openide.nodes.NodeOp;
import org.openide.util.Exceptions;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.datatransfer.ExTransferable;
import org.openide.util.lookup.Lookups;

@NbBundle.Messages({
  "LAB_EnsoSources=Enso Sources",
  "LAB_EnsoDocumentation=Documentation",
  "LAB_EnsoData=Data",
  "LAB_EnsoPolyglot=Polyglot Sources"
})
public final class EnsoYamlProject implements Project {

  private final FileObject prj;
  private final FileObject root;
  private final ProjectState ps;
  private final Lookup lkp;

  EnsoYamlProject(FileObject fo, FileObject root, ProjectState ps) {
    this.prj = fo;
    this.root = root;
    this.ps = ps;
    this.lkp = Lookups.fixed(
      this,
      new LogicalView(),
      new OwnSubproject(),
      new EnsoActionProvider(this)
    );
  }

  final FileObject getRoot() {
    return root;
  }

  public static Project create(FileObject fo, ProjectState ps) throws IOException {
    var dev000 = fo;
    if (fo.getFileObject("package.yaml") == null) {
      dev000 = fo.getFileObject("0.0.0-dev");
      if (dev000 == null) {
        throw new IOException();
      }
    } else {
      if (fo.getNameExt().equals("0.0.0-dev")) {
        throw new IOException();
      }
    }
    return new EnsoYamlProject(fo, dev000, ps);
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

  private final class OwnSubproject implements ProjectContainerProvider, SubprojectProvider {

    @Override
    public Set<? extends Project> getSubprojects() {
      if (GraphicsEnvironment.isHeadless()) {
        return Collections.emptySet();
      } else {
        return Collections.singleton(EnsoYamlProject.this);
      }
    }

    @Override
    public void addChangeListener(ChangeListener cl) {
    }

    @Override
    public void removeChangeListener(ChangeListener cl) {
    }

    @Override
    public ProjectContainerProvider.Result getContainedProjects() {
      var result = new ProjectContainerProvider.Result(getSubprojects(), false);
      return result;
    }
  }

  private static class JavaLibsChildren extends FilterNode.Children {
    public JavaLibsChildren(Node node) {
      super(node);
    }

    @Override
    protected Node copyNode(Node node) {
      var jar = node.getLookup().lookup(FileObject.class);
      if (jar != null && FileUtil.isArchiveFile(jar)) {
        var root = FileUtil.getArchiveRoot(jar);
        var group = new SourceGroup() {
          @Override
          public FileObject getRootFolder() {
            return root;
          }

          @Override
          public String getName() {
            return node.getName();
          }

          @Override
          public String getDisplayName() {
            return node.getDisplayName();
          }

          @Override
          public Icon getIcon(boolean opened) {
            return ImageUtilities.image2Icon(opened ? node.getOpenedIcon(BeanInfo.ICON_COLOR_32x32) : node.getIcon(BeanInfo.ICON_COLOR_32x32));
          }

          @Override
          public boolean contains(FileObject file) {
            return true;
          }

          @Override
          public void addPropertyChangeListener(PropertyChangeListener listener) {
          }

          @Override
          public void removePropertyChangeListener(PropertyChangeListener listener) {
          }
        };
        return PackageView.createPackageView(group);
      }
      return node.cloneNode();
    }
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
        return switch (o) {
            case String path -> NodeOp.findChild(node, path);
            case FileObject fileToFind -> {
                if (FileUtil.isArchiveArtifact(fileToFind)) {
                    var jar = FileUtil.getArchiveFile(fileToFind);
                    if (jar != null) {
                        fileToFind = jar;
                    }
                }

                for (var rootNode : node.getChildren().getNodes(true)) {
                    var rootFile = rootNode.getLookup().lookup(FileObject.class);
                    if (rootFile == null) {
                        continue;
                    }
                    if (rootFile.equals(fileToFind)) {
                        yield rootNode;
                    }
                    if (FileUtil.isParentOf(rootFile, fileToFind)) {
                        var path = FileUtil.getRelativePath(rootFile, fileToFind).split("/");
                        Node subNode;
                        try {
                            subNode = NodeOp.findPath(rootNode, path);
                        } catch (NodeNotFoundException ex) {
                            subNode = ex.getClosestNode();
                            for (var ssn :ex.getClosestNode().getChildren().getNodes(true)) {
                                if (fileToFind.equals(ssn.getLookup().lookup(FileObject.class))) {
                                    subNode = ssn;
                                }
                            }
                        }
                        yield subNode;
                    }
                }
                yield null;
            }
            default -> null;
        };
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
      return CommonProjectActions.forType(EnsoYamlActions.ID);
    }
  }

  private static final class LogicalNode extends ContainerNode {
    private final EnsoYamlProject project;

    private LogicalNode(EnsoYamlProject p) {
      super(createChildren(p), p.getLookup());
      this.project = p;
      var nameDir = p.getProjectDirectory();
      setName(nameDir.getNameExt());
    }

    private static Children createChildren(EnsoYamlProject p) {
      var ch = new Children.Array();
      try {
        var src = p.root.getFileObject("src", false);
        var srcNode = DataObject.find(src).getNodeDelegate().cloneNode();
        srcNode.setDisplayName(Bundle.LAB_EnsoSources());
        ch.add(new Node[]{srcNode});
      } catch (DataObjectNotFoundException ex) {
        Exceptions.printStackTrace(ex);
      }
      try {
        var poly = p.root.getFileObject("polyglot", true);
        if (poly != null) {
          var polyOrigNode = DataObject.find(poly).getNodeDelegate();
          var polyNode = new FilterNode(polyOrigNode, new FilterNode.Children(polyOrigNode) {
            @Override
            protected Node copyNode(Node node) {
              if ("java".equals(node.getName()) && node.getLookup().lookup(FileObject.class) instanceof FileObject folder) {
                return new FilterNode(node, new JavaLibsChildren(node));
              } else {
                return node.cloneNode();
              }
            }
          });
          polyNode.setDisplayName(Bundle.LAB_EnsoPolyglot());
          ch.add(new Node[]{polyNode});
        }
      } catch (DataObjectNotFoundException ex) {
        Exceptions.printStackTrace(ex);
      }
      try {
        var docs = p.root.getFileObject("docs", true);
        if (docs != null) {
          var docsNode = DataObject.find(docs).getNodeDelegate().cloneNode();
          docsNode.setDisplayName(Bundle.LAB_EnsoDocumentation());
          ch.add(new Node[]{docsNode});
        }
      } catch (DataObjectNotFoundException ex) {
        Exceptions.printStackTrace(ex);
      }
      try {
        var data = p.root.getFileObject("data", true);
        if (data != null) {
          var dataNode = new FilterNode(DataObject.find(data).getNodeDelegate()) {
              @Override
              public Transferable clipboardCopy() throws IOException {
                  var t = ExTransferable.create(super.clipboardCopy());
                  var dataDir = new ExTransferable.Single(DataFlavor.stringFlavor) {
                      @Override
                      protected Object getData() throws IOException, UnsupportedFlavorException {
                          return "Meta.Enso_Project.enso_project.data";
                      }
                  };
                  t.put(dataDir);
                  return t;
              }
          };
          dataNode.setDisplayName(Bundle.LAB_EnsoData());
          ch.add(new Node[]{dataNode});
        }
      } catch (DataObjectNotFoundException ex) {
        Exceptions.printStackTrace(ex);
      }
      try {
        var yaml = p.root.getFileObject("package.yaml", false);
        var srcNode = DataObject.find(yaml).getNodeDelegate().cloneNode();
        ch.add(new Node[]{srcNode});
      } catch (DataObjectNotFoundException ex) {
        Exceptions.printStackTrace(ex);
      }
      return ch;
    }
  }
}
