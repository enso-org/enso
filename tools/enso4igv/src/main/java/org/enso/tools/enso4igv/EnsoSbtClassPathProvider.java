package org.enso.tools.enso4igv;

import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Properties;
import javax.swing.Icon;
import javax.swing.event.ChangeListener;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.classpath.GlobalPathRegistry;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.api.java.queries.SourceForBinaryQuery;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.spi.java.classpath.ClassPathProvider;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.netbeans.spi.java.project.support.ProjectPlatform;
import org.netbeans.spi.java.queries.BinaryForSourceQueryImplementation2;
import org.netbeans.spi.java.queries.CompilerOptionsQueryImplementation;
import org.netbeans.spi.java.queries.SourceForBinaryQueryImplementation2;
import org.netbeans.spi.java.queries.SourceLevelQueryImplementation2;
import org.netbeans.spi.project.support.GenericSources;
import org.netbeans.spi.project.ui.ProjectOpenedHook;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.util.Exceptions;
import org.openide.util.ImageUtilities;

final class EnsoSbtClassPathProvider extends ProjectOpenedHook
implements ClassPathProvider, SourceLevelQueryImplementation2, CompilerOptionsQueryImplementation,
Sources, BinaryForSourceQueryImplementation2<EnsoSbtClassPathProvider.EnsoSources>, SourceForBinaryQueryImplementation2 {
    private static final String BOOT = "classpath/boot";
    private static final String SOURCE = "classpath/source";
    private static final String COMPILE = "classpath/compile";
    private final EnsoSbtProject project;
    private final EnsoSources[] sources;

    EnsoSbtClassPathProvider(EnsoSbtProject prj) {
        this.project = prj;
        this.sources = computeSbtClassPath(prj);
    }

    @Override
    public ClassPath findClassPath(FileObject file, String type) {
        for (var i : sources) {
            if (i.controlsSource(file)) {
                return switch (type) {
                    case SOURCE -> i.srcCp;
                    case COMPILE -> i.cp;
                    case BOOT -> i.platform.getBootstrapLibraries();
                    default -> null;
                };
            }
        }
        return null;
    }

    @Override
    public void projectOpened() {
        for (var i : sources) {
            GlobalPathRegistry.getDefault().register(COMPILE, new ClassPath[] { i.cp });
            GlobalPathRegistry.getDefault().register(SOURCE, new ClassPath[] { i.srcCp });
        }
    }

    @Override
    public void projectClosed() {
        for (var i : sources) {
            GlobalPathRegistry.getDefault().unregister(COMPILE, new ClassPath[] { i.cp });
            GlobalPathRegistry.getDefault().unregister(SOURCE, new ClassPath[] { i.srcCp });
        }
    }

    private static EnsoSources[] computeSbtClassPath(EnsoSbtProject prj) {
        var sources = new ArrayList<EnsoSources>();
        var platform = JavaPlatform.getDefault();
        var roots = new LinkedHashSet<>();
        var generatedSources = new LinkedHashSet<>();
        var source = "19";
        var options = new ArrayList<String>();
        for (FileObject ch : prj.getProjectDirectory().getChildren()) {
            if (ch.getNameExt().startsWith(".enso-sources-")) {
                Properties p = new Properties();
                try (InputStream is = ch.getInputStream()) {
                    p.load(is);
                } catch (IOException ex) {
                    Exceptions.printStackTrace(ex);
                }
                if (p.get("java.home") instanceof String javaHome) {
                    var javaHomeFile = new File(javaHome);
                    var javaHomeFo = FileUtil.toFileObject(javaHomeFile);
                    platform = ProjectPlatform.forProject(prj, javaHomeFo, javaHomeFile.getName(), "j2se");
                }

                for (var i = 0; ; i++) {
                    final String prop = p.getProperty("options." + i);
                    if (prop == null) {
                        break;
                    }
                    var next = p.getProperty("options." + (i + 1));
                    if ("-source".equals(prop) && next != null) {
                        source = next;
                        i++;
                        continue;
                    }
                    if ("-classpath".equals(prop) && next != null) {
                        var paths = next.split(File.pathSeparator);
                        for (var element : paths) {
                            File file = new File(element);
                            FileObject fo = FileUtil.toFileObject(file);
                            if (fo != null) {
                                if (fo.isFolder()) {
                                    roots.add(fo);
                                } else {
                                    var jarRoot = FileUtil.getArchiveRoot(fo);
                                    roots.add(jarRoot);
                                }
                            }
                        }
                        i++;
                        continue;
                    }
                    if ("-s".equals(prop) && next != null) {
                        var fo = FileUtil.toFileObject(new File(next));
                        if (fo != null) {
                            generatedSources.add(fo);
                        }
                    }
                    options.add(prop);
                }
                var srcRoots = new LinkedHashSet<>();

                var inputSrc = p.getProperty("input");
                var inputDir = inputSrc != null ? FileUtil.toFileObject(new File(inputSrc)) : null;
                if (inputDir != null) {
                  if (inputDir.getNameExt().equals("org")) {
                    // lib/rust/parser doesn't follow typical project conventions
                    inputDir = inputDir.getParent();
                  }
                  srcRoots.add(inputDir);
                }

                var srcDir = prj.getProjectDirectory().getFileObject("src");
                if (srcDir != null) {
                    for (var group : srcDir.getChildren()) {
                        if (group.isFolder()) {
                            for (var child : group.getChildren()) {
                                if (child.isFolder()) {
                                    srcRoots.add(child);
                                }
                            }
                        }
                    }
                }
                srcRoots.addAll(generatedSources);

                var outputSrc = p.getProperty("output");
                var outputDir = outputSrc != null ? FileUtil.toFileObject(new File(outputSrc)) : null;

                var generatedSrc = p.getProperty("generated");
                var generatedDir = generatedSrc != null ? FileUtil.toFileObject(new File(generatedSrc)) : null;
                if (generatedDir != null) {
                  srcRoots.add(generatedDir);
                }


                var cp = ClassPathSupport.createClassPath(roots.toArray(new FileObject[0]));
                var srcCp = ClassPathSupport.createClassPath(srcRoots.toArray(new FileObject[0]));
                var s = new EnsoSources(cp, srcCp, platform, outputDir, source, options);
                if ("main".equals(s.getName())) {
                  sources.add(0, s);
                } else {
                  sources.add(s);
                }
            }
        }
        return sources.toArray(new EnsoSources[0]);
    }

    @Override
    public SourceLevelQueryImplementation2.Result getSourceLevel(FileObject fo) {
        for (var i : sources) {
            if (i.controlsSource(fo)) {
                return new SourceLevelQueryImplementation2.Result() {
                    @Override
                    public String getSourceLevel() {
                        return i.source;
                    }

                    @Override
                    public void addChangeListener(ChangeListener cl) {
                    }

                    @Override
                    public void removeChangeListener(ChangeListener cl) {
                    }
                };
            }
        }
        return null;
    }

    @Override
    public CompilerOptionsQueryImplementation.Result getOptions(FileObject fo) {
        for (var i : sources) {
            if (i.controlsSource(fo)) {
                return new CompilerOptionsQueryImplementation.Result() {
                    @Override
                    public List<? extends String> getArguments() {
                        return i.options;
                    }

                    @Override
                    public void addChangeListener(ChangeListener cl) {
                    }

                    @Override
                    public void removeChangeListener(ChangeListener cl) {
                    }
                };
            }
        }
        return null;
    }

    @Override
    public SourceGroup[] getSourceGroups(String type) {
        if (Sources.TYPE_GENERIC.equals(type)) {
            var dir = project.getProjectDirectory();
            var displayname = FileUtil.getFileDisplayName(dir);
            var icon = ImageUtilities.loadImageIcon("org/enso/tools/enso4igv/enso.svg", true);
            var genericGroup = GenericSources.group(project, dir, dir.getNameExt(), displayname, icon, icon);
            return new SourceGroup[] { genericGroup};
        }
        return sources;
    }

    @Override
    public void addChangeListener(ChangeListener cl) {
    }

    @Override
    public void removeChangeListener(ChangeListener cl) {
    }

    @Override
    public EnsoSources findBinaryRoots2(URL url) {
        var fo = URLMapper.findFileObject(url);
        for (var i : sources) {
          if (i.outputsTo(fo) || i.controlsSource(fo)) {
            return i;
          }
        }
        return null;
    }

    @Override
    public URL[] computeRoots(EnsoSources result) {
        return new URL[] { result.output().toURL() };
    }

    @Override
    public boolean computePreferBinaries(EnsoSources result) {
        return true;
    }

    @Override
    public void computeChangeListener(EnsoSources result, boolean bln, ChangeListener cl) {
    }

    @Override
    public SourceForBinaryQueryImplementation2.Result findSourceRoots2(URL url) {
        var fo = URLMapper.findFileObject(url);
        if (fo == null) {
            return null;
        }
        for (var i : sources) {
            if (i.outputsTo(fo) || i.controlsSource(fo)) {
                return new SourceForBinaryQueryImplementation2.Result() {
                    @Override
                    public boolean preferSources() {
                        return false;
                    }

                    @Override
                    public FileObject[] getRoots() {
                        return i.getRoots();
                    }

                    @Override
                    public void addChangeListener(ChangeListener l) {
                    }

                    @Override
                    public void removeChangeListener(ChangeListener l) {
                    }
                };
            }
        }
        return null;
    }

    @Override
    public SourceForBinaryQuery.Result findSourceRoots(URL binaryRoot) {
      return findSourceRoots2(binaryRoot);
    }

    record EnsoSources(
        ClassPath cp, ClassPath srcCp,
        JavaPlatform platform,
        FileObject output,
        String source, List<String> options
    ) implements SourceGroup {
        @Override
        public FileObject getRootFolder() {
            return srcCp.getRoots()[0];
        }

        private FileObject[] getRoots() {
            return srcCp.getRoots();
        }

        @Override
        public String getName() {
            return getRootFolder().getParent().getNameExt();
        }

        @Override
        public String getDisplayName() {
            return getName();
        }

        @Override
        public Icon getIcon(boolean bln) {
            return null;
        }

        @Override
        public boolean contains(FileObject fo) {
            if (getRootFolder().equals(fo)) {
                return true;
            }
            return FileUtil.isParentOf(getRootFolder(), fo);
        }

        @Override
        public void addPropertyChangeListener(PropertyChangeListener pl) {
        }

        @Override
        public void removePropertyChangeListener(PropertyChangeListener pl) {
        }

        private boolean controlsSource(FileObject fo) {
            return contains(fo) || srcCp.contains(fo);
        }

        private boolean outputsTo(FileObject fo) {
            if (fo == null || output() == null) {
                return false;
            }
            if (fo.equals(output())) {
                return true;
            }
            return FileUtil.isParentOf(output(), fo);
        }

        public String toString() {
            return "EnsoSources[name=" + getName() + ",root=" + getRootFolder() + ",output=" + output() + "]";
        }
    }
}
