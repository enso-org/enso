package org.enso.tools.enso4igv;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Properties;
import javax.swing.event.ChangeListener;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.classpath.GlobalPathRegistry;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.spi.java.classpath.ClassPathProvider;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.netbeans.spi.java.project.support.ProjectPlatform;
import org.netbeans.spi.java.queries.CompilerOptionsQueryImplementation;
import org.netbeans.spi.java.queries.SourceLevelQueryImplementation2;
import org.netbeans.spi.project.ui.ProjectOpenedHook;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Exceptions;

final class EnsoSbtClassPathProvider extends ProjectOpenedHook
implements ClassPathProvider, SourceLevelQueryImplementation2, CompilerOptionsQueryImplementation {
    private static final String BOOT = "classpath/boot";
    private static final String SOURCE = "classpath/source";
    private static final String COMPILE = "classpath/compile";
    private final EnsoSbtProject project;
    private final EnsoSources info;

    EnsoSbtClassPathProvider(EnsoSbtProject prj) {
        this.project = prj;
        this.info = computeSbtClassPath(prj);
    }

    @Override
    public ClassPath findClassPath(FileObject file, String type) {
        if (FileUtil.isParentOf(project.getProjectDirectory(), file)) {
            return switch (type) {
                case SOURCE -> info.srcCp;
                case COMPILE -> info.cp;
                case BOOT -> info.platform.getBootstrapLibraries();
                default -> null;
            };
        } else {
            return null;
        }
    }

    @Override
    public void projectOpened() {
        GlobalPathRegistry.getDefault().register(COMPILE, new ClassPath[] { info.cp });
        GlobalPathRegistry.getDefault().register(SOURCE, new ClassPath[] { info.srcCp });
    }

    @Override
    public void projectClosed() {
        GlobalPathRegistry.getDefault().unregister(COMPILE, new ClassPath[] { info.cp });
        GlobalPathRegistry.getDefault().unregister(SOURCE, new ClassPath[] { info.srcCp });
    }

    private static EnsoSources computeSbtClassPath(EnsoSbtProject prj) {
        var platform = JavaPlatform.getDefault();
        var roots = new LinkedHashSet<>();
        var generatedSources = new LinkedHashSet<>();
        var source = "11";
        var options = new ArrayList<String>();
        for (FileObject ch : prj.getProjectDirectory().getChildren()) {
            if (ch.getNameExt().startsWith(".enso-sources")) {
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
            }
        }

        var srcRoots = new LinkedHashSet<>();
        var srcDir = prj.getProjectDirectory().getFileObject("src");
        if (srcDir != null) {
            for (var group : srcDir.getChildren()) {
                if (group.isFolder()) {
                    for (var ch : group.getChildren()) {
                        if (ch.isFolder()) {
                            srcRoots.add(ch);
                        }
                    }
                }
            }
        }
        srcRoots.addAll(generatedSources);
        var cp = ClassPathSupport.createClassPath(roots.toArray(new FileObject[0]));
        var srcCp = ClassPathSupport.createClassPath(srcRoots.toArray(new FileObject[0]));
        return new EnsoSources(cp, srcCp, platform, source, options);
    }

    @Override
    public SourceLevelQueryImplementation2.Result getSourceLevel(FileObject fo) {
        return new SourceLevelQueryImplementation2.Result() {
            @Override
            public String getSourceLevel() {
                return info.source;
            }

            @Override
            public void addChangeListener(ChangeListener cl) {
            }

            @Override
            public void removeChangeListener(ChangeListener cl) {
            }
        };
    }

    @Override
    public CompilerOptionsQueryImplementation.Result getOptions(FileObject fo) {
        return new CompilerOptionsQueryImplementation.Result() {
            @Override
            public List<? extends String> getArguments() {
                return info.options;
            }

            @Override
            public void addChangeListener(ChangeListener cl) {
            }

            @Override
            public void removeChangeListener(ChangeListener cl) {
            }
        };
    }



    private record EnsoSources(
        ClassPath cp, ClassPath srcCp,
        JavaPlatform platform,
        String source, List<String> options
    ) {
    }
}
