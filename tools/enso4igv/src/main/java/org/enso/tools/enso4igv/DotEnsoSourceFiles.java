package org.enso.tools.enso4igv;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import static org.enso.tools.enso4igv.Installer.LOG;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.netbeans.spi.java.project.support.ProjectPlatform;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Exceptions;

/** Support for reading the value from the .enso-source files. */
abstract class DotEnsoSourceFiles {
    /** directory with the .enso-source* files */
    private final FileObject dir;

    DotEnsoSourceFiles(FileObject dir) {
        this.dir = dir;
    }

    SourceGroup[] getSourceGroups() {
        var sources = new ArrayList<SourceGroup>();
        String platformPath = null;
        var roots = new LinkedHashSet<FileObject>();
        var modulePath = new LinkedHashSet<FileObject>();
        var generatedSources = new LinkedHashSet<FileObject>();
        var source = "21";
        var options = new ArrayList<String>();
        for (var ch : dir.getChildren()) {
            if (ch.getNameExt().startsWith(".enso-sources-")) {
                Properties p = new Properties();
                try (InputStream is = ch.getInputStream()) {
                    p.load(is);
                } catch (IOException ex) {
                    Exceptions.printStackTrace(ex);
                }
                if (p.get("java.home") instanceof String javaHome) {
                    platformPath = javaHome;
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
                            FileObject fo = findProjectFileObject(element);
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
                    if ("--module-path".equals(prop) && next != null) {
                        var paths = next.split(File.pathSeparator);
                        for (var element : paths) {
                            FileObject fo = findProjectFileObject(element);
                            if (fo != null) {
                                if (fo.isFolder()) {
                                    modulePath.add(fo);
                                } else {
                                    var jarRoot = FileUtil.getArchiveRoot(fo);
                                    modulePath.add(jarRoot);
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
                var srcRoots = new LinkedHashSet<FileObject>();

                var inputSrc = p.getProperty("input");
                var inputDir = findProjectFileObject(inputSrc);
                if (inputDir != null) {
                  var addSibblings = true;
                  if (inputDir.getNameExt().equals("org")) {
                    // lib/rust/parser doesn't follow typical project conventions
                    inputDir = inputDir.getParent();
                    addSibblings = false;
                  }
                  srcRoots.add(inputDir);
                  if (addSibblings) {
                    for (var sibbling : inputDir.getParent().getChildren()) {
                      if (sibbling.isFolder() && sibbling != inputDir) {
                        srcRoots.add(sibbling);
                      }
                    }
                  }
                } else {
                  var srcDir = findProjectFileObject("src");
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
                }
                srcRoots.addAll(generatedSources);

                var outputSrc = p.getProperty("output");
                FileObject outputDir = findProjectFileObject(outputSrc);

                var generatedSrc = p.getProperty("generated");
                FileObject generatedDir = findProjectFileObject(generatedSrc);
                if (generatedDir != null) {
                  srcRoots.add(generatedDir);
                }

                for (var r : srcRoots) {
                  assert r.isFolder() : "Expecting folders in " + srcRoots;
                }

                var cp = ClassPathSupport.createClassPath(roots.toArray(new FileObject[0]));
                var moduleCp = ClassPathSupport.createClassPath(modulePath.toArray(new FileObject[0]));
                var srcCp = ClassPathSupport.createClassPath(srcRoots.toArray(new FileObject[0]));
                var s = createSourceGroup(cp, moduleCp, srcCp, platformPath, outputDir, source, options);
                if (s.getRootFolder() == null) {
                    LOG.log(Level.WARNING, "Cannot find root folder for {0}", dir);
                } else {
                    if ("main".equals(s.getName())) {
                      sources.add(0, s);
                    } else {
                      sources.add(s);
                    }
                }
            }
        }
        if (dir.getFileObject("src") instanceof FileObject src && src.isFolder()) {
            for (var kind : src.getChildren()) {
              TYPE: for (var type : kind.getChildren()) {
                    for (var e : sources) {
                      if (e.getRootFolder().equals(type)) {
                        continue TYPE;
                      }
                    }

                    var s = new EnsoSbtClassPathProvider.OtherEnsoSources(kind.getNameExt(), type);
                    sources.add(s);
                }
            }
        }
        return sources.toArray(new SourceGroup[0]);
    }

    protected abstract EnsoSbtClassPathProvider.EnsoSources createSourceGroup(
        ClassPath cp, ClassPath moduleCp, ClassPath srcCp,
        String platformPath,
        FileObject outputDir, String source, List<String> options
    );

    private FileObject findProjectFileObject(String path) {
        if (path == null) {
            return null;
        }
        if (path.startsWith("./")) {
            return dir.getFileObject(path.substring(2));
        } else {
            return FileUtil.toFileObject(new File(path));
        }
    }


}
