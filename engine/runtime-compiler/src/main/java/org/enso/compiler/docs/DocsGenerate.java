package org.enso.compiler.docs;

import static org.enso.scala.wrapper.ScalaConversions.asJava;
import static org.enso.scala.wrapper.ScalaConversions.asScala;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.IdentityHashMap;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.filesystem.FileSystem;
import org.enso.pkg.QualifiedName;

/** Generator of documentation for an Enso project. */
public final class DocsGenerate {
  private DocsGenerate() {}

  /**
   * Iterate over all provide modules and generate documentation using {@code pkg}'s {@link
   * FileSystem}.
   *
   * @param <File> abstract file to operate with
   * @param visitor visitor to use to generate the output
   * @param pkg library to generate the documentation for
   * @param modules parsed modules found in the library
   * @return directory where the output was generated
   * @throws IOException when I/O problem occurs
   */
  public static <File> File write(
      DocsVisit visitor, org.enso.pkg.Package<File> pkg, Iterable<CompilerContext.Module> modules)
      throws IOException {
    var fs = pkg.fileSystem();
    var apiDir = defaultOutputDir(pkg);
    fs.createDirectories(apiDir);

    for (var module : modules) {
      if (module.isSynthetic()) {
        continue;
      }
      var ir = module.getIr();
      assert ir != null : "need IR for " + module;
      if (ir.isPrivate()) {
        continue;
      }
      var moduleName = module.getName();
      var dir = createDirs(fs, apiDir, stripNamespace(moduleName));
      var md = fs.getChild(dir, moduleName.item() + ".md");
      try (var mdWriter = fs.newBufferedWriter(md);
          var pw = new PrintWriter(mdWriter)) {
        visitModule(visitor, moduleName, ir, pw);
      }
    }
    return apiDir;
  }

  public static <File> File defaultOutputDir(org.enso.pkg.Package<File> pkg) {
    var fs = pkg.fileSystem();
    var docs = fs.getChild(pkg.root(), "docs");
    var api = fs.getChild(docs, "api");
    return api;
  }

  /**
   * Strips namespace part from the given qualified {@code name}.
   *
   * @param name
   */
  private static QualifiedName stripNamespace(QualifiedName name) {
    if (!name.isSimple()) {
      var path = name.pathAsJava();
      assert path.size() >= 2;
      var dropped = path.subList(2, path.size());
      return new QualifiedName(asScala(dropped), name.item());
    } else {
      return name;
    }
  }

  private static <File> File createDirs(FileSystem<File> fs, File root, QualifiedName pkg)
      throws IOException {
    var dir = root;
    for (var item : pkg.pathAsJava()) {
      dir = fs.getChild(dir, item);
    }
    fs.createDirectories(dir);
    return dir;
  }

  public static void visitModule(
      DocsVisit visitor, QualifiedName moduleName, Module ir, PrintWriter w) throws IOException {
    var dispatch = DocsDispatch.create(visitor, w);

    if (dispatch.dispatchModule(moduleName, ir)) {
      var moduleBindings = BindingSorter.sortedBindings(ir);
      var alreadyDispatched = new IdentityHashMap<IR, IR>();
      for (var b : moduleBindings) {
        if (alreadyDispatched.containsKey(b)) {
          continue;
        }
        switch (b) {
          case Definition.Type t -> {
            if (dispatch.dispatchType(t)) {
              for (var d : BindingSorter.sortConstructors(asJava(t.members()))) {
                if (!d.isPrivate()) {
                  dispatch.dispatchConstructor(t, d);
                }
              }
              for (var mb : moduleBindings) {
                if (mb instanceof Method.Explicit m) {
                  if (m.isStaticWrapperForInstanceMethod() || m.isPrivate()) {
                    alreadyDispatched.put(m, m);
                    continue;
                  }
                  var p = m.methodReference().typePointer();
                  if (p.isDefined()) {
                    var methodTypeName = p.get().name();
                    if (methodTypeName.equals(t.name().name())) {
                      dispatch.dispatchMethod(t, m);
                      alreadyDispatched.put(m, m);
                    }
                  }
                }
              }
            }
          }
          case Method.Explicit m -> {
            if (!m.isPrivate()) {
              dispatch.dispatchMethod(null, m);
            }
          }
          case Method.Conversion c -> dispatch.dispatchConversion(c);
          default -> throw new AssertionError("unknown type " + b.getClass());
        }
      }
    }
  }
}
