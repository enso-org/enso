package org.enso.compiler.dump;

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
import scala.collection.immutable.Seq;
import scala.jdk.CollectionConverters;

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
    var docs = fs.getChild(pkg.root(), "docs");
    var api = fs.getChild(docs, "api");
    fs.createDirectories(api);

    for (var module : modules) {
      var ir = module.getIr();
      assert ir != null : "need IR for " + module;
      if (ir.isPrivate()) {
        continue;
      }
      var moduleName = module.getName();
      var dir = createPkg(fs, api, moduleName);
      var md = fs.getChild(dir, moduleName.item() + ".md");
      try (var mdWriter = fs.newBufferedWriter(md);
          var pw = new PrintWriter(mdWriter)) {
        visitModule(visitor, moduleName, ir, pw);
      }
    }
    return api;
  }

  private static <File> File createPkg(FileSystem<File> fs, File root, QualifiedName pkg)
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
      var moduleBindings = asJava(ir.bindings());
      var alreadyDispatched = new IdentityHashMap<IR, IR>();
      for (var b : moduleBindings) {
        if (alreadyDispatched.containsKey(b)) {
          continue;
        }
        switch (b) {
          case Definition.Type t -> {
            if (dispatch.dispatchType(t)) {
              for (var d : asJava(t.members())) {
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

  private static <T> Iterable<T> asJava(Seq<T> seq) {
    return CollectionConverters.IterableHasAsJava(seq).asJava();
  }
}
