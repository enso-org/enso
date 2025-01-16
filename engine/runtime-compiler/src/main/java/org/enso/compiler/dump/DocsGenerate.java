package org.enso.compiler.dump;

import java.io.IOException;
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
   * @param pkg library to generate the documentation for
   * @param modules parsed modules found in the library
   * @throws IOException when I/O problem occurs
   */
  public static <File> void write(
      org.enso.pkg.Package<File> pkg, Iterable<CompilerContext.Module> modules) throws IOException {
    var fs = pkg.fileSystem();
    var docs = fs.getChild(pkg.root(), "docs");
    var api = fs.getChild(docs, "api");
    fs.createDirectories(api);

    var visitor = new DocsEmitMarkdown();

    for (var module : modules) {
      var ir = module.getIr();
      assert ir != null : "need IR for " + module;
      if (ir.isPrivate()) {
        continue;
      }
      var moduleName = module.getName();
      var md = fs.getChild(api, moduleName + ".md");
      try (var mdWriter = fs.newBufferedWriter(md)) {
        visitModule(visitor, moduleName, ir, mdWriter);
      }
    }
    System.out.println("Documentation generated into " + api);
  }

  public static void visitModule(
      DocsVisit visitor, QualifiedName moduleName, Module ir, Appendable w) throws IOException {
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
                dispatch.dispatchConstructor(t, d);
              }
              for (var mb : moduleBindings) {
                if (mb instanceof Method.Explicit m) {
                  if (m.isStaticWrapperForInstanceMethod()) {
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
          case Definition.Data d -> {
            dispatch.dispatchConstructor(null, d);
          }
          case Definition.SugaredType s -> {
            w.append("#### sugar " + s.name().name() + "\n");
          }
          case Method.Explicit m -> dispatch.dispatchMethod(null, m);
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
