package org.enso.compiler.dump;

import java.io.IOException;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.pass.resolve.DocumentationComments;
import org.enso.compiler.pass.resolve.DocumentationComments$;
import org.enso.filesystem.FileSystem;
import scala.collection.immutable.Seq;
import scala.jdk.CollectionConverters;

/** Generator of documentation for an Enso project. */
public final class GenerateDocs {
  private GenerateDocs() {}

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

    for (var module : modules) {
      var ir = module.getIr();
      assert ir != null : "need IR for " + module;
      var md = fs.getChild(api, module.getName() + ".md");
      DONE:
      try (var w = fs.newBufferedWriter(md)) {
        w.append("## Documentation for " + module.getName() + "\n");

        if (ir.isPrivate()) {
          w.append("This module is **private**!\n");
          break DONE;
        }

        for (var b : asJava(ir.bindings())) {
          switch (b) {
            case Definition.Type t -> w.append("#### **type** " + t.name().name() + "\n");
            case Definition.Data d -> w.append("#### data " + d.name().name() + "\n");
            case Definition.SugaredType s -> w.append("#### sugar " + s.name().name() + "\n");
            case Method.Explicit m -> w.append("#### method " + m.methodName().name() + "\n");
            case Method.Conversion c -> w.append("#### conversion " + c.methodName().name() + "\n");
            default -> throw new AssertionError("unknown type " + b.getClass());
          }
          var option = b.passData().get(DocumentationComments$.MODULE$);
          if (option.isDefined()) {
            var doc = (DocumentationComments.Doc) option.get();
            w.append(doc.documentation());
            w.append("\n\n\n");
          }
        }
      }
    }

    System.out.println("Documentation generated into " + api);
  }

  private static <T> Iterable<T> asJava(Seq<T> seq) {
    return CollectionConverters.IterableHasAsJava(seq).asJava();
  }
}
