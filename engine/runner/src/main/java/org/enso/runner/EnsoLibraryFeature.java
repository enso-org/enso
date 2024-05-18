package org.enso.runner;

import static scala.jdk.javaapi.CollectionConverters.asJava;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import org.enso.compiler.core.ir.module.scope.imports.Polyglot;
import org.enso.pkg.PackageManager$;
import org.graalvm.nativeimage.hosted.Feature;
import org.graalvm.nativeimage.hosted.RuntimeReflection;

public final class EnsoLibraryFeature implements Feature {

  @Override
  public void beforeAnalysis(BeforeAnalysisAccess access) {
    var libs = new LinkedHashSet<Path>();
    for (var p : access.getApplicationClassPath()) {
      var p1 = p.getParent();
      if (p1 != null && p1.getFileName().toString().equals("java")) {
        var p2 = p1.getParent();
        if (p2 != null
            && p2.getFileName().toString().equals("polyglot")
            && p2.getParent() != null) {
          libs.add(p2.getParent());
        }
      }
    }

    try (var parser = new org.enso.compiler.core.EnsoParser()) {
      for (var p : libs) {
        var result = PackageManager$.MODULE$.Default().loadPackage(p.toFile());
        if (result.isSuccess()) {
          var pkg = result.get();
          for (var src : pkg.listSourcesJava()) {
            var code = Files.readString(src.file().toPath());
            var ir = parser.compile(code);
            for (var imp : asJava(ir.imports())) {
              if (imp instanceof Polyglot poly && poly.entity() instanceof Polyglot.Java entity) {
                var name = new StringBuilder(entity.getJavaName());
                Class<?> clazz;
                for (; ; ) {
                  clazz = access.findClassByName(name.toString());
                  if (clazz != null) {
                    break;
                  }
                  int at = name.toString().lastIndexOf('.');
                  if (at < 0) {
                    throw new IllegalStateException("Cannot load " + entity.getJavaName());
                  }
                  name.setCharAt(at, '$');
                }

                RuntimeReflection.registerAllConstructors(clazz);
                RuntimeReflection.registerAllFields(clazz);
                RuntimeReflection.registerAllMethods(clazz);
              }
            }
          }
        }
      }
    } catch (Exception ex) {
      ex.printStackTrace();
      throw new IllegalStateException(ex);
    }
  }
}
