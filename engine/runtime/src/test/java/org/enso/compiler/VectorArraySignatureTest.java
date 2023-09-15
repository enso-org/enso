package org.enso.compiler;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.HashMap;
import org.enso.compiler.core.EnsoParser;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.expression.Comment;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.junit.AfterClass;
import static org.junit.Assert.assertEquals;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import scala.Function1;

public class VectorArraySignatureTest {
  private static EnsoParser ensoCompiler;

  @BeforeClass
  public static void initEnsoParser() {
    ensoCompiler = new EnsoParser();
  }

  @AfterClass
  public static void closeEnsoParser() throws Exception {
    ensoCompiler.close();
  }

  @Test
  public void testParseVectorAndArray() throws Exception {
    var p = Paths.get("../../distribution/").toFile().getCanonicalFile();
    final var vectorAndArray = new Path[2];
    Files.walkFileTree(p.toPath(), new FileVisitor<Path>() {
      @Override
      public FileVisitResult preVisitDirectory(Path t, BasicFileAttributes bfa) throws IOException {
        return FileVisitResult.CONTINUE;
      }

      @Override
      public FileVisitResult visitFile(Path t, BasicFileAttributes bfa) throws IOException {
        if (t.getFileName().toString().equals("Array.enso")) {
          assertNull(vectorAndArray[0]);
          vectorAndArray[0] = t;
        }
        if (t.getFileName().toString().equals("Vector.enso")) {
          assertNull(vectorAndArray[1]);
          vectorAndArray[1] = t;
        }
        return FileVisitResult.CONTINUE;
      }

      @Override
      public FileVisitResult visitFileFailed(Path t, IOException ioe) throws IOException {
        return FileVisitResult.CONTINUE;
      }

      @Override
      public FileVisitResult postVisitDirectory(Path t, IOException ioe) throws IOException {
        return FileVisitResult.CONTINUE;
      }
    });
    assertNotNull("Found Array", vectorAndArray[0]);
    assertNotNull("Found Vector", vectorAndArray[1]);

    var arraySrc = Files.readString(vectorAndArray[0]);
    var vectorSrc = Files.readString(vectorAndArray[1]);

    var arrayIR = ensoCompiler.compile(arraySrc).preorder().filter((v) -> {
      if (v instanceof Definition.SugaredType t) {
        if ("Array".equals(t.name().name())) {
          return true;
        }
      }
      return false;
    }).head();
    var vectorIR = ensoCompiler.compile(vectorSrc).preorder().filter((v) -> {
      if (v instanceof Definition.SugaredType t) {
        if ("Vector".equals(t.name().name())) {
          return true;
        }
      }
      return false;
    }).head();

    var documentation = new HashMap<Function.Binding, Comment.Documentation>();
    var lastDoc = new Comment.Documentation[1];
    final Function1<IR, Object> filter = (v) -> {
      if (v instanceof Comment.Documentation d) {
        lastDoc[0] = d;
      }
      if (v instanceof Function.Binding b && b.arguments().head().name() instanceof Name.Self) {
        documentation.put(b, lastDoc[0]);
        lastDoc[0] = null;
      }
      return v instanceof Function.Binding b && b.arguments().head().name() instanceof Name.Self;
    };
    var arrayFns = arrayIR.preorder().filter(filter);
    var vectorFns = vectorIR.preorder().filter(filter);
    var arrayNames = arrayFns.map((v) -> v instanceof Function.Binding b ? b.name().name() : null);
    var vectorNames = vectorFns.map((v) -> v instanceof Function.Binding b ? b.name().name() : null);

    var missingInArray = vectorFns.filter((v) -> v instanceof Function.Binding b && !arrayNames.contains(b.name().name()));
    var missingNamesInArray = missingInArray.map((v) -> v instanceof Function.Binding b ? b.name().name() : null);

    var missingInVector = arrayFns.filter((v) -> v instanceof Function.Binding b && !vectorNames.contains(b.name().name()));
    var missingNamesInVector = missingInVector.map((v) -> v instanceof Function.Binding b ? b.name().name() : null);

    assertTrue("Nothing is missing in vector: " + missingNamesInVector, missingNamesInVector.isEmpty());
    assertTrue("Nothing is missing in array: " + missingNamesInArray, missingNamesInArray.isEmpty());

    vectorFns.foreach((v) -> {
      if (v instanceof Function.Binding b) {
        var name = b.name().name();
        var d = documentation.get(b);
        if (!d.doc().contains("PRIVATE")) {
          assertNotNull("Documentation for " + name, d);

          var head = arrayFns.filter((av) -> av instanceof Function.Binding ab && ab.name().name().equals(b.name().name())).head();
          var ad = documentation.get(head);
          assertNotNull("No documentation for Array." + name, ad);
          var exp = d.doc()
                  .replace("]", "].to_array")
                  .replace("a vector", "an array")
                  .replace("vector", "array");
          assertEquals("Bad documentation for Array." + name, exp, ad.doc());
        }
      }
      return null;
    });
  }
}
