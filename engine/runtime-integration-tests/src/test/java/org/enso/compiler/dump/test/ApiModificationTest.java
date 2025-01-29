package org.enso.compiler.dump.test;

import static org.enso.test.utils.ContextUtils.defaultContextBuilder;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

import java.io.IOException;
import java.util.function.BiConsumer;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/** Tests recognitions of API changes in Enso code. */
public final class ApiModificationTest {
  private static Context ctx;

  @BeforeClass
  public static void initCtx() {
    ctx = defaultContextBuilder().build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
  }

  @Test
  public void reorderingMethods_DoesNotModifyApi() throws IOException {
    var prevSrc = """
        method_1 = 1
        method_2 = 2
        """;
    var newSrc = """
        method_2 = 2
        method_1 = 1
        """;
    compareSignatures(
        prevSrc,
        newSrc,
        (prevSignature, newSignature) -> {
          assertThat(
              "Signature not changed when reordering methods", prevSignature, is(newSignature));
        });
  }

  @Test
  public void reorderingTypes_DoesNotModifyApi() throws IOException {
    var prevSrc = """
        type Type_1
        type Type_2
        """;
    var newSrc = """
        type Type_2
        type Type_1
        """;
    compareSignatures(
        prevSrc,
        newSrc,
        (prevSignature, newSignature) -> {
          assertThat(
              "Signature not changed when reordering types", prevSignature, is(newSignature));
        });
  }

  @Test
  public void reorderingTypeAndMethod_DoesNotModifyApi() throws IOException {
    var prevSrc = """
        type Type
        method = 1
        """;
    var newSrc = """
        method = 1
        type Type
        """;
    compareSignatures(
        prevSrc,
        newSrc,
        (prevSignature, newSignature) -> {
          assertThat(
              "Signature not changed when reordering type and method",
              prevSignature,
              is(newSignature));
        });
  }

  @Test
  public void reorderingMethodsInType_DoesNotModifyApi() throws IOException {
    var prevSrc =
        """
        type Type
            method_1 = 1
            method_2 = 2
        """;
    var newSrc =
        """
        type Type
            method_2 = 2
            method_1 = 1
        """;
    compareSignatures(
        prevSrc,
        newSrc,
        (prevSignature, newSignature) -> {
          assertThat("Signature not changed when methods in type", prevSignature, is(newSignature));
        });
  }

  @Test
  public void reorderingConstructorsInType_DoesNotModifyApi() throws IOException {
    var prevSrc = """
        type Type
            Cons_2
            Cons_1
        """;
    var newSrc = """
        type Type
            Cons_1
            Cons_2
        """;
    compareSignatures(
        prevSrc,
        newSrc,
        (prevSignature, newSignature) -> {
          assertThat(
              "Signature not changed when reordering constructors in type",
              prevSignature,
              is(newSignature));
        });
  }

  @Test
  public void addingMethod_ModifiesApi() throws IOException {
    var prevSrc = """
        method_1 = 1
        """;
    var newSrc = """
        method_1 = 1
        method_2 = 2
        """;
    compareSignatures(
        prevSrc,
        newSrc,
        (prevSignature, newSignature) -> {
          assertThat("Different signatures", prevSignature, is(not(newSignature)));
          assertThat(
              "No method_2 in prev signature", prevSignature, not(containsString("method_2")));
          assertThat("method_2 in new signature", newSignature, containsString("method_2"));
        });
  }

  @Test
  public void renamingMethod_ModifiesApi() throws IOException {
    var prevSrc = """
        method = 1
        """;
    var newSrc = """
        renamed_method = 2
        """;
    compareSignatures(
        prevSrc,
        newSrc,
        (prevSignature, newSignature) -> {
          assertThat("Different signatures", prevSignature, is(not(newSignature)));
        });
  }

  @Test
  public void removingMethod_ModifiesApi() throws IOException {
    var prevSrc = """
        method_1 = 1
        method_2 = 2
        """;
    var newSrc = """
        method_2 = 2
        """;
    compareSignatures(
        prevSrc,
        newSrc,
        (prevSignature, newSignature) -> {
          assertThat("Different signatures", prevSignature, is(not(newSignature)));
          assertThat(newSignature, not(containsString("method_1")));
        });
  }

  private static void compareSignatures(
      String prevSource, String newSource, BiConsumer<String, String> signatureComparator)
      throws IOException {
    var modName = "local.Proj.Main";
    var prevSignature = DumpTestUtils.generateSignatures(ctx, prevSource, modName);
    var newSignature = DumpTestUtils.generateSignatures(ctx, newSource, modName);
    assertThat("Signature was generated", prevSignature.isEmpty(), is(false));
    assertThat("Signature was generated", newSignature.isEmpty(), is(false));
    signatureComparator.accept(prevSignature, newSignature);
  }
}
