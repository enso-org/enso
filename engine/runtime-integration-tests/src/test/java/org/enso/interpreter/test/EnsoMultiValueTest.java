package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.HashSet;
import java.util.List;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.TypeLiteral;
import org.junit.AfterClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class EnsoMultiValueTest {
  private static Context ctx;
  @Rule public final TemporaryFolder dir = new TemporaryFolder();

  private static Context ctx() {
    if (ctx == null) {
      ctx = ContextUtils.defaultContextBuilder().build();
    }
    return ctx;
  }

  @AfterClass
  public static void disposeCtx() throws Exception {
    if (ctx != null) {
      ctx.close();
      ctx = null;
    }
  }

  @Test
  public void keepIdentityOfAandB() throws Exception {
    var types =
        """
    from project.PrivateConversion import all
    type A
        A_Ctor x

        id_a self -> A = self
    type B
        B_Ctor x

    ab =
        a = A.A_Ctor 1
        (a : A & B)
    """;

    var privateConversion =
        """
    from project.Types import all

    B.from (that : A) = B.B_Ctor that
    """;

    var main =
        """
    from project.Types import all

    main =
        v = ab.id_a
        [v.to_text, (v:A).to_text, (v:B).to_text]
    """;

    var prjDir = dir.newFolder();
    var sources = new HashSet<SourceModule>();
    sources.add(new SourceModule(QualifiedName.fromString("Types"), types));
    sources.add(new SourceModule(QualifiedName.fromString("PrivateConversion"), privateConversion));
    sources.add(new SourceModule(QualifiedName.fromString("Main"), main));
    ProjectUtils.createProject("Keep_Id", sources, prjDir.toPath());

    ProjectUtils.testProjectRun(
        prjDir.toPath(),
        (tripple) -> {
          var texts = tripple.as(new TypeLiteral<List<String>>() {});
          assertEquals(3, texts.size());
          assertStartsWith("(A_Ctor", texts.get(0));
          assertStartsWith("(A_Ctor", texts.get(1));
          assertStartsWith("(B_Ctor", texts.get(2));
        });
  }

  @Test
  public void sameFieldAccessAandB() {
    sameFieldAccess("A & B");
  }

  @Test
  public void sameFieldAccessBandA() {
    sameFieldAccess("B & A");
  }

  private void sameFieldAccess(String cast) {
    var code =
        """
    type A
        A_Ctor x y

        x_from_a self = self.x

    type B
        B_Ctor x y

        x_from_b self = self.x

    B.from (that : A) = B.B_Ctor "B" that.y

    pair =
        a = A.A_Ctor "A" 1
        both = (a : $cast)

        v_a = both.x_from_a
        v_b = both.x_from_b
        [v_a, v_b]
    """
            .replace("$cast", cast);

    var pair = ContextUtils.evalModule(ctx(), code, "fields.enso", "pair");
    var texts = pair.as(new TypeLiteral<List<String>>() {});
    assertEquals(2, texts.size());
    assertEquals("A", texts.get(0));
    assertEquals("B", texts.get(1));
  }

  @Test
  public void trippleCastConfusion() {
    var code =
        """
    type A
        A_Ctor x
    type B
        B_Ctor x
    type C
        C_Ctor x

    B.from (that : A) = B.B_Ctor that
    C.from (that:B) = C.C_Ctor that

    texts =
        a = A.A_Ctor 1
        ab = (a : A & B)
        abc = ab:(A & B & C)
        c = abc:C

        text_a = (c:A).to_text
        text_b = (c:B).to_text
        [text_a, text_b, c.to_text]
    """;

    var tripple = ContextUtils.evalModule(ctx(), code, "tripple.enso", "texts");
    var texts = tripple.as(new TypeLiteral<List<String>>() {});
    assertEquals(3, texts.size());
    assertStartsWith("(A_Ctor", texts.get(0));
    assertStartsWith("(B_Ctor", texts.get(1));
    assertStartsWith("(C_Ctor", texts.get(2));
  }

  private static void assertStartsWith(String exp, String actual) {
    if (actual.startsWith(exp)) {
      return;
    }
    fail("Expecting " + exp + " in " + actual);
  }
}
