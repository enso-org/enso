package org.enso.interpreter.test.builtins;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.ByteArrayOutputStream;
import org.enso.test.utils.ContextRule;
import org.junit.After;
import org.junit.ClassRule;
import org.junit.Test;

/**
 * In these tests, we call Java methods from Enso. Java methods have different signatures that
 * accept Enso values in different ways.
 */
public class BuiltinsJavaInteropTest {
  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();

  @ClassRule public static final ContextRule ctxRule = ContextRule.createWithCapturedOut(out);

  @After
  public void resetOutput() {
    out.reset();
  }

  /**
   * This test reflects the state of many Java methods in stdlibs that accept Enso values as {@link
   * java.lang.Object}. If the Java method has a single argument of type {@link java.lang.Object},
   * and we pass {@code Date_Time} in it, we expect the host interop conversion to convert it to
   * {@link java.time.LocalDateTime}.
   */
  @Test
  public void javaMethodAcceptsEnsoTimeOfDay_AsObject() {
    var src =
        """
        from Standard.Base import Date_Time
        polyglot java import org.enso.example.PolyglotTestClass

        main =
            dt = Date_Time.now
            PolyglotTestClass.isPolyglotDate_Object dt
        """;
    var result = ctxRule.evalModule(src);
    assertThat(result.asBoolean(), is(true));
  }

  @Test
  public void javaMethodAcceptsEnsoTimeOfDay_AsLocalDate() {
    var src =
        """
        from Standard.Base import Date_Time
        polyglot java import org.enso.example.PolyglotTestClass

        main =
            dt = Date_Time.now
            PolyglotTestClass.isPolyglotDate_LocalDate dt
        """;
    var result = ctxRule.evalModule(src);
    assertThat(result.asBoolean(), is(true));
  }

  @Test
  public void javaMethodAcceptsEnsoTimeOfDay_AsValue() {
    var src =
        """
        from Standard.Base import Date_Time
        polyglot java import org.enso.example.PolyglotTestClass

        main =
            dt = Date_Time.now
            PolyglotTestClass.isPolyglotDate_Value dt
        """;
    var result = ctxRule.evalModule(src);
    assertThat(result.asBoolean(), is(true));
  }
}
