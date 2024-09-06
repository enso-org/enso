package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import org.enso.common.MethodNames;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.junit.Test;

public class SymbolResolutionTest extends ContextTest {

  @Test
  public void resolvingLocalSymbol() throws Exception {
    final URI uri = new URI("memory://resolvingLocalSymbol.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    my_symbol = 42
                    entry_point = my_symbol
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var result = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "entry_point");
    assertTrue(result.isNumber());
    assertEquals(42, result.asInt());
  }

  @Test
  public void resolvingSymbolQualifiedThroughCurrentModuleName() throws Exception {
    final URI uri = new URI("memory://currentModuleName.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    my_symbol = 42
                    entry_point = currentModuleName.my_symbol
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var result = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "entry_point");
    assertTrue(result.isNumber());
    assertEquals(42, result.asInt());
  }

  @Test
  public void resolvingSymbolQualifiedThroughCurrentModuleName2() throws Exception {
    final URI uri = new URI("memory://Current_Module_Name.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    type My_Type
                        my_value = 43
                    entry_point = Current_Module_Name.My_Type.my_value
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var result = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "entry_point");
    assertTrue(result.isNumber());
    assertEquals(43, result.asInt());
  }

  /**
   * When a symbol in the local scope is defined such that aliases the module name, the locally
   * defined symbol takes precedence over the module name.
   */
  @Test
  public void aliasedModuleName() throws Exception {
    final URI uri = new URI("memory://aliasedModuleName.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    aliasedModuleName = 43
                    entry_point = aliasedModuleName
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var result = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "entry_point");
    assertTrue(result.isNumber());
    assertEquals(43, result.asInt());
  }

  @Test
  public void aliasedModuleName2() throws Exception {
    final URI uri = new URI("memory://Aliased_Module_Name.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    type Aliased_Module_Name
                        my_value = 44
                    entry_point = Aliased_Module_Name.my_value
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var result = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "entry_point");
    assertTrue(result.isNumber());
    assertEquals(44, result.asInt());
  }

  // https://github.com/enso-org/enso/issues/10748
  @Test
  public void unresolvedSymbolErrorMessage() {
    var code =
        """
        type My_Type
            static_method = 23

        main =
            static_method
        """;
    try (var ctx = ContextUtils.createDefaultContext()) {
      try {
        var res = ContextUtils.evalModule(ctx, code);
        fail("Should throw exception. Instead got: " + res);
      } catch (PolyglotException ex) {
        var panic = (PanicException) ContextUtils.unwrapValue(ctx, ex.getGuestObject());
        var errMsg = panic.getPayload().toString();
        assertThat(errMsg, allOf(containsString("The name"), containsString("could not be found")));
      }
    }
  }
}
