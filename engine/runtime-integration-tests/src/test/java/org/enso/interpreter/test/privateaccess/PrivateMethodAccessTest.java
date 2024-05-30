package org.enso.interpreter.test.privateaccess;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.enso.test.utils.ContextUtils;
import org.junit.Test;

public class PrivateMethodAccessTest {
  @Test
  public void moduleDoesNotExposePrivateMethodsToPolyglot() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      var module =
          ctx.eval(
              LanguageInfo.ID,
              """
          private priv_method x = x
          pub_method x = x
          """);
      var assocType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
      var privMethod = module.invokeMember(Module.GET_METHOD, assocType, "priv_method");
      assertThat("private method must not be exposed to polyglot", privMethod.isNull(), is(true));
      var pubMethod = module.invokeMember(Module.GET_METHOD, assocType, "pub_method");
      assertThat("public method is exposed to polyglot", pubMethod.canExecute(), is(true));
    }
  }

  @Test
  public void typeDoesNotExposePrivateMethodsToPolyglot() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      var module =
          ctx.eval(
              LanguageInfo.ID,
              """
          type My_Type
              private priv_method x = x
              pub_method x = x
          """);
      var myType = module.invokeMember(Module.GET_TYPE, "My_Type");
      var privMethod = module.invokeMember(Module.GET_METHOD, myType, "priv_method");
      assertThat("private method must not be exposed to polyglot", privMethod.isNull(), is(true));
      var pubMethod = module.invokeMember(Module.GET_METHOD, myType, "pub_method");
      assertThat("public method is exposed to polyglot", pubMethod.canExecute(), is(true));
    }
  }
}
