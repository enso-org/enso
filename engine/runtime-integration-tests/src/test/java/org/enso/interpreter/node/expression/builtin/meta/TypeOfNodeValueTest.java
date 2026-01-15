package org.enso.interpreter.node.expression.builtin.meta;

import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;

import com.oracle.truffle.api.RootCallTarget;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConstructor;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertTrue;


public class TypeOfNodeValueTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();
  private static RootCallTarget testTypesCall;

  @BeforeClass
  public static void init() {
    var node = TypeOfNode.create();
    var root =
        new TestRootNode(
            (frame) -> {
              var arg = frame.getArguments()[0];
              var allTypes = (boolean) frame.getArguments()[1];
              var t = node.findTypeOrError(arg);
              var all = node.findAllTypesOrNull(arg, allTypes);
              return new Object[] {t, all};
            });
    root.insertChildren(node);
    testTypesCall = root.getCallTarget();
  }

  @AfterClass
  public static void disposeCtx() throws Exception {
    testTypesCall = null;
  }

  @Test
  public void typeOfUnresolvedConstructor() {
    var cnstr = UnresolvedConstructor.build(null, "Unknown_Name");
    var arr = (Object[]) testTypesCall.call(cnstr, true);
    var type = (Type) arr[0];
    var allTypes = (Type[]) arr[1];
    assertEquals("Function", type.getName());
    assertEquals("One array", 1, allTypes.length);
    assertEquals("Also function type", type, allTypes[0]);
  }

  @Test
  public void typeOfUnresolvedSymbol() {
    var cnstr = UnresolvedSymbol.build("Unknown_Name", null);
    var arr = (Object[]) testTypesCall.call(cnstr, true);
    var type = (Type) arr[0];
    var allTypes = (Type[]) arr[1];
    assertEquals("Function", type.getName());
    assertEquals("One array", 1, allTypes.length);
    assertEquals("Also function type", type, allTypes[0]);
  }

  @Test
  public void multiValueWithHiddenType() {
    var ensoCtx = EnsoContext.get(testTypesCall.getRootNode());
    var types =
        new Type[] {ensoCtx.getBuiltins().number().getInteger(), ensoCtx.getBuiltins().text()};
    var multi =
        EnsoMultiValue.NewNode.getUncached().newValue(types, 1, 0, new Object[] {42L, "Meaning"});
    var arr = (Object[]) testTypesCall.call(multi, true);
    var allTypes = (Type[]) arr[1];
    assertEquals("Two types", 2, allTypes.length);
    assertEquals("Integer", types[0], allTypes[0]);
    assertEquals("Text", types[1], allTypes[1]);

    var arr1 = (Object[]) testTypesCall.call(multi, false);
    var allTypes1 = (Type[]) arr1[1];
    assertEquals("Just one type", 1, allTypes1.length);
    assertEquals("Integer", types[0], allTypes1[0]);
  }

  @Test
  public void customWarningType() {
    var ret = ctxRule.evalModule("""
        from Standard.Base import Warning
        type My_Warn
            Value reason
        main =
            with_warn = Warning.attach (My_Warn.Value "my_warn") 42
            warn = Warning.get_all with_warn . first
            [My_Warn, warn]
        """);
    assertThat(ret.hasArrayElements(), is(true));
    var myWarnTypeExpected = ctxRule.unwrapValue(ret.getArrayElement(0));
    var myWarn = ctxRule.unwrapValue(ret.getArrayElement(1));
    var myWarnTypeActual = typeOf(myWarn);
    assertTrue("is type", myWarnTypeExpected instanceof Type);
    assertTrue("is type", myWarnTypeActual instanceof Type);
    var expectedTypeName = ((Type) myWarnTypeExpected).getQualifiedName().toString();
    assertThat(expectedTypeName, containsString("My_Warn"));
    var actualTypeName = ((Type) myWarnTypeActual).getQualifiedName().toString();
    assertThat(actualTypeName, is(expectedTypeName));
  }

  private Object typeOf(Object value) {
    return ((Object[]) testTypesCall.call(value, false))[0];
  }
}
