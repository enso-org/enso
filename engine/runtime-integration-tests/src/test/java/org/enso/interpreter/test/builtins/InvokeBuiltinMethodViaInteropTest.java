package org.enso.interpreter.test.builtins;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;
import org.junit.Test;

/**
 * This test tries to invoke some builtin methods on builtin types via the {@link
 * com.oracle.truffle.api.interop.InteropLibrary interop} protocol.
 */
public class InvokeBuiltinMethodViaInteropTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void invokeGetMethodOnRef()
      throws UnsupportedMessageException,
          UnknownIdentifierException,
          UnsupportedTypeException,
          ArityException {
    var code =
        """
        import Standard.Base.Runtime.Ref.Ref

        main = Ref.new 42
        """;
    var ref = ctxRule.evalModule(code);
    var interop = InteropLibrary.getUncached();
    var refUnwrapped = ctxRule.unwrapValue(ref);
    assertThat(
        "Ref builtin object should not have any members",
        interop.hasMembers(refUnwrapped),
        is(false));
    assertThat(
        "Ref should have a meta-object (Ref type)", interop.hasMetaObject(refUnwrapped), is(true));
    var refMeta = interop.getMetaObject(refUnwrapped);
    assertThat(
        "Ref meta-object should have a 'get' method",
        interop.isMemberInvocable(refMeta, "get"),
        is(true));
    var res = interop.invokeMember(refMeta, "get", new Object[] {refUnwrapped});
    assertThat("Ref.get should return a number", interop.isNumber(res), is(true));
    assertThat("Ref.get should return 42", interop.asInt(res), is(42));
  }

  @Test
  public void invokePathMethodOnFile() {
    var code =
        """
        from Standard.Base import File

        main =
            File.current_directory
        """;
    var file = ctxRule.evalModule(code);
    var fileType = file.getMetaObject();
    assertThat(fileType, is(notNullValue()));
    assertThat(fileType.hasMember("path"), is(true));
    var res = fileType.invokeMember("path", new Object[] {file});
    assertThat("path method can be invoked", res, is(notNullValue()));
    assertThat("path method returns correct result", res.isString(), is(true));
  }

  @Test
  public void invokeToTextOnVector() {
    var code = """
        main = [1,2,3]
        """;
    var vec = ctxRule.evalModule(code);
    var vecType = vec.getMetaObject();
    assertThat(vecType, is(notNullValue()));
    assertThat(vecType.hasMember("to_text"), is(true));
    var res = vecType.invokeMember("to_text", new Object[] {vec});
    assertThat("to_text method can be invoked", res, is(notNullValue()));
    assertThat("to_text method returns correct result", res.isString(), is(true));
  }

  /**
   * 'Text.reverse' is an extension method defined outside builtins module scope, so it cannot be
   * resolved.
   */
  @Test
  public void extensionMethodOnBuiltinTypeIsNotResolved() throws UnsupportedMessageException {
    var text = ctxRule.evalModule("main = 'Hello'");
    var interop = InteropLibrary.getUncached();
    var textUnwrapped = ctxRule.unwrapValue(text);
    var textMeta = interop.getMetaObject(textUnwrapped);
    assertThat(
        "Text type should not be able to resolve 'reverse' method",
        interop.isMemberInvocable(textMeta, "reverse"),
        is(false));
  }

  @Test
  public void invokePlusOnTextWithParameter()
      throws UnsupportedMessageException,
          UnknownIdentifierException,
          UnsupportedTypeException,
          ArityException {
    var text1 = ctxRule.evalModule("main = 'First'");
    var text2 = ctxRule.evalModule("main = 'Second'");
    var interop = InteropLibrary.getUncached();
    var text1Unwrapped = ctxRule.unwrapValue(text1);
    var text2Unwrapped = ctxRule.unwrapValue(text2);
    var textMeta = interop.getMetaObject(text1Unwrapped);
    assertThat(
        "Text type should have a '+' method", interop.isMemberInvocable(textMeta, "+"), is(true));
    var res = interop.invokeMember(textMeta, "+", text1Unwrapped, text2Unwrapped);
    assertThat("Text.+ should return a text", interop.isString(res), is(true));
    assertThat("Text.+ should return 'FirstSecond'", interop.asString(res), is("FirstSecond"));
  }
}
