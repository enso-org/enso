package org.enso.interpreter.test.interop;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.enso.compiler.core.ConstantsNames;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.Rule;
import org.junit.Test;

public class TypeMembersTest {
  @Rule public final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void checkAtomMembers() throws Exception {
    final URI uri = new URI("memory://how_long.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                from Standard.Base.Data.Boolean import True, False

                type IntList
                    End
                    Head h t

                    is_empty self = case self of
                        IntList.End -> True
                        _ -> False

                    tail self = case self of
                        IntList.Head _ t -> t
                        _ -> IntList.End

                    head self = case self of
                        IntList.Head h _ -> h
                        _ -> -1

                list1 = IntList.Head 7 <| IntList.Head 3 <| IntList.End

                """,
                "compare.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctxRule.eval(src);

    var headAtom = module.invokeMember("eval_expression", "list1");
    var seven = module.invokeMember("eval_expression", "list1.head");
    var three = module.invokeMember("eval_expression", "list1.tail.head");
    var endAtom = module.invokeMember("eval_expression", "list1.tail.tail");

    assertTrue("seven is number-like: " + seven, seven.fitsInInt());
    assertTrue("three is number-like: " + three, three.fitsInInt());
    assertFalse("list1 is not number-like: " + headAtom, headAtom.fitsInInt());
    assertFalse("list2 is not number-like: " + endAtom, endAtom.fitsInInt());

    assertEquals("seven check", 7, seven.asInt());
    assertEquals("three check", 3, three.asInt());

    assertMembers("Keys in list1", headAtom, "head", "tail", "is_empty");
    assertMembers("Keys in list2", endAtom, "head", "tail", "is_empty");
    assertMembers("Keys in list1", headAtom, "h", "t");
  }

  @Test
  public void ensureNonBuiltinMembersArePresent() throws Exception {
    final URI uri = new URI("memory://how_long.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                @Builtin_Type
                type Compile_Error
                    Error message

                    to_display_text self = "Compile error: "+self.message

                v = Compile_Error.Error "foo"
                """,
                "to_display_text.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctxRule.eval(src);
    var compileError = module.invokeMember("eval_expression", "v");
    assertEquals(
        "all members",
        Set.of(
            ConstantsNames.TO_DISPLAY_TEXT,
            "message",
            ConstantsNames.TO_TEXT,
            "==",
            "catch_primitive",
            "pretty"),
        compileError.getMemberKeys());
  }

  @Test
  public void builtinMethodIsPresent() {
    var refType =
        ctxRule.evalModule(
            """
            import Standard.Base.Runtime.Ref.Ref
            main = Ref
            """);
    assertThat(refType.hasMember("new"), is(true));
  }

  @Test
  public void inheritedMembersFromAnyAreIncluded()
      throws InvalidArrayIndexException, UnsupportedMessageException {
    var type =
        ctxRule.evalModule(
            """
            from Standard.Base.Any import all

            type My_Type
                method self = 42

            main = My_Type
            """);
    var typeUnwrapped = ctxRule.unwrapValue(type);
    var memberNames = getAllMemberNames(typeUnwrapped);
    var anyMethods = ctxRule.allMethodsFromAny();
    for (var anyMethod : anyMethods) {
      assertThat("Has method from Any", memberNames, hasItem(containsString(anyMethod)));
    }
  }

  @Test
  public void typeMemberNames_AreNotQualified()
      throws InvalidArrayIndexException, UnsupportedMessageException {
    var type =
        ctxRule.evalModule(
            """
            from Standard.Base.Any import all

            type My_Type
                method self = 42

            main = My_Type
            """);
    var typeUnwrapped = ctxRule.unwrapValue(type);
    var memberNames = getAllMemberNames(typeUnwrapped);
    assertThat("Member names are not qualified", memberNames, not(hasItem(containsString("."))));
  }

  @Test
  public void canInvokeInheritedStaticMethod_OnType() {
    var myType =
        ctxRule.evalModule(
            """
            from Standard.Base.Any import all

            type My_Type
                method self = 42

            main = My_Type
            """);
    var displayTextRes = myType.invokeMember(ConstantsNames.TO_DISPLAY_TEXT);
    assertThat("Has correct result type", displayTextRes.isString(), is(true));
    assertThat("Has correct result value", displayTextRes.asString(), is("My_Type"));
  }

  @Test
  public void canInvokeInstanceMethod() {
    var atom =
        ctxRule.evalModule(
            """
            type My_Type
                Cons
                method self = 42

            main = My_Type.Cons
            """);
    assertTrue(atom.hasMember("method"));
    assertTrue(atom.canInvokeMember("method"));
    var res = atom.invokeMember("method");
    assertThat("Method invocation returns correct result", res.asInt(), is(42));
  }

  /**
   * @param obj {@link ContextUtils#unwrapValue(Value) unwrapped} {@link Value value}.
   */
  private List<String> getAllMemberNames(Object obj)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    var interop = InteropLibrary.getUncached();
    var allMembers = interop.getMembers(obj, true);
    var memberNames = new ArrayList<String>();
    for (var i = 0; i < interop.getArraySize(allMembers); i++) {
      var member = interop.readArrayElement(allMembers, i);
      var memberName = interop.asString(member);
      memberNames.add(memberName);
    }
    return memberNames;
  }

  private static void assertMembers(String msg, Value v, String... keys) {
    var realKeys = v.getMemberKeys();
    for (var k : keys) {
      assertTrue(msg + " - found " + k + " in " + realKeys, realKeys.contains(k));
      assertTrue(msg + " - has member " + k, v.hasMember(k));
      assertNotNull(msg + " - can be invoked", v.invokeMember(k));
    }
  }
}
