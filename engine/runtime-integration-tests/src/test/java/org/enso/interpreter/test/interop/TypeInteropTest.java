package org.enso.interpreter.test.interop;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.util.ArrayList;
import java.util.List;
import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.hamcrest.Matchers;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

@RunWith(Enclosed.class)
public final class TypeInteropTest {
  @ClassRule
  public static final ContextUtils ctxRule = ContextUtils.newBuilder().assertGC(false).build();

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

  public static final class TestMeta {
    @Test
    public void type_isMetaObject() {
      var interop = InteropLibrary.getUncached();
      var myTypeValue =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
              main = My_Type
              """);
      var myType = (Type) ctxRule.unwrapValue(myTypeValue);
      assertThat(interop.isMetaObject(myType), is(true));
    }

    @Test
    public void singletonType_isMetaObject() {
      var interop = InteropLibrary.getUncached();
      var myTypeValue =
          ctxRule.evalModule(
              """
              type My_Type
              main = My_Type
              """);
      var myType = (Type) ctxRule.unwrapValue(myTypeValue);
      assertThat(interop.isMetaObject(myType), is(true));
    }

    @Test
    public void type_HasAnyAsMetaParent_Polyglot() {
      var myType =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons

              main = My_Type
              """);
      assertThat(myType.hasMetaParents(), is(true));
      var metaParents = myType.getMetaParents();
      assertThat(metaParents.hasArrayElements(), is(true));
      assertThat("Has just one meta parent - Any", metaParents.getArraySize(), is(1L));
      var anyType = metaParents.getArrayElement(0);
      assertThat(anyType.getMetaSimpleName(), is("Any"));
    }

    @Test
    public void type_hasAnyAsMetaParent_InteropLibrary()
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var interop = InteropLibrary.getUncached();
      var myTypeValue =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
              main = My_Type
              """);
      var myType = (Type) ctxRule.unwrapValue(myTypeValue);
      assertThat(interop.hasMetaParents(myType), is(true));
      var parents = interop.getMetaParents(myType);
      assertThat("Type has exactly one meta parent", interop.getArraySize(parents), is(1L));
      var anyParent = interop.readArrayElement(parents, 0);
      assertThat(interop.isMetaObject(anyParent), is(true));
      var parentName = interop.asString(interop.getMetaQualifiedName(anyParent));
      assertThat(parentName, containsString("Any"));
    }
  }

  public static final class TestConstructors {
    @Test
    public void atomConstructor_IsMemberOfType() {
      var myType =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons

              main = My_Type
              """);
      assertThat(myType.hasMember("Cons"), is(true));
    }

    @Test
    public void atomConstructors_AreMembersOfType() {
      var myType =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons_1
                  Cons_2

              main = My_Type
              """);
      assertThat("type has constructors as members", myType.hasMembers(), Matchers.is(true));
      assertThat(myType.getMemberKeys(), containsInAnyOrder("Cons_1", "Cons_2"));
      assertThat(
          "Constructor (type member) is instantiable",
          myType.getMember("Cons_1").canInstantiate(),
          Matchers.is(true));
    }

    @Test
    public void atomConstructor_IsPublicMemberOfType() {
      var interop = InteropLibrary.getUncached();
      var myTypeValue =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons

              main = My_Type
              """);
      var myType = (Type) ctxRule.unwrapValue(myTypeValue);
      assertThat(
          "Atom constructor is not internal member",
          interop.isMemberInternal(myType, "Cons"),
          is(false));
    }

    @Test
    public void atomConstructorMember_CanBeInstantiated() {
      var myType =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons

              main = My_Type
              """);
      var consMember = myType.getMember("Cons");
      assertThat("Atom constructor is instantiable", consMember.canInstantiate(), is(true));
    }

    @Test
    public void atomConstructorMember_WithParameters_CanBeInstantiated() {
      var myType =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons x y

              main = My_Type
              """);
      var consMember = myType.getMember("Cons");
      assertThat(
          "Atom constructor with parameters is instantiable",
          consMember.canInstantiate(),
          is(true));
    }

    @Test
    public void privateAtomConstructorMember_CannotBeInstantiated() {
      var myType =
          ctxRule.evalModule(
              """
              type My_Type
                  private Cons

              main = My_Type
              """);
      var consMember = myType.getMember("Cons");
      assertThat(
          "Private atom constructor is NOT instantiable", consMember.canInstantiate(), is(false));
    }
  }

  public static final class TestMethods {
    @Test
    public void ensureNonBuiltinMembersArePresent() {
      var builtinType =
          ctxRule.evalModule(
              """
              @Builtin_Type
              type Compile_Error
                  Error message

                  to_display_text self = "Compile error: "+self.message

              main = Compile_Error.Error "foo"
              """);
      assertThat(
          builtinType.getMemberKeys(),
          hasItems("to_display_text", "message", "to_text", "==", "catch_primitive", "pretty"));
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
    public void inheritedMethodsFromAnyAreIncluded()
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
    public void staticMethod_IsInternalMember() {
      var interop = InteropLibrary.getUncached();
      var myTypeValue =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
                  method foo = 42
              main = My_Type
              """);
      var myType = (Type) ctxRule.unwrapValue(myTypeValue);
      assertThat(
          "Static method is internal member", interop.isMemberInternal(myType, "foo"), is(true));
    }

    @Test
    public void staticMethod_CanBeExecuted() {
      var myType =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
                  method foo = 42
              main = My_Type
              """);
      var method = myType.getMember("method");
      assertThat("Static method can be executed", method.canExecute(), is(true));
    }

    @Test
    public void privateStaticMethod_IsInternalMember() {
      var interop = InteropLibrary.getUncached();
      var myTypeValue =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
                  private method = 42
              main = My_Type
              """);
      var myType = (Type) ctxRule.unwrapValue(myTypeValue);
      assertThat(
          "Private static method is internal member",
          interop.isMemberInternal(myType, "foo"),
          is(true));
    }

    @Test
    public void privateStaticMethod_CannotBeInvoked() {
      var myType =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
                  private method foo = 42
              main = My_Type
              """);
      assertThat(
          "Private static method cannot be invoked", myType.canInvokeMember("foo"), is(false));
    }

    @Test
    public void inheritedStaticMethod_CanBeCalled() {
      var myType =
          ctxRule.evalModule(
              """
              from Standard.Base.Any import all

              type My_Type
                  Cons
                  method self = 42

              main = My_Type
              """);
      var displayTextRes = myType.invokeMember("to_display_text");
      assertThat("Has correct result type", displayTextRes.isString(), is(true));
      assertThat("Has correct result value", displayTextRes.asString(), is("My_Type"));
    }

    @Test
    public void instanceMethod_IsMember() {
      var interop = InteropLibrary.getUncached();
      var myTypeValue =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
                  method self = 42
              main = My_Type.Cons
              """);
      var myType = (Type) ctxRule.unwrapValue(myTypeValue);
      assertThat(
          "Instance method is member of type",
          interop.isMemberExisting(myType, "method"),
          is(true));
    }

    @Test
    public void instanceMethod_CanBeInvokedStatically() {
      var atom =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
                  method self = 42
              main = My_Type.Cons
              """);
      var type = atom.getMetaObject();
      var res = type.invokeMember("method", atom);
      assertThat(res.asInt(), Matchers.is(42));
    }

    @Test
    public void privateInstanceMethod_IsMember() {
      var interop = InteropLibrary.getUncached();
      var myTypeValue =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
                  private method self = 42
              main = My_Type.Cons
              """);
      var myType = (Type) ctxRule.unwrapValue(myTypeValue);
      assertThat(
          "Private instance method is a member of type",
          interop.isMemberExisting(myType, "method"),
          is(true));
    }

    @Test
    public void privateInstanceMethod_IsInternalMember() {
      var interop = InteropLibrary.getUncached();
      var myTypeValue =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
                  private method self = 42
              main = My_Type.Cons
              """);
      var myType = (Type) ctxRule.unwrapValue(myTypeValue);
      assertThat(
          "Private instance method is internal member",
          interop.isMemberInternal(myType, "method"),
          is(true));
    }

    @Test
    public void privateInstanceMethod_CannotBeInvoked() {
      var atom =
          ctxRule.evalModule(
              """
              type My_Type
                  Cons
                  private method self = 42
              main = My_Type.Cons
              """);
      var type = atom.getMetaObject();
      assertThat(
          "Private instance method cannot be invoked", type.canInvokeMember("method"), is(false));
    }
  }

  /**
   * @param obj {@link ContextUtils#unwrapValue(Value) unwrapped} {@link Value value}.
   */
  private static List<String> getAllMemberNames(Object obj)
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
