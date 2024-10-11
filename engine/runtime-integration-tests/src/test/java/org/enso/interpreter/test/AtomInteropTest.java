package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests various {@link com.oracle.truffle.api.interop.InteropLibrary interop} contracts for {@link
 * org.enso.interpreter.runtime.data.atom.Atom atoms}.
 */
public class AtomInteropTest {
  private Context ctx;

  @Before
  public void initCtx() {
    ctx = ContextUtils.createDefaultContext();
  }

  @After
  public void disposeCtx() {
    ctx.close();
    ctx = null;
  }

  @Test
  public void atomMembersAreConstructorFields_SingleConstructor() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons field_1 field_2

        main =
            My_Type.Cons 1 2
        """);
    assertThat(myTypeAtom.hasMembers(), is(true));
    var memberNames = myTypeAtom.getMemberKeys();
    assertThat("Has two fields", memberNames.size(), is(2));
    assertThat(
        "Member names are not qualified", memberNames, containsInAnyOrder("field_1", "field_2"));
    for (var memberName : memberNames) {
      var member = myTypeAtom.getMember(memberName);
      assertThat("Member " + memberName + " should be readable", member, is(notNullValue()));
      assertThat("All fields are numbers", member.isNumber(), is(true));
    }
  }

  @Test
  public void atomIsNotMetaObject() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons field_1 field_2

        main =
            My_Type.Cons 1 2
        """);
    assertThat(myTypeAtom.isMetaObject(), is(false));
    assertThat(myTypeAtom.getMetaObject().getMetaSimpleName(), is("My_Type"));
  }

  @Test
  public void typeHasAnyAsSuperType() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons

        main = My_Type.Cons
        """);
    var myType = myTypeAtom.getMetaObject();
    assertThat(myType.hasMetaParents(), is(true));
    var metaParents = myType.getMetaParents();
    assertThat(metaParents.hasArrayElements(), is(true));
    assertThat("Has just one meta parent - Any", metaParents.getArraySize(), is(1L));
    var anyType = metaParents.getArrayElement(0);
    assertThat(anyType.getMetaSimpleName(), is("Any"));
  }

  @Test
  public void atomMembersAreConstructorFields_ManyConstructors() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons_1 f1 f2 f3 f4 f5 f6
            Cons_2 g1 g2 g3
            Cons_3 h1 h2 h3 h4 h5 h6 h7 h8 h9

        main = My_Type.Cons_2 "g1" "g2" "g3"
        """);
    assertThat(
        "Member names correspond to constructor field names for a single constructor",
        myTypeAtom.getMemberKeys(),
        containsInAnyOrder("g1", "g2", "g3"));
  }

  @Test
  public void methodIsAtomMember() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons a b
            method self = 42

        main = My_Type.Cons "a" "b"
        """);
    assertThat("Method is a member of the atom", myTypeAtom.getMemberKeys(), hasItem("method"));
    assertThat("method is an invokable member", myTypeAtom.canInvokeMember("method"), is(true));
  }

  @Test
  public void methodIsAtomMember_InteropLibrary() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons a b
            method self = 42

        main = My_Type.Cons "a" "b"
        """);
    var atom = ContextUtils.unwrapValue(ctx, myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat("Atom has members", interop.hasMembers(atom), is(true));
    assertThat("Method is readable", interop.isMemberReadable(atom, "method"), is(true));
    assertThat("Method is invocable", interop.isMemberInvocable(atom, "method"), is(true));
    assertThat("Field is readable", interop.isMemberReadable(atom, "a"), is(true));
  }

  @Test
  public void fieldsFromPrivateConstructorAreInternalMembers() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            private Cons a

        main = My_Type.Cons "a"
        """);
    var atom = ContextUtils.unwrapValue(ctx, myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat("field a is internal", interop.isMemberInternal(atom, "a"), is(true));
  }

  @Test
  public void fieldFromPrivateConstructorIsReadable() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            private Cons a

        main = My_Type.Cons "a"
        """);
    ContextUtils.executeInContext(
        ctx,
        () -> {
          var atom = ContextUtils.unwrapValue(ctx, myTypeAtom);
          var interop = InteropLibrary.getUncached();
          assertThat(
              "Field from private constructor is readable",
              interop.isMemberReadable(atom, "a"),
              is(true));
          assertThat(
              "Field from private constructor is invocable",
              interop.isMemberInvocable(atom, "a"),
              is(true));
          assertThat(
              "Field from private constructor can be read",
              interop.asString(interop.readMember(atom, "a")),
              is("a"));
          assertThat(
              "Field from private constructor can be invoked",
              interop.asString(interop.invokeMember(atom, "a")),
              is("a"));
          return null;
        });
  }

  @Test
  public void allMethodsAreInternalMembers() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons a
            pub_method self = 42
            private priv_method self = 42

        main = My_Type.Cons "a"
        """);
    var atom = ContextUtils.unwrapValue(ctx, myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat(
        "public method is internal member", interop.isMemberInternal(atom, "pub_method"), is(true));
    assertThat(
        "private method is internal member",
        interop.isMemberInternal(atom, "priv_method"),
        is(true));
  }

  @Test
  public void allMembersAreReadableAndInvocable()
      throws UnsupportedMessageException, InvalidArrayIndexException {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons a
            pub_method self = 42
            private priv_method self = 42

        main = My_Type.Cons "a"
        """);
    var atom = ContextUtils.unwrapValue(ctx, myTypeAtom);
    var interop = InteropLibrary.getUncached();
    var members = interop.getMembers(atom, true);
    for (long i = 0; i < interop.getArraySize(members); i++) {
      var memberName = interop.asString(interop.readArrayElement(members, i));
      assertThat(
          "Member " + memberName + " should be readable",
          interop.isMemberReadable(atom, memberName),
          is(true));
      assertThat(
          "Member " + memberName + " should be invocable",
          interop.isMemberInvocable(atom, memberName),
          is(true));
    }
  }

  @Test
  public void constructorIsNotAtomMember() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons a b
            method self = 42

        main = My_Type.Cons "a" "b"
        """);
    assertThat("Cons is not atom member", myTypeAtom.getMemberKeys(), not(hasItem("Cons")));
  }

  @Test
  public void fieldIsInvocable() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons a b

        main = My_Type.Cons 1 2
        """);
    ContextUtils.executeInContext(
        ctx,
        () -> {
          var atom = ContextUtils.unwrapValue(ctx, myTypeAtom);
          var interop = InteropLibrary.getUncached();
          assertThat("Field a is invocable", interop.isMemberInvocable(atom, "a"), is(true));
          var aField = interop.invokeMember(atom, "a");
          assertThat("Field is a number", interop.asInt(aField), is(1));
          assertThat("Field b is invocable", interop.isMemberInvocable(atom, "b"), is(true));
          return null;
        });
  }

  @Test
  public void fieldIsReadable() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons a

        main = My_Type.Cons 1
        """);
    ContextUtils.executeInContext(
        ctx,
        () -> {
          var atom = ContextUtils.unwrapValue(ctx, myTypeAtom);
          var interop = InteropLibrary.getUncached();
          assertThat("Field a is readable", interop.isMemberReadable(atom, "a"), is(true));
          return null;
        });
  }

  @Test
  public void staticMethodIsNotAtomMember() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons
            static_method = 42

        main = My_Type.Cons
        """);
    assertThat(
        "Static method is not atom member",
        myTypeAtom.getMemberKeys(),
        not(hasItem(containsString("static_method"))));
  }

  @Test
  public void constructorIsNotAtomMember_InteropLibrary() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons a b
            method self = 42

        main = My_Type.Cons "a" "b"
        """);
    var atom = ContextUtils.unwrapValue(ctx, myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat("Cons is not atom member", interop.isMemberExisting(atom, "Cons"), is(false));
  }

  @Test
  public void typeMembersAreConstructors() {
    var myType =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons_1
            Cons_2

        main = My_Type
        """);
    assertThat("type has constructors as members", myType.hasMembers(), is(true));
    assertThat(myType.getMemberKeys(), containsInAnyOrder("Cons_1", "Cons_2"));
    assertThat(
        "Constructor (type member) is instantiable",
        myType.getMember("Cons_1").canInstantiate(),
        is(true));
  }
}
