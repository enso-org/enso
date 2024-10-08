package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

import com.oracle.truffle.api.interop.InteropLibrary;
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
    assertThat(
        "Method is a member of the atom", myTypeAtom.getMemberKeys(), hasItem("My_Type.method"));
    assertThat(
        "method is an invokable member", myTypeAtom.canInvokeMember("My_Type.method"), is(true));
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
    assertThat("Method is readable", interop.isMemberReadable(atom, "My_Type.method"), is(true));
    assertThat("Method is invocable", interop.isMemberInvocable(atom, "My_Type.method"), is(true));
    assertThat("Field is readable", interop.isMemberReadable(atom, "a"), is(true));
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
  public void fieldIsNotInvocable() {
    var myTypeAtom =
        ContextUtils.evalModule(
            ctx,
            """
        type My_Type
            Cons a b

        main = My_Type.Cons "a" "b"
        """);
    var atom = ContextUtils.unwrapValue(ctx, myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat("Field is not invocable", interop.isMemberInvocable(atom, "a"), is(false));
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
