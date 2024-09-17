package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
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

  @Ignore("https://github.com/enso-org/enso/issues/10675")
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
