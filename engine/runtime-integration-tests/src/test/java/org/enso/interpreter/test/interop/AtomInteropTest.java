package org.enso.interpreter.test.interop;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import java.util.ArrayList;
import java.util.List;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.ClassRule;
import org.junit.Test;

/**
 * Tests various {@link com.oracle.truffle.api.interop.InteropLibrary interop} contracts for {@link
 * org.enso.interpreter.runtime.data.atom.Atom atoms}.
 */
public class AtomInteropTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void atomMemberNames_AreNotQualified() {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            import Standard.Base.Any.Any

            type My_Type
                Cons field_1 field_2

            main =
                My_Type.Cons 1 2
            """);
    assertThat(myTypeAtom.hasMembers(), is(true));
    var memberNames = myTypeAtom.getMemberKeys();
    assertThat("Member names are not qualified", memberNames, hasItem(not(containsString("."))));
  }

  @Test
  public void atomMembersAreConstructorFields_SingleConstructor() {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            type My_Type
                Cons field_1 field_2

            main =
                My_Type.Cons 1 2
            """);
    assertThat(myTypeAtom.hasMembers(), is(true));
    var memberNames = myTypeAtom.getMemberKeys();
    assertThat("Has more than two fields", memberNames.size(), is(greaterThan(2)));
    for (var consName : List.of("field_1", "field_2")) {
      var member = myTypeAtom.getMember(consName);
      assertThat("Member " + consName + " should be readable", member, is(notNullValue()));
      assertThat("Cons field is number", member.isNumber(), is(true));
    }
  }

  @Test
  public void atomIsNotMetaObject() {
    var myTypeAtom =
        ctxRule.evalModule(
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
        ctxRule.evalModule(
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
        ctxRule.evalModule(
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
        allOf(hasItem("g1"), hasItem("g2"), hasItem("g3"), not(hasItem("h1"))));
  }

  @Test
  public void methodIsAtomMember() {
    var myTypeAtom =
        ctxRule.evalModule(
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
        ctxRule.evalModule(
            """
            type My_Type
                Cons a b
                method self = 42

            main = My_Type.Cons "a" "b"
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat("Atom has members", interop.hasMembers(atom), is(true));
    assertThat("Method is readable", interop.isMemberReadable(atom, "method"), is(true));
    assertThat("Method is invocable", interop.isMemberInvocable(atom, "method"), is(true));
    assertThat("Field is readable", interop.isMemberReadable(atom, "a"), is(true));
  }

  @Test
  public void fieldsFromPrivateConstructorAreInternalMembers() {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            type My_Type
                private Cons a

            main = My_Type.Cons "a"
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat("field a is internal", interop.isMemberInternal(atom, "a"), is(true));
  }

  @Test
  public void methodsAreVisibleEvenWithPrivateConstructors() throws Exception {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            type My_Type
                private Cons a

                read self = self.a

            main = My_Type.Cons "a"
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
    var interop = InteropLibrary.getUncached();
    var read = interop.readMember(atom, "read");
    assertNotNull("Found read member", read);
    assertEquals("a", read.toString());
  }

  @Test
  public void methodsAreVisibleOnTypeEvenWithPrivateConstructors() throws Exception {
    var atom =
        ctxRule.evalModule(
            """
            type My_Type
                private Cons a

                read self = self.a

            main = My_Type.Cons "a"
            """);
    var type = ctxRule.unwrapValue(atom.getMetaObject());
    var rawAtom = ctxRule.unwrapValue(atom);
    var interop = InteropLibrary.getUncached();
    var read = interop.readMember(type, "read");
    assertNotNull("Found read member", read);
    assertTrue("Can read", interop.isExecutable(read));
    assertEquals("a", interop.execute(read, rawAtom).toString());
  }

  @Test
  public void fieldFromPrivateConstructorIsReadable()
      throws UnsupportedMessageException,
          UnknownIdentifierException,
          UnsupportedTypeException,
          ArityException {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            type My_Type
                private Cons a

            main = My_Type.Cons "a"
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
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
  }

  @Test
  public void allMethodsAreInternalMembers() {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            type My_Type
                Cons a
                pub_method self = 42
                private priv_method self = 42

            main = My_Type.Cons "a"
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat(
        "public method is internal member", interop.isMemberInternal(atom, "pub_method"), is(true));
    assertThat(
        "private method is internal member",
        interop.isMemberInternal(atom, "priv_method"),
        is(true));
  }

  /**
   * Builtin methods from Any are present even if the Standard.Base.Any module is not imported.
   *
   * @throws Exception
   */
  @Test
  public void internalMembersIncludeMethodsFromAny_WithoutImport() throws Exception {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            type My_Type
                Cons a

            main = My_Type.Cons "a"
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
    var memberNames = getAllMemberNames(atom);
    var anyBuiltinMethods = ctxRule.builtinMethodsFromAny();
    for (var method : anyBuiltinMethods) {
      assertThat("Builtin method (from Any) is a member of atom", memberNames, hasItem(method));
    }
  }

  /**
   * When Standard.Base.Any module is imported, all the methods (both builtin and non-builtin) from
   * Any should be present as internal members of the atom.
   */
  @Test
  public void internalMembersIncludeMethodsFromAny_WithImport() throws Exception {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            from Standard.Base.Any import all

            type My_Type
                Cons a

            main = My_Type.Cons "a"
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
    var memberNames = getAllMemberNames(atom);
    var anyMethods = ctxRule.allMethodsFromAny();
    for (var method : anyMethods) {
      assertThat("Non-builtin method (from Any) is a member of atom", memberNames, hasItem(method));
    }
  }

  @Test
  public void allMembersAreReadableAndInvocable()
      throws UnsupportedMessageException, InvalidArrayIndexException {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            type My_Type
                Cons a
                pub_method self = 42
                private priv_method self = 42

            main = My_Type.Cons "a"
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
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
        ctxRule.evalModule(
            """
            type My_Type
                Cons a b
                method self = 42

            main = My_Type.Cons "a" "b"
            """);
    assertThat("Cons is not atom member", myTypeAtom.getMemberKeys(), not(hasItem("Cons")));
  }

  @Test
  public void fieldIsInvocable()
      throws UnsupportedMessageException,
          UnknownIdentifierException,
          UnsupportedTypeException,
          ArityException {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            type My_Type
                Cons a b

            main = My_Type.Cons 1 2
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat("Field a is invocable", interop.isMemberInvocable(atom, "a"), is(true));
    var aField = interop.invokeMember(atom, "a");
    assertThat("Field is a number", interop.asInt(aField), is(1));
    assertThat("Field b is invocable", interop.isMemberInvocable(atom, "b"), is(true));
  }

  @Test
  public void fieldIsReadable() {
    var myTypeAtom =
        ctxRule.evalModule(
            """
            type My_Type
                Cons a

            main = My_Type.Cons 1
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat("Field a is readable", interop.isMemberReadable(atom, "a"), is(true));
  }

  @Test
  public void staticMethodIsNotAtomMember() {
    var myTypeAtom =
        ctxRule.evalModule(
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
        ctxRule.evalModule(
            """
            type My_Type
                Cons a b
                method self = 42

            main = My_Type.Cons "a" "b"
            """);
    var atom = ctxRule.unwrapValue(myTypeAtom);
    var interop = InteropLibrary.getUncached();
    assertThat("Cons is not atom member", interop.isMemberExisting(atom, "Cons"), is(false));
  }

  @Test
  public void typeMembersAreConstructors() {
    var myType =
        ctxRule.evalModule(
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

  @Test
  public void invokeLazyField_DoesNotCauseStackOverflow()
      throws UnsupportedMessageException,
          UnknownIdentifierException,
          UnsupportedTypeException,
          ArityException {
    var atom =
        ctxRule.evalModule(
            """
            from Standard.Base.Any import all

            type Generator
                Value n ~next

            natural =
                gen n = Generator.Value n (gen n+1)
                gen 2

            main =
                natural
            """);
    var atomUnwrapped = ctxRule.unwrapValue(atom);
    var interop = InteropLibrary.getUncached();
    var next = interop.invokeMember(atomUnwrapped, "next");
    assertThat("Returns next atom", interop.hasMembers(next), is(true));
  }

  @Test
  public void instanceMethod_CanBeInvokedViaAtom() {
    var atom =
        ctxRule.evalModule(
            """
            type My_Type
                Cons
                method self = 42
            main = My_Type.Cons
            """);
    assertThat(atom.hasMember("method"), is(true));
    assertThat(atom.canInvokeMember("method"), is(true));
    var res = atom.invokeMember("method");
    assertThat(res.asInt(), is(42));
  }

  @Test
  public void instanceMethod_IsMemberOfType() {
    var type =
        ctxRule.evalModule(
            """
            type My_Type
                Cons
                method self = 42
            main = My_Type
            """);
    assertThat(type.hasMember("method"), is(true));
    assertThat(type.canInvokeMember("method"), is(true));
  }

  @Test
  public void instanceMethod_CanBeInvokedViaType() {
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
    assertThat(res.asInt(), is(42));
  }

  @Test
  public void invokeVsReadAndExecute() throws Exception {
    var atom =
        ctxRule.evalModule(
            """
            from Standard.Base.Any import all

            type Generator
                Value n ~next

                ahead self n = if n <= 1 then self.next else
                    @Tail_Call self.next.ahead n-1

            main =
                gen n = Generator.Value n (gen n+1)
                gen 2
            """);
    var atomUnwrapped = ctxRule.unwrapValue(atom);
    var interop = InteropLibrary.getUncached();

    assertTrue("ahead method is invocable", interop.isMemberInvocable(atomUnwrapped, "ahead"));
    var invokeAhead = interop.invokeMember(atomUnwrapped, "ahead", 5);
    assertEquals("2+5", 7L, interop.readMember(invokeAhead, "n"));

    var aheadFn = interop.readMember(atomUnwrapped, "ahead");
    assertTrue("Function can be executed", interop.isExecutable(aheadFn));
    var readNext = interop.execute(aheadFn, 5);

    assertSame("invokeMember yields the same as readMember+execute", invokeAhead, readNext);
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
}
