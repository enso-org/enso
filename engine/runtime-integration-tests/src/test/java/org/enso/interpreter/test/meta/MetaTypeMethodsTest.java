package org.enso.interpreter.test.meta;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.isIn;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.enso.interpreter.node.expression.builtin.meta.GetTypeMethodsNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.test.ValuesGenerator;
import org.enso.interpreter.test.ValuesGenerator.Language;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

/**
 * Tests consistency between {@code Meta.get_type_methods} and {@link
 * com.oracle.truffle.api.interop.InteropLibrary#getMembers(Object) members interop message} for
 * {@link org.enso.interpreter.runtime.data.Type}nd {@link
 * org.enso.interpreter.runtime.data.atom.Atom}.
 */
public class MetaTypeMethodsTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  private static GetTypeMethodsNode getTypeMethodsNode;
  private static TestRootNode testRootNode;
  private static ValuesGenerator valuesGenerator;

  @BeforeClass
  public static void initCtx() {
    valuesGenerator = ValuesGenerator.create(ctxRule, Language.ENSO);
    getTypeMethodsNode = GetTypeMethodsNode.build();
    testRootNode = new TestRootNode();
    testRootNode.insertChildren(getTypeMethodsNode);
  }

  @AfterClass
  public static void disposeCtx() {
    valuesGenerator.close();
    valuesGenerator = null;
    getTypeMethodsNode = null;
    testRootNode = null;
  }

  @Test
  public void testConsistencyBetweenMeta_And_TypeInterop() throws Exception {
    var allTypes = valuesGenerator.allTypes();
    for (var type : allTypes) {
      var typeMethods = metaGetTypeMethods(type);
      var interopMembers = interopGetMembers(type);
      var errMsg =
          """
          Methods returned from `Meta.get_type_methods` and `InteropLibrary.getMembers` must be the same.
          Type: %s
          Return value of `Meta.get_type_methods`: %s
          Return value of `InteropLibrary.getMembers`: %s
          """
              .formatted(type, typeMethods, interopMembers);
      assertThat(errMsg, typeMethods, containsInAnyOrder(interopMembers.toArray(String[]::new)));
    }
  }

  @Test
  public void inheritedMembersFromNumberAreIncluded()
      throws InvalidArrayIndexException, UnsupportedMessageException {
    var integerType =
        ctxRule.evalModule(
            """
            import Standard.Base.Any.Any
            import Standard.Base.Data.Numbers.Number
            import Standard.Base.Data.Numbers.Integer

            main = Integer
            """);
    var anyMethods = methodsFrom("Standard.Base.Any", "Any");
    var numberMethods = methodsFrom("Standard.Base.Data.Numbers", "Number");
    var integerMethods = methodsFrom("Standard.Base.Data.Numbers", "Integer");
    var actualMethodNames =
        metaGetTypeMethods(integerType).stream().collect(Collectors.toUnmodifiableSet());
    assertSubset("Has method from Any", anyMethods, actualMethodNames);
    assertSubset("Has method from Number", numberMethods, actualMethodNames);
    assertSubset("Has method from Integer", integerMethods, actualMethodNames);
  }

  private Set<String> methodsFrom(String moduleName, String typeName) {
    var ensoCtx = ctxRule.ensoContext();
    var mod = ensoCtx.findModule(moduleName).get();
    var tp = mod.getScope().getType(typeName, true);
    var methods = mod.getScope().getMethodsForType(tp);
    return methods.stream()
        .map(Function::getName)
        .map(MetaTypeMethodsTest::unqualified)
        .collect(Collectors.toUnmodifiableSet());
  }

  private static <T> void assertSubset(String msg, Set<T> subset, Set<T> superSet) {
    var errMsg =
        """
        %s: Expected subset to be a subset of superSet.
        Subset: %s
        SuperSet: %s
        """
            .formatted(msg, subset, superSet);
    for (var item : subset) {
      assertThat(errMsg, item, isIn(superSet));
    }
  }

  private static String unqualified(String name) {
    if (name.contains(".")) {
      return name.substring(name.lastIndexOf('.') + 1);
    }
    return name;
  }

  private List<String> metaGetTypeMethods(Value type)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    var unwrapped = ctxRule.unwrapValue(type);
    var interop = InteropLibrary.getUncached();
    var typeMethodNames = new ArrayList<String>();
    var typeMethods = getTypeMethodsNode.execute(unwrapped);
    for (var i = 0; i < interop.getArraySize(typeMethods); i++) {
      var typeMethod = interop.readArrayElement(typeMethods, i);
      typeMethodNames.add(interop.asString(typeMethod));
    }
    return typeMethodNames;
  }

  private List<String> interopGetMembers(Value type)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    var unwrapped = ctxRule.unwrapValue(type);
    var interop = InteropLibrary.getUncached();
    var memberNames = new ArrayList<String>();
    var members = interop.getMembers(unwrapped, true);
    for (var i = 0; i < interop.getArraySize(members); i++) {
      var member = interop.readArrayElement(members, i);
      memberNames.add(interop.asString(member));
    }
    return memberNames;
  }
}
