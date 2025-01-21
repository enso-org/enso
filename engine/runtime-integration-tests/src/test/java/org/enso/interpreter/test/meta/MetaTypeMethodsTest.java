package org.enso.interpreter.test.meta;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.util.ArrayList;
import java.util.List;
import org.enso.interpreter.node.expression.builtin.meta.GetTypeMethodsNode;
import org.enso.interpreter.test.ValuesGenerator;
import org.enso.interpreter.test.ValuesGenerator.Language;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests consistency between {@code Meta.get_type_methods} and {@link
 * com.oracle.truffle.api.interop.InteropLibrary#getMembers(Object) members interop message} for
 * {@link org.enso.interpreter.runtime.data.Type}nd {@link
 * org.enso.interpreter.runtime.data.atom.Atom}.
 */
public class MetaTypeMethodsTest {
  private static Context ctx;
  private static GetTypeMethodsNode getTypeMethodsNode;
  private static TestRootNode testRootNode;
  private static ValuesGenerator valuesGenerator;

  @BeforeClass
  public static void initCtx() {
    ctx = ContextUtils.createDefaultContext();
    valuesGenerator = ValuesGenerator.create(ctx, Language.ENSO);
    ContextUtils.executeInContext(
        ctx,
        () -> {
          getTypeMethodsNode = GetTypeMethodsNode.build();
          testRootNode = new TestRootNode();
          testRootNode.insertChildren(getTypeMethodsNode);
          return null;
        });
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
  }

  @Test
  public void testConsistencyBetweenMeta_And_TypeInterop() throws Exception {
    var allTypes = valuesGenerator.allTypes();
    ContextUtils.executeInContext(
        ctx,
        () -> {
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
            assertThat(
                errMsg, typeMethods, containsInAnyOrder(interopMembers.toArray(String[]::new)));
          }
          return null;
        });
  }

  private List<String> metaGetTypeMethods(Value type)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    var unwrapped = ContextUtils.unwrapValue(ctx, type);
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
    var unwrapped = ContextUtils.unwrapValue(ctx, type);
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
