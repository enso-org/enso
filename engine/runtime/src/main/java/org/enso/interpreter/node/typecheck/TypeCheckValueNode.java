package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import java.util.Arrays;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Stream;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.util.CachingSupplier;

/** A node and a factory for nodes performing type checks (including necessary conversions). */
public final class TypeCheckValueNode extends Node {
  private @Child AbstractTypeCheckNode check;
  private final boolean allTypes;

  TypeCheckValueNode(AbstractTypeCheckNode check, boolean allTypes) {
    assert check != null;
    this.check = check;
    this.allTypes = allTypes;
  }

  /**
   * Wraps expression node with additional type check.
   *
   * @param original the expression node
   * @param check node performing type check or {@code null}
   * @return wrapper around {@code original} or directly {@code original} if there is {@code null}
   *     check
   */
  public static ExpressionNode wrap(ExpressionNode original, TypeCheckValueNode check) {
    if (check == null) {
      return original;
    } else {
      return new TypeCheckExpressionNode(original, check);
    }
  }

  /**
   * Executes check or conversion of the value.
   *
   * @param frame frame requesting the conversion
   * @param value the value to convert
   * @param expr the expression node that produced the {@code value}
   * @return {@code null} when the check isn't satisfied and conversion isn't possible or non-{@code
   *     null} value that can be used as a result
   */
  public final Object handleCheckOrConversion(
      VirtualFrame frame, Object value, ExpressionNode expr) {
    var result = check.executeCheckOrConversion(frame, value, expr);
    if (result == null) {
      throw panicAtTheEnd(value);
    }
    return result;
  }

  /**
   * Combines existing type checks into "all of" check.
   *
   * @param comment description of the check meaning
   * @param checks existing type checks
   * @return node the composed check or {@code null} if no check is needed
   */
  public static TypeCheckValueNode allOf(String comment, TypeCheckValueNode... checks) {
    if (checks == null) {
      return null;
    }
    var list = Arrays.asList(checks);
    var flatten =
        list.stream()
            .filter(n -> n != null)
            .map(n -> n.check)
            .flatMap(
                n ->
                    n instanceof AllOfTypesCheckNode all
                        ? Arrays.asList(all.getChecks()).stream()
                        : Stream.of(n))
            .toList();
    var arr = toArray(flatten);
    return switch (arr.length) {
      case 0 -> null;
      case 1 -> new TypeCheckValueNode(arr[0], true);
      default -> new TypeCheckValueNode(new AllOfTypesCheckNode(comment, arr), true);
    };
  }

  /**
   * Combines existing checks into "one of" check.
   *
   * @param comment description of the check meaning
   * @param checks existing type checks
   * @return node the composed check or {@code null} if no check is needed
   */
  public static TypeCheckValueNode oneOf(String comment, TypeCheckValueNode... checks) {
    if (checks == null) {
      return null;
    }
    var list = Stream.of(checks).filter(n -> n != null).toList();
    return switch (list.size()) {
      case 0 -> null;
      case 1 -> list.get(0);
      default -> {
        var abstractTypeCheckList = list.stream().map(n -> n.check).toList();
        var abstractTypeCheckArr = toArray(abstractTypeCheckList);
        yield new TypeCheckValueNode(new OneOfTypesCheckNode(comment, abstractTypeCheckArr), true);
      }
    };
  }

  /**
   * Constructs "single type" check.
   *
   * @param comment description of the check meaning
   * @param expectedType the type to check for - it shouldn't be {@code Any}
   * @return node performing the check
   */
  public static TypeCheckValueNode single(String comment, Type expectedType) {
    var typeCheckNodeImpl = SingleTypeCheckNodeGen.create(comment, expectedType);
    return new TypeCheckValueNode(typeCheckNodeImpl, true);
  }

  /**
   * Constructs node to check for {@code polyglot java import} checks.
   *
   * @param comment description of the check meaning
   * @param metaObjectSupplier provider of the meta object to check for
   * @return node performing the check
   */
  public static TypeCheckValueNode meta(
      String comment, Supplier<? extends Object> metaObjectSupplier) {
    var cachingSupplier = CachingSupplier.wrap(metaObjectSupplier);
    var typeCheckNodeImpl = MetaTypeCheckNodeGen.create(comment, cachingSupplier);
    return new TypeCheckValueNode(typeCheckNodeImpl, true);
  }

  /**
   * Creates new node with different {@code allTypes} state. Otherwise the behavior is the same as
   * {@code node}. All types state influences the behavior of type checking {@link EnsoMultiValue} -
   * should all types the value is convertible to be used or only those types that the value has
   * already been converted to?
   *
   * <pre>
   * method arg:Text = # this check has allTypes == false
   *   num = arg:Number # this check has allTypes == true
   * </pre>
   *
   * @param allTypes the value of all types state for the new node
   * @param node the previous node with type checking logic
   */
  public static TypeCheckValueNode allTypes(boolean allTypes, TypeCheckValueNode node) {
    return node == null ? null : new TypeCheckValueNode(node.check, allTypes);
  }

  /**
   * Check whether given function is "lazy thunk". E.g. if it is a thunk wrapped by "lazy type
   * check".
   *
   * @param fn function to check
   * @return result of the check
   */
  public static boolean isWrappedThunk(Function fn) {
    if (fn.getSchema() == LazyCheckRootNode.SCHEMA) {
      return fn.getPreAppliedArguments()[0] instanceof Function wrappedFn && wrappedFn.isThunk();
    }
    return false;
  }

  private final PanicException panicAtTheEnd(Object v) {
    var expectedTypeMessage = check.getExpectedTypeMessage();
    var ctx = EnsoContext.get(this);
    Text msg;
    if (v instanceof UnresolvedConstructor) {
      msg = Text.create("Cannot find constructor {got} among {exp}");
    } else {
      msg = check.getComment();
    }
    var err = ctx.getBuiltins().error().makeTypeErrorOfComment(expectedTypeMessage, v, msg);
    throw new PanicException(err, this);
  }

  private static AbstractTypeCheckNode[] toArray(List<AbstractTypeCheckNode> list) {
    if (list == null) {
      return new AbstractTypeCheckNode[0];
    }
    var cnt = (int) list.stream().filter(n -> n != null).count();
    var arr = new AbstractTypeCheckNode[cnt];
    var it = list.iterator();
    for (int i = 0; i < cnt; ) {
      var element = it.next();
      if (element != null) {
        arr[i++] = element;
      }
    }
    return arr;
  }

  final boolean isAllTypes() {
    return allTypes;
  }
}
