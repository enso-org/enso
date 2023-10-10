package org.enso.interpreter.node.callable.argument;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.enso.compiler.core.ir.Name;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.BaseNode.TailStatus;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.ApplicationNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.meta.AtomWithAHoleNode;
import org.enso.interpreter.node.expression.builtin.meta.IsValueOfTypeNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.node.expression.literal.LiteralNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.Annotation;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition.ExecutionMode;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.graalvm.collections.Pair;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.InvalidAssumptionException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeUtil;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class ReadArgumentCheckNode extends Node {
  private final String name;
  @CompilerDirectives.CompilationFinal
  private String expectedTypeMessage;

  ReadArgumentCheckNode(String name) {
    this.name = name;
  }

  /**
   *
   */
  public static ExpressionNode wrap(ExpressionNode original, ReadArgumentCheckNode check) {
    return new TypeCheckExpressionNode(original, check);
  }

  /** Executes check or conversion of the value.abstract
   * @param frame frame requesting the conversion
   * @param value the value to convert
   * @return {@code null} when the check isn't satisfied and conversion isn't possible or non-{@code null} value that can be used as a result
   */
  public final Object handleCheckOrConversion(VirtualFrame frame, Object value) {
    var result = executeCheckOrConversion(frame, value);
    if (result == null) {
      throw panicAtTheEnd(value);
    }
    return result;
  }

  abstract Object executeCheckOrConversion(VirtualFrame frame, Object value);
  abstract String expectedTypeMessage();

  final PanicException panicAtTheEnd(Object v) {
    if (expectedTypeMessage == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      expectedTypeMessage = expectedTypeMessage();
    }
    var ctx = EnsoContext.get(this);
    var msg = name == null ? "expression" : name;
    var err = ctx.getBuiltins().error().makeTypeError(expectedTypeMessage, v, msg);
    throw new PanicException(err, this);
  }

  public static ReadArgumentCheckNode allOf(Name argumentName, ReadArgumentCheckNode... checks) {
    var list = Arrays.asList(checks);
    var flatten = list.stream().flatMap(n -> n instanceof AllOfNode all ? Arrays.asList(all.checks).stream() : Stream.of(n)).toList();
    var arr = toArray(flatten);
    return switch (arr.length) {
      case 0 -> null;
      case 1 -> arr[0];
      default -> new AllOfNode(argumentName.name(), arr);
    };
  }

  public static ReadArgumentCheckNode oneOf(Name argumentName, List<ReadArgumentCheckNode> checks) {
    var arr = toArray(checks);
    return switch (arr.length) {
      case 0 -> null;
      case 1 -> arr[0];
      default -> new OneOfNode(argumentName.name(), arr);
    };
  }

  public static ReadArgumentCheckNode build(Name argumentName, Type expectedType) {
    var n = argumentName == null ? null : argumentName.name();
    return ReadArgumentCheckNodeFactory.TypeCheckNodeGen.create(n, expectedType);
  }

  public static boolean isWrappedThunk(Function fn) {
    if (fn.getSchema() == LazyCheckRootNode.SCHEMA) {
      return fn.getPreAppliedArguments()[0] instanceof Function wrappedFn && wrappedFn.isThunk();
    }
    return false;
  }

  private static ReadArgumentCheckNode[] toArray(List<ReadArgumentCheckNode> list) {
    if (list == null) {
      return new ReadArgumentCheckNode[0];
    }
    var cnt = (int) list.stream().filter(n -> n != null).count();
    var arr = new ReadArgumentCheckNode[cnt];
    var it = list.iterator();
    for (int i = 0; i < cnt;) {
      var element = it.next();
      if (element != null) {
        arr[i++] = element;
      }
    }
    return arr;
  }

  static final class AllOfNode extends ReadArgumentCheckNode {
    @Children
    private ReadArgumentCheckNode[] checks;
    @Child
    private TypesLibrary types;

    AllOfNode(String name, ReadArgumentCheckNode[] checks) {
      super(name);
      this.checks = checks;
      this.types = TypesLibrary.getFactory().createDispatched(checks.length);
    }

    @Override
    @ExplodeLoop
    Object executeCheckOrConversion(VirtualFrame frame, Object value) {
      var values = new Object[checks.length];
      var valueTypes = new Type[checks.length];
      var at = 0;
      for (var n : checks) {
        var result = n.executeCheckOrConversion(frame, value);
        if (result == null) {
          return null;
        }
        values[at] = result;
        valueTypes[at] = types.getType(result);
        at++;
      }
      return EnsoMultiValue.create(valueTypes, values);
    }

    @Override
    String expectedTypeMessage() {
      return Arrays.stream(checks).map(n -> n.expectedTypeMessage()).collect(Collectors.joining(" & "));
    }
  }

  static final class OneOfNode extends ReadArgumentCheckNode {
    @Children
    private ReadArgumentCheckNode[] checks;

    OneOfNode(String name, ReadArgumentCheckNode[] checks) {
      super(name);
      this.checks = checks;
    }

    @Override
    @ExplodeLoop
    Object executeCheckOrConversion(VirtualFrame frame, Object value) {
      for (var n : checks) {
        if (n instanceof TypeCheckNode typeCheck) {
          var result = typeCheck.findAmongTypes(value);
          if (result != null) {
            return result;
          }
        }
      }
      for (var n : checks) {
        var result = n.executeCheckOrConversion(frame, value);
        if (result != null) {
          return result;
        }
      }
      return null;
    }

    @Override
    String expectedTypeMessage() {
      return Arrays.stream(checks).map(n -> n.expectedTypeMessage()).collect(Collectors.joining(" | "));
    }
  }

  static abstract class TypeCheckNode extends ReadArgumentCheckNode {
    private final Type expectedType;
    @Child
    IsValueOfTypeNode checkType;
    @CompilerDirectives.CompilationFinal
    private String expectedTypeMessage;
    @CompilerDirectives.CompilationFinal
    private LazyCheckRootNode lazyCheck;

    TypeCheckNode(String name, Type expectedType) {
      super(name);
      this.checkType = IsValueOfTypeNode.build();
      this.expectedType = expectedType;
    }

    @Specialization
    Object doPanicSentinel(VirtualFrame frame, PanicSentinel panicSentinel) {
      throw panicSentinel;
    }

    @Specialization(rewriteOn = InvalidAssumptionException.class)
    Object doCheckNoConversionNeeded(VirtualFrame frame, Object v) throws InvalidAssumptionException {
      var ret = findAmongTypes(v);
      if (ret != null) {
        return ret;
      } else {
        throw new InvalidAssumptionException();
      }
    }

    @Specialization(limit = "10", guards = {
      "cachedType != null",
      "findType(typeOfNode, v) == cachedType"
    })
    Object doWithConversionCached(
            VirtualFrame frame, Object v,
            @Shared("typeOfNode")
            @Cached TypeOfNode typeOfNode,
            @Cached("findType(typeOfNode, v)") Type cachedType,
            @Cached("findConversionNode(cachedType)") ApplicationNode convertNode
    ) {
      return handleWithConversion(frame, v, convertNode);
    }

    @Specialization(replaces = "doWithConversionCached")
    Object doWithConversionUncached(
            VirtualFrame frame, Object v,
            @Shared("typeOfNode")
            @Cached TypeOfNode typeOfNode
    ) {
      var type = findType(typeOfNode, v);
      return doWithConversionUncachedBoundary(frame == null ? null : frame.materialize(), v, type);
    }

    private static boolean isAllFitValue(Object v) {
      return v instanceof DataflowError || AtomWithAHoleNode.isHole(v);
    }

    @ExplodeLoop
    private Object findAmongTypes(Object v) {
      if (isAllFitValue(v)) {
        return v;
      }
      if (v instanceof Function fn && fn.isThunk()) {
        if (lazyCheck == null) {
          CompilerDirectives.transferToInterpreter();
          var enso = EnsoLanguage.get(this);
          var node = (ReadArgumentCheckNode) copy();
          lazyCheck = new LazyCheckRootNode(enso, node);
        }
        var lazyCheckFn = lazyCheck.wrapThunk(fn);
        return lazyCheckFn;
      }
      if (v instanceof EnsoMultiValue mv) {
        var result = mv.castTo(expectedType);
        if (result != null) {
          return result;
        }
      }
      if (checkType.execute(expectedType, v)) {
        return v;
      }
      return null;
    }

    private Pair<Function, Type> findConversion(Type from) {
      if (expectedType == from) {
        return null;
      }
      var ctx = EnsoContext.get(this);

      if (getRootNode() instanceof EnsoRootNode root) {
        var convert = UnresolvedConversion.build(root.getModuleScope());
        var conv = convert.resolveFor(ctx, expectedType, from);
        if (conv != null) {
          return Pair.create(conv, expectedType);
        }
      }
      return null;
    }

    ApplicationNode findConversionNode(Type from) {
      var convAndType = findConversion(from);

      if (convAndType != null) {
        if (NodeUtil.findParent(this, ReadArgumentNode.class) instanceof ReadArgumentNode ran) {
          CompilerAsserts.neverPartOfCompilation();
          var convNode = LiteralNode.build(convAndType.getLeft());
          var intoNode = LiteralNode.build(convAndType.getRight());
          var valueNode = ran.plainRead();
          var args = new CallArgument[]{
            new CallArgument(null, intoNode),
            new CallArgument(null, valueNode)
          };
          return ApplicationNode.build(convNode, args, DefaultsExecutionMode.EXECUTE);
        } else if (NodeUtil.findParent(this, TypeCheckExpressionNode.class) instanceof TypeCheckExpressionNode tcen) {
          CompilerAsserts.neverPartOfCompilation();
          var convNode = LiteralNode.build(convAndType.getLeft());
          var intoNode = LiteralNode.build(convAndType.getRight());
          var valueNode = tcen.original;
          var args = new CallArgument[]{
            new CallArgument(null, intoNode),
            new CallArgument(null, valueNode)
          };
          return ApplicationNode.build(convNode, args, DefaultsExecutionMode.EXECUTE);
        }
      }
      return null;
    }

    Type findType(TypeOfNode typeOfNode, Object v) {
      if (typeOfNode.execute(v) instanceof Type from) {
        return from;
      }
      return null;
    }

    private Object handleWithConversion(
            VirtualFrame frame, Object v, ApplicationNode convertNode
    ) throws PanicException {
      if (convertNode == null) {
        var ret = findAmongTypes(v);
        if (ret != null) {
          return ret;
        }
        return null;
      } else {
        var converted = convertNode.executeGeneric(frame);
        return converted;
      }
    }

    @CompilerDirectives.TruffleBoundary
    private Object doWithConversionUncachedBoundary(MaterializedFrame frame, Object v, Type type) {
      var convertNode = findConversionNode(type);
      return handleWithConversion(frame, v, convertNode);
    }

    @Override
    String expectedTypeMessage() {
      if (expectedTypeMessage != null) {
        return expectedTypeMessage;
      }
      CompilerDirectives.transferToInterpreterAndInvalidate();
      expectedTypeMessage = expectedType.toString();
      return expectedTypeMessage;
    }
  }

  private static final class LazyCheckRootNode extends RootNode {

    @Child
    private ThunkExecutorNode evalThunk;
    @Child
    private ReadArgumentCheckNode check;

    static final FunctionSchema SCHEMA = new FunctionSchema(
            FunctionSchema.CallerFrameAccess.NONE,
            new ArgumentDefinition[]{new ArgumentDefinition(0, "delegate", null, null, ExecutionMode.EXECUTE)},
            new boolean[]{true},
            new CallArgumentInfo[0],
            new Annotation[0]
    );

    LazyCheckRootNode(TruffleLanguage<?> language, ReadArgumentCheckNode check) {
      super(language);
      this.check = check;
      this.evalThunk = ThunkExecutorNode.build();
    }

    Function wrapThunk(Function thunk) {
      return new Function(getCallTarget(), thunk.getScope(), SCHEMA, new Object[]{thunk}, null);
    }

    @Override
    public Object execute(VirtualFrame frame) {
      var state = Function.ArgumentsHelper.getState(frame.getArguments());
      var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
      assert args.length == 1;
      assert args[0] instanceof Function fn && fn.isThunk();
      var raw = evalThunk.executeThunk(frame, args[0], state, TailStatus.NOT_TAIL);
      var result = check.handleCheckOrConversion(frame, raw);
      return result;
    }
  }

  private static final class TypeCheckExpressionNode extends ExpressionNode {
    @Child
    private ExpressionNode original;
    @Child
    private ReadArgumentCheckNode check;

    TypeCheckExpressionNode(ExpressionNode original, ReadArgumentCheckNode check) {
      this.check = check;
      this.original = original;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
      var value = original.executeGeneric(frame);
      var result = check.handleCheckOrConversion(frame, value);
      return result;
    }

    @Override
    public boolean isInstrumentable() {
      return false;
    }
  }

}
