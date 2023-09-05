package org.enso.interpreter.node.callable.argument;

import java.util.Arrays;
import java.util.stream.Collectors;

import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.BaseNode.TailStatus;
import org.enso.interpreter.node.EnsoRootNode;
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
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
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
import com.oracle.truffle.api.nodes.RootNode;

public abstract class ReadArgumentCheckNode extends Node {
  private final String name;
  @Child IsValueOfTypeNode checkType;
  @CompilerDirectives.CompilationFinal(dimensions = 1)
  private final Type[] expectedTypes;
  @CompilerDirectives.CompilationFinal
  private String expectedTypeMessage;
  @CompilerDirectives.CompilationFinal
  private LazyCheckRootNode lazyCheck;

  ReadArgumentCheckNode(String name, Type[] expectedTypes) {
    this.name = name;
    this.checkType = IsValueOfTypeNode.build();
    this.expectedTypes = expectedTypes;
  }

  public static ReadArgumentCheckNode build(String argumentName, Type[] expectedTypes) {
    if (expectedTypes == null || expectedTypes.length == 0) {
      return null;
    } else {
      return ReadArgumentCheckNodeGen.create(argumentName, expectedTypes);
    }
  }

  public abstract Object executeCheckOrConversion(VirtualFrame frame, Object value);

  public static boolean isWrappedThunk(Function fn) {
    if (fn.getSchema() == LazyCheckRootNode.SCHEMA) {
      return fn.getPreAppliedArguments()[0] instanceof Function wrappedFn && wrappedFn.isThunk();
    }
    return false;
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
    @Shared("typeOfNode") @Cached TypeOfNode typeOfNode,
    @Cached("findType(typeOfNode, v)") Type cachedType,
    @Cached("findConversionNode(cachedType)") ApplicationNode convertNode
  ) {
    return handleWithConversion(frame, v, convertNode);
  }

  @Specialization(replaces = "doWithConversionCached")
  Object doWithConversionUncached(
    VirtualFrame frame, Object v,
    @Shared("typeOfNode") @Cached TypeOfNode typeOfNode
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
    for (Type t : expectedTypes) {
      if (checkType.execute(t, v)) {
        return v;
      }
    }
    return null;
  }

  @ExplodeLoop
  private Pair<Function, Type> findConversion(Type from) {
    var ctx = EnsoContext.get(this);

    if (getRootNode() instanceof EnsoRootNode root) {
      var convert = UnresolvedConversion.build(root.getModuleScope());
      for (Type into : expectedTypes) {
        var conv = convert.resolveFor(ctx, into, from);
        if (conv != null) {
          return Pair.create(conv, into);
        }
      }
    }
    return null;
  }

  ApplicationNode findConversionNode(Type from) {
    var convAndType = findConversion(from);

    if (convAndType != null && getParent() instanceof ReadArgumentNode ran) {
      CompilerAsserts.neverPartOfCompilation();
      var convNode = LiteralNode.build(convAndType.getLeft());
      var intoNode = LiteralNode.build(convAndType.getRight());
      var valueNode = ran.plainRead();
      var args = new CallArgument[]{
        new CallArgument(null, intoNode),
        new CallArgument(null, valueNode)
      };
      return ApplicationNode.build(convNode, args, DefaultsExecutionMode.EXECUTE);
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
      throw panicAtTheEnd(v);
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

  private PanicException panicAtTheEnd(Object v) {
    var ctx = EnsoContext.get(this);
    var err = ctx.getBuiltins().error().makeTypeError(expectedTypeMessage(), v, name);
    throw new PanicException(err, this);
  }

  private String expectedTypeMessage() {
    if (expectedTypeMessage != null) {
      return expectedTypeMessage;
    }
    CompilerDirectives.transferToInterpreterAndInvalidate();
    expectedTypeMessage = expectedTypes.length == 1 ?
            expectedTypes[0].toString() :
            Arrays.stream(expectedTypes).map(Type::toString).collect(Collectors.joining(" | "));
    return expectedTypeMessage;
  }

  private static final class LazyCheckRootNode extends RootNode {
    @Child
    private ThunkExecutorNode evalThunk;
    @Child
    private ReadArgumentCheckNode check;

    static final FunctionSchema SCHEMA = new FunctionSchema(
      FunctionSchema.CallerFrameAccess.NONE,
      new ArgumentDefinition[] { new ArgumentDefinition(0, "delegate", null, null, ExecutionMode.EXECUTE) },
      new boolean[] { true },
      new CallArgumentInfo[0],
      new Annotation[0]
    );

    LazyCheckRootNode(TruffleLanguage<?> language, ReadArgumentCheckNode check) {
      super(language);
      this.check = check;
      this.evalThunk = ThunkExecutorNode.build();
    }

    Function wrapThunk(Function thunk) {
      return new Function(getCallTarget(), thunk.getScope(), SCHEMA, new Object[] { thunk }, null);
    }

    @Override
    public Object execute(VirtualFrame frame) {
      var state = Function.ArgumentsHelper.getState(frame.getArguments());
      var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
      assert args.length == 1;
      assert args[0] instanceof Function fn && fn.isThunk();
      var raw = evalThunk.executeThunk(frame, args[0], state, TailStatus.NOT_TAIL);
      var result = check.executeCheckOrConversion(frame, raw);
      return result;
    }
  }
}
