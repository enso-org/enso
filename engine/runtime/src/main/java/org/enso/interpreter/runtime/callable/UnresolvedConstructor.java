package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Arrays;
import java.util.Objects;
import java.util.UUID;
import org.enso.compiler.context.LocalScope;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.ApplicationNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.argument.ReadArgumentNode;
import org.enso.interpreter.node.callable.function.BlockNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;

/**
 * Value representing a by-name identified constructor of a yet unknown {@link Type}. Create new
 * instance by providing name to {@link #build(String)} invokeConstructor. Then apply arguments to
 * it via {@link #withArguments(Object[])} invokeConstructor.
 *
 * <p>Let the object flow thru the interpreter and resolve it when the required {@link Type} is
 * known.
 */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class UnresolvedConstructor implements EnsoObject {
  private static final CallArgumentInfo[] NONE = new CallArgumentInfo[0];
  private final String name;
  private final Node where;
  final CallArgumentInfo[] descs;
  private final Object[] args;

  /**
   * Creates a new unresolved name.
   *
   * @param name constructor name
   * @param where where the constructor was constructed
   * @param descs argument descriptions to apply to the constructor
   * @param args argument values to apply to the constructor
   */
  private UnresolvedConstructor(String name, Node where, CallArgumentInfo[] descs, Object[] args) {
    this.name = name;
    this.where = where;
    this.descs = descs;
    this.args = args;
  }

  final String getName() {
    return name;
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return ".." + name;
  }

  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
    return toString();
  }

  /**
   * Creates an instance of this node.
   *
   * @param where location of the created constructor
   * @param name the name (of the constructor) we are searching for
   * @return a object representing unresolved (constructor)
   */
  public static UnresolvedConstructor build(Node where, String name) {
    return new UnresolvedConstructor(name, where, NONE, NONE);
  }

  /**
   * Marks this object as executable through the interop library.
   *
   * @param where location where the unresolved constructor is constructed
   * @param additionalDescriptions description of the applied arguments
   * @param additionalArguments new arguments to add to the unresolved constructor
   * @return always true @ExportMessage public boolean isExecutable() { return true; }
   */
  public UnresolvedConstructor withArguments(
      Node where, CallArgumentInfo[] additionalDescriptions, Object[] additionalArguments) {
    if (this.args == NONE) {
      return new UnresolvedConstructor(
          this.name, where, additionalDescriptions, additionalArguments);
    } else {
      var newDescs = join(this.descs, additionalDescriptions);
      var newArgs = join(this.args, additionalArguments);
      return new UnresolvedConstructor(this.name, where, newDescs, newArgs);
    }
  }

  final UnresolvedConstructor asPrototype() {
    return new UnresolvedConstructor(this.name, this.where, this.descs, null);
  }

  final boolean sameAsPrototyped(UnresolvedConstructor other) {
    if (descs.length != other.descs.length) {
      return false;
    }
    if (where != other.where) {
      return false;
    }
    if (!name.equals(other.name)) {
      return false;
    }
    for (var i = 0; i < descs.length; i++) {
      if (!Objects.equals(descs[i].getName(), other.descs[i].getName())) {
        return false;
      }
    }
    return true;
  }

  private static <T> T[] join(T[] arr1, T[] arr2) {
    var ret = Arrays.copyOf(arr1, arr1.length + arr2.length);
    System.arraycopy(arr2, 0, ret, arr1.length, arr2.length);
    return ret;
  }

  /**
   * Converts an instance of {@link UnresolvedConstructor} to a real {@code Atom} by finding the
   * right {@link AtomConstructor} to invoke. Just pass the right {@code expectedType} into the
   * {@link #execute} method.
   */
  public abstract static class ConstructNode extends Node {
    @CompilerDirectives.CompilationFinal SourceSection where;

    /**
     * Convert the {@code unresolved} constructor to a real instance based on the specified {@code
     * expectedType}.
     *
     * @param frame frame of the Enso method
     * @param state state of the Enso interpreter
     * @param expectedType the requested type
     * @param unresolved the unresolved constructor to convert
     * @return result of the conversion or {@code null} if no constructor of the right {@link
     *     UnresolvedConstructor#getName()} is found in the specified {@code expectedType}
     */
    public abstract Object execute(
        VirtualFrame frame, State state, Type expectedType, UnresolvedConstructor unresolved);

    @Override
    public SourceSection getSourceSection() {
      return where;
    }

    static DirectCallNode buildApplication(UnresolvedConstructor prototype) {
      UUID id = null;
      SourceSection section = null;
      var scope =
          prototype.where.getRootNode() instanceof EnsoRootNode root ? root.getModuleScope() : null;
      for (var where = prototype.where; where != null; where = where.getParent()) {
        if (where instanceof ExpressionNode withId && withId.getId() != null) {
          if (!(where instanceof ApplicationNode)) {
            id = withId.getId();
          }
          section = withId.getSourceSection();
          scope = withId.getRootNode() instanceof EnsoRootNode root ? root.getModuleScope() : null;
          break;
        } else if (where instanceof InvokeCallableNode callable && callable.getId() != null) {
          id = callable.getId();
        }
      }
      var fn = ReadArgumentNode.build(0, null, null);
      var args = new CallArgument[prototype.descs.length];
      for (var i = 0; i < args.length; i++) {
        args[i] =
            new CallArgument(
                prototype.descs[i].getName(), ReadArgumentNode.build(1 + i, null, null));
      }
      var expr = ApplicationNode.build(fn, args, DefaultsExecutionMode.EXECUTE);
      if (id != null) {
        expr.setId(id);
      }
      if (section != null) {
        expr.setSourceLocation(section.getCharIndex(), section.getCharLength());
      }
      var lang = EnsoLanguage.get(null);
      var body = BlockNode.buildSilent(new ExpressionNode[0], expr);
      body.adoptChildren();
      var root =
          ClosureRootNode.build(
              lang, LocalScope.empty(), scope, body, section, prototype.getName(), true, true);
      root.adoptChildren();
      assert Objects.equals(expr.getSourceSection(), section)
          : "Expr: " + expr.getSourceSection() + " orig: " + section;
      return DirectCallNode.create(root.getCallTarget());
    }

    @Specialization(
        guards = {"cachedType == expectedType", "prototype.sameAsPrototyped(unresolved)"},
        limit = "10")
    Object instantiateCached(
        VirtualFrame frame,
        State state,
        Type expectedType,
        UnresolvedConstructor unresolved,
        @Cached("expectedType") Type cachedType,
        @Cached("unresolved.asPrototype()") UnresolvedConstructor prototype,
        @Cached("buildApplication(prototype)") DirectCallNode callNode,
        @Cached("expectedType.getConstructors().get(prototype.getName())") AtomConstructor c) {
      if (c == null) {
        return checkSingleton(expectedType, unresolved);
      } else {
        return invokeConstructor(c, prototype, unresolved, state, callNode);
      }
    }

    @Specialization(replaces = "instantiateCached")
    @CompilerDirectives.TruffleBoundary
    Object instantiateUncached(
        MaterializedFrame frame, State state, Type expectedType, UnresolvedConstructor unresolved) {
      var c = expectedType.getConstructors().get(unresolved.getName());
      if (c == null) {
        return checkSingleton(expectedType, unresolved);
      }
      var callNode = buildApplication(unresolved);
      return invokeConstructor(c, unresolved.asPrototype(), unresolved, state, callNode);
    }

    private Object invokeConstructor(
        AtomConstructor c,
        UnresolvedConstructor prototype,
        UnresolvedConstructor unresolved,
        State state,
        DirectCallNode callNode) {
      var builtins = EnsoContext.get(callNode).getBuiltins();
      if (c == builtins.bool().getTrue()) {
        return true;
      }
      if (c == builtins.bool().getFalse()) {
        return false;
      }
      var fn = c.getConstructorFunction();
      var args = new Object[prototype.descs.length + 1];
      System.arraycopy(unresolved.args, 0, args, 1, prototype.descs.length);
      args[0] = fn;
      var helper = Function.ArgumentsHelper.buildArguments(fn, null, state, args);
      var r = callNode.call(helper);
      if (r instanceof Atom) {
        return r;
      } else if (r instanceof DataflowError) {
        return r;
      } else {
        var ctx = EnsoContext.get(this);
        var err = ctx.getBuiltins().error().makeTypeError(c.getType(), r, prototype.toString());
        throw new PanicException(err, this);
      }
    }

    private static Object checkSingleton(Type c, UnresolvedConstructor unresolved) {
      if (!c.isEigenType()) {
        return null;
      } else {
        return equalTypeAndConstructorName(c, unresolved) ? c : null;
      }
    }

    @CompilerDirectives.TruffleBoundary
    private static boolean equalTypeAndConstructorName(Type c, UnresolvedConstructor unresolved) {
      return c.getName().equals(unresolved.getName());
    }
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@Bind("$node") Node node) {
    var ctx = EnsoContext.get(node);
    return ctx.getBuiltins().function();
  }
}
