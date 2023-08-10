package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.atom.unboxing.Layout;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * A node instantiating a constant {@link AtomConstructor} with values computed based on the
 * children nodes.
 */
@NodeInfo(shortName = "Instantiate", description = "Instantiates a constant Atom constructor")
public abstract class InstantiateNode extends ExpressionNode {
  final AtomConstructor constructor;
  private @Children ExpressionNode[] arguments;
  private @Child WarningsLibrary warnings = WarningsLibrary.getFactory().createDispatched(3);
  private @CompilationFinal(dimensions = 1) CountingConditionProfile[] profiles;
  private @CompilationFinal(dimensions = 1) CountingConditionProfile[] warningProfiles;
  private @CompilationFinal(dimensions = 1) BranchProfile[] sentinelProfiles;
  private final CountingConditionProfile anyWarningsProfile = CountingConditionProfile.create();

  InstantiateNode(AtomConstructor constructor, ExpressionNode[] arguments) {
    this.constructor = constructor;
    this.arguments = arguments;
    this.profiles = new CountingConditionProfile[arguments.length];
    this.sentinelProfiles = new BranchProfile[arguments.length];
    this.warningProfiles = new CountingConditionProfile[arguments.length];
    for (int i = 0; i < arguments.length; ++i) {
      this.profiles[i] = CountingConditionProfile.create();
      this.sentinelProfiles[i] = BranchProfile.create();
      this.warningProfiles[i] = CountingConditionProfile.create();
    }
  }

  /**
   * Creates an instance of this node.
   *
   * @param constructor the {@link AtomConstructor} this node will be instantiating
   * @param arguments the expressions that produce field values
   * @return a node that instantiates {@code constructor}
   */
  public static InstantiateNode build(AtomConstructor constructor, ExpressionNode[] arguments) {
    return InstantiateNodeGen.create(constructor, arguments);
  }

  /**
   * Executes the node, by executing all its children and putting their values as fields of the
   * newly created {@link AtomConstructor} instance.
   *
   * @param frame the stack frame for execution
   * @return the newly created {@link AtomConstructor} instance.
   */
  @Specialization
  @ExplodeLoop
  Object doExecute(
      VirtualFrame frame,
      @Cached(parameters = {"constructor"}) CreateInstanceNode createInstanceNode) {
    Object[] argumentValues = new Object[arguments.length];
    boolean anyWarnings = false;
    ArrayRope<Warning> accumulatedWarnings = new ArrayRope<>();
    for (int i = 0; i < arguments.length; i++) {
      CountingConditionProfile profile = profiles[i];
      CountingConditionProfile warningProfile = warningProfiles[i];
      BranchProfile sentinelProfile = sentinelProfiles[i];
      Object argument = arguments[i].executeGeneric(frame);
      if (profile.profile(TypesGen.isDataflowError(argument))) {
        return argument;
      } else if (warningProfile.profile(warnings.hasWarnings(argument))) {
        anyWarnings = true;
        try {
          accumulatedWarnings = accumulatedWarnings.append(warnings.getWarnings(argument, this));
          argumentValues[i] = warnings.removeWarnings(argument);
        } catch (UnsupportedMessageException e) {
          throw new IllegalStateException(e);
        }
      } else if (TypesGen.isPanicSentinel(argument)) {
        sentinelProfile.enter();
        throw TypesGen.asPanicSentinel(argument);
      } else {
        argumentValues[i] = argument;
      }
    }
    if (anyWarningsProfile.profile(anyWarnings)) {
      return WithWarnings.appendTo(
          EnsoContext.get(this), createInstanceNode.execute(argumentValues), accumulatedWarnings);
    } else {
      return createInstanceNode.execute(argumentValues);
    }
  }

  @ReportPolymorphism
  public abstract static class CreateInstanceNode extends Node {
    @NeverDefault
    static CreateInstanceNode create(AtomConstructor constructor) {
      if (Layout.isAritySupported(constructor.getArity())) {
        return Layout.CreateUnboxedInstanceNode.create(constructor);
      } else {
        return FallbackCreateInstanceNode.create(constructor);
      }
    }

    public abstract Object execute(Object[] arguments);
  }

  static class FallbackCreateInstanceNode extends CreateInstanceNode {
    private final AtomConstructor constructor;

    public static CreateInstanceNode create(AtomConstructor constructor) {
      return new FallbackCreateInstanceNode(constructor);
    }

    FallbackCreateInstanceNode(AtomConstructor constructor) {
      this.constructor = constructor;
    }

    @Override
    public Object execute(Object[] arguments) {
      return constructor.newInstance(arguments);
    }
  }
}
