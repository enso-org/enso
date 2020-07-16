package org.enso.interpreter.node;

import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.Stateful;

/**
 * This node represents the root of Enso closures and closure-like structures.
 *
 * <p>All new computations in Enso must be executed from within an {@link MethodRootNode}, as
 * determined by the API provided by Truffle.
 */
@ReportPolymorphism
@NodeInfo(shortName = "Closure", description = "A root node for Enso closures.")
public class MethodRootNode extends ClosureRootNode {

  private final AtomConstructor atomConstructor;
  private final String methodName;

  private MethodRootNode(
      Language language,
      LocalScope localScope,
      ModuleScope moduleScope,
      ExpressionNode body,
      SourceSection section,
      AtomConstructor atomConstructor,
      String methodName) {
    super(
        language,
        localScope,
        moduleScope,
        body,
        section,
        shortName(atomConstructor.getName(), methodName));
    this.atomConstructor = atomConstructor;
    this.methodName = methodName;
  }

  private static String shortName(String atomName, String methodName) {
    return atomName + "." + methodName;
  }

  /**
   * Creates an instance of this node.
   *
   * @param language the language identifier
   * @param localScope a description of the local scope
   * @param moduleScope a description of the module scope
   * @param body the program body to be executed
   * @param section a mapping from {@code body} to the program source
   * @param name a name for the node
   * @return a node representing the specified closure
   */
  public static MethodRootNode build(
      Language language,
      LocalScope localScope,
      ModuleScope moduleScope,
      ExpressionNode body,
      SourceSection section,
      AtomConstructor atomConstructor,
      String methodName) {
    return new MethodRootNode(
        language, localScope, moduleScope, body, section, atomConstructor, methodName);
  }

  @Override
  public String getQualifiedName() {
    return getModuleScope().getModule().getName().toString()
        + "::"
        + atomConstructor.getQualifiedName().toString()
        + "::"
        + methodName;
  }

  public AtomConstructor getAtomConstructor() {
    return atomConstructor;
  }

  public String getMethodName() {
    return methodName;
  }
}
