package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.Source;
import java.util.function.Supplier;
import org.enso.compiler.context.LocalScope;
import org.enso.compiler.core.ir.Location;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.scope.ModuleScope;

/**
 * This node represents the root of Enso closures and closure-like structures.
 *
 * <p>All new computations in Enso must be executed from within an {@link ClosureRootNode}, as
 * determined by the API provided by Truffle.
 */
@ReportPolymorphism
@NodeInfo(shortName = "Closure", description = "A root node for Enso closures.")
public class ClosureRootNode extends EnsoRootNode {

  @Child private ExpressionNode body;
  private final boolean subjectToInstrumentation;
  private final boolean usedInBinding;

  ClosureRootNode(
      EnsoLanguage language,
      LocalScope localScope,
      ModuleScope moduleScope,
      ExpressionNode body,
      Supplier<Source> source,
      Location location,
      String name,
      Boolean subjectToInstrumentation,
      boolean usedInBinding) {
    super(language, localScope, moduleScope, name, source, location);
    this.body = body;
    this.subjectToInstrumentation = Boolean.TRUE.equals(subjectToInstrumentation);
    this.usedInBinding = usedInBinding;
  }

  /**
   * Creates an instance of this node.
   *
   * @param language the language identifier
   * @param localScope a description of the local scope
   * @param moduleScope a description of the module scope
   * @param body the program body to be executed
   * @param source lazy provider of the associated source
   * @param location the position in the source or {@code null}
   * @param name a name for the node
   * @param subjectToInstrumentation shall this node be instrumented
   * @param usedInBinding is this node directly used in a variable binding
   * @return a node representing the specified closure
   */
  public static ClosureRootNode build(
      EnsoLanguage language,
      LocalScope localScope,
      ModuleScope moduleScope,
      ExpressionNode body,
      Supplier<Source> source,
      Location location,
      String name,
      Boolean subjectToInstrumentation,
      boolean usedInBinding) {
    return new ClosureRootNode(
        language,
        localScope,
        moduleScope,
        body,
        source,
        location,
        name,
        subjectToInstrumentation,
        usedInBinding);
  }

  /**
   * Executes the node.
   *
   * @param frame the stack frame to execute in
   * @return the result of executing this node
   */
  @Override
  public Object execute(VirtualFrame frame) {
    if (CompilerDirectives.inCompilationRoot() || CompilerDirectives.inInterpreter()) {
      com.oracle.truffle.api.TruffleSafepoint.poll(this);
    }
    return body.executeGeneric(frame);
  }

  final ExpressionNode getBody() {
    return body;
  }

  public boolean isSubjectToInstrumentation() {
    return subjectToInstrumentation;
  }

  public boolean isUsedInBinding() {
    return usedInBinding;
  }
}
