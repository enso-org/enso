package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import java.util.function.Supplier;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;

@ReportPolymorphism
@NodeInfo(shortName = "Method", description = "A root node for Enso methods.")
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
   * @param body the program provider to be executed
   * @param section a mapping from {@code provider} to the program source
   * @param atomConstructor the constructor this method is defined for
   * @param methodName the name of this method
   * @return a node representing the specified closure
   */
  public static MethodRootNode build(
      Language language,
      LocalScope localScope,
      ModuleScope moduleScope,
      Supplier<ExpressionNode> body,
      SourceSection section,
      AtomConstructor atomConstructor,
      String methodName) {
    return build(
        language,
        localScope,
        moduleScope,
        new LazyBodyNode(body),
        section,
        atomConstructor,
        methodName);
  }

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

  /**
   * Computes the fully qualified name of this method.
   *
   * <p>The name has a form of [method's module]::[qualified type name]::[method name].
   *
   * @return the qualified name of this method.
   */
  @Override
  public String getQualifiedName() {
    return getModuleScope().getModule().getName().toString()
        + "::"
        + atomConstructor.getQualifiedName().toString()
        + "::"
        + methodName;
  }

  /** @return the constructor this method was defined for */
  public AtomConstructor getAtomConstructor() {
    return atomConstructor;
  }

  /** @return the method name */
  public String getMethodName() {
    return methodName;
  }

  @Override
  public Node deepCopy() {
    LazyBodyNode.replaceLazyNode(getBody());
    return super.deepCopy();
  }

  @Override
  public Node copy() {
    LazyBodyNode.replaceLazyNode(getBody());
    return super.copy();
  }

  private static class LazyBodyNode extends ExpressionNode {
    private final Supplier<ExpressionNode> provider;

    LazyBodyNode(Supplier<ExpressionNode> body) {
      this.provider = body;
    }

    static void replaceLazyNode(Node n) {
      if (n instanceof LazyBodyNode lazy) {
        lazy.replaceItself();
      }
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
      ExpressionNode newNode = replaceItself();
      return newNode.executeGeneric(frame);
    }

    final ExpressionNode replaceItself() {
        ExpressionNode newNode = replace(provider.get());
        notifyInserted(newNode);
        return newNode;
    }
  }
}
