package org.enso.runtime.parser.processor;

import java.util.List;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import org.enso.runtime.parser.dsl.GenerateIR;

/**
 * Represents a class annotated with {@link org.enso.runtime.parser.dsl.GenerateIR} that is
 * currently being processed by the {@link IRProcessor}.
 */
public final class ProcessedClass {
  private final TypeElement clazz;
  private final ExecutableElement ctor;
  private final TypeElement irInterfaceElem;
  private final List<TypeElement> interfaces;

  /**
   * @param clazz Class being processed by the processor, annotated with {@link GenerateIR}
   * @param ctor Constructor annotated with {@link org.enso.runtime.parser.dsl.GenerateFields}.
   * @param irInterfaceElem Interface that the generated superclass must implement. Must be subtype
   *     of {@code org.enso.compiler.core.IR}.
   * @param interfaces All interfaces to implement. See {@link GenerateIR#interfaces()}.
   */
  ProcessedClass(
      TypeElement clazz,
      ExecutableElement ctor,
      TypeElement irInterfaceElem,
      List<TypeElement> interfaces) {
    this.clazz = clazz;
    this.ctor = ctor;
    this.irInterfaceElem = irInterfaceElem;
    this.interfaces = interfaces;
  }

  public TypeElement getClazz() {
    return clazz;
  }

  public ExecutableElement getCtor() {
    return ctor;
  }

  /**
   * Returns the interface that the generated superclass must implement. Is a subtype of {@code
   * org.enso.compiler.core.IR}.
   */
  public TypeElement getIrInterfaceElem() {
    return irInterfaceElem;
  }

  /**
   * Returns all interfaces that the generated superclass must implement. See {@link
   * GenerateIR#interfaces()}.
   */
  public List<TypeElement> getInterfaces() {
    return interfaces;
  }
}
