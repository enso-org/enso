package org.enso.runtime.parser.processor.utils;

import javax.lang.model.element.TypeElement;

/**
 * A visitor for traversing the interface hierarchy of an interface - it iterates over all the super
 * interfaces until it encounters {@code org.enso.compiler.ir.IR} interface. The iteration can be
 * stopped by returning a non-null value from the visitor. Follows a similar pattern as {@link
 * com.oracle.truffle.api.frame.FrameInstanceVisitor}.
 */
@FunctionalInterface
public interface InterfaceHierarchyVisitor<T> {
  /**
   * Visits the interface hierarchy of the given interface.
   *
   * @param iface the interface to visit
   * @return If not-null, the iteration is stopped and the value is returned. If null, the iteration
   *     continues.
   */
  T visitInterface(TypeElement iface);
}
