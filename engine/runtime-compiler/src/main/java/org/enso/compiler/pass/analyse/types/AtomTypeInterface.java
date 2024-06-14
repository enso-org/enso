package org.enso.compiler.pass.analyse.types;

import java.util.List;

/**
 * Describes the visible interface of an Atom Type.
 *
 * <p>This interface declares what methods can be called on instances of that type (or statically)
 * and what constructors may be called on it to create new instances.
 */
interface AtomTypeInterface {
  List<? extends Constructor> constructors();

  interface Constructor {
    String name();

    List<? extends Argument> arguments();
  }

  interface Argument {
    String name();

    boolean hasDefaultValue();

    /**
     * The ascribed type of the argument.
     *
     * <p>It may be {@code null} if the type is not known.
     */
    TypeRepresentation getType(TypeResolver resolver);
  }

  // TODO in next iteration: static and member methods
}
