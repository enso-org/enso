package org.enso.compiler.pass.analyse.types.scope;

import java.util.List;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;

public final class AtomTypeDefinition {
  private final String name;
  private final List<Constructor> constructors;

  /**
   * Constructs the type definition.
   *
   * @param name The simple name of the type, e.g. the "Name" in `type Name`
   * @param constructors the list of constructor representations
   */
  public AtomTypeDefinition(String name, List<Constructor> constructors) {
    this.name = name;
    this.constructors = constructors;
  }

  /** Returns the short name of the type. */
  public String getName() {
    return name;
  }

  /**
   * Returns the constructor of the type with the given name, or {@code null} if a constructor with
   * that name does not exist.
   */
  public Constructor getConstructor(String name) {
    return constructors.stream().filter(c -> c.name().equals(name)).findFirst().orElse(null);
  }

  /**
   * Represents a constructor of the atom type.
   *
   * @param name the name of the constructor
   * @param isProjectPrivate whether the constructor is project private
   * @param type the type ascribed to the constructor, it may be null if it is unknown TODO the type
   *     will soon be always non-null - once we can handle default arguments
   */
  public record Constructor(String name, boolean isProjectPrivate, TypeRepresentation type) {}
}
