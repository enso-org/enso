package org.enso.compiler.pass.analyse.types;

import java.util.ArrayList;
import java.util.HashSet;

/**
 * Computes the effective form of a sum of types.
 *
 * <p>If the sum contains the `Any` type, e.g. `A | Any`, it can be simplified to just `Any`.
 * Additionally, redundant parts are deduplicated - e.g `A | A` can be simplified to just `A`.
 */
class SumTypeSimplifier {
  public static TypeRepresentation simplifySumOfTypes(Iterable<TypeRepresentation> types) {
    var simplifier = new SumTypeSimplifier();
    types.forEach(simplifier::traverse);
    return simplifier.build();
  }

  private SumTypeSimplifier() {}

  private final HashSet<TypeRepresentation> parts = new HashSet<>();
  boolean hasAny = false;

  /**
   * Adds the given type (and recursively visits its constituents if it's a complex type) to the
   * sum.
   */
  private void traverse(TypeRepresentation type) {
    switch (type) {
      case TypeRepresentation.SumType sumType -> {
        for (TypeRepresentation t : sumType.types()) {
          traverse(t);
        }
      }
      case TypeRepresentation.TopType any -> hasAny = true;
      default -> parts.add(type);
    }
  }

  private TypeRepresentation build() {
    if (hasAny) {
      return TypeRepresentation.ANY;
    } else if (parts.isEmpty()) {
      throw new IllegalStateException(
          "SumTypeSimplifier: build may be called after at least one traverse call.");
    } else if (parts.size() == 1) {
      return parts.iterator().next();
    } else {
      return new TypeRepresentation.SumType(new ArrayList<>(parts));
    }
  }
}
