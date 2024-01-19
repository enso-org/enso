package org.enso.compiler.pass.analyse.types;

import java.util.ArrayList;
import java.util.HashSet;

public class SumTypeSimplifier {
  private final HashSet<TypeRepresentation> parts = new HashSet<>();
  boolean hasAny = false;

  public void traverse(TypeRepresentation type) {
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

  public TypeRepresentation build() {
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
