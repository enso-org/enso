package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.data.BindingsMap;
import org.enso.pkg.QualifiedName;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public sealed interface TypeRepresentation
    permits TypeRepresentation.ArrowType,
    TypeRepresentation.AtomType,
    TypeRepresentation.IntersectionType,
    TypeRepresentation.SumType,
    TypeRepresentation.TopType,
    TypeRepresentation.TypeObject,
    TypeRepresentation.UnresolvedSymbol {
  TypeRepresentation ANY = new TopType();
  // In the future we may want to split this unknown type to be a separate entity.
  TypeRepresentation UNKNOWN = ANY;

  static TypeRepresentation buildSimplifiedSumType(List<TypeRepresentation> types) {
    var simplifier = new SumTypeSimplifier();
    types.forEach(simplifier::traverse);
    return simplifier.build();
  }

  static TypeRepresentation buildFunction(
      List<TypeRepresentation> arguments, TypeRepresentation result) {
    var reversed = new ArrayList<>(arguments);
    Collections.reverse(reversed);
    return reversed.stream().reduce(result, (acc, arg) -> new ArrowType(arg, acc));
  }

  record TopType() implements TypeRepresentation {
    @Override
    public String toString() {
      return "Any";
    }
  }

  /**
   * Represents the type that is associated with values (atoms) of a given type.
   * <p>
   * Instances that are assigned this type are built with one of the available constructors, but statically we do not necessarily know which one.
   */
  record AtomType(QualifiedName fqn, BindingsMap.Type typeDescription) implements TypeRepresentation {
    @Override
    public String toString() {
      return fqn.item();
    }

    @Override
    public boolean equals(Object obj) {
      if (obj instanceof AtomType atomType) {
        return fqn.equals(atomType.fqn);
      } else {
        return false;
      }
    }

    @Override
    public int hashCode() {
      return fqn.hashCode() * 31;
    }
  }

  record ArrowType(TypeRepresentation argType, TypeRepresentation resultType)
      implements TypeRepresentation {
    @Override
    public String toString() {
      return "(" + argType + " -> " + resultType + ")";
    }
  }

  record SumType(List<TypeRepresentation> types) implements TypeRepresentation {
    public SumType {
      if (types.size() < 2) {
        throw new IllegalArgumentException("Sum type must have at least 2 types");
      }
    }

    @Override
    public String toString() {
      String repr =
          types.stream()
              .map(TypeRepresentation::toString)
              .reduce((a, b) -> a + " | " + b)
              .orElse("");
      return "(" + repr + ")";
    }

    private Set<TypeRepresentation> asSet() {
      return new HashSet<>(types);
    }

    @Override
    public boolean equals(Object obj) {
      if (obj instanceof SumType sumType) {
        return asSet().equals(sumType.asSet());
      } else {
        return false;
      }
    }

    @Override
    public int hashCode() {
      return asSet().hashCode();
    }
  }

  record IntersectionType(List<TypeRepresentation> types) implements TypeRepresentation {
    public IntersectionType {
      if (types.size() < 2) {
        throw new IllegalArgumentException("Compound type must have at least 2 types");
      }
    }

    @Override
    public String toString() {
      String repr =
          types.stream()
              .map(TypeRepresentation::toString)
              .reduce((a, b) -> a + " & " + b)
              .orElse("");
      return "(" + repr + ")";
    }
  }

  /**
   * Represents a type object, i.e. an object that is an instance of a type's identity.
   *
   * <p>This object allows to call static methods on that type or create instances of this type
   * using its constructors, which will be assigned the corresponding AtomType.
   *
   * @param name            the qualified name of the type
   * @param typeDescription the type description from the BindingsMap
   */
  record TypeObject(QualifiedName name, BindingsMap.Type typeDescription) implements TypeRepresentation {
    @Override
    public String toString() {
      return "(type " + name.item() + ")";
    }

    /**
     * Creates a TypeRepresentation representing a constructed instance of the type represented by
     * this TypeObject.
     */
    public TypeRepresentation instantiate() {
      return new AtomType(name, typeDescription);
    }

    @Override
    public boolean equals(Object obj) {
      if (obj instanceof TypeObject typeObject) {
        return name.equals(typeObject.name);
      } else {
        return false;
      }
    }

    @Override
    public int hashCode() {
      return name.hashCode() * 97;
    }
  }

  record UnresolvedSymbol(String name) implements TypeRepresentation {
    @Override
    public String toString() {
      return "UnresolvedSymbol<" + name + ">";
    }
  }
}
