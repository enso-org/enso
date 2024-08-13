package org.enso.compiler.pass.analyse.types;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.enso.pkg.QualifiedName;

public sealed interface TypeRepresentation
    permits TypeRepresentation.ArrowType,
        TypeRepresentation.AtomType,
        TypeRepresentation.IntersectionType,
        TypeRepresentation.ModuleReference,
        TypeRepresentation.SumType,
        TypeRepresentation.TopType,
        TypeRepresentation.TypeObject,
        TypeRepresentation.UnresolvedSymbol {
  TopType ANY = new TopType();

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

    public QualifiedName getAssociatedType() {
      return QualifiedName.fromString(BuiltinTypes.anyQualifiedName);
    }
  }

  /**
   * Represents the type that is associated with values (atoms) of a given type.
   *
   * <p>Instances that are assigned this type are built with one of the available constructors, but
   * statically we do not necessarily know which one.
   */
  record AtomType(QualifiedName fqn, AtomTypeInterface typeInterface)
      implements TypeRepresentation {
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
      String arg = argType.toString();
      String res = resultType.toString();

      // If the inner type is complex (e.g. nested function), wrap it in parentheses.
      if (arg.contains(" ")) {
        arg = "(" + arg + ")";
      }
      if (res.contains(" ")) {
        res = "(" + res + ")";
      }

      return arg + " -> " + res;
    }

    public QualifiedName getAssociatedType() {
      return QualifiedName.fromString(BuiltinTypes.functionQualifiedName);
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
   * @param name the qualified name of the type
   * @param typeInterface the declared interface of the type
   */
  record TypeObject(QualifiedName name, AtomTypeInterface typeInterface)
      implements TypeRepresentation {
    @Override
    public String toString() {
      return "(type " + name.item() + ")";
    }

    /**
     * Creates a TypeRepresentation representing a constructed instance of the type represented by
     * this TypeObject.
     */
    public TypeRepresentation instanceType() {
      if (BuiltinTypes.isAny(name)) {
        return TypeRepresentation.ANY;
      }

      if (BuiltinTypes.isFunction(name)) {
        return new ArrowType(TypeRepresentation.ANY, TypeRepresentation.ANY);
      }

      return new AtomType(name, typeInterface);
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

  /**
   * A type describing a module.
   *
   * <p>This is similar to TypeObject, but one cannot create instances of a module.
   */
  record ModuleReference(QualifiedName name) implements TypeRepresentation {}

  /** Represents a type of an unresolved symbol, like `.Foo` or `.bar`. */
  record UnresolvedSymbol(String name) implements TypeRepresentation {
    @Override
    public String toString() {
      return "UnresolvedSymbol<" + name + ">";
    }
  }
}
