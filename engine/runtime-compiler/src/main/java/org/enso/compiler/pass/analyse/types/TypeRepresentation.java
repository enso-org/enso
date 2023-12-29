package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.data.BindingsMap;
import org.enso.pkg.QualifiedName;
import org.enso.pkg.QualifiedName$;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public sealed interface TypeRepresentation
    permits TypeRepresentation.ArrowType, TypeRepresentation.AtomType, TypeRepresentation.IntersectionType, TypeRepresentation.SumType, TypeRepresentation.TopType, TypeRepresentation.TypeObject, TypeRepresentation.UnresolvedSymbol {
  record TopType() implements TypeRepresentation {
    @Override
    public String toString() {
      return "Any";
    }
  }

  record AtomType(QualifiedName fqn) implements TypeRepresentation {
    @Override
    public String toString() {
      return fqn.item();
    }
  }

  record ArrowType(TypeRepresentation argType, TypeRepresentation resultType) implements TypeRepresentation {
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
      String repr = types.stream().map(TypeRepresentation::toString).reduce((a, b) -> a + " | " + b).orElse("");
      return "(" + repr + ")";
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
      String repr = types.stream().map(TypeRepresentation::toString).reduce((a, b) -> a + " & " + b).orElse("");
      return "(" + repr + ")";
    }
  }

  /**
   * Represents a type object, i.e. an object that is an instance of a type's identity.
   * <p>
   * This object allows to call static methods on that type or create instances of this type using its constructors.
   * <p>
   * TODO I'm not sure if storing BindingsMap.Type here is the best idea, later we may want to reduce coupling; however for now, I'm just trying to keep it simple to make the PoC work.
   *
   * @param name the qualified name of the type
   * @param shape the type that this type object represents
   */
  record TypeObject(QualifiedName name, BindingsMap.Type shape) implements TypeRepresentation {
    @Override
    public String toString() {
      return "(type " + name.item() + ")";
    }

    /** Creates a TypeRepresentation representing a constructed instance of the type represented by this TypeObject. */
    public TypeRepresentation instantiate() {
      return fromQualifiedName(name);
    }
  }

  record UnresolvedSymbol(String name) implements TypeRepresentation {
    @Override
    public String toString() {
      return "UnresolvedSymbol<" + name + ">";
    }
  }

  static TypeRepresentation buildFunction(List<TypeRepresentation> arguments, TypeRepresentation result) {
    var reversed = new ArrayList<>(arguments);
    Collections.reverse(reversed);
    return reversed.stream().reduce(result, (acc, arg) -> new ArrowType(arg, acc));
  }

  TypeRepresentation INTEGER = fromQualifiedName("Standard.Base.Data.Numbers.Integer");
  TypeRepresentation FLOAT = fromQualifiedName("Standard.Base.Data.Numbers.Float");
  TypeRepresentation TEXT = fromQualifiedName("Standard.Base.Data.Text.Text");
  TypeRepresentation ANY = new TypeRepresentation.TopType();

  // In the future we may want to split this unknown type to be a separate entity.
  TypeRepresentation UNKNOWN = ANY;
  TypeRepresentation NOTHING = fromQualifiedName("Standard.Base.Nothing.Nothing");

  static TypeRepresentation fromQualifiedName(QualifiedName fqn) {
    String str = fqn.toString();
    if (str.equals("Standard.Base.Any.Any") || str.equals("Standard.Base.Any")) return ANY;

    return new TypeRepresentation.AtomType(fqn);
  }

  static TypeRepresentation fromQualifiedName(String fqn) {
    QualifiedName qualifiedName = QualifiedName$.MODULE$.fromString(fqn);
    return fromQualifiedName(qualifiedName);
  }
}
