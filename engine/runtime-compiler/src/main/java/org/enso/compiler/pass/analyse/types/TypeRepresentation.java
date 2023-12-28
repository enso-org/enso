package org.enso.compiler.pass.analyse.types;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public sealed interface TypeRepresentation {
  record TopType() implements TypeRepresentation {}
  record AtomType(String fqn) implements TypeRepresentation {}
  record ArrowType(TypeRepresentation argType, TypeRepresentation resultType) implements TypeRepresentation {}

  static TypeRepresentation buildFunction(List<TypeRepresentation> arguments, TypeRepresentation result) {
    var reversed = new ArrayList<>(arguments);
    Collections.reverse(reversed);
    return reversed.stream().reduce(result, (acc, arg) -> new ArrowType(arg, acc));
  }

  TypeRepresentation INTEGER = new TypeRepresentation.AtomType("Standard.Base.Data.Numbers.Integer");
  TypeRepresentation FLOAT = new TypeRepresentation.AtomType("Standard.Base.Data.Numbers.Float");
  TypeRepresentation TEXT = new TypeRepresentation.AtomType("Standard.Base.Data.Text.Text");
  TypeRepresentation ANY = new TypeRepresentation.TopType();
  TypeRepresentation NOTHING = new TypeRepresentation.AtomType("Standard.Base.Nothing.Nothing");
}
