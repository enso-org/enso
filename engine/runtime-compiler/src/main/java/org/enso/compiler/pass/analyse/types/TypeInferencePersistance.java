package org.enso.compiler.pass.analyse.types;

import org.enso.persist.Persistable;
import org.enso.pkg.QualifiedName;

@Persistable(clazz = InferredType.class, id = 34000)
@Persistable(clazz = TypeRepresentation.TopType.class, id = 34001)
@Persistable(clazz = TypeRepresentation.TypeObject.class, id = 34002)
@Persistable(clazz = TypeRepresentation.ArrowType.class, id = 34003)
@Persistable(clazz = TypeRepresentation.AtomType.class, id = 34004)
@Persistable(clazz = TypeRepresentation.IntersectionType.class, id = 34005)
@Persistable(clazz = TypeRepresentation.SumType.class, id = 34006)
@Persistable(clazz = TypeRepresentation.UnresolvedSymbol.class, id = 34007)
@Persistable(clazz = QualifiedName.class, id = 34012)
public final class TypeInferencePersistance {
  private TypeInferencePersistance() {}
}
