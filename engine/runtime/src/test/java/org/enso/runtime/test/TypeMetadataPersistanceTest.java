package org.enso.runtime.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.List;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.analyse.types.AtomTypeInterfaceFromBindingsMap;
import org.enso.compiler.pass.analyse.types.InferredType;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.persist.Persistance;
import org.enso.pkg.QualifiedName;
import org.junit.Test;
import scala.jdk.javaapi.CollectionConverters$;

/**
 * Currently the static type inference pass is optional and it is not computed as part of the cache
 * indexing.
 */
public class TypeMetadataPersistanceTest {
  private static <T> scala.collection.immutable.List<T> makeScalaList(List<T> list) {
    return CollectionConverters$.MODULE$.asScala(list).toList();
  }

  private static <T> T serde(Class<T> clazz, T l) throws IOException {
    var arr = Persistance.write(l, null);
    var ref = Persistance.read(arr, null);
    return ref.get(clazz);
  }

  @Test
  public void writeSomeInferredType() throws Exception {
    var obj = mockObject();
    var type =
        new TypeRepresentation.ArrowType(
            obj,
            new TypeRepresentation.SumType(List.of(TypeRepresentation.ANY, obj.instanceType())));
    var inferredType = new InferredType(type);
    var out = serde(InferredType.class, inferredType);
    assertEquals(inferredType.type(), out.type());
  }

  private TypeRepresentation.TypeObject mockObject() {
    var fqn = new QualifiedName(makeScalaList(List.of("mod")), "Test");
    return new TypeRepresentation.TypeObject(fqn, mockAtomType());
  }

  private AtomTypeInterfaceFromBindingsMap mockAtomType() {
    scala.collection.immutable.List<String> params = makeScalaList(List.of());
    var ctorArgs =
        makeScalaList(
            List.of(new BindingsMap.Argument("arg", false, Persistance.Reference.none())));
    var constructors = makeScalaList(List.of(new BindingsMap.Cons("ctor", ctorArgs, false)));
    return new AtomTypeInterfaceFromBindingsMap(
        new BindingsMap.Type("Test", params, constructors, false));
  }
}
