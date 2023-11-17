package org.enso.persistance;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Target;

/**
 * Annotation for an automatic persistance of a class. Use to generate implementation and
 * registration of {@link Persistance} subclass to read and write simple records and case classes:
 *
 * <p>{@snippet file="org/enso/persistance/PersistanceTest.java" region="annotation"}
 */
@Target(ElementType.TYPE)
@Repeatable(Persistable.Group.class)
public @interface Persistable {
  Class<?> clazz();

  int id();

  @Target(ElementType.TYPE)
  public @interface Group {
    Persistable[] value();
  }
}
