package org.enso.persistance;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Target;

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
