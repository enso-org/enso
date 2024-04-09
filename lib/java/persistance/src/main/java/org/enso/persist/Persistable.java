package org.enso.persist;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Target;

/**
 * Annotation for an automatic persistance of a class. Use to generate implementation and
 * registration of {@link Persistance} subclass to read and write simple records and case classes:
 *
 * <p>{@snippet file="org/enso/persist/PersistanceTest.java" region="annotation"}
 */
@Target(ElementType.TYPE)
@Repeatable(Persistable.Group.class)
public @interface Persistable {

  /**
   * The class to generate {@link Persistance} for. If the value is omitted then the code is
   * generated for the class that is annotated by this annotation. Example of multiple
   * {@code @Persistable} annotations on a single element.
   *
   * <p>{@snippet file="org/enso/persist/PersistanceTest.java" region="annotation"}
   *
   * <p>Example of self annotated class:
   *
   * <p>{@snippet file="org/enso/persist/PersistanceTest.java" region="self-annotation"}
   *
   * @return the class to generate read/write persistance code for
   */
  Class<?> clazz() default Object.class;

  /**
   * ID of the class. Each registered {@link Persistance} sub class must have a unique ID. This
   * attribute specifies it for the generated code. When serialization format changes (for example
   * by changing the number or type of constructor arguments), change also the ID.
   *
   * @return unique ID for the persisted {@link #clazz()}
   */
  int id();

  /**
   * Should the generated code use {@link Persistance.Output#writeInline(Class<T>, T)} or not. By
   * default all {@code final} or <em>sealed</em> classes are inlined. Inlining is however not very
   * helpful when a single object is shared between multiple other objects.
   *
   * @return
   */
  boolean allowInlining() default true;

  /** Multiple {@link Persistable} annotations. */
  @Target(ElementType.TYPE)
  public @interface Group {
    /**
     * The array of {@link Persistable} annotations.
     *
     * @return the array of annotations.
     */
    Persistable[] value();
  }
}
