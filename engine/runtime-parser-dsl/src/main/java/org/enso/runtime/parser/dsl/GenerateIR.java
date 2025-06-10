package org.enso.runtime.parser.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * A class annotated with this annotation will be processed by the IR processor. The processor will
 * generate a super class from the {@code extends} clause of the annotated class. If the annotated
 * class does not have {@code extends} clause, an error is generated. Moreover, if the class in the
 * {@code extends} clause already exists, an error is generated.
 *
 * <p>The generated class will have the same package as the annotated class. Majority of the methods
 * in the generated class will be either private or package-private, so that they are not accessible
 * from the outside.
 *
 * <p>The class can be enclosed (nested inside) an interface.
 *
 * <p>The class must contain a single constructor annotated with {@link GenerateFields}.
 */
@Retention(RetentionPolicy.SOURCE)
@Target(ElementType.TYPE)
public @interface GenerateIR {

  /**
   * Interfaces that the generated superclass will implement. The list of the interfaces will simply
   * be put inside the {@code implements} clause of the generated class. All the generated classes
   * implement {@code org.enso.compiler.core.IR} by default.
   */
  Class[] interfaces() default {};
}
