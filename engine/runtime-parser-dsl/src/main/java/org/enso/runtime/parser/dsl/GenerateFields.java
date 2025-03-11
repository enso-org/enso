package org.enso.runtime.parser.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An annotation for a constructor. Parameters of the constructor annotated with this annotation
 * will be scanned by the IR processor and <b>fields</b> will be generated for them. There can be
 * only a single constructor with this annotation in a class. The enclosing class must be annotated
 * with {@link GenerateIR}.
 *
 * <h2>Fields</h2>
 *
 * <p>The generated class will contain 4 <b>meta</b> fields that are required to be present inside
 * every IR element:
 *
 * <ul>
 *   <li>{@code private DiagnosticStorage diagnostics}
 *   <li>{@code private MetadataStorage passData}
 *   <li>{@code private IdentifiedLocation location}
 *   <li>{@code private UUID id}
 * </ul>
 *
 * <p>Apart from these <b>meta</b> fields, the generated class will also contain <b>user-defined</b>
 * fields. User-defined fields are inferred from all the parameters of the constructor annotated
 * with {@link GenerateFields}. The parameter of the constructor can be one of the following:
 *
 * <ul>
 *   <li>Any reference, or primitive type annotated with {@link IRField}
 *   <li>A subtype of {@code org.enso.compiler.ir.IR} annotated with {@link IRChild}
 *   <li>One of the <emph>meta</emph> types mentioned above
 * </ul>
 *
 * <p>For every user-defined field, there will be a getter generated in the superclass. The getter
 * has the same name as the field, and will be public. The body of the getter depends on the
 * parameter type. See {@link IRChild}.
 *
 * <p>A user-defined field generated out of constructor parameter annotated with {@link IRChild} is
 * a child element of this IR element. That means that it will be included in generated
 * implementation of IR methods that iterate over the IR tree. For example {@code mapExpressions} or
 * {@code children}.
 *
 * <p>A user-defined field generated out of constructor parameter annotated with {@link IRField}
 * will not be part of the IR tree traversal methods.
 *
 * <p>For a constructor parameter of a meta type, there will be no user-defined field generated, as
 * the meta fields are always generated.
 *
 * <p>Other types of constructor parameters are forbidden.
 *
 * <p>For examples, see the tests in {@code
 * org.enso.runtime.parser.processor.test.TestIRProcessorInline}.
 */
@Retention(RetentionPolicy.SOURCE)
@Target(ElementType.CONSTRUCTOR)
public @interface GenerateFields {}
