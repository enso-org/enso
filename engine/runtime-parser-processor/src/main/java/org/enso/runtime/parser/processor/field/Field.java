package org.enso.runtime.parser.processor.field;

import java.util.List;
import java.util.function.Function;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.processor.IRProcessingException;
import org.enso.runtime.parser.processor.utils.Utils;

/** Represents a field in the generated super class. */
public abstract class Field {
  protected final TypeMirror type;
  protected final String name;
  private final ProcessingEnvironment procEnv;

  protected Field(TypeMirror type, String name, ProcessingEnvironment procEnv) {
    this.type = type;
    this.name = name;
    this.procEnv = procEnv;
  }

  /** Name (identifier) of the field. */
  public String getName() {
    return name;
  }

  /** Returns type of this field. Must not be null. */
  public TypeMirror getType() {
    return type;
  }

  /**
   * Does not return null. If the type is generic, the type parameter is included in the name.
   * Returns non-qualified name.
   */
  public String getSimpleTypeName() {
    return Utils.simpleTypeName(type);
  }

  /**
   * Returns true if this field is annotated with {@link org.enso.runtime.parser.dsl.IRChild}.
   *
   * @return
   */
  public abstract boolean isChild();

  /**
   * Returns true if this field is child with {@link IRChild#required()} set to false.
   *
   * @return
   */
  public abstract boolean isNullable();

  /**
   * Returns list of (fully-qualified) types that are necessary to import in order to use simple
   * type names.
   */
  public List<String> getImportedTypes() {
    return Utils.getImportedTypes(type);
  }

  /** Returns true if this field is a scala immutable list. */
  public boolean isList() {
    return Utils.isScalaList(type, procEnv);
  }

  /** Returns true if this field is {@code scala.Option}. */
  public boolean isOption() {
    return Utils.isScalaOption(type, procEnv);
  }

  /** Returns true if the type of this field is Java primitive. */
  public boolean isPrimitive() {
    return type.getKind().isPrimitive();
  }

  /**
   * Returns true if this field extends {@link org.enso.compiler.core.ir.Expression}.
   *
   * <p>This is useful, e.g., for the {@link org.enso.compiler.core.IR#mapExpressions(Function)}
   * method.
   *
   * @return true if this field extends {@link org.enso.compiler.core.ir.Expression}
   */
  public boolean isExpression() {
    return Utils.isSubtypeOfExpression(type, procEnv);
  }

  /** Returns the type parameter, if this field is a generic type. Otherwise null. */
  public TypeElement getTypeParameter() {
    if (type.getKind() == TypeKind.DECLARED) {
      var declared = (DeclaredType) type;
      var typeArgs = declared.getTypeArguments();
      if (typeArgs.isEmpty()) {
        return null;
      } else if (typeArgs.size() == 1) {
        var typeArg = typeArgs.get(0);
        return (TypeElement) procEnv.getTypeUtils().asElement(typeArg);
      } else {
        throw new IRProcessingException(
            "Unexpected number of type arguments: " + typeArgs.size(), null);
      }
    }
    return null;
  }
}
