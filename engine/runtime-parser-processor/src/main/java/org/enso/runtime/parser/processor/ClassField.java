package org.enso.runtime.parser.processor;

import java.util.Objects;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.type.TypeMirror;
import org.enso.runtime.parser.processor.utils.Utils;

/** Declared field in the generated class. */
public final class ClassField {

  private final String modifiers;
  private final TypeMirror type;
  private final String name;
  private final String initializer;
  private final boolean canBeNull;
  private final ProcessingEnvironment procEnv;

  public static Builder builder() {
    return new Builder();
  }

  /**
   * @param modifiers e.g. "private final"
   * @param initializer Initial value of the field. Can be, e.g., {@code "null"}.
   */
  private ClassField(
      String modifiers,
      TypeMirror type,
      String name,
      String initializer,
      boolean canBeNull,
      ProcessingEnvironment procEnv) {
    this.modifiers = modifiers;
    this.type = type;
    this.name = name;
    this.initializer = initializer;
    this.canBeNull = canBeNull;
    this.procEnv = procEnv;
  }

  public String name() {
    return name;
  }

  public String modifiers() {
    return modifiers;
  }

  public TypeMirror getType() {
    return type;
  }

  public String getTypeName() {
    return type.toString();
  }

  public String getSimpleTypeName() {
    return Utils.simpleTypeName(type);
  }

  public boolean isPrimitive() {
    return type.getKind().isPrimitive();
  }

  /**
   * @return May be null. In that case, initializer is unknown. Note that the class field can be
   *     primitive.
   */
  public String initializer() {
    return initializer;
  }

  public boolean canBeNull() {
    return canBeNull;
  }

  @Override
  public String toString() {
    return modifiers + " " + type + " " + name;
  }

  public static final class Builder {
    private TypeMirror type;
    private String name;
    private String modifiers = null;
    private String initializer = null;
    private Boolean canBeNull = null;
    private ProcessingEnvironment procEnv;

    public Builder modifiers(String modifiers) {
      this.modifiers = modifiers;
      return this;
    }

    public Builder type(TypeMirror type) {
      this.type = type;
      return this;
    }

    public Builder name(String name) {
      this.name = name;
      return this;
    }

    public Builder canBeNull(boolean canBeNull) {
      this.canBeNull = canBeNull;
      return this;
    }

    public Builder initializer(String initializer) {
      this.initializer = initializer;
      return this;
    }

    public Builder procEnv(ProcessingEnvironment procEnv) {
      this.procEnv = procEnv;
      return this;
    }

    public ClassField build() {
      Objects.requireNonNull(type);
      Objects.requireNonNull(name);
      Objects.requireNonNull(procEnv);
      Objects.requireNonNull(canBeNull);
      var modifiers = this.modifiers != null ? this.modifiers : "";
      return new ClassField(modifiers, type, name, initializer, canBeNull, procEnv);
    }
  }
}
