package org.enso.compiler.core.ir;

import java.util.function.Function;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;
import scala.collection.immutable.List;

public interface Pattern extends IR {

  @Override
  Pattern mapExpressions(Function<Expression, Expression> fn);

  @Override
  Pattern setLocation(Option<IdentifiedLocation> location);

  @Override
  Pattern duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  /**
   * A named pattern.
   *
   * <p>Named patterns take the form of a single identifier (e.g. `a` or `_`). As a result they can
   * be used to represent a catch all pattern (e.g. `_ -> ...` or `a -> ...`).
   */
  @GenerateIR(interfaces = {Pattern.class})
  final class Name extends PatternNameGen {
    /**
     * @param name the name that constitutes the pattern
     */
    @GenerateFields
    public Name(
        @IRChild org.enso.compiler.core.ir.Name name,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(name, identifiedLocation, passData);
    }

    public static Name create(org.enso.compiler.core.ir.Name name) {
      return new Builder().name(name).build();
    }

    public Name copyWithName(org.enso.compiler.core.ir.Name newName) {
      return new Builder(this).name(newName).build();
    }

    @Override
    public String showCode(int indent) {
      return name().showCode(indent);
    }
  }

  /**
   * A pattern that destructures a constructor application.
   *
   * <p>The first part of the pattern must be a referent name. The fields of the constructor may be
   * any available kind of pattern.
   */
  @GenerateIR(interfaces = {Pattern.class})
  final class Constructor extends PatternConstructorGen {

    /**
     * @param constructor the constructor being matched on
     * @param fields the asserted fields of the constructor
     */
    @GenerateFields
    public Constructor(
        @IRChild org.enso.compiler.core.ir.Name constructor,
        @IRChild List<Pattern> fields,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(constructor, fields, identifiedLocation, passData);
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    public Constructor copyWithFields(List<Pattern> newFields) {
      return new Builder(this).fields(newFields).build();
    }

    public Constructor copyWithName(org.enso.compiler.core.ir.Name newName) {
      return new Builder(this).constructor(newName).build();
    }

    @Override
    public String showCode(int indent) {
      var fieldsStr = fields().map(field -> "(" + field.showCode(indent) + ")").mkString(" ");
      return constructor().name() + " " + fieldsStr;
    }

    /**
     * Checks if the constructor pattern has been desugared.
     *
     * <p>A constructor pattern has been desugared if all of its fields are {@link Name}.
     *
     * @return `true` if the pattern has been desugared, `false` otherwise
     */
    public boolean isDesugared() {
      return fields()
          .forall(
              field ->
                  switch (field) {
                    case Constructor _ -> false;
                    case Literal _, Type _, Name _ -> true;
                    case Documentation _ ->
                        throw new CompilerError(
                            "Branch documentation should not be present "
                                + "inside a constructor pattern.");
                    case org.enso.compiler.core.ir.expression.errors.Pattern _ -> true;
                    default -> throw new CompilerError("Unexpected value: " + field);
                  });
    }

    /**
     * Unsafely gets the pattern's fields as if they are {@link Name}.
     *
     * @return the fields from `this`
     */
    public List<Name> unsafeFieldsAsNamed() {
      return fields().map(field -> (Name) field);
    }
  }

  /**
   * A literal pattern.
   *
   * <p>A literal pattern matches on constants.
   */
  @GenerateIR(interfaces = {Pattern.class})
  final class Literal extends PatternLiteralGen {
    /**
     * @param literal the literal representing the pattern.
     */
    @GenerateFields
    public Literal(
        @IRChild org.enso.compiler.core.ir.Literal literal,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(literal, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      return literal().showCode(indent);
    }
  }

  /** True/False pattern. */
  @GenerateIR(interfaces = {Pattern.class})
  final class Bool extends PatternBoolGen {
    /**
     * @param condition the boolean value to check for
     */
    @GenerateFields
    public Bool(
        @IRField boolean condition,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(condition, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      return " ".repeat(indent) + condition();
    }
  }

  /**
   * A type pattern.
   *
   * <p>A type pattern matches on types. Type pattern is composed of two parts: - a single
   * identifier (e.g. `a` or `_`) - a (potentially fully qualified) type name E.g., `a : Foo -> ...`
   * or `_ : Bar -> ...`
   */
  @GenerateIR(interfaces = {Pattern.class})
  final class Type extends PatternTypeGen {
    /**
     * @param name the name of the bound variable, or wildcard
     * @param tpe the name of the type to match on
     */
    @GenerateFields
    public Type(
        @IRChild org.enso.compiler.core.ir.Name name,
        @IRChild org.enso.compiler.core.ir.Name tpe,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(name, tpe, identifiedLocation, passData);
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @Override
    public String showCode(int indent) {
      return name().showCode(indent) + " : " + tpe().showCode(indent);
    }
  }

  /**
   * A dummy pattern used for storing documentation comments between branches in a pattern match.
   *
   * <p>To store a documentation comment next to a branch, a dummy branch is created with its
   * pattern being an instance of this Doc and expression being empty.
   */
  @GenerateIR(interfaces = {Pattern.class})
  final class Documentation extends PatternDocumentationGen {
    /**
     * @param doc the documentation entity.
     */
    @GenerateFields
    public Documentation(
        @IRField String doc, IdentifiedLocation identifiedLocation, MetadataStorage passData) {
      super(doc, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    @Override
    public String showCode(int indent) {
      return "## " + doc();
    }
  }
}
