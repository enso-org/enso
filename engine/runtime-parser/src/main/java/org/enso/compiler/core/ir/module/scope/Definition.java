package org.enso.compiler.core.ir.module.scope;

import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.module.Scope;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;
import scala.collection.immutable.List;

/** A representation of top-level definitions. */
public interface Definition extends Scope {

  @Override
  Definition mapExpressions(Function<Expression, Expression> fn);

  @Override
  Definition setLocation(Option<IdentifiedLocation> location);

  @Override
  Definition duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  /**
   * The definition of a union type and its members.
   *
   * <p>NB: this should probably be removed once we propagate the union types logic through the
   * runtime and implement statics – the whole notion of desugaring complex type definitions becomes
   * obsolete then.
   */
  @GenerateIR(interfaces = {Definition.class, IRKind.Primitive.class})
  final class Type extends DefinitionTypeGen {

    /**
     * @param name the name of the union
     * @param members the members of this union
     * @param identifiedLocation the source location that the node corresponds to
     * @param passData the pass metadata associated with this node
     */
    @GenerateFields
    public Type(
        @IRChild Name name,
        @IRChild List<DefinitionArgument> params,
        @IRChild List<Data> members,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(name, params, members, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder().members(nil());
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    public Type copyWithMembers(List<Data> members) {
      return copyBuilder().members(members).build();
    }

    @Override
    public String showCode(int indent) {
      var fields = members().map(m -> m.showCode(indent)).mkString(" | ");
      return "type " + name().showCode(indent) + " = " + fields;
    }
  }

  /** The definition of an atom constructor and its associated arguments. */
  @GenerateIR(interfaces = {Definition.class, IRKind.Primitive.class})
  final class Data extends DefinitionDataGen {
    /**
     * @param name the name of the atom
     * @param arguments the arguments to the atom constructor
     * @param annotations the list of annotations
     * @param identifiedLocation the source location that the node corresponds to
     * @param isPrivate if the constructor is private (project-private).
     * @param passData the pass metadata associated with this node
     */
    @GenerateFields
    public Data(
        @IRChild Name name,
        @IRChild List<DefinitionArgument> arguments,
        @IRChild List<Name.GenericAnnotation> annotations,
        @IRField boolean isPrivate,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(name, arguments, annotations, isPrivate, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder().annotations(nil()).isPrivate(false);
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    public Data copyWithAnnotations(List<Name.GenericAnnotation> annotations) {
      return copyBuilder().annotations(annotations).build();
    }

    public Data copyWithArguments(List<DefinitionArgument> arguments) {
      return copyBuilder().arguments(arguments).build();
    }

    @Override
    public String showCode(int indent) {
      var fields = arguments().map(a -> a.showCode(indent)).mkString(" ");
      return "type " + name().showCode(indent) + " " + fields;
    }
  }

  /**
   * The definition of a complex type definition that may contain multiple atom and method
   * definitions.
   */
  @GenerateIR(interfaces = {Definition.class, IRKind.Sugar.class})
  final class SugaredType extends DefinitionSugaredTypeGen {
    /**
     * @param name the name of the complex type
     * @param arguments the (type) arguments to the complex type
     * @param body the body of the complex type
     * @param identifiedLocation the source location that the node corresponds to
     * @param passData the pass metadata associated with this node
     */
    @GenerateFields
    public SugaredType(
        @IRChild Name name,
        @IRChild List<DefinitionArgument> arguments,
        @IRChild List<IR> body,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(name, arguments, body, identifiedLocation, passData);
    }

    public SugaredType copyWithBody(List<IR> body) {
      return builder(this).body(body).build();
    }

    @Override
    public String showCode(int indent) {
      var headerArgs = arguments().map(a -> a.showCode(indent)).mkString(" ");
      var header = "type " + name().name() + " " + headerArgs;
      var newIndent = indent + indentLevel;
      var bodyStr =
          body().map(b -> IR.mkIndent(newIndent) + b.showCode(newIndent)).mkString("\n\n");
      return header + "\n" + bodyStr;
    }
  }

  @SuppressWarnings("unchecked")
  private static <T> scala.collection.immutable.List<T> nil() {
    return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
  }
}
