package org.enso.compiler.core.ir;

import java.util.function.Function;
import org.enso.compiler.core.ConstantsNames;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;
import scala.collection.immutable.List;

public interface JName extends Expression, IRKind.Primitive {
  String name();

  /**
   * Checks whether a name is a call-site method name.
   *
   * @return `true` if the name was created through a method call
   */
  default boolean isMethod() {
    return false;
  }

  @Override
  JName mapExpressions(Function<Expression, Expression> fn);

  @Override
  JName setLocation(Option<IdentifiedLocation> location);

  @Override
  JName duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(interfaces = {JName.class, IRKind.Sugar.class})
  final class MethodReference extends NameMethodReferenceGen {
    @GenerateFields
    public MethodReference(
        @IRChild Option<JName> typePointer,
        @IRChild JName methodName,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(typePointer, methodName, identifiedLocation, passData, diagnostics);
    }

    @Override
    public String showCode(int indent) {
      var tPointer = typePointer().map(tp -> tp.showCode(indent) + ".").getOrElse(() -> "");
      return tPointer + methodName().showCode(indent);
    }

    @Override
    public String name() {
      return showCode();
    }

    public boolean isSameReferenceAs(MethodReference that) {
      if (typePointer().isDefined() && that.typePointer().isDefined()) {
        var thisTP = typePointer().get();
        var thatTP = that.typePointer().get();
        return thisTP.name().equals(thatTP.name());
      }
      return false;
    }
  }

  /** A representation of a qualified (multi-part) name. */
  @GenerateIR(interfaces = {JName.class, IRKind.Primitive.class})
  final class Qualified extends NameQualifiedGen {

    /**
     * @param parts the segments of the name
     * @param identifiedLocation the source location that the node corresponds to
     * @param passData the pass metadata associated with this node
     */
    @GenerateFields
    public Qualified(
        @IRChild List<JName> parts,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(parts, identifiedLocation, passData, diagnostics);
    }

    @Override
    public String name() {
      return parts().map(JName::name).mkString(".");
    }

    @Override
    public String showCode(int indent) {
      return name();
    }
  }

  /** Represents occurrences of blank (`_`) expressions. */
  @GenerateIR(interfaces = {JName.class, IRKind.Sugar.class})
  final class Blank extends NameBlankGen {
    @GenerateFields
    public Blank(
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(identifiedLocation, passData, diagnostics);
    }

    @Override
    public String name() {
      return "_";
    }

    @Override
    public String showCode(int indent) {
      return "_";
    }
  }

  @GenerateIR(interfaces = {JName.class, IRKind.Sugar.class})
  final class Special extends NameSpecialGen {
    enum Ident {
      NewRef,
      ReadRef,
      WriteRef,
      RunThread,
      JoinThread
    }

    @GenerateFields
    public Special(
        @IRField Ident specialName,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(specialName, identifiedLocation, passData);
    }

    @Override
    public String name() {
      return "<special::" + specialName() + ">";
    }

    @Override
    public String showCode(int indent) {
      return name();
    }
  }

  @GenerateIR(interfaces = {JName.class})
  final class Literal extends NameLiteralGen {
    @GenerateFields
    public Literal(
        @IRField String name,
        @IRField boolean isMethod,
        @IRField JName origName,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnosticStorage) {
      super(name, isMethod, origName, identifiedLocation, passData, diagnosticStorage);
    }

    @Override
    public String showCode(int indent) {
      return name();
    }
  }

  interface Annotation extends JName, Definition {
    @Override
    Annotation mapExpressions(Function<Expression, Expression> fn);

    @Override
    Annotation setLocation(Option<IdentifiedLocation> location);

    @Override
    Annotation duplicate(
        boolean keepLocations,
        boolean keepMetadata,
        boolean keepDiagnostics,
        boolean keepIdentifiers);

    /**
     * These {@code duplicate$default$N} default methods need to be here, otherwise {@code javac}
     * would fail to compile with: "interface Annotation inherits unrelated defaults for {@code
     * duplicate$default$N} from types Expression and Definition". The {@link Annotation} interface
     * inherits from both {@link Expression} and {@link Definition} traits, and they both have their
     * own {@code duplicate$default$N} methods.
     */
    default boolean duplicate$default$1() {
      return true;
    }

    default boolean duplicate$default$2() {
      return true;
    }

    default boolean duplicate$default$3() {
      return true;
    }

    default boolean duplicate$default$4() {
      return false;
    }
  }

  @GenerateIR(interfaces = {Annotation.class, IRKind.Primitive.class})
  final class BuiltinAnnotation extends NameBuiltinAnnotationGen {
    @GenerateFields
    public BuiltinAnnotation(
        @IRField String name, IdentifiedLocation identifiedLocation, MetadataStorage passData) {
      super(name, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      return "@" + name();
    }
  }

  /** Common annotations of form {@code @name expression} */
  @GenerateIR(interfaces = {Annotation.class})
  final class GenericAnnotation extends NameGenericAnnotationGen {

    /**
     * @param name the annotation text of the name
     * @param expression the annotation expression
     */
    @GenerateFields
    public GenericAnnotation(
        @IRField String name,
        @IRChild Expression expression,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(name, expression, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      return "@" + name() + " " + expression().showCode(indent);
    }
  }

  @GenerateIR(interfaces = {JName.class})
  final class Self extends NameSelfGen {
    @GenerateFields
    public Self(
        @IRField boolean synthetic,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(synthetic, identifiedLocation, passData);
    }

    @Override
    public String name() {
      return ConstantsNames.SELF_ARGUMENT;
    }

    @Override
    public String showCode(int indent) {
      return name();
    }
  }

  /** A representation of the name `Self`, used to refer to the current type. */
  @GenerateIR(interfaces = {JName.class})
  final class SelfType extends NameSelfTypeGen {
    @GenerateFields
    public SelfType(IdentifiedLocation identifiedLocation, MetadataStorage passData) {
      super(identifiedLocation, passData);
    }

    @Override
    public String name() {
      return ConstantsNames.SELF_TYPE_ARGUMENT;
    }

    @Override
    public String showCode(int indent) {
      return name();
    }
  }
}
