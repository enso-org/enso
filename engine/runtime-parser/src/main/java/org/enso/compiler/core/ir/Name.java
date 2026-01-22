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
import scala.jdk.javaapi.CollectionConverters;

public interface Name extends Expression, IRKind.Primitive {
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
  Name mapExpressions(Function<Expression, Expression> fn);

  @Override
  Name setLocation(Option<IdentifiedLocation> location);

  @Override
  Name duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(interfaces = {Name.class, IRKind.Sugar.class})
  final class MethodReference extends NameMethodReferenceGen {
    @GenerateFields
    public MethodReference(
        @IRChild Option<Name> typePointer,
        @IRChild Name methodName,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(typePointer, methodName, identifiedLocation, passData, diagnostics);
    }

    public static Builder builder() {
      return new Builder().typePointer(Option.empty());
    }

    public MethodReference copyWithTypePointer(Option<Name> typePointer) {
      return new Builder(this).typePointer(typePointer).build();
    }

    @Override
    public MethodReference mapExpressions(Function<Expression, Expression> fn) {
      Option<Name> newTypePointer;
      if (typePointer().isDefined()) {
        newTypePointer = Option.apply(typePointer().get().mapExpressions(fn));
      } else {
        newTypePointer = Option.empty();
      }
      var newMethodName = methodName().mapExpressions(fn);
      return new Builder(this).typePointer(newTypePointer).methodName(newMethodName).build();
    }

    /**
     * Generates a location for the reference from the segments.
     *
     * @param segments the reference segments
     * @return a location for the method reference
     */
    public static IdentifiedLocation genLocation(List<Name> segments) {
      int start = -1;
      int end = -1;
      for (var segment : CollectionConverters.asJava(segments)) {
        var segmentLoc = segment.identifiedLocation();
        if (start == -1) {
          if (segmentLoc != null) {
            start = segmentLoc.start();
            end = segmentLoc.end();
          }
        } else {
          // extend the accumulated location's end using this segment's end if present
          if (segmentLoc != null) {
            end = segmentLoc.end();
          }
        }
      }
      if (start != -1 && end != -1) {
        return new IdentifiedLocation(new Location(start, end));
      } else {
        return null;
      }
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

    /**
     * Checks whether `this` and `that` reference the same method.
     *
     * @param that the other method reference to check against
     * @return `true`, if `this` and `that` represent the same method, otherwise `false`
     */
    public boolean isSameReferenceAs(MethodReference that) {
      boolean sameTypePointer;
      if (typePointer().isDefined() && that.typePointer().isDefined()) {
        var thisTP = typePointer().get();
        var thatTP = that.typePointer().get();
        sameTypePointer = thisTP.name().equals(thatTP.name());
      } else {
        sameTypePointer = typePointer().isEmpty() && that.typePointer().isEmpty();
      }
      var sameMethodName = methodName().name().equals(that.methodName().name());
      return sameTypePointer && sameMethodName;
    }
  }

  /** A representation of a qualified (multi-part) name. */
  @GenerateIR(interfaces = {Name.class, IRKind.Primitive.class})
  final class Qualified extends NameQualifiedGen {

    /**
     * @param parts the segments of the name
     * @param identifiedLocation the source location that the node corresponds to
     * @param passData the pass metadata associated with this node
     */
    @GenerateFields
    public Qualified(
        @IRChild List<Name> parts,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(parts, identifiedLocation, passData, diagnostics);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @Override
    public Qualified mapExpressions(Function<Expression, Expression> fn) {
      return this;
    }

    @Override
    public String name() {
      return parts().map(Name::name).mkString(".");
    }

    @Override
    public String showCode(int indent) {
      return name();
    }
  }

  /** Represents occurrences of blank (`_`) expressions. */
  @GenerateIR(interfaces = {Name.class, IRKind.Sugar.class})
  final class Blank extends NameBlankGen {
    @GenerateFields
    public Blank(
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(identifiedLocation, passData, diagnostics);
    }

    public static Builder builder() {
      return new Builder();
    }

    public static Blank create() {
      return new Builder().build();
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

  @GenerateIR(interfaces = {Name.class, IRKind.Sugar.class})
  final class Special extends NameSpecialGen {
    public enum Ident {
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

    public static Special create(Ident specialName) {
      return new Builder().specialName(specialName).build();
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

  @GenerateIR(interfaces = {Name.class})
  final class Literal extends NameLiteralGen {
    @GenerateFields
    public Literal(
        @IRField String name,
        @IRField boolean isMethod,
        @IRField(required = false) Name origName,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(name.intern(), isMethod, origName, identifiedLocation, passData, diagnostics);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    public Option<Name> originalName() {
      return Option.apply(origName());
    }

    @Override
    public String showCode(int indent) {
      return name();
    }
  }

  interface Annotation extends Name, Definition {
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

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @Override
    public String showCode(int indent) {
      return "@" + name() + " " + expression().showCode(indent);
    }
  }

  @GenerateIR(interfaces = {Name.class})
  final class Self extends NameSelfGen {
    @GenerateFields
    public Self(
        @IRField boolean synthetic,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(synthetic, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder().synthetic(false);
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
  @GenerateIR(interfaces = {Name.class})
  final class SelfType extends NameSelfTypeGen {
    @GenerateFields
    public SelfType(IdentifiedLocation identifiedLocation, MetadataStorage passData) {
      super(identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
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
