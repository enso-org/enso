package org.enso.compiler.core.ir.expression.errors;

import java.util.function.Function;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.Error;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import scala.Function1;
import scala.Option;

public interface Redefined extends Error {

  @Override
  Redefined mapExpressions(Function<Expression, Expression> fn);

  @Override
  Redefined setLocation(Option<IdentifiedLocation> location);

  // TODO: Can be deleted?
  @Override
  default Option<IdentifiedLocation> location() {
    return Error.super.location();
  }

  @Override
  Redefined duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(
      interfaces = {Redefined.class, IRKind.Primitive.class, Diagnostic.Kind.Interactive.class})
  final class SelfArg extends RedefinedSelfArgErrorGen {
    @GenerateFields
    public SelfArg(IdentifiedLocation identifiedLocation, MetadataStorage passData) {
      super(identifiedLocation, passData);
    }

    public static SelfArg createFromLocation(IdentifiedLocation location) {
      return builder().location(location).build();
    }

    @Override
    public String showCode(int indent) {
      return "(Redefined This_Arg)";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[0];
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Methods must have only one definition of the `self` argument, and "
          + "it must be the first.";
    }

    @Override
    public SelfArg mapExpressions(Function<Expression, Expression> fn) {
      return this;
    }
  }

  /** An error representing the redefinition of a conversion in a given module */
  @GenerateIR(
      interfaces = {
        Redefined.class,
        IRKind.Primitive.class,
        Definition.class,
        Diagnostic.Kind.Interactive.class
      })
  final class Conversion extends RedefinedConversionErrorGen {

    /**
     * @param targetType the name of the atom the conversion was being redefined on
     * @param sourceType the source type for the conversion
     */
    @GenerateFields
    public Conversion(
        @IRChild Option<Name> targetType,
        @IRChild Name sourceType,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(targetType, sourceType, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    @Override
    public String showCode(int indent) {
      var targetTpe = targetType().map(tpe -> tpe.showCode() + ".").getOrElse(() -> "");
      var src = sourceType().showCode();
      return "(Redefined (Conversion " + targetTpe + "from " + src + "))";
    }

    @Override
    public Object[] diagnosticKeys() {
      if (targetType().isDefined()) {
        return new Object[] {targetType().get(), sourceType().name()};
      } else {
        return new Object[] {sourceType().name()};
      }
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      var targetTpe = targetType().map(tpe -> tpe.showCode() + ".").getOrElse(() -> "");
      var src = sourceType().showCode();
      return "Ambiguous conversion: "
          + targetTpe
          + "from "
          + src
          + " is defined multiple times in this module.";
    }

    @Override
    public Conversion mapExpressions(Function<Expression, Expression> fn) {
      return this;
    }
  }

  @GenerateIR(
      interfaces = {
        Redefined.class,
        IRKind.Primitive.class,
        Definition.class,
        Diagnostic.Kind.Interactive.class
      })
  final class Method extends RedefinedMethodErrorGen {
    @GenerateFields
    public Method(
        @IRChild Option<Name> typeName,
        @IRChild Name methodName,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(typeName, methodName, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      var typeName = typeName().map(tpe -> tpe.showCode() + ".").getOrElse(() -> "");
      return "Method overloads are not supported: "
          + typeName
          + methodName().name()
          + " is defined multiple times in this module.";
    }

    @Override
    public String showCode(int indent) {
      var typeName = typeName().map(tpe -> tpe.showCode() + ".").getOrElse(() -> "");
      return "(Redefined (Method " + typeName + methodName() + "))";
    }

    @Override
    public Object[] diagnosticKeys() {
      if (typeName().isDefined()) {
        return new Object[] {typeName().get(), methodName().name()};
      } else {
        return new Object[] {methodName().name()};
      }
    }

    @Override
    public Method mapExpressions(Function<Expression, Expression> fn) {
      return this;
    }
  }

  /**
   * An error representing the redefinition of a method in a given module, when the module defines a
   * method with the same name as an atom. This is also known as a name clash.
   */
  @GenerateIR(
      interfaces = {
        Redefined.class,
        IRKind.Primitive.class,
        Definition.class,
        Diagnostic.Kind.Interactive.class
      })
  final class MethodClashWithAtom extends RedefinedMethodClashWithAtomErrorGen {
    /**
     * @param atomName the name of the atom that clashes with the method
     * @param methodName the method name being redefined in the module
     */
    @GenerateFields
    public MethodClashWithAtom(
        @IRChild Name atomName,
        @IRChild Name methodName,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(atomName, methodName, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Method definitions with the same name as atoms are not supported. "
          + "Method "
          + methodName().name()
          + " clashes with the atom "
          + atomName().name()
          + " in this module.";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {methodName().name(), atomName().name()};
    }

    @Override
    public String showCode(int indent) {
      return "(Redefined (MethodClash " + atomName().name() + " " + methodName().name() + "))";
    }

    @Override
    public MethodClashWithAtom mapExpressions(Function<Expression, Expression> fn) {
      return this;
    }
  }

  /** An error representing the redefinition of an atom in a given module. */
  @GenerateIR(
      interfaces = {
        Redefined.class,
        IRKind.Primitive.class,
        Definition.class,
        Diagnostic.Kind.Interactive.class
      })
  final class Type extends RedefinedTypeErrorGen {
    @GenerateFields
    public Type(
        @IRChild Name typeName, IdentifiedLocation identifiedLocation, MetadataStorage passData) {
      super(typeName, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Redefining atoms is not supported: "
          + typeName().name()
          + " is "
          + "defined multiple times in this module.";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {typeName().name()};
    }

    @Override
    public String showCode(int indent) {
      return "(Redefined (Type " + typeName().name() + "))";
    }

    @Override
    public org.enso.compiler.core.ir.expression.errors.Redefined.Type mapExpressions(
        Function<Expression, Expression> fn) {
      return this;
    }
  }

  @GenerateIR(
      interfaces = {
        Redefined.class,
        IRKind.Primitive.class,
        Definition.class,
        Diagnostic.Kind.Interactive.class
      })
  final class Arg extends RedefinedArgErrorGen {
    @GenerateFields
    public Arg(
        @IRChild Name name, IdentifiedLocation identifiedLocation, MetadataStorage passData) {
      super(name, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Redefining arguments is not supported: "
          + name().name()
          + " is "
          + "defined multiple times.";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {name().name()};
    }

    @Override
    public String showCode(int indent) {
      return "(Redefined (Argument " + name().name() + "))";
    }

    @Override
    public Arg mapExpressions(Function<Expression, Expression> fn) {
      return this;
    }
  }

  /** An error representing the redefinition of an atom in a given module. */
  @GenerateIR(
      interfaces = {Redefined.class, IRKind.Primitive.class, Diagnostic.Kind.Interactive.class})
  final class Binding extends RedefinedBindingErrorGen {
    @GenerateFields
    public Binding(@IRChild Expression.Binding invalidBinding, MetadataStorage passData) {
      super(invalidBinding, passData);
    }

    public static Binding create(Expression.Binding invalidBinding) {
      return builder().invalidBinding(invalidBinding).build();
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Variable " + invalidBinding().name().name() + " is being redefined.";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {invalidBinding().name().name()};
    }

    @Override
    public String showCode(int indent) {
      return "(Redefined (Binding " + invalidBinding() + "))";
    }

    @Override
    public Binding mapExpressions(Function<Expression, Expression> fn) {
      return this;
    }

    @Override
    public Binding setLocation(Option<IdentifiedLocation> location) {
      return this;
    }

    @Override
    public IdentifiedLocation identifiedLocation() {
      return invalidBinding().identifiedLocation();
    }

    @Override
    public Option<IdentifiedLocation> location() {
      return invalidBinding().location();
    }
  }
}
