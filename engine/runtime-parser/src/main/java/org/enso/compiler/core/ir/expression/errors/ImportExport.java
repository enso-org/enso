package org.enso.compiler.core.ir.expression.errors;

import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.expression.Error;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Function1;
import scala.Option;

/** An erroneous import or export statement. */
@GenerateIR(
    interfaces = {
      Error.class,
      Diagnostic.Kind.Interactive.class,
      IRKind.Primitive.class,
      Import.class,
      Export.class
    })
public final class ImportExport extends ErrorImportExportGen {

  /**
   * @param ir The original import statement
   * @param reason The reason it's erroneous.
   */
  @GenerateFields
  public ImportExport(
      @IRChild IR ir,
      @IRField Reason reason,
      MetadataStorage passData,
      DiagnosticStorage diagnostics) {
    super(ir, reason, passData, diagnostics);
  }

  public static ImportExport create(IR ir, Reason reason) {
    return builder().ir(ir).reason(reason).build();
  }

  @Override
  public ImportExport mapExpressions(Function<Expression, Expression> fn) {
    return this;
  }

  @Override
  public ImportExport setLocation(Option<IdentifiedLocation> location) {
    return this;
  }

  @Override
  public IdentifiedLocation identifiedLocation() {
    return ir().identifiedLocation();
  }

  @Override
  public Option<IdentifiedLocation> location() {
    return ir().location();
  }

  @Override
  public String message(Function1<IdentifiedLocation, String> source) {
    return reason().message(source::apply);
  }

  @Override
  public Object[] diagnosticKeys() {
    return new Object[] {reason()};
  }

  @Override
  public String showCode(int indent) {
    return "Import_Export_Error";
  }

  public sealed interface Reason {
    String message(Function<IdentifiedLocation, String> source);
  }

  /**
   * Used when the `project` keyword is used in an impossible position.
   *
   * @param statementType the type of statement being affected, see the implementation for its
   *     grammatical use.
   */
  public record ProjectKeywordUsedButNotInProject(String statementType) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "The `project` keyword was used in an "
          + statementType
          + " statement,"
          + " but the module does not belong to a project.";
    }
  }

  /**
   * Used when an import statement triggers loading of a package that could not be loaded.
   *
   * @param name the module name.
   */
  public record PackageCouldNotBeLoaded(String name, String reason) implements Reason {

    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "Package containing the module " + name + " could not be loaded: " + reason;
    }
  }

  /**
   * Used when an import statement refers to a module that does not exist.
   *
   * @param name the module name.
   */
  public record ModuleDoesNotExist(String name) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "The module " + name + " does not exist.";
    }
  }

  public record TypeDoesNotExist(String typeName, String moduleName) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "The type " + typeName + " does not exist in the module " + moduleName;
    }
  }

  public record SymbolDoesNotExist(String symbolName, String moduleOrTypeName) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "The symbol "
          + symbolName
          + "(module, type, method, or constructor) does not exist in "
          + moduleOrTypeName
          + ".";
    }
  }

  public record IllegalImportFromMethod(String moduleName, String methodName) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "Cannot import symbols from method '" + moduleName + "." + methodName + "'";
    }
  }

  public record NoSuchConstructor(String typeName, String constructorName) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "No such constructor " + constructorName + " in type " + typeName;
    }
  }

  public record NoSuchModuleMethod(String moduleName, String methodName) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "No such method " + methodName + " on module " + moduleName;
    }
  }

  public record NoSuchStaticMethod(String moduleName, String typeName, String methodName)
      implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "No such static method "
          + methodName
          + " on type "
          + typeName
          + " in module "
          + moduleName;
    }
  }

  public record NoSuchConversionMethod(
      String moduleName, String targetTypeName, String sourceTypeName) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "No conversion method from "
          + sourceTypeName
          + " to "
          + targetTypeName
          + " in module "
          + moduleName;
    }
  }

  public record ExportSymbolsFromPrivateModule(String moduleName) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "Cannot export any symbol from module '" + moduleName + "': The module is private";
    }
  }

  public record ExportPrivateModule(String moduleName) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "Cannot export private module '" + moduleName + "'";
    }
  }

  public record ImportPrivateModule(String moduleName) implements Reason {
    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      return "Cannot import private module '" + moduleName + "'";
    }
  }

  /**
   * Represents an ambiguous import resolution error, where the same symbol is imported more than
   * once refereing to different objects. The objects are represented by their physical path in the
   * project.
   *
   * @param originalImport the original import statement.
   * @param originalSymbolPath the original symbol path.
   * @param symbolName the symbol name that is ambiguous.
   * @param symbolPath the symbol path that is different than {@code originalSymbolPath}.
   */
  public record AmbiguousImport(
      Import originalImport, String originalSymbolPath, String symbolName, String symbolPath)
      implements Reason {

    @Override
    public String message(Function<IdentifiedLocation, String> source) {
      var origLocation = originalImport.identifiedLocation();
      String originalImportRepr;
      if (origLocation != null) {
        originalImportRepr = source.apply(origLocation);
      } else {
        originalImportRepr = originalImport.showCode();
      }
      return "Symbol '"
          + symbolName
          + "' resolved ambiguously to '"
          + symbolPath
          + "' in the import Statement. The symbol was first resolved to '"
          + originalSymbolPath
          + "' in the import statement '"
          + originalImportRepr
          + "'.";
    }
  }
}
