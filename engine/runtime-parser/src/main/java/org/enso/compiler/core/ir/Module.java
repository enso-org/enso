package org.enso.compiler.core.ir;

import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.collection.immutable.List;

/**
 * A representation of a top-level Enso module.
 *
 * <p>Modules may only contain imports and top-level bindings, with no top-level executable code.
 */
@GenerateIR(interfaces = {IRKind.Primitive.class})
public final class Module extends ModuleGen {
  /**
   * Most rich constructor.
   *
   * @param imports the import statements that bring other modules into scope
   * @param exports the export statements for this module
   * @param bindings the top-level bindings for this module
   * @param isPrivate whether or not this module is private (project-private)
   * @param identifiedLocation the source location that the node corresponds to
   * @param passData the pass metadata associated with this node
   * @param diagnostics the compiler diagnostics
   */
  @GenerateFields
  public Module(
      @IRChild List<Import> imports,
      @IRChild List<Export> exports,
      @IRChild List<Definition> bindings,
      @IRField boolean isPrivate,
      IdentifiedLocation identifiedLocation,
      MetadataStorage passData,
      DiagnosticStorage diagnostics) {
    super(imports, exports, bindings, isPrivate, identifiedLocation, passData, diagnostics);
  }

  public Module(
      @IRChild List<Import> imports,
      @IRChild List<Export> exports,
      @IRChild List<Definition> bindings,
      @IRField boolean isPrivate,
      IdentifiedLocation identifiedLocation,
      MetadataStorage passData) {
    super(imports, exports, bindings, isPrivate, identifiedLocation, passData, null);
  }

  /**
   * Creates a copy of `this`.
   *
   * @param imports the import statements that bring other modules into scope
   * @param exports the export statements for this module
   * @return new instances of this module if imports or exports differ
   */
  public Module copyWithImportsAndExports(List<Import> imports, List<Export> exports) {
    return builder(this).imports(imports).exports(exports).build();
  }

  /**
   * Creates a copy of `this`.
   *
   * @param bindings the top-level bindings for this module
   * @return new instances of this module if bindings are different
   */
  public Module copyWithBindings(List<Definition> bindings) {
    return builder(this).bindings(bindings).build();
  }

  @Override
  public String showCode(int indent) {
    var importsString = imports().map(i -> i.showCode(indent)).mkString("\n");
    var exportsString = exports().map(e -> e.showCode(indent)).mkString("\n");
    var defsString = bindings().map(b -> b.showCode(indent)).mkString("\n\n");

    return importsString + "\n\n" + exportsString + "\n\n" + defsString;
  }
}
