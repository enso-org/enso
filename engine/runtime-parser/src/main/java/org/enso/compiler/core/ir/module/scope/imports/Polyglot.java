package org.enso.compiler.core.ir.module.scope.imports;

import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;

/** An import of a polyglot class. */
@GenerateIR(interfaces = {Import.class, IRKind.Primitive.class})
public final class Polyglot extends PolyglotImportGen {
  /**
   * @param entity language-specific information on the imported entity
   * @param rename the name this object should be visible under in the importing scope
   */
  @GenerateFields
  public Polyglot(
      @IRField Polyglot.Entity entity,
      @IRField Option<String> rename,
      IdentifiedLocation identifiedLocation,
      MetadataStorage passData) {
    super(entity, rename, identifiedLocation, passData);
  }

  /**
   * Returns the name this object is visible as from Enso code.
   *
   * @return the visible name of this object
   */
  public String getVisibleName() {
    if (rename().isDefined()) {
      return rename().get();
    } else {
      return entity().getVisibleName();
    }
  }

  @Override
  public String showCode(int indent) {
    var renamePart = rename().isDefined() ? " as " + rename().get() : "";
    return "polyglot "
        + entity().langName()
        + " import "
        + entity().showCode(indent)
        + " "
        + renamePart;
  }

  /** Represents language-specific polyglot import data. */
  public sealed interface Entity {
    String langName();

    /**
     * Returns the name this object is visible as from Enso code.
     *
     * @return the visible name of this object
     */
    String getVisibleName();

    String showCode(int indent);
  }

  /**
   * Represents an import of a Java class.
   *
   * @param packageName the name of the package containing the imported class
   * @param className the class name
   */
  public record Java(String packageName, String className) implements Entity {
    @Override
    public String langName() {
      return "java";
    }

    @Override
    public String getVisibleName() {
      return className;
    }

    /**
     * Returns the fully qualified Java name of this object.
     *
     * @return the Java-side name of the imported entity
     */
    public String getJavaName() {
      return packageName + "." + className;
    }

    @Override
    public String showCode(int indent) {
      return packageName + "." + className;
    }
  }
}
