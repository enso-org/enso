package org.enso.compiler.pass.analyse;

import java.io.IOException;
import java.util.UUID;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ExternalID;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.Identifier;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.pass.IRPass;
import scala.Option;

/**
 * A representation of dependency information for dataflow analysis.
 *
 * @param dependents information on the dependents of program components, mapping from a component
 *     to the components that depend on it
 * @param dependencies information on the dependencies of program components, mapping from a
 *     component to the components that it depends on
 */
public record DependencyInfo(DependencyMapping dependents, DependencyMapping dependencies)
    implements IRPass.IRMetadata {
  public DependencyInfo() {
    this(DependencyMapping.newBuilder().build(), DependencyMapping.newBuilder().build());
  }

  public String metadataName() {
    return "DataflowAnalysis.DependencyInfo";
  }

  /**
   * Finds dependency info in the provided IR element.
   *
   * @return non-{@code null} dependency info
   * @throws exception or error when the info isn't attached to the element
   */
  public static DependencyInfo find(IR ir) {
    var pass = DataflowAnalysis$.MODULE$;
    var opt = ir.passData().get(pass);
    if (opt.isEmpty()) {
      var newIr =
          switch (ir) {
            case org.enso.compiler.core.ir.Module m -> pass.runModule(m, null);
            case Expression exp -> pass.runExpression(exp, null);
            default -> throw new AssertionError();
          };
      opt = ir.passData().get(pass);
      assert opt.nonEmpty();
    }
    var raw = opt.get();
    assert raw != null;
    return (DependencyInfo) raw;
  }

  /**
   * Combines two dependency information containers.
   *
   * @param that the other container to combine with `this`
   * @return the result of combining `this` and `that`
   */
  DependencyInfo combine(DependencyInfo that) {
    var dependents = DependencyMapping.newBuilder(this.dependents());
    var dependencies = DependencyMapping.newBuilder(this.dependencies());
    dependents.combine(that.dependents());
    dependencies.combine(that.dependencies());
    return new DependencyInfo(dependents.build(), dependencies.build());
  }

  @Override
  public Option<IRPass.IRMetadata> duplicate() {
    return Option.empty();
  }

  /**
   * @inheritdoc
   */
  @Override
  public DependencyInfo prepareForSerialization(CompilerContext compiler) {
    throw raiseNoSerde(RuntimeException.class);
  }

  /**
   * @inheritdoc
   */
  @Override
  public Option<IRPass.IRMetadata> restoreFromSerialization(CompilerContext compiler) {
    throw raiseNoSerde(RuntimeException.class);
  }

  @SuppressWarnings("unchecked")
  private static <T extends Exception> T raiseNoSerde(Class<T> ignore) throws T {
    throw (T) new IOException("DependencyInfo shall never be subject to serde");
  }

  /** The type of identification for a program component. */
  public sealed interface Type {
    Option<@ExternalID UUID> externalId();

    /**
     * Program components identified by their unique identifier.
     *
     * @param id the unique identifier of the program component
     * @param externalId the external identifier corresponding to the program component
     */
    record Static(@Identifier UUID id, Option<@ExternalID UUID> externalId) implements Type {}

    /**
     * Program components identified by their symbol.
     *
     * @param name the name of the symbol
     * @param externalId the external identifier corresponding to the program component
     */
    record Dynamic(String name, Option<@ExternalID UUID> externalId) implements Type {}

    // === Utility Functions ================================================

    /**
     * Creates a static dependency on an IR node.
     *
     * @param ir the IR node to create a dependency on
     * @return a static dependency on `ir`
     */
    static Type.Static asStatic(IR ir) {
      return new Type.Static(ir.getId(), ir.getExternalId());
    }

    /**
     * Creates a dynamic dependency on an IR node.
     *
     * @param ir the IR node to create a dependency on
     * @return a dynamic dependency on `ir`
     */
    static Type.Dynamic asDynamic(Name ir) {
      return new Type.Dynamic(ir.name(), ir.getExternalId());
    }
  }
}
