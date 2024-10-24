package org.enso.compiler.core.ir.module.scope;

import java.util.List;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.JDefinitionArgument;
import org.enso.compiler.core.ir.JName;
import org.enso.compiler.core.ir.module.JScope;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRNode;

public interface JDefinition extends JScope {
  @IRNode
  interface JType extends JDefinition {
    @IRChild
    JName name();

    @IRChild
    List<JDefinitionArgument> params();

    @IRChild
    List<JData> members();
  }

  /** The definition of an atom constructor and its associated arguments. */
  @IRNode
  interface JData extends JDefinition {
    /** The name of the atom */
    @IRChild
    JName name();

    /** The arguments of the atom constructor. */
    @IRChild
    List<JDefinitionArgument> arguments();

    @IRChild
    List<JName.JGenericAnnotation> annotations();

    /** If the constructor is project-private. */
    boolean isPrivate();
  }

  /**
   * The definition of a complex type definition that may contain multiple atom and method
   * definitions.
   */
  @IRNode
  interface JSugaredType extends JDefinition {

    /** The name of the complex type. */
    @IRChild
    JName name();

    @IRChild
    List<JDefinitionArgument> arguments();

    @IRChild
    List<IR> body();
  }
}
